from __future__ import annotations
from typing import Set, List, Tuple, Generator, TypeVar
from queue import SimpleQueue
from graphviz import Digraph
from PIL import Image
import json
import copy
import io

T = TypeVar('T') 

def sget(s: Set[T]) -> T:
    result = s.pop()
    s.add(result)
    return result


class Counter:
    def __init__(self, initial_value):
        self._counter = initial_value
    
    def inc(self):
        self._counter += 1

    def get(self):
        return self._counter


class StateMachine:
    def __init__(self, 
                states_count: int, 
                initial_state: int, 
                final_states: Set[int], 
                state_transition: Set[Tuple[int, str, int]],
                states_labels: List[str]=None,
                alphabet: Set[str]=None):
        if alphabet is None:
            self._alphabet = self._derive_alphabet(state_transition)
        else:
            self._alphabet = sorted(alphabet)
        self._alphabet_size = len(self._alphabet)
        self._symbol_to_index = dict( zip(self._alphabet, range(self._alphabet_size)) )
        self._symbol_to_index[""] = self._alphabet_size

        if states_labels is not None:
            self._states_labels = states_labels
        else:
            self._states_labels = self._build_default_states_labels(states_count)

        self._states_count = states_count
        self._initial_state = initial_state
        self._final_states = final_states
        self._stock_states = set()
        self._state_transition_matrix = self._build_state_transition_matrix(state_transition)
        self._is_deterministic = self._check_if_deterministic()

    def _derive_alphabet(self, state_transition: Set[Tuple[int, str, int]]) -> List[str]:
        alphabet = set()
        for (q1, c, q2) in state_transition:
            if c != '':
                alphabet.add(c)
        return sorted(alphabet)

    def _alphabet_iter(self, with_epsilon: bool=True):
        yield from enumerate(self._alphabet)
        if with_epsilon:
            yield (self._alphabet_size, "")

    def _build_default_states_labels(self, count: int) -> List[str]:
        return [f"q_{q}" for q in range(count)]
            
    def _check_if_deterministic(self) -> bool:
        for q1 in range(self._states_count):
            for c_index, c in self._alphabet_iter():
                if c == "" and self._state_transition_matrix[q1][c_index] != {q1} or\
                    len(self._state_transition_matrix[q1][c_index]) > 1:
                    return False
        return True                    

    def _build_state_transition_matrix(self, state_transition: Set[Tuple[int, str, int]]) -> List[List[Set[int]]]:
        matrix = [[set() for _ in range(self._alphabet_size + 1)] for _ in range(self._states_count)]
        
        for (q1, c, q2) in state_transition:
            c_index = self._symbol_to_index[c]
            matrix[q1][c_index].add(q2)

        # find stock states
        stock = None
        for q1 in range(self._states_count):
            if q1 in self._final_states:
                continue

            is_stock = True
            for c_index in range(self._alphabet_size + 1):
                if len(matrix[q1][c_index] - {q1}) > 0:
                    is_stock = False
                    break
            if is_stock:
                self._stock_states.add(q1)
                stock = q1

        # add arrows to stock state and stock state themself if needed   
        for q in range(self._states_count):
            for c_index in range(self._alphabet_size + 1):
                if len(matrix[q][c_index]) == 0:
                    if c_index == self._symbol_to_index[""]:
                        # transition on epsilon symbol
                        matrix[q][c_index].add(q)
                        continue

                    if stock is None:
                        # no stock was found in previous block, so artificial needed
                        stock = self._states_count
                        self._stock_states.add(stock)
                        matrix.append([{stock} for _ in range(self._alphabet_size + 1)])  # add loops into stock state 
                        
                        # add new label
                        if len(self._states_labels) <= stock:
                            for i in range(stock + 1):
                                if f"q_{i}" not in self._states_labels:
                                    self._states_labels.append(f"q_{i}")
                                    break
                        self._states_count += 1

                    matrix[q][c_index].add(stock)       
        return matrix

    def _build_inverse_state_transition_matrix(self, state_transition_matrix=None) -> List[List[Set[int]]]:
        if state_transition_matrix is None:
            state_transition_matrix = self._state_transition_matrix

        inverse_matrix = [[set() for _ in self._alphabet_iter()] for _ in range(self._states_count)]
        for q1 in range(self._states_count):
            for c_index, _ in self._alphabet_iter():
                for q2 in state_transition_matrix[q1][c_index]:
                    inverse_matrix[q2][c_index].add(q1)
        return inverse_matrix

    def is_deterministic(self) -> bool:
        return self._is_deterministic

    def delete_epsilon_transitions(self) -> StateMachine:
        state_transition_matrix = copy.deepcopy(self._state_transition_matrix)
        final_states = copy.deepcopy(self._final_states)
        eps_index = self._symbol_to_index[""]

        inverse_transition = self._build_inverse_state_transition_matrix()

        # 1. Transitive closure
        # Transitive closure of subgraph where only epsilon edges left
        # BFS

        for q0 in range(self._states_count):
            queue = SimpleQueue()
            queue.put(q0)
            visited = {q0}

            while not queue.empty():
                q2 = queue.get()
                for q1 in inverse_transition[q2][eps_index]:
                    if q1 not in visited:
                        visited.add(q1)
                        queue.put(q1)
                        state_transition_matrix[q1][eps_index].add(q0)
        
        inverse_transition = self._build_inverse_state_transition_matrix(state_transition_matrix)

        # 2. Update final states

        for q2 in self._final_states:
            for q1 in inverse_transition[q2][eps_index]:
                final_states.add(q1)
        
        # 3. Add transitive edges
                    
        for q2 in range(self._states_count):
            for q1 in inverse_transition[q2][eps_index]:
                for (c_index, _) in self._alphabet_iter():                    
                    state_transition_matrix[q1][c_index].update(state_transition_matrix[q2][c_index])

        # 4. Delete epsilone transitions

        for q1 in range(self._states_count):
            state_transition_matrix[q1][eps_index] = {q1}
        
        # Form result - new state machine

        alphabet = copy.deepcopy(self._alphabet)
        states_count = self._states_count
        initial_state = self._initial_state
        states_labels = copy.deepcopy(self._states_labels)
        state_transition = {tuple(t) for t in 
                                self._state_transition_matrix_to_triples_list(transition_matrix=state_transition_matrix) }
        return StateMachine(states_count, initial_state, final_states, state_transition, states_labels=states_labels, alphabet=alphabet)

    def _determinize_without_epsilon_transitions(self) -> StateMachine:
        # implying that no nontrivial (not into itself) epsilon transition exists

        q0 = (self._initial_state, )
        new_states = {q0}
        new_transitions = []

        queue = SimpleQueue()
        queue.put(q0)

        while not queue.empty():
            q1 = queue.get()
            for c_index, c in self._alphabet_iter():
                q2 = set()
                for q1n in q1:
                    q2.update(self._state_transition_matrix[q1n][c_index])
                q2 -= self._stock_states

                if len(q2) == 0:
                    continue

                q2 = tuple(sorted(q2))

                new_transitions.append((q1, c, q2))
                if q2 not in new_states:
                    queue.put(q2)
                    new_states.add(q2)
        
        new_states = list(new_states)
        state_to_index = dict(zip(new_states, range(len(new_states))))

        # Form result - new state machine
        
        alphabet = copy.deepcopy(self._alphabet)
        states_count = len(new_states)
        initial_state = new_states.index((self._initial_state, ))
        final_states = {i for i, q1 in enumerate(new_states) if len(set(q1) & self._final_states) != 0}
        state_transition = {(state_to_index[q1], c, state_to_index[q2]) for (q1, c, q2) in new_transitions}

        states_labels = []
        for q1 in new_states:
            if len(q1) == 1:
                label = self._states_labels[q1[0]]
            else:
                label = "{" + ",".join(self._states_labels[q1n] for q1n in q1) + "}"
            states_labels.append(label)

        return StateMachine(states_count, initial_state, final_states, state_transition, states_labels=states_labels, alphabet=alphabet)         

    def determinize(self):
        return self.delete_epsilon_transitions()._determinize_without_epsilon_transitions()

    def _deterministic_only(self) -> None:
        if not self._is_deterministic:
            raise Exception("This method can be called on deterministic finite automata only")

    def is_word_accepted(self, word: str) -> bool:
        self._deterministic_only()

        q = self._initial_state
        for c in word:
            if c not in self._alphabet:
                return False
            q = sget(self._state_transition_matrix[q][self._symbol_to_index[c]])
        
        return q in self._final_states
            
    def list_words_lexicographical(self, n: int) -> Generator[str, None, None]:
        self._deterministic_only()
        
        if len(self._final_states) == 0:
            return

        queue = SimpleQueue()
        queue.put((self._initial_state, ""))
        if self._initial_state in self._final_states:
            yield ""
        
        while not (n <= 0 or queue.empty()):
            q1, s = queue.get()
            for (c_index, c) in self._alphabet_iter(with_epsilon=False):
                q2 = sget(self._state_transition_matrix[q1][c_index])

                if q2 in self._stock_states:
                    continue

                new_s = s + c

                if q2 in self._final_states:
                    yield new_s
                    n -= 1
                
                queue.put((q2, new_s))

    def render(self, with_stock_state: bool=True) -> Image.Image:
        def form_label(transition_symbols: List[int]) -> str:
            transition_symbols.sort(key=lambda s: (len(s), s))
            groups = []
            for i in range(len(transition_symbols)):
                if i == 0   or len(transition_symbols[i-1]) > 1 or len(transition_symbols[i]) > 1 \
                            or ord(transition_symbols[i-1]) + 1 != ord(transition_symbols[i]):
                    groups.append([])         
                groups[-1].append(transition_symbols[i])
            
            words = []
            for group in groups:
                if len(group) <= 1:
                    words.extend(group)
                else:
                    words.append(f"{group[0]}-{group[-1]}")

            return ", ".join(words)


        f = Digraph('finite_state_machine', format="png")
        f.attr(rankdir='LR')

        def draw_state(q: int):
            if q in self._final_states:
                f.attr('node', shape='doublecircle')
            else:
                f.attr('node', shape='circle')
            f.node(self._states_labels[q])

        # nodes
        draw_state(self._initial_state)        
        for q in range(self._states_count):
            if q == self._initial_state or (not with_stock_state and q in self._stock_states):
                continue
            draw_state(q)

        # edges
        for q1 in range(self._states_count):
            transition_symbols = [[] for _ in range(self._states_count)]
            for c_index, c in self._alphabet_iter():
                if c == "":
                    if self._state_transition_matrix[q1][c_index] == {q1}:
                        continue
                    c = "ε"

                for q2 in self._state_transition_matrix[q1][c_index]:
                    if not with_stock_state and q2 in self._stock_states:
                        continue

                    transition_symbols[q2].append(c)
        
            for q2, symbols in enumerate(transition_symbols):
                if len(symbols) != 0:
                    label = form_label(symbols)
                    f.edge(self._states_labels[q1], self._states_labels[q2], label=label)

        # draw arrow to initial state
        f.attr('node', shape='none', height='0', width='0')
        f.node('')
        f.edge('', self._states_labels[self._initial_state])

        byte_array = f.pipe(format="png")
        return Image.open(io.BytesIO(byte_array))

    def delete_unreachable_states(self) -> StateMachine:
        # find reachable states
        reachable = {self._initial_state}

        stack = [self._initial_state]
        while stack:
            q1 = stack.pop()
            for c_index, _ in self._alphabet_iter():
                for q2 in self._state_transition_matrix[q1][c_index]: 
                    if q2 not in reachable:
                        reachable.add(q2)
                        stack.append(q2)
        
        # build new state machine without unreachable states
        new_to_old_state_map = sorted(reachable)
        old_to_new_state_map = dict(zip(sorted(reachable), range(len(reachable))))

        alphabet = copy.deepcopy(self._alphabet)
        states_count = len(reachable)
        initial_state = old_to_new_state_map[self._initial_state]
        final_states = {old_to_new_state_map[q] for q in (self._final_states & reachable)}
        states_labels = [self._states_labels[q] for q in sorted(reachable)]
        state_transition = set()
        for q1 in range(states_count):
            for c_index, c in self._alphabet_iter():
                for old_q2 in self._state_transition_matrix[new_to_old_state_map[q1]][c_index]:
                    q2 = old_to_new_state_map[old_q2]
                    state_transition.add((q1, c, q2))
        return StateMachine(states_count, initial_state, final_states, state_transition, states_labels=states_labels, alphabet=alphabet)

    def factor_state_machine(self, equivalence_classes: List[Set[int]]) -> StateMachine:
        # group states to equivalence classes
        # treat equivalence classes as new states 
        self._deterministic_only()

        old_to_new_state_map = self._states_count*[None]
        for q_new, clss in enumerate(equivalence_classes):
            for q_old in clss:
                old_to_new_state_map[q_old] = q_new

        alphabet = copy.deepcopy(self._alphabet)
        states_count = len(equivalence_classes)
        initial_state = old_to_new_state_map[self._initial_state]
        final_states = set(q_new for q_new, clss in enumerate(equivalence_classes) if all(map(lambda q: q in self._final_states, clss)))
        state_transition = set()
        states_labels = [",".join(self._states_labels[q] for q in clss) for clss in equivalence_classes]
        for q1, clss in enumerate(equivalence_classes):
            q0 = clss.pop(); clss.add(q0)
            for c_index, c in enumerate(alphabet):   
                q2_old = sget(self._state_transition_matrix[q0][c_index])
                q2 = old_to_new_state_map[q2_old]
                if all(map(lambda q: old_to_new_state_map[sget(self._state_transition_matrix[q][c_index])] == q2, clss)):
                    state_transition.add((q1, c, q2))
        return StateMachine(states_count, initial_state, final_states, state_transition, states_labels=states_labels, alphabet=alphabet)

    def get_equivalent_states(self) -> List[Set[int]]:
        inverse_state_transition = self._build_inverse_state_transition_matrix()

        # (q1, q2) q1 > q2
        queue = SimpleQueue()
        marked = [q1*[False] for q1 in range(self._states_count)]

        for q1 in range(self._states_count):
            for q2 in range(q1):
                if (q1 in self._final_states) != (q2 in self._final_states):
                    queue.put((q1, q2))
                    marked[q1][q2] = True

        while not queue.empty():
            u, v = queue.get()
            for c_index in range(self._alphabet_size):
                for q1 in inverse_state_transition[u][c_index]:
                    for q2 in inverse_state_transition[v][c_index]:
                        _q1, _q2 = max(q1, q2), min(q1, q2)
                        if not marked[_q1][_q2]:
                            marked[_q1][_q2] = True
                            queue.put((_q1, _q2))
        
        equivalence_class_found = self._states_count*[False]
        equivalence_classes = []
        for q2 in range(self._states_count):
            if not equivalence_class_found[q2]:
                equivalence_class_found[q2] = True
                equivalence_class = {q2}
                for q1 in range(q2 + 1, self._states_count):
                    if not marked[q1][q2]:
                        equivalence_class.add(q1)
                        equivalence_class_found[q1] = True
                equivalence_classes.append(equivalence_class)
        
        return equivalence_classes          

    def minimize(self) -> StateMachine:
        self._deterministic_only()

        sm = self.delete_unreachable_states()
        return sm.factor_state_machine(sm.get_equivalent_states())

    def get_states_count(self, with_stock_state: bool=True) -> int:
        if with_stock_state:
            return self._states_count
        else:
            return self._states_count - len(self._stock_states)

    def _state_transition_matrix_to_triples_list(self, transition_matrix=None, alphabet=None):
        if transition_matrix is None:
            transition_matrix = self._state_transition_matrix
        if alphabet is None:
            alphabet = self._alphabet

        result = []
        if len(transition_matrix) > 0:
            for q1 in range(len(transition_matrix)):
                for c_index, c in enumerate(alphabet + [""]):
                    for q2 in transition_matrix[q1][c_index]:
                        result.append([q1, c, q2])
        return result

    def save_to_file(self, filename: str) -> None:
        """
        {
            "alphabet": ["a", "b", "c"],
            "states_count": 5,
            "initial_state": 0,
            "final_states": [4],
            "state_transition_function": [
                [0, "a", 0], [0, "b", 1], [0, "c", 0],
                [1, "a", 0], [1, "b", 3], [1, "c", 2],
                [2, "a", 0], [2, "b", 1], [2, "c", 4],
                [3, "a", 4], [3, "b", 3], [3, "c", 2],
                [4, "a", 4], [4, "b", 4], [4, "c", 4]
            ]
        }
        """
        sm_json = dict()
        sm_json["alphabet"] = self._alphabet
        sm_json["states_count"] = self._states_count
        sm_json["initial_state"] = self._initial_state
        sm_json["final_states"] = list(self._final_states)
        sm_json["state_transition_function"] = self._state_transition_matrix_to_triples_list()

        with open(filename, "w") as f:
            json.dump(sm_json, f)

    def set_labels(self, labels: List[str]=None) -> None:
        # this method is only mutator of this class
        # maybe return new StateMachine insted?
        if labels is None:
            self._states_labels = self._build_default_states_labels(self._states_count)
        else:
            self._states_labels = labels

    @staticmethod
    def from_file(filename: str) -> StateMachine:
        with open(filename) as f:
            description = json.load(f)
            alphabet = set(description["alphabet"])
            states_count = description["states_count"]
            initial_state = description["initial_state"]
            final_states = set(description["final_states"])
            state_transition = {tuple(x) for x in description["state_transition_function"]}
            states_labels = description.get("states_labels", None)
            return StateMachine(states_count, initial_state, final_states, state_transition, states_labels=states_labels, alphabet=alphabet)

    @staticmethod
    def from_regexp(regexp: str) -> StateMachine:
        return RegExp(regexp).to_state_machine()

class RegExp:
    def __init__(self, regexp: str):
        self._regexp = regexp
    
    def _delete_external_brackets(self, regexp: str) -> str:
        external_brackets_count = 0
        while regexp[external_brackets_count] == '(' and regexp[-external_brackets_count - 1] == ')':
            external_brackets_count += 1
        
        if external_brackets_count == 0:
            return regexp

        min_level = level = external_brackets_count

        for i in range(external_brackets_count, len(regexp) - external_brackets_count):
            c = regexp[i]
            if c == '(':
                level += 1
            elif c == ')':
                level -= 1
                min_level = min(level, min_level)
        
        if min_level == 0:
            return regexp
        return regexp[min_level:-min_level]

    def _iter_with_level(self, regexp: str):
        level = 0
        for c in regexp:
            if c == '(':
                level += 1
            elif c == ')':
                level -= 1
            yield (level, c)

    def _find_alternation(self, regexp: str) -> int:
        for i, (level, c) in enumerate(self._iter_with_level(regexp)):
            if c == '|' and level == 0:
                return i
        return -1

    def _find_concatenation(self, regexp: str) -> int:
        i = 0
        if regexp[0] == '[':
            while regexp[i] != ']':
                i += 1
        elif regexp[0] == '(':
            for i, (level, c) in enumerate(self._iter_with_level(regexp)):
                if c == ')' and level == 0:
                    break
        elif regexp[0] == '\\':
            i += 1
    
        while i + 1 < len(regexp) and regexp[i + 1] in "+*?":
            i += 1

        if i != len(regexp) - 1:
            return i + 1
        return -1

    def _parse_recursive(self,  transition: Tuple[int, str, int], 
                                state_transition_function: Set[Tuple[int, str, int]],
                                states_count: Counter) -> None:
        q1, regexp, q2 = transition

        # 1. delete external brackets
        regexp = self._delete_external_brackets(regexp)
            
        # 2. split by alternation |
        alternation_position = self._find_alternation(regexp)
        if alternation_position != -1:
            left, right = regexp[:alternation_position], regexp[alternation_position + 1:]
            self._parse_recursive((q1, left, q2), state_transition_function, states_count)
            self._parse_recursive((q1, right, q2), state_transition_function, states_count)
            return
        
        # 3. split by concatenation
        concatenation_position = self._find_concatenation(regexp)
        if concatenation_position != -1:
            left, right = regexp[:concatenation_position], regexp[concatenation_position:]
            q12 = states_count.get()
            states_count.inc()
            self._parse_recursive((q1, left, q12), state_transition_function, states_count)
            self._parse_recursive((q12, right, q2), state_transition_function, states_count)
            return
        
        # 4. modifiers *, +, ?
        if regexp[-1] == '*' and regexp[-2] != '\\':
            state_transition_function.add((q1, "", q2))
            self._parse_recursive((q1, regexp[:-1], q1), state_transition_function, states_count)
            return
        elif regexp[-1] == '+' and regexp[-2] != '\\':
            # A+ = AA*
            q12 = states_count.get()
            states_count.inc()
            self._parse_recursive((q1, regexp[:-1], q12), state_transition_function, states_count)
            self._parse_recursive((q12, regexp[:-1]+'*', q2), state_transition_function, states_count)
            return
        elif regexp[-1] == '?' and regexp[-2] != '\\':
            state_transition_function.add((q1, "", q2))
            self._parse_recursive((q1, regexp[:-1], q2), state_transition_function, states_count)
            return

        # 5. [a1-a2]
        if regexp[0] == '[':
            a1 = regexp[1]
            a2 = regexp[3]
            for char_code in range(ord(a1), ord(a2) + 1):
                state_transition_function.add((q1, chr(char_code), q2))
            return
        
        # 6. just one symbol 'x'
        assert len(regexp) <= 2
        if regexp[0] == '\\':
            c = regexp[1]
        else:
            c = regexp[0]
        state_transition_function.add((q1, c, q2))

        
    def to_state_machine(self) -> StateMachine:
        state_transition_function = set()
        states_count = Counter(2)
        self._parse_recursive((0, self._regexp, 1), state_transition_function, states_count)
        
        initial_state = 0
        final_states = {1}
        return StateMachine(states_count.get(), initial_state, final_states, state_transition_function)


if __name__ == "__main__":
    # rxsm = StateMachine.from_regexp("([a-z]|[A-Z]|_)([0-9]|[a-z]|[A-Z]|_)*")
    # rxsm = StateMachine.from_regexp("test|oleg|testosteron")
    rxsm = StateMachine.from_regexp("[0-9]([0-9]|_)*(.([0-9]|_)+)?((e|E)(-|\+)?([0-9]|_)+)?")

    rxsm.render(with_stock_state=False).show()
    rxsm = rxsm.determinize().minimize()
    rxsm.render(with_stock_state=False).show()
    for i, w in enumerate(rxsm.list_words_lexicographical(1000)):
        if i % 10 == 0:
            print(w)
    print(rxsm.is_word_accepted("123.123E+22"))
    """
    sm_keywords = StateMachine.load_from_file("../HW3/1/keywords.json")
    sm_ident = StateMachine.load_from_file("../HW3/1/ident.json")
    sm_numbers = StateMachine.load_from_file("../HW3/1/numbers.json")
    sm_numbers.minimize().render(with_stock_state=False).show() 
    """