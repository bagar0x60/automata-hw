from __future__ import annotations
from typing import Set, List, Tuple, Generator
from queue import SimpleQueue
from graphviz import Digraph
from PIL import Image
import json
import copy
import io

class StateMachine:
    def __init__(self, alphabet: Set[str],
                states_count: int, 
                initial_state: int, 
                final_states: Set[int], 
                state_transition: Set[Tuple[int, str, int]],
                states_labels: List[str]=None):
        self._alphabet = sorted(alphabet)
        self._alphabet_size = len(self._alphabet)
        self._symbol_to_index = dict( zip(self._alphabet, range(self._alphabet_size)) )

        if states_labels is not None:
            self._states_labels = states_labels
        else:
            self._states_labels = [f"q_{q}" for q in range(states_count)]

        self._states_count = states_count
        self._initial_state = initial_state
        self._final_states = final_states
        self._stock_states = set()
        self._state_transition_matrix = self._build_state_transition_matrix(state_transition)

    def _build_state_transition_matrix(self, state_transition: Set[Tuple[int, str, int]]) -> List[List[int]]:
        matrix = [self._alphabet_size*[None] for _ in range(self._states_count)]
        
        for (q1, c, q2) in state_transition:
            c_index = self._symbol_to_index[c]
            matrix[q1][c_index] = q2

        # find stock states
        stock = None
        for q1 in range(self._states_count):
            if q1 in self._final_states:
                continue

            is_stock = True
            for c_index in range(self._alphabet_size):
                if matrix[q1][c_index] not in {q1, None}:
                    is_stock = False
                    break
            if is_stock:
                self._stock_states.add(q1)
                stock = q1

        # add arrows to stock state and stock state themself if needed   
        for q in range(self._states_count):
            for c in range(self._alphabet_size):
                if matrix[q][c] is None:
                    if stock is None:
                        # no stock was found in previous block, so artificial needed
                        stock = self._states_count
                        self._stock_states.add(stock)
                        matrix.append(self._alphabet_size*[stock])  # add loops into stock state 
                        self._states_labels.append(f"q_{stock}")
                        self._states_count += 1

                    matrix[q][c] = stock        
        return matrix

    def is_word_accepted(self, word: str) -> bool:
        q = self._initial_state
        for c in word:
            if c not in self._alphabet:
                return False
            q = self._state_transition_matrix[q][self._symbol_to_index[c]]
        
        return q in self._final_states
            
    def list_words_lexicographical(self, n: int) -> Generator[str, None, None]:
        if len(self._final_states) == 0:
            return

        queue = SimpleQueue()
        queue.put((self._initial_state, ""))
        if self._initial_state in self._final_states:
            yield ""
        
        while not (n <= 0 or queue.empty()):
            q1, s = queue.get()
            for (c_index, c) in enumerate(self._alphabet):
                q2 = self._state_transition_matrix[q1][c_index]

                if q2 in self._stock_states:
                    continue

                new_s = s + c

                if q2 in self._final_states:
                    yield new_s
                    n -= 1
                
                queue.put((q2, new_s))

    def render(self, with_stock_state: bool=True) -> Image.Image:
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
            labels = [[] for _ in range(self._states_count)]
            for c_index, c in enumerate(self._alphabet):
                q2 = self._state_transition_matrix[q1][c_index]

                if not with_stock_state and q2 in self._stock_states:
                    continue

                labels[q2].append(c)
        
            for q2, label in enumerate(labels):
                if len(label) != 0:
                    label = ", ".join(label)
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
            for c_index in range(len(self._alphabet)):
                q2 = self._state_transition_matrix[q1][c_index]
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
            for c_index, c in enumerate(self._alphabet):
                q2 = old_to_new_state_map[self._state_transition_matrix[new_to_old_state_map[q1]][c_index]]
                state_transition.add((q1, c, q2))
        return StateMachine(alphabet, states_count, initial_state, final_states, state_transition, states_labels=states_labels)

    def factor_state_machine(self, equivalence_classes: List[Set[int]]) -> StateMachine:
        # group states to equivalence classes
        # treat equivalence classes as new states 
        
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
                q2 = old_to_new_state_map[self._state_transition_matrix[q0][c_index]]
                if all(map(lambda q: old_to_new_state_map[self._state_transition_matrix[q][c_index]] == q2, clss)):
                    state_transition.add((q1, c, q2))
        return StateMachine(alphabet, states_count, initial_state, final_states, state_transition, states_labels=states_labels)

    def get_equivalent_states(self) -> List[Set[int]]:
        # build inverse state transition function
        inverse_state_transition = [[[] for _ in range(self._alphabet_size)] for _ in range(self._states_count)]
        for q1 in range(self._states_count):
            for c_index in range(self._alphabet_size):
                q2 = self._state_transition_matrix[q1][c_index]
                inverse_state_transition[q2][c_index].append(q1)
        
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
        sm = self.delete_unreachable_states()
        return sm.factor_state_machine(sm.get_equivalent_states())

    def get_states_count(self, with_stock_state: bool=True) -> int:
        if with_stock_state:
            return self._states_count
        else:
            return self._states_count - len(self._stock_states)

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
        sm_json["state_transition_function"] = [    [q, c, self._state_transition_matrix[q][c_index]] 
                                                    for q in range(self._states_count) 
                                                    for c_index, c in enumerate(self._alphabet)]

        with open(filename, "w") as f:
            json.dump(sm_json, f)

    @staticmethod
    def load_from_file(filename: str) -> StateMachine:
        with open(filename) as f:
            description = json.load(f)
            alphabet = set(description["alphabet"])
            states_count = description["states_count"]
            initial_state = description["initial_state"]
            final_states = set(description["final_states"])
            state_transition = {tuple(x) for x in description["state_transition_function"]}
            states_labels = description.get("states_labels", None)
            return StateMachine(alphabet, states_count, initial_state, final_states, state_transition, states_labels=states_labels)

if __name__ == "__main__":
    sm = StateMachine.load_from_file("../3/test5.json")
    sm.render().show()
    sm.minimize().render().show()