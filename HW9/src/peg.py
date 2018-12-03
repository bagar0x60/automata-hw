from __future__ import annotations
from typing import List, Union, Tuple, Optional, Dict
from enum import Enum, auto
from dataclasses import dataclass, field


class NodeType(Enum):
    TERMINAL    = auto()
    NONTERMINAL = auto()
    OPERATION   = auto()


class Operation(Enum):
    SEQUENCE        = auto()
    CHOICE          = auto()
    ZERO_OR_MORE    = auto()
    ONE_OR_MORE     = auto()
    OPTIONAL        = auto()
    AND_PREDICATE   = auto()
    NOT_PREDICATE   = auto()


@dataclass
class ParseTree:
    node_content: str
    childs: List[ParseTree] = field(default_factory=list)

    def draw(self, lvl: int=0):
        print(4*lvl*" " + self.node_content)
        for child in self.childs:
            child.draw(lvl + 1)


@dataclass
class PEG_AST:
    node_type: NodeType
    node_content: Union[str, Operation]
    childs: List[PEG_AST] = field(default_factory=list)

    @staticmethod
    def from_parse_tree(parsing_expression: ParseTree, 
                        nonterminals_asts: Dict[str, PEG_AST]) -> PEG_AST:
        
        def get_unit_ast(unit: ParseTree) -> PEG_AST:
            if len(unit.childs) == 1:
                term = unit.childs[0].childs[0].node_content
                if unit.childs[0].node_content == "TERMINAL":
                    return PEG_AST(NodeType.TERMINAL, term)
                elif unit.childs[0].node_content == "NONTERMINAL":
                    return nonterminals_asts[term]
            elif len(unit.childs) == 3 and unit.childs[1].node_content == "PARSING_EXPRESSION":
                return PEG_AST.from_parse_tree(unit.childs[1], nonterminals_asts)

        parsing_expression_ast = PEG_AST(NodeType.OPERATION, Operation.CHOICE, [])
        for concatination in parsing_expression.childs[::2]:
            concatination_ast = PEG_AST(NodeType.OPERATION, Operation.SEQUENCE, [])
            
            for operation in concatination.childs[::2]:
                if len(operation.childs) == 2:
                    operation_ast = None
                    if operation.childs[0].node_content == "PREFIX_OPERATOR":
                        prefix_operator, unit = operation.childs
                        unit_ast = get_unit_ast(unit)
                        if prefix_operator.childs[0].node_content == "!":
                            operation_ast = PEG_AST( NodeType.OPERATION, 
                                                                Operation.NOT_PREDICATE, 
                                                                [unit_ast])
                        if prefix_operator.childs[0].node_content == "&":
                            operation_ast = PEG_AST( NodeType.OPERATION, 
                                                                Operation.AND_PREDICATE, 
                                                                [unit_ast])
                    
                    if operation.childs[1].node_content == "POSTFIX_OPERATOR":
                        unit, postfix_operator = operation.childs
                        unit_ast = get_unit_ast(unit)
                        if postfix_operator.childs[0].node_content == "*":
                            operation_ast = PEG_AST( NodeType.OPERATION, 
                                                                Operation.ZERO_OR_MORE, 
                                                                [unit_ast])
                        if postfix_operator.childs[0].node_content == "+":
                            operation_ast = PEG_AST( NodeType.OPERATION, 
                                                                Operation.ONE_OR_MORE, 
                                                                [unit_ast])
                        if postfix_operator.childs[0].node_content == "?":
                            operation_ast = PEG_AST( NodeType.OPERATION, 
                                                                Operation.OPTIONAL, 
                                                                [unit_ast])
                    concatination_ast.childs.append(operation_ast)

                elif len(operation.childs) == 1:
                    unit = operation.childs[0]
                    unit_ast = get_unit_ast(unit)
                    concatination_ast.childs.append(unit_ast)
            
            if len(concatination_ast.childs) == 1:
                concatination_ast = concatination_ast.childs[0]

            parsing_expression_ast.childs.append(concatination_ast)
        
        if len(parsing_expression_ast.childs) == 1:
            parsing_expression_ast = parsing_expression_ast.childs[0]

        return parsing_expression_ast


class PEG:
    def __init__(self, peg_ast: PEG_AST):
        self._peg_ast = peg_ast

    def parse(self, text: str) -> ParseTree:
        res = self._parse_recursive(text, 0, self._peg_ast)
        if res is None:
            return None
        else:
            new_pos, parse_trees = res
            if new_pos < len(text) or not parse_trees:
                return None
            return parse_trees[0]

    def _parse_terminal(self, text: str, pos: int, ast: PEG_AST) -> Optional[Tuple[int, List[ParseTree]]]:
        terminal = ast.node_content
        new_pos = pos + len(terminal)
        if text[pos:new_pos] == terminal:
            return (new_pos, [ParseTree(terminal)])
        else:
            return None

    def _parse_nonterminal(self, text, pos, ast):
        nonterminal = ast.node_content
        if nonterminal == "_eps":
            return (pos, [ParseTree("ε")])

        res = self._parse_recursive(text, pos, ast.childs[0])
        if res is None:
            return None
        new_pos, parse_trees = res
        return (new_pos, [ParseTree(nonterminal, parse_trees)])

    def _parse_sequence(self, text, pos, ast):
        new_pos = pos
        res_parse_trees = []
        for child in ast.childs:
            res = self._parse_recursive(text, new_pos, child)
            if res is None:
                return None
            new_pos, parse_trees = res
            res_parse_trees += parse_trees
        return (new_pos, res_parse_trees)

    def _parse_choice(self, text, pos, ast):
        for child in ast.childs:
            res = self._parse_recursive(text, pos, child)
            if res is None:
                continue
            new_pos, parse_trees = res
            return (new_pos, parse_trees)
        return None

    def _parse_zero_or_more(self, text, pos, ast):
        child = ast.childs[0]
        new_pos = pos
        res_parse_trees = []

        res = self._parse_recursive(text, new_pos, child)                
        while res is not None:
            new_pos, parse_trees = res
            res_parse_trees += parse_trees
            res = self._parse_recursive(text, new_pos, child)
        return (new_pos, res_parse_trees)

    def _parse_one_or_more(self, text, pos, ast):
        child = ast.childs[0]
        new_pos = pos
        res_parse_trees = []

        res = self._parse_recursive(text, new_pos, child)  
        if res is None:
            return None              
        while res is not None:
            new_pos, parse_trees = res
            res_parse_trees += parse_trees
            res = self._parse_recursive(text, new_pos, child)
        return (new_pos, res_parse_trees) 

    def _parse_optional(self, text, pos, ast):
        child = ast.childs[0]
        res = self._parse_recursive(text, pos, child)  
        if res is None:
            return (pos, [])
        else:
            new_pos, parse_trees = res
            return (new_pos, parse_trees)

    def _parse_not_predicate(self, text, pos, ast):
        child = ast.childs[0]
        res = self._parse_recursive(text, pos, child)  
        if res is None:
            return (pos, [])
        else:
            return None

    def _parse_and_prediacte(self, text, pos, ast):
        child = ast.childs[0]
        res = self._parse_recursive(text, pos, child)  
        if res is None:
            return None
        else:
            return (pos, [])

    def _parse_recursive(self, text: str, pos: int, ast: PEG_AST) -> Optional[Tuple[int, List[ParseTree]]]:
        if ast.node_type is NodeType.TERMINAL:
            return self._parse_terminal(text, pos, ast)

        if ast.node_type is NodeType.NONTERMINAL:
            return self._parse_nonterminal(text, pos, ast)
        
        if ast.node_type is NodeType.OPERATION:
            if ast.node_content is Operation.SEQUENCE:
                return self._parse_sequence(text, pos, ast)

            if ast.node_content is Operation.CHOICE:
                return self._parse_choice(text, pos, ast)

            if ast.node_content is Operation.ZERO_OR_MORE:
                return self._parse_zero_or_more(text, pos, ast)
            
            if ast.node_content is Operation.ONE_OR_MORE:
                return self._parse_one_or_more(text, pos, ast)

            if ast.node_content is Operation.OPTIONAL:
                 return self._parse_optional(text, pos, ast)
            
            if ast.node_content is Operation.NOT_PREDICATE:
                return self._parse_not_predicate(text, pos, ast)

            if ast.node_content is Operation.AND_PREDICATE:
                return self._parse_and_prediacte(text, pos, ast)

    @staticmethod
    def from_file(file_name: str) -> PEG:
        with open(file_name, 'r') as f:
            starting_expression = f.readline().strip()

            terminals = f.readline().strip().split()  

            nonterminals = f.readline().strip().split()
            nonterminals.append(".")
            nonterminals.append("_eps")

            nonterminals_asts = {nonterm: PEG_AST(NodeType.NONTERMINAL, nonterm)
                                    for nonterm in nonterminals}

            peg_parse_ast = PEG.build_parse_ast(terminals, nonterminals)
            parser = PEG(peg_parse_ast)
       
            # dot operator "." support
            nonterminals_asts["."].childs.append(
                PEG_AST(NodeType.OPERATION, Operation.CHOICE, 
                    [PEG_AST(NodeType.TERMINAL, term) for term in terminals]
                )
            )

            for production in f:
                nonterminal, parsing_expression = production.strip().split(" -> ", 1)
                parsing_expression_parse_tree = parser.parse(parsing_expression)
                if parsing_expression_parse_tree is None:
                    print(f'Incorrect parsing expresion: "{parsing_expression}"')
                    exit(1)
                nonterminals_asts[nonterminal].childs.append(
                    PEG_AST.from_parse_tree(
                        parsing_expression_parse_tree, nonterminals_asts)) 

            peg_ast = PEG_AST.from_parse_tree(
                    parser.parse(starting_expression), nonterminals_asts)
            if peg_ast is None:
                print(f'Incorrect starting expresion: "{starting_expression}"')
                exit(1)

            return PEG(peg_ast)

    @staticmethod
    def build_parse_ast(terminals: List[str], nonterminals: List[str]) -> PEG_AST:
        """
        PEG for parsing PEG

        PARSING_EXPRESSION -> CONCATENATION (" / " CONCATENATION)*
        CONCATENATION -> OPERATION (" " OPERATION)*
        OPERATION -> PREFIX_OPERATOR UNIT / UNIT POSTFIX_OPERATOR / UNIT
        UNIT -> "(" PARSING_EXPRESSION ")" / TERMINAL / NONTERMINAL
        PREFIX_OPERATOR -> "!" / "&"
        POSTFIX_OPERATOR -> "+" / "*" / "?"
        TERMINAL -> "a" / "b" / ... 
        NONTERMINAL -> "A" / "B" / ... / "." / "_eps"
        """

        PARSING_EXPRESSION = PEG_AST(NodeType.NONTERMINAL, "PARSING_EXPRESSION")
        CONCATENATION = PEG_AST(NodeType.NONTERMINAL, "CONCATENATION")
        OPERATION = PEG_AST(NodeType.NONTERMINAL, "OPERATION")
        UNIT = PEG_AST(NodeType.NONTERMINAL, "UNIT")
        PREFIX_OPERATOR = PEG_AST(NodeType.NONTERMINAL, "PREFIX_OPERATOR")
        POSTFIX_OPERATOR = PEG_AST(NodeType.NONTERMINAL, "POSTFIX_OPERATOR")
        TERMINAL = PEG_AST(NodeType.NONTERMINAL, "TERMINAL")
        NONTERMINAL = PEG_AST(NodeType.NONTERMINAL, "NONTERMINAL")

        PARSING_EXPRESSION.childs.append(
            PEG_AST(NodeType.OPERATION, Operation.SEQUENCE, [
                CONCATENATION,
                PEG_AST(NodeType.OPERATION, Operation.ZERO_OR_MORE, [
                    PEG_AST(NodeType.OPERATION, Operation.SEQUENCE, [
                        PEG_AST(NodeType.TERMINAL, " / "),
                        CONCATENATION                    
                    ])
                ])            
            ])
        )

        CONCATENATION.childs.append(
            PEG_AST(NodeType.OPERATION, Operation.SEQUENCE, [
                OPERATION,
                PEG_AST(NodeType.OPERATION, Operation.ZERO_OR_MORE, [
                    PEG_AST(NodeType.OPERATION, Operation.SEQUENCE, [
                        PEG_AST(NodeType.TERMINAL, " "),
                        OPERATION
                    ])
                ])            
            ])
        )

        OPERATION.childs.append(
            PEG_AST(NodeType.OPERATION, Operation.CHOICE, [
                PEG_AST(NodeType.OPERATION, Operation.SEQUENCE, [
                    PREFIX_OPERATOR,
                    UNIT
                ]),
                PEG_AST(NodeType.OPERATION, Operation.SEQUENCE, [
                    UNIT,
                    POSTFIX_OPERATOR
                ]),
                UNIT
            ])
        )

        UNIT.childs.append(
            PEG_AST(NodeType.OPERATION, Operation.CHOICE, [
                PEG_AST(NodeType.OPERATION, Operation.SEQUENCE, [
                    PEG_AST(NodeType.TERMINAL, "("),
                    PARSING_EXPRESSION,
                    PEG_AST(NodeType.TERMINAL, ")")
                ]),
                TERMINAL,
                NONTERMINAL
            ])
        )

        PREFIX_OPERATOR.childs.append(
            PEG_AST(NodeType.OPERATION, Operation.CHOICE, [
                PEG_AST(NodeType.TERMINAL, "!"),
                PEG_AST(NodeType.TERMINAL, "&")
            ])
        )

        POSTFIX_OPERATOR.childs.append(
            PEG_AST(NodeType.OPERATION, Operation.CHOICE, [
                PEG_AST(NodeType.TERMINAL, "*"),
                PEG_AST(NodeType.TERMINAL, "+"),
                PEG_AST(NodeType.TERMINAL, "?")
            ])
        )

        TERMINAL.childs.append(
            PEG_AST(NodeType.OPERATION, Operation.CHOICE, [
                PEG_AST(NodeType.TERMINAL, term) for term in terminals
            ])
        )

        NONTERMINAL.childs.append(
            PEG_AST(NodeType.OPERATION, Operation.CHOICE, [
                PEG_AST(NodeType.TERMINAL, term) for term in nonterminals
            ])
        )

        return PARSING_EXPRESSION