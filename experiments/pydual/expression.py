from __future__ import annotations
from typing import List, Union

from identifier import get_idx

class Expression:
    idx: int
    operation: str
    dependers: List[Expression]

    def __add__(self, other: Expression) -> Expression:
        add = Addition(self, other)
        self.dependers.append(add)

    def __init__(self):
        self.dependers = []
        self.idx = get_idx()
        self.operation = 'nil'

    def tree(self, depth = 0) -> str:
        stride = ' ' * depth

        return f'{stride}- Empty expression ({self.idx})'

class Addition(Expression):
    _adjoint: Union[None, float]
    alpha: Expression
    beta: Expression
    dependers: List[Expression]
    idx: int
    operation: str

    def __add__(self, other: Expression) -> Expression:
        add = Addition(self, other)
        self.dependers.append(add)

    def __init__(self, alpha: Expression, beta: Expression):
        self._adjoint = None
        self.alpha = alpha
        self.beta = beta
        self.beta.dependers.append(self)
        self.dependers = []
        self.idx = get_idx()
        self.operation = 'add'

    def tree(self, depth = 0) -> str:
        stride = ' ' * depth
        
        return f'{stride}- Addition ({self.idx}):\n{self.alpha.tree(depth + 2)}\n{self.beta.tree(depth + 2)}'

class Variable(Expression):
    _adjoint: Union[None, float]
    dependers: List[Expression]
    idx: int
    operation: str
    
    def __add__(self, other: Expression) -> Expression:
        add = Addition(self, other)
        self.dependers.append(add)
    
    def __init__(self):
        self._adjoint = None
        self.dependers = []
        self.idx = get_idx()
        self.operation = 'var'

    def adjoint(self) -> float:
        if self._adjoint is None:
            self._adjoint = sum([depender.adjoint() for depender in self.dependers])

        return self._adjoint

    def tree(self, depth = 0) -> str:
        stride = ' ' * depth

        return f'{stride}- Variable ({self.idx})'