from __future__ import annotations
from math import cos
from typing import Dict, Callable, List, Set, Tuple, Union

class Derivative:
    @staticmethod
    def add(parent: Variable, child: Variable) -> float:
        return parent.get_dual()

    @staticmethod
    def mul(parent: Variable, child: Variable) -> float:
        return parent.get_dual() * parent.other_child(child).primal

    @staticmethod
    def sin(parent: Variable, child: Variable) -> float:
        return parent.get_dual() * cos(parent.other_child(child).primal)

derivative_methods: Dict[str, Callable[[Variable, Variable], float]] = {
    'add': Derivative.add,
    'mul': Derivative.mul,
    'sin': Derivative.sin
}

idx = -1        # Global id counter

def get_id() -> int:
    '''Returns a unique id

    Returns:
        int: The generated id
    '''
    global idx
    idx += 1                                        # Add 1 to the global id counter

    return idx                                      # Return the id

class Variable():
    children: Union[Variable, Tuple[Variable, Variable]]
    _dual: float
    idx: int
    name: str
    parents: List[Variable]
    op: str
    primal: float
    _seed: float

    def __init__(self, primal: float, name: str, children: Union[Variable, Tuple[Variable, Variable]] = None, op: str = None):
        self.children = children                        # Set children from argument
        self._dual = None                               # Set the dual accumulator to None to specify not calculated
        self.idx = get_id()                             # Get id from get_id
        self.name = name                                # Set name from argument
        self.op = op                                    # Set operator from argument
        self.parents = []                               # Set parents to empty list
        self.primal = primal                            # Set primal value from argument
        self._seed = 0                                  # Set seed to zero

        if self.children is not None:                   # If children are given
            for child in self.children:                     # Loop over all children
                child.parents.append(self)                      # Add this node as their parent

    def get_outputs(self, ids: Set[float] = None) -> List[Variable]:
        '''Gets the outputs affected by this variable

        Args:
            ids (Set[float], optional): The ids already gathered. Defaults to None.

        Returns:
            List[Variable]: The found final outputs
        '''
        if len(self.parents) == 0:                      # If this variable has no parents
            return [self]                                   # Return self, because it apparently is a final output

        if ids is None:                                 # If ids is not defined
            ids = set()                                     # Create a new ids set

        outputs = []                                    # Create emtpy list to store the outputs of all parents

        for parent in self.parents:                     # Loop through all the parents
            if parent.idx not in ids:                       # Check whether we've not already gathered this node
                ids.add(parent.idx)                             # Add this node to the set
                outputs.extend(parent.get_outputs(False, ids))  # Get the outputs affected by all the parents

        return outputs                                  # Return the found outputs

    def get_child_dual(self, child: Variable) -> float:
        '''Calculates and returns the dual value of a child based on the operator of this variable

        Args:
            child (Variable): The child that's asking for its value

        Returns:
            float: The partial value of the dual of the child based on this parent
        '''
        if self.op is None:                             # If the operator of this node is not set
            raise RuntimeError('No operator set')           # Raise a runtime error

        return derivative_methods[self.op](self, child)

    def get_dual(self) -> float:
        '''Gets the dual value for this variable

        Returns:
            float: The dual value
        '''
        if self._dual is not None:                      # If the dual is already calculated
            return self._dual                               # Return the calculated value

        if len(self.parents) == 0:                      # If this variable has no parents
            self._dual = self._seed                          # Set the dual value to the seed value
            
            return self._dual                               # Return the seed value as the dual value

        self._dual = sum([parent.get_child_dual(self) for parent in self.parents])  # Calculate the dual value

        return self._dual                               # Return the calculated value

    def other_child(self, child: Variable) -> Variable:
        if len(self.children) != 2:
            raise RuntimeError('No other child to get')

        if self.children[0].idx == child.idx:
            return self.children[1]

        return self.children[0]

    def reset(self) -> None:
        '''Resets the expression tree'''
        self._dual = None
        self._seed = 0.0

        for parent in self.parents:
            parent.reset()

    def seed(self, seed: float = 1.0) -> None:
        '''Sets the seed of this variable for reverse AD

        Args:
            seed (float, optional): The seed to set. Defaults to 1.0.
        '''
        self._seed = seed                                # Set the seed from argument

    def __add__(self, other: Variable) -> Variable:
        if type(other) != Variable:
            raise TypeError("Variables can only add to variables")

        return Variable(self.primal + other.primal, "(%s + %s)" % (self.name, other.name), [self, other], 'add')

    def __mul__(self, other: Variable) -> Variable:
        if type(other) != Variable:
            raise TypeError("Variables can only multiply with variables")

        return Variable(self.primal * other.primal, "(%s * %s)" % (self.name, other.name), [self, other], 'mul')

    def __sub__(self, other: Variable) -> Variable:
        if type(other) != Variable:
            raise TypeError("Variables can only subtract from variables")
        
        return Variable(self.primal - other.primal, "(%s - %s)" % (self.name, other.name), [self, other], 'sub')