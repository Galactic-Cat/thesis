from math import cos

class Derivative:
    @staticmethod
    def sin(value: float) -> float:
        return cos(value)

derivative_methods = {
    'sin': Derivative.sin
}