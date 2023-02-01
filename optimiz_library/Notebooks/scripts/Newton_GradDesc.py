import numpy as np
from sympy import *
from fractions import Fraction


class OptimizLibrary:
    def __init__(self, x0, f, dx, dy, dxdx, dydy, dxdy):
        self.x0 = x0
        self.f = f
        self.dx = dx
        self.dy = dy
        self.dxdx = dxdx
        self.dydy = dydy
        self.dxdy = dxdy

    def grad(self, x: tuple) -> np.array:
        return np.array([self.dx(x), self.dy(x)])

    def H(self, x: tuple) -> np.array:
        res = np.array([[self.dxdx(x), self.dxdy(x)],
                        [self.dxdy(x), self.dydy(x)]])
        return res

    # Only works for 1 Dim and 2 Dim
    def newton(self, n: int, dim: int = 1) -> float:
        x = self.x0
        print(f"Initialize x_0 = {x}")
        for i in range(n):
            print(f"Iteration {i}")
            if dim == 1:
                print(f"x_{i + 1} = x_{i} - dx({x}) / dxdx({x}) =")
                print(f"= {x} - {self.dx(x)} / {self.dxdx(x)}")
                x = x - self.dx(x) / self.dxdx(x)
                print(f"= {x}")
            elif dim == 2:
                print(f"x_{i + 1} = x_{i} - H_f({x})^-1 * ∇_f(x_{i}) =")
                print(f"= ({x} - {np.linalg.pinv(self.H(x))} * {self.grad(x)}) ")
                x = x - np.dot(np.linalg.pinv(self.H(x)), self.grad(x))
                print(f"= {x}")
            elif dim == 3:
                # TODO
                pass
            else:
                print("Calculate manually.")
        print(f"Result is (x_i y_i)^2 = {x}^T")
        return x

    def grad_descent(self, n: int, mode: str) -> float:
        β = 1
        x = self.x0

        # start with normal step gradient descent
        β *= 2
        while self.f(x - β * self.grad(x)) >= self.f(x):
            β /= 2
            print(f"β = {β} = {Fraction(β)}:")
            print(f"({x} - {β} * {self.grad(x)}) = {x - β * self.grad(x)}")
            print(f"{self.f(x - β * self.grad(x))} !< {self.f(x)}")
            print()
        print(f"{self.f(x - β * self.grad(x))} < {self.f(x)} done!")

        x = self.x0
        for i in range(n):
            print(f"x_{i + 1} = x_{i} - β * ∇_f(x_{i}) =")
            print(f"= ({x} - {β} * {self.grad(x)})")
            x = x - np.dot(β, self.grad(x))
            print(f"= {x}")
        print(f"Result is (x_i y_i)^2 = {x}^T with function value = {self.f(x)}")

        if mode == "parabola":
            x = self.x0
            print("\nParabola fitting")
            P0 = self.f(x - (0 * β) * self.grad(x))
            print(f"P0 = self.f({x} - (0 * {β}) * {self.grad(x)}) = {P0}")
            PB = self.f(x - β * self.grad(x))
            print(f"PB = self.f({x} - {β} * {self.grad(x)}) = {PB}")
            P2B = self.f(x - (2 * β) * self.grad(x))
            print(f"P2B = self.f({x} - (2 * {β}) * {self.grad(x)}) = {P2B}")
            print()
            β_star = (β / 2) * (3 * P0 - 4 * PB + P2B) / (P0 - 2 * PB + P2B)
            print(f"β^* = {β_star} = ({β} / 2) * (3 * {P0} - 4 * {PB} + {P2B}) / ({P0} - 2 * {PB} + {P2B})")
            print()
            print(f"x_1 = x_0 - β^* * ∇_f(x_0) =")
            print(f"= ({x} - {β_star} * {self.grad(x)}) = {x - np.dot(β_star, self.grad(x))}")
            x = x - np.dot(β_star, self.grad(x))
            print(f"Result is (x_i y_i)^2 = {x}^T with function value = {self.f(x)}")

        return x

    def broyden(self, n: int) -> float:
        x_0 = self.x0
        print(f"Initialize x_0 = {x_0}")
        print(f"First iteration with Newton method")
        print(f"x_1 = x_0 - H_f({x_0})^-1 * ∇_f(x_0) =")
        print(f"= ({x_0} - {np.linalg.pinv(self.H(x_0))} * {self.grad(x_0)}) ")
        x_1 = x_0 - np.dot(np.linalg.pinv(self.H(x_0)), self.grad(x_0))
        print(f"= {x_1}")

        for i in range(n):
            i += 1
            print(f"Broyden's method iteration {i}")
            if i == 1:
                A_0_inv = np.linalg.pinv(self.H(x_0))
            print(f"A_0^-1 = {A_0_inv}")
            g_i = self.grad(x_1) - self.grad(x_0)
            print(f"g_{i} = ∇_f(x_{i}) - ∇_f(x_{i-1}) = {self.grad(x_1)} - {self.grad(x_0)} = {g_i}")
            d_i = x_1 - x_0
            print(f"d_{i} = x_{i} - x_{i-1} = {x_1} - {x_0} = {d_i}")
            A_1_inv = A_0_inv - (np.dot(np.outer((np.dot(A_0_inv, g_i) - d_i), d_i.T), A_0_inv)) / (np.dot(np.dot(d_i.T, A_0_inv), g_i))
            print(f"A_{i+1}^-1 = A_{i}^-1 - (A_{i}^-1 * g_{i} - d_{i}) * d_{i}^T * A_{i}^-1 / (d_{i}^T * A_{i}^-1 * g_{i}) = {A_1_inv}")
            x_1 = x_1 - np.dot(A_1_inv, self.grad(x_1))
            print(f"x_{i+1} = x_{i} - A_{i}^-1 * ∇_f(x_{i}) = {x_1} with function value f(x_{i+1}) = {self.f(x_1)}")

            x_0 = x_1
            A_0_inv = A_1_inv



    def aitken(self, x_i: float, x_i1: float, x_i2: float) -> None:
        y_i = x_i - ((x_i - x_i1)**2 / (x_i - 2 * x_i1 + x_i2))
        print(f"Result for y_i is = {y_i}")


