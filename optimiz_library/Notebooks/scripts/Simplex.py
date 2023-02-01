import numpy as np
from pprint import pprint
from IPython.display import display, Math, Latex, Markdown
from sympy import Matrix, print_latex, latex
from sympy.abc import i, j, k, l, N


class Simplex(object):
    def __init__(self, A: np.ndarray, b: np.ndarray, c: np.ndarray, start = np.ndarray):
        self.A = A
        self.b = b
        self.c = c
        self.start = start
        self.iteration = 1
        self.l = None
        dimension=A.shape[1]
    def run(self):
        self.iteration = 1
        bases = []
        for i in range(0, self.A.shape[0]):
            bases.append(i)
        self.l = self.nchoosek(n=bases,k=self.A.shape[1],s=0,r=np.zeros((1,self.A.shape[1])),l = np.zeros((1,self.A.shape[1])))
        
        for i in range(0,self.l.shape[0]):
            base=self.l[i,:].reshape(1,self.A.shape[1]).astype(int)
            abi = self.A[base[0,0]]
            bbi = np.zeros((self.A.shape[1], 1))
            for k in range(1, self.A.shape[1]):
                abi = np.vstack([abi,self.A[base[0,k]]])
            for k in range(0, self.A.shape[1]):
                bbi[k, 0] = self.b[base[0,k],0]
            if np.linalg.det(abi) != 0:
                vi = np.linalg.pinv(abi).dot(bbi)
                if np.array_equiv(vi.round(), self.start.T):
                    self.simplex(base=base)
                    print('Finished')
                    return
                
    def simplex(self, base):
        
        display(Markdown(f'# Iteration {self.iteration}'))
        display(Latex(f'Base selection is $ {latex(Matrix(base.reshape(-1)+1))} $ because this hyperplanes are intersect with $ {latex(Matrix(self.start))} $'))  # 0 indexed....
        display(Markdown('## Step 1'))
        ab = self.A[base[0,0]]
        bb = np.zeros((self.A.shape[1],1))
        for i in range(1,self.A.shape[1]):
            ab = np.vstack([ab,self.A[base[0,i]]])
        for i in range(0,self.A.shape[1]):
            bb[i,0] = self.b[base[0,i],0]
        ab_inverse =np.linalg.pinv(ab)
        v = ab_inverse.dot(bb)
        display(Latex(f"$$ v: {latex(Matrix(v))} $$"))
        display(Latex(f"Base selection : $ \n{latex(Matrix(base.reshape(-1)+1))} $")) # 0 indexed....
        display(Math(f"A_B : {latex(Matrix(ab))}"))
        display(Latex("$$ A_B^{−1}: " + latex(Matrix(ab_inverse.round(3))) + " $$"))
        
        display(Markdown('## Step 2'))
        u = self.c.dot(ab_inverse)
        display(Latex(f"$$ u = " + "c*A_B^{-1} = " + f"{latex(Matrix(u.round(3)))} = {latex(Matrix(self.c.round(3)))} * {latex(Matrix(ab_inverse.round(3)))} $$"))
        
        
        display(Markdown('## Step 3'))
        if np.all(u > 0):
            display(Latex("All $ u $ are bigger than $ 0 $"))
            display(Latex(f"$$ u >= 0 $$  $$ f(v) = c*v = {latex(Matrix(self.c))}*{latex(Matrix(v))} = {latex(Matrix(np.dot(self.c,v).round(3)))} $$"))
            display(Latex(f"Optimum is Point $ {latex(Matrix(v))} $ with cost $ {latex(Matrix(np.dot(self.c,v).round(3)))} $"))
            return
        else:
            display(Latex("Not all $ u $ are bigger than $ 0 $"))
            display(Latex(f"$ u >= 0 $"))
            display(Latex("Continue with next step"))
        
        display(Markdown('## Step 4, getting index with smallest column value'))
        min_i = -1
        min_val = np.inf
        for i in range(0,self.A.shape[1]):
            if u[0,i] < min_val:
                min_i = i
                min_val =u[0,i]
        j = base[0,min_i]
        display(Latex(f"$ {latex(Matrix(u.round(3)))} $"))
        display(Latex(f"Min value is {i}, leaving hyperplane with index {j+1}"))
        
        display(Markdown('## Step 5 - get direction'))
        display(Latex("$$ d = - A_{B}^{-1} $$"))
        
        display(Latex("$ A_B^{-1}: " + latex(Matrix((ab_inverse.round(3)))) + " $ - get column with index " + str(min_i+1)))
        display(Latex(f"Column {min_i+1} $ " + latex(Matrix((ab_inverse[:, min_i].round(3)))) + " $"))
        d = -(ab_inverse[:, min_i])
        
        display(Markdown('## Step 6'))
        display(Latex(f"$ A*d = {latex(Matrix(self.A.dot(d).round(3)))} " + f" = {latex(Matrix(self.A))} * {latex(Matrix(d.round(3)))} $"))
        
        if np.all(self.A.dot(d).round(3) <= 0):
            display(Latex("All values in $ A*d $ are negative! Found optimal solution."))
            display(Latex(f"$$ A*d <= 0 $$"))
            display(Latex(f"$$ c*v={latex(Matrix(np.dot(self.c,v).round(3)))} $"))
            display(Latex(f"Optimum is Point {latex(Matrix(v))} with cost $ C*V = {latex(Matrix(np.dot(self.c,v)))} $"))
            return
        else:
            display(Latex(f"Found at least one positive value in $ A*d = {latex(Matrix(np.dot(self.A,d).round(3)))} $, continue"))
                
        display(Markdown('## Step 7'))
        m = self.A.shape[0]
        k = -1
        _lambda=np.inf
        
        
        display(Latex(f"Searching for lambda."))
        display(Latex(f"$ Av * \lambda Ad \leq b $"))
        display(Latex(f"$ {latex(Matrix(self.A))}{latex(Matrix(v))}  * \lambda {latex(Matrix(self.A))}{latex(Matrix(d.round(3)))} \leq {latex(Matrix(self.b))} $"))
        display(Latex(f"$ {latex(Matrix(np.dot(self.A, v).round(3)))}  * \lambda {latex(Matrix(np.dot(self.A, d).round(3)))} \leq {latex(Matrix(self.b))}$"))
        
        display(Latex(f"Evaluate columns with $ Ad = {latex(Matrix(np.dot(self.A, d).round(3)))} $ positive $ \\rightarrow {latex(Matrix(np.where(np.dot(self.A, d).round(3)>0)))} $"))
        
        for i in list(np.where(np.dot(self.A, d).round(3)>0))[0]:
            aid = np.dot((self.A[i,:]),d.T)
            if aid>0:
                baiv = (self.b[i,0]-np.dot(self.A[i,:],v.T.reshape(-1)))
                la =(baiv/(aid))
                display(Latex(f"Evaluate row {i+1}:" + " $ \\frac{b^i - a^i v}{A_i d} " + "= \\frac{" + str(baiv.round(3)) + "}{" + str(aid.round(3)) + "} = {" + str(la.round(3)) + "}$"))
                if la<_lambda:
                    k = i
                    _lambda = la
            else:
                raise Error()
        
        display(Latex(f"Taking smaller value $ \\lambda = {_lambda.round(3)} $ which has index $ k = {k+1} $"))
        
        display(Markdown("## Step 8"))
        move = _lambda * d.T
        display(Latex(f"Test $ v = v + \\lambda d = {latex(Matrix(v))} + {round(_lambda, 3)} * {latex(Matrix(d.T))} = {latex(Matrix(v))} + {latex(Matrix(move))} = {latex(Matrix(np.add(v.reshape(-1), move)))} $"))
        for i in range(0,self.A.shape[1]):
            if base[0,i] == j:
                base[0,i] = k
                display(Latex(f'Leaving now hyperplane $ H_{j+1} $ and continue on hyperplane $ H_{k+1} $'))
        base.sort()
        self.simplex(base)
        
    def nchoosek(self, n,k,s,r, l):
        if k == 0:
            l = np.vstack([l, r])
            return l
        for i in range(s, len(n) - k + 1):
            r[0,r.shape[1]-k]=n[i]
            l = self.nchoosek(n,k-1,s+1,r, l)
        return l 