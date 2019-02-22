import numpy as np
import matplotlib.pyplot as plt
from sympy.solvers import solve
from sympy import Symbol

def sec1(x,s):
    return s-x

def sec2(x,s):
    return s+x  

def sec3(x,s):
    return -s+x  

def sec4(x,s):
    return -s-x  

def lasso_contour(b1,b2):
    y1 = 0
    x1 = 0.5
    fun = 2*(y1-(b1+b2)*x1)**2
    return fun

s = 3    
b1_neg = np.linspace(-s, 0, num = 50)
b1_pos = np.linspace(0, s, num = 50)

xvar = np.linspace(-3, 3, num = 50)
yvar = np.linspace(-3, 3, num = 50)
Xvar, Yvar = np.meshgrid(xvar, yvar)
Zvar = lasso_contour(Xvar,Yvar)

x = Symbol('x')
x1, =  solve(sec1(x,s)-sec2(x,s))
y1 = sec1(x1,s)
x2, =  solve(sec1(x,s)-sec3(x,s))
y2 = sec1(x2,s)
x3, =  solve(sec3(x,s)-sec4(x,s))
y3 = sec3(x3,s)
x4, =  solve(sec2(x,s)-sec4(x,s))
y4 = sec2(x4,s)

plt.plot(x1,y1,'ro-',markersize=10)
plt.plot(x2,y2,'ro-',markersize=10)
plt.plot(x3,y3,'ro-',markersize=10)
plt.plot(x4,y4,'ro-',markersize=10)

plt.plot(b1_pos,s-b1_pos,'k',lw=1.5)
plt.plot(b1_neg,s+b1_neg,'k',lw=1.5)
plt.plot(b1_neg,-s-b1_neg,'k',lw=1.5)
plt.plot(b1_pos,-s+b1_pos,'k',lw=1.5)
plt.contour(Xvar,Yvar,Zvar)

plt.fill([x1,x2,x3,x4,x1],[y1,y2,y3,y4,y1],'blue',alpha=0.5)
