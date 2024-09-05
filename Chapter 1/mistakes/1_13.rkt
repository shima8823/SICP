from: https://sicp.iijlab.net/solution/ex1.2.html

冪乗を↑であらわす.

φ=(1+√5)/2
ψ=(1-√5)/2

Fib(0) = (φ↑0 - ψ↑0)/√5 = (1 - 1)/√5 = 0
Fib(1) = (φ↑1 - ψ↑1)/√5 = ((1+√5)/2 - (1-√5)/2)/√5 
  = √5/√5 = 1
Fib(n) = Fib(n-1)+Fib(n-2) 
  = (φ↑(n-1) - ψ↑(n-1))/√5 + (φ↑(n-2) - ψ↑(n-2))/√5
  = (φ↑(n-2)×(φ+1) - ψ↑(n-2)×(ψ+1))/√5

φ↑2 = (1 + 2√5 +5)/4 = (3 + √5)/2 = φ+1
ψ↑2 = (1 - 2√5 +5)/4 = (3 - √5)/2 = ψ+1

∴ Fib(n) = (φ↑n - ψ↑n)/√5

上の式で引く方の(ψ↑n)/√5の大きさを調べてみると

ψ↑0/√5 = (/ 1 (sqrt 5))= .4472135954999579
ψ↑1/√5 = (1-√5)/2/√5 = (/ (- 1 (sqrt 5)) 2 (sqrt 5))
          = -.27639320225002106
ψ↑n/√5 = ((1-√5)/2)↑n/√5
          = (-.6180339887498949↑n)/2.23606797749979    n>=2のとき

はいずれも絶対値が0.5より小さく, Fib(n) は φ↑n/√5 にもっとも近い整数となる. 