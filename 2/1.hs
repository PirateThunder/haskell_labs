fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

fact 1 = 1
fact n = n * fact (n-1)

foo1 n m l = fib ( fact n + 2 * max m l - min m l )
