# SAT

This is an applicative written for F# interactive.
I wanted to implement an algorithm to solve the SAT problem.
Actually there are 2 functions that solve it, one generating first the space of soluction and than trying to find the assignment that could set the formula satisfied.
It's present also the function that instead of generating first all the possible souctions, it generates the next one if and only if there the previous one doesn't satisfy the formula given.

USAGE:
\#load "booleana.fsx"
use booleana
let str ="(~A V ~B)\n(A V C)\n(A V B)\n(B V D)\n(~C V D)\n(D V E)\n(~A)\n"
let mybexpr,space = getAndB str
(*the function tabverita generates the space of soluctions*)
let spaces =(tabverita space)
(*the function findsol takes 2 arguments, the boolean expression( the type is an algebrical one: type B) and all the combinations of literals's values to try*)
findsol mybexpr spaces
(*function findsol2 generate all the combnations only at the wrost case*) 
findsol2 mybexpr mystack
(*if neither an assignment can satisfy the given formula the exception "insoddisfacibile" is rised*)
