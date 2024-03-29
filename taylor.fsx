// Print a table of a given function f, computed by taylor series
//3 вариант
// function to compute
open System

let rec while1 exitCond act cur = 
    if (exitCond cur) then cur
    else while1 exitCond act (act cur) 

let power (x: float) (n: int) =
    let exitCondition (n, _, _) = n < 1
    let action (n, x, acc) = (n - 1, x, acc * x)
    let (_, _, res) = while1 exitCondition action (n, x, 1.)
    res

let builtinCalc x = Math.Log(1.0 + x - 2.0*x*x)

let rec taylor_naive_log (x: float) (n: float) (i: float) (acc: float) (epsilon: float) =
    let prev = ((power(-1.) (int i+1)) * (power (2.) (int i)) - 1.) * ((power (x) (int i))) / i
    let cur = ((power (-1.) (int i)) * (power (2.) (int i)) - 1.) * (power (x) (int i+1)) / (i+1.)
    if (abs(cur - prev) < epsilon) then
        (acc, i)
    else
        let acc = acc + prev
        taylor_naive_log x n (i + 1.) acc epsilon



let smartTaylor x =
    let rec loop n acc result =
        let newAcc = ((power (-1.0)  (n+1)) * (power (2.0) (n)) - 1.0) * (power (x) (n)) / float n
        let newResult = acc + newAcc
        if Math.Abs(newAcc) < 0.0001 then newResult, n
        else loop (n+1) newResult newResult
    loop 1 0.0 0.0
let n = 10
let a = -0.2
let b = 0.3
let epsilon = 0.0001
printfn "| x     | Builtin | Smart Taylor | # terms | Dumb Taylor | # terms |"

let main =
    for i = 0 to n do
        let x = a + (float i) / (float n) * (b - a)
        let xFloat = float x
        let builtinResult = builtinCalc xFloat
        let smartResult, smartTerms = smartTaylor xFloat
        let (taylor_naive_res, taylor_naive_iter) = taylor_naive_log x i 1. 0. epsilon
        printfn "| %.2f | %.4f | %.4f | %d | %.4f | %0.0f |" xFloat builtinResult smartResult smartTerms taylor_naive_res taylor_naive_iter

main
