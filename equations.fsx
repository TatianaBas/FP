
let eps = 1e-3

let abs x = 
    if x >= 0.0 then x
    else -x

let rec while1 exitCond act cur = 
    if (exitCond cur) then cur
    else while1 exitCond act (act cur) 

let power (x: float) (n: int) =
    let exitCondition (n, _, _) = n < 1
    let action (n, x, acc) = (n - 1, x, acc * x)
    let (_, _, res) = while1 exitCondition action (n, x, 1.)
    res


// Определите функции для решения алгебраических уравнений
let dichotomy f a b = 

    let exitCondition (a, b) = ((b - a) < eps)

    let action (a, b) = 
        let c = (a + b) / 2.0
        if (f b) * (f c) < 0.0 then (c, b)
        else (a, c)
    
    let (u, v) = while1 exitCondition action (a, b)
    (u + v) / 2.0


let iterations phi x0 = 
    let action x = phi x
    let exitCondition x = abs(x - phi x) < eps
    while1 exitCondition action x0


let newthon f f' x0 = 
    let expr x = (x - (f x / (f' x)))
    iterations expr x0

// используйте функцию 'iterations'

// Решите 3 уравнения (начиная со своего номера варианта) с использованием 3-х методов
let f1 x = 1.0 - x + sin x - log(1.0 + x)
let f2 x = 3.0*x - 14.0 + exp(x) - exp(-x)
let f3 x = sqrt(1.0 - x) - (sin x)/(cos x)

let f1' x = cos x - (x+2.0)/(x+1.0)
let f2' x = exp(-x)*(exp(2.0*x) + 1.0) + 3.0
let f3' x = -1.0*((cos x)*(cos x) + 2.0*sqrt(1.0 - x))/(2.0*sqrt(1.0-x)*(cos x)*(cos x))

let phi1 x = 1.0 + sin x - log(1.0 + x)
let phi2 x = (14.0 - exp(x) + exp(-x))/3.0
let phi3 x = atan(sqrt(1.0 - x))


let main = 
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f1 1.0 1.5) (iterations phi1 1.5)  (newthon f1 f1' 1.5)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f2 1.0 3.0) (iterations phi2 2.0) (newthon f2 f2' 2.0)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f3 0.0 1.0) (iterations phi3 0.5) (newthon f3 f3' 0.5)
main 
