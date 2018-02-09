// Factorial digit sum
// Problem 20 
// n! means n × (n − 1) × ... × 3 × 2 × 1
//
// For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
// and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
//
//Find the sum of the digits in the number 100!
module Question20

let factorial n =
    seq { 1I .. n } |> Seq.fold (*) 1I

let digits number =
    number.ToString()
    |> Seq.map ((fun c -> c.ToString()) >> System.Int32.Parse)

let sumOfDigits number =
    digits number |> Seq.sum

let answer () = factorial 100I |> sumOfDigits