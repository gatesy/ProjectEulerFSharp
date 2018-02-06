// Non-abundant sums
// Problem 23 
//
// A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. 
// For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that
// 28 is a perfect number.
//
// A number n is called deficient if the sum of its proper divisors is less than n and it is called 
// abundant if this sum exceeds n.
//
// As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be 
// written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that 
// all integers greater than 28123 can be written as the sum of two abundant numbers. However, this 
// upper limit cannot be reduced any further by analysis even though it is known that the greatest 
// number that cannot be expressed as the sum of two abundant numbers is less than this limit.
//
// Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

module Question23

let divisorSumsArray upTo =
    let addDivisors sum n =
        [ n - 1 .. n .. Array.length sum - 1]
        |> List.tail
        |> List.iter (fun index -> sum.[index] <- sum.[index] + n)

    let sums = Array.zeroCreate upTo
    [ 1 .. upTo / 2 ]
    |> List.iter (addDivisors sums)
    sums

let abundantNumbersArray divisorSums =
    divisorSums
    |> Array.mapi (fun i sum -> (i+1, sum))
    |> Array.choose (fun (i,sum) -> if sum > i then Some i else None)

let summableNumbers maxN numbers =
    let canSum = Array.create maxN false
    let addSumsFor numbers n =
        Array.iter (fun y -> if n + y <= maxN then canSum.[n + y - 1] <- true) numbers
    Array.iter (addSumsFor numbers) numbers
    canSum

let answer () =
    let upTo = 28123

    divisorSumsArray upTo
    |> abundantNumbersArray
    |> summableNumbers upTo
    |> Array.mapi (fun i x -> if x then None else Some (i+1))
    |> Array.choose id
    |> Array.sum