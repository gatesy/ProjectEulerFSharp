// Amicable numbers
// Problem 21 

// Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
// If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are 
// called amicable numbers.

// For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore 
// d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

// Evaluate the sum of all the amicable numbers under 10000.
module Question21

let addDivisors sum n =
    [ n - 1 .. n .. Array.length sum - 1]
    |> List.tail
    |> List.iter (fun index -> sum.[index] <- sum.[index] + n)

let sumDivisors sum =
    [ 1 .. Array.length sum / 2]
    |> List.iter (addDivisors sum)

let findAmicablePair divisorSums a =
    let b = Array.get divisorSums (a - 1)
    if Array.get divisorSums (b - 1) = a then Some b else None

let answer () = 
    let divisorSums = Array.zeroCreate 40000
    sumDivisors divisorSums
    seq { 1 .. 10000 }
    |> Seq.map (findAmicablePair divisorSums)