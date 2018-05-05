// Reciprocal cycles
// Problem 26 
//
// A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
//
// 1/2  = 0.5
// 1/3  = 0.(3)
// 1/4  = 0.25
// 1/5  = 0.2
// 1/6  = 0.1(6)
// 1/7  = 0.(142857)
// 1/8  = 0.125
// 1/9  = 0.(1)
// 1/10 = 0.1
// Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen 
// that 1/7 has a 6-digit recurring cycle.
//
// Find the value of d < 1000 for which 1/d contains the longest recurring cycle in
// its decimal fraction part.
module Question26

let fractionToDecimal (numerator, denominator) =
    let divides = numerator / denominator
    let remainder = numerator % denominator

    match (divides,remainder) with
        | (0,0) -> None
        | (_,0) -> Some (divides, (0,1))
        | (0,_) -> Some (divides, ((numerator * 10), denominator))
        | (_,_) -> Some (divides, ((remainder * 10), denominator))

// search for a recurring cycle. start by looking at the current element and the previous one,
// then move to the current, previous and the two before those etc.
let findRecurringOfLength numbers length =
    match Array.length numbers with
    | l when l < length * 2 
        -> None
    | l when Array.sub numbers (l - length) length = Array.sub numbers (l - length * 2) length 
        -> Some (Array.sub numbers (l - length) length)
    | _ 
        -> None

let rec trimZeros numbers = 
    match numbers with
    | 0 :: ns -> trimZeros ns
    | _ -> numbers

let findRecurring decimalSeq =
    let seqGenerator = Seq.initInfinite (fun index -> index + 1)
    let f 

    Seq.map (fun length -> findRecurringOfLength decimalSeq length) seqGenerator
    //seq [1 .. 10] |> Seq.map (fun n -> Seq.take n decimalSeq)
    //[1 .. Array.length numbers / 2] |> List.map (findRecurringOfLength numbers)

let fractionToDecimalSeq = Seq.unfold fractionToDecimal

let answer () = 
    //seq [1 .. 10]
    //|> Seq.map (fun denominator -> fractionToDecimalSeq (1, denominator))
    
    //fractionToDecimalSeq (6,7) |> Seq.take 20 |> Seq.toList
    //findRecurringOfLength [|3;1;2;1;2;3|] 2
    findRecurring [|1;3;3;1;3;1|] |> Seq.take 20