// Number letter counts
// Problem 17 
// If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there 
// are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
//
// If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many 
// letters would be used?

// NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 
// letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out 
// numbers is in compliance with British usage.
module Question17

let rec nameThatNumber n =
    match n with
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four"
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine"
    | 10 -> "ten"
    | 11 -> "eleven"
    | 12 -> "twelve"
    | 13 -> "thirteen"
    | 14 -> "fourteen"
    | 15 -> "fifteen"
    | 16 -> "sixteen"
    | 17 -> "seventeen"
    | 18 -> "eighteen"
    | 19 -> "nineteen"
    | 20 -> "twenty"
    | 30 -> "thirty"
    | 40 -> "fourty"
    | 50 -> "fifty"
    | 60 -> "sixty"
    | 70 -> "sevety"
    | 80 -> "eighty"
    | 90 -> "ninety"
    | 1000 -> "onethousand"
    | n when n % 100 = 0 -> nameThatNumber ((n - n % 100) / 100) + "hundred" 
    | n when n > 20 && n < 100 -> nameThatNumber (n - (n % 10)) + nameThatNumber (n % 10)
    | n when n > 100 && n < 1000 -> 
        let remainder = n % 100
        nameThatNumber (n - n % 100)  + "and" + nameThatNumber remainder
    | _ -> ""

let countLetters fromN toN =
    seq { fromN .. toN } |> Seq.sumBy (nameThatNumber >> String.length)

let answer () = countLetters 1 1000