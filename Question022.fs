//Names scores
//Problem 22 
//
// Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand
// first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for 
// each name, multiply this value by its alphabetical position in the list to obtain a name score.
//
// For example, when the list is sorted into alphabetical order, COLIN, which is worth 
// 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
//
// What is the total of all the name scores in the file?
module Question22
open System.IO

let implode (xs: char list) =
    let sb = System.Text.StringBuilder(List.length xs)
    xs |> List.iter (sb.Append >> ignore)
    string sb

let explode str =
    [for c in str -> c]

let letterValue c = 
    (int c) - (int 'A') + 1

let parseData filename =
    let wordBuilder words nextChar =
        match words, nextChar with
        | _, '"' -> words // Ignore quotes
        | _, ',' -> [] :: words // comma is the separator - start a new word
        | (x::xs), _ -> (nextChar :: x) :: xs // add the character to the front of the current word
        | [], _ -> failwith "state must always have an element"

    File.ReadAllLines filename
    |> Seq.concat
    |> Seq.fold wordBuilder [[]]
    |> List.map (List.rev >> implode)
    |> List.sort
    |> List.map (explode >> List.sumBy letterValue)
    |> List.mapi (fun i wordValue -> (i+1) * wordValue)
    |> List.sum

let answer () = 
    parseData "Question022.txt"