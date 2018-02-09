// Lexicographic permutations
// Problem 24
//
// A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation 
// of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, 
// we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
//
// 012   021   102   120   201   210
//
// What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

module Question24
open System

let pickOneItem items =
    let itemsWithoutI i = items |> List.indexed |> List.choose (fun (j,x) -> if i <> j then Some x else None)
    items |> List.mapi (fun i el -> (el, itemsWithoutI i))
 
let rec permutations digits =
    let combine (x,others) = permutations others |> List.map (fun permutation -> x :: permutation)

    match digits with
    | [] -> []
    | [x] -> [[x]]
    | _ -> digits |> pickOneItem |> List.collect combine

let answer () = 
    permutations [0;1;2;3;4;5;6;7;8;9] 
    |> List.item (1000000 - 1)
    |> List.map string
    |> List.fold (+) ""
    |> Int64.Parse