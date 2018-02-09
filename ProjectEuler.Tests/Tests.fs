module ProjectEuler.Tests

open NUnit.Framework
open FsUnit

[<Test>]
let q1 () = Question1.answer () |> should equal 233168

[<Test>]
let q15 () = Question15.answer () |> should equal 137846528820L

[<Test>]
let q16 () = Question17.answer () |> should equal 21124

[<Test>]
let q18 () = Question18.answer "Question018.txt" |> should equal 1074

[<Test>]
let q19 () = Question19.answer () |> should equal 171
 
[<Test>]
let q20 () = Question20.answer () |> should equal 648

[<Test>]
let q21 () = Question21.answer () |> should equal 31626

[<Test>]
let q22 () = Question22.answer "Question022.txt" |> should equal 871198282

[<Test>]
let q23 () = Question23.answer () |> should equal 4179871

[<Test>]
let q24 () = Question24.answer () |> should equal 2783915460L

[<Test>]
let q25 () = Question25.answer () |> should equal 4782

[<Test>]
let q67 () = Question67.answer "Question067.txt"|> should equal 7273