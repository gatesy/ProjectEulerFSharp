// Counting Sundays
// Problem 19
// You are given the following information, but you may prefer to do some research for yourself.

// 1 Jan 1900 was a Monday.
// Thirty days has September,
// April, June and November.
// All the rest have thirty-one,
// Saving February alone,
// Which has twenty-eight, rain or shine.
// And on leap years, twenty-nine.
// A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible
// by 400.
// How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
module Question19

let startDate = System.DateTime.Parse "1998-01-01" 

let endDate = System.DateTime.Parse "2000-12-31"

let isLeapYear year =
    year % 400 = 0 || (year % 4 = 0 && year % 100 <> 0)

let daysInMonth year month =
    match month with
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    | 9 | 4 | 6 | 11 -> 30
    | 2 when isLeapYear year -> 29
    | 2 -> 28
    | _ -> invalidArg "month" "valid months are 1-12 (inclusive)"

let daysThatAre1stMonth year =
    seq { 1 .. 11 } |> Seq.map (daysInMonth year) |> Seq.scan (+) 1

let firstOfTheMonth (startDate: System.DateTime) (endDate: System.DateTime) =
    let startingYear = startDate.Year
    let startingMonth = if startDate.Day = 1 then startDate.Month else startDate.Month + 1
    let endingYear = endDate.Year
    let excludeMonths (year,month) =
        match year, month with
        | startDate.Year, _ when System.DateTime (year, month, 1) < startDate -> true
        | endingYear, _ when System.DateTime (year, month, 1) > endDate -> true
        | _ -> false

    [ for year in startingYear .. endingYear do for month in 1 .. 12 -> year,month]
    |> List.filter excludeMonths

let rec findNextSunday (date: System.DateTime) =
    match date.DayOfWeek with
    | System.DayOfWeek.Sunday -> date
    | _ -> findNextSunday (date.AddDays 1.0)

let sundays startDate endDate =
    let firstSunday = ((findNextSunday startDate) - startDate)
    let total = endDate - startDate
    seq { firstSunday.Days .. 7 .. total.Days}

let answer () = firstOfTheMonth startDate endDate