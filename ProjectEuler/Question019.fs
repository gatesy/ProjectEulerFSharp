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

let startDate () = System.DateTime.Parse "1901-01-01" 

let endDate () = System.DateTime.Parse "2000-12-31"

let isLeapYear year =
    year % 400 = 0 || (year % 4 = 0 && year % 100 <> 0)

let daysInMonth (year, month) =
    match month with
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    | 9 | 4 | 6 | 11 -> 30
    | 2 when isLeapYear year -> 29
    | 2 -> 28
    | _ -> invalidArg "month" "valid months are 1-12 (inclusive)"

let daysUntilNext1st (date: System.DateTime) =
    match date.Day with
    | 1 -> 0
    | _ -> daysInMonth (date.Year, date.Month) - date.Day + 1

let firstsOfTheMonth ((startDate: System.DateTime), (endDate: System.DateTime)) =
    let excludeMonths (year,month) =
        match year, month with
        | _, _ when year = startDate.Year && System.DateTime (year, month, 1) < startDate -> false
        | _, _ when year = endDate.Year && System.DateTime (year, month, 1) > endDate -> false
        | _ -> true

    [ for year in startDate.Year .. endDate.Year do for month in 1 .. 12 -> year,month]
    |> List.filter excludeMonths
    |> List.map daysInMonth
    |> List.scan (+) (daysUntilNext1st startDate)

let daysUntilNextSunday (date: System.DateTime) =
    let rec goThroughDays (fromDate: System.DateTime) dayCount =
        match fromDate.DayOfWeek with
        | System.DayOfWeek.Sunday -> dayCount
        | _ -> goThroughDays (fromDate.AddDays 1.0) dayCount + 1
    goThroughDays date 0

let sundays (startDate, endDate) =
    [ daysUntilNextSunday startDate .. 7 .. (endDate - startDate).Days ]

let rec intersect sortedList1 sortedList2 =
    match (sortedList1, sortedList2) with
    | (x::xs, y::ys) -> 
        if  x = y then x :: intersect xs ys
        else if x < y then intersect xs (y::ys) 
        else intersect (x::xs) ys
    | [],_ | _,[] -> []

let answer () = 
    let dateRange = (startDate (), endDate ())
    intersect (firstsOfTheMonth dateRange) (sundays dateRange)
    |> List.length