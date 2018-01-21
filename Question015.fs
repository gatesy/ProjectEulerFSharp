// Lattice paths
// Problem 15 
// Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, 
// there are exactly 6 routes to the bottom right corner.
//
// How many such routes are there through a 20×20 grid?
module Question15
    let expand gridDimensions point =
        match point with
        | (x,y) -> [(x+1,y); (x,y+1)] 
        |> List.filter (fun (x,y) -> x <= fst gridDimensions && y <= snd gridDimensions)

    let expandPath gridDimensions path =
        List.head path 
        |> expand gridDimensions
        |> List.map (fun newPoint -> newPoint :: path)

    let rec expandPaths gridDimensions paths =
        let expandedPaths = paths |> List.collect (expandPath gridDimensions)
        //printfn "%A" expandedPaths
        match expandedPaths with
        | [] -> paths
        | _ -> expandPaths gridDimensions expandedPaths 

    let answer = expandPaths (15,15) [[(0,0)]] |> List.length