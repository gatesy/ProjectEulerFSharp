// Lattice paths
// Problem 15 
// Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, 
// there are exactly 6 routes to the bottom right corner.
//
// How many such routes are there through a 20×20 grid?
module Question15
    let expand point gridWidth gridHeight =
        match point with
        | (x,y) -> [(x+1,y); (y+1,x)] 
        |> List.filter (fun (x,y) -> x < gridWidth && y < gridHeight)

    let answer = expand (0,0) 2 2