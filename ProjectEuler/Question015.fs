// Lattice paths
// Problem 15 
// Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, 
// there are exactly 6 routes to the bottom right corner.
//
// How many such routes are there through a 20×20 grid?
module Question15

let initGrid (gridWidth, gridHeight) =
    let initValue x y =
        match x,y with
        | 0,0 -> 1L
        | _,_ -> 0L
    initValue |> Array2D.init (gridHeight + 1) (gridWidth + 1)
    
let propogateXPath (x,y) grid = 
    match x with
    | x when x+1 >= Array2D.length1 grid -> grid
    | _ -> grid.[x+1, y] <- grid.[x+1,y] + grid.[x, y]
           grid

let propogateYPath (x,y) grid =
    match y with
    | y when y+1 >= Array2D.length2 grid -> grid
    | _ -> grid.[x, y+1] <- grid.[x,y+1] + grid.[x,y]
           grid

let addPathsForPoint grid point =
    grid |> propogateXPath point |> propogateYPath point

let generatePoints (gridWidth, gridHeight) =
    seq { for y in 0 .. gridHeight do for x in 0 .. gridWidth -> (x,y)}

let bottomRightValue grid =
    Array2D.get grid (Array2D.length1 grid - 1) (Array2D.length2 grid - 1)

let answer () = 
    let gridDimensions = (20,20)
    let grid = initGrid gridDimensions
    generatePoints gridDimensions 
    |> Seq.fold addPathsForPoint grid
    |> bottomRightValue
