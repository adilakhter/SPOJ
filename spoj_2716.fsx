let inline getMaxQuadrilateralArea (a,b,c,d) = 
  0.50 *(a+b+c+d)
  |> (fun s -> System.Math.Sqrt((s-a)*(s-b)*(s-c)*(s-d)))
let inline parseLine() = 
  let l = System.Console.ReadLine().Split()
  (l.[0]|>float,l.[1]|>float,l.[2]|>float,l.[3]|>float)
let solveSpoj2716() = 
  let s = System.Console.ReadLine()
  //printfn "string cases : %s" s
  let numCases = System.Int32.Parse(s)
  //printfn "cases : %d" numCases 
  for j = 1 to numCases do 
    parseLine()
    |>  getMaxQuadrilateralArea
    |> printfn "%f" 
solveSpoj2716()