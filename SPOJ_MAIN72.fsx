open System;

let computeSubsetSum (set:int array, n:int, sum:int):int = 
  // dp[i,j] = True, if subset of [0..i-1] sum 
  // equals to : 
  // j-set[i]  => ith item is included  
  // or 
  // j         => ith item is not included
  let dp:bool[,] = Array2D.zeroCreate (n+1) (sum+1)

  // This dp tries answer folloowing question, given a sum 
  // j, whether we can derive it by summing any subset of 
  // [0..i]. It computes this answer in bottom up manner, 
  // hence, if starting from (i,j) if it can reach (i,0), the 
  // answer is yes. Otherwise, if sum<>0 but i=0, then answer is
  // no due to the fact that, using any subsets, the sum cannot be 
  // computed. 

  // Therefore. 
  // base case 1 : given sum = 0, answer is true
  // sum is zero with the provided set of items.
  // Hence, storing it as 0
  for i = 0 to n do 
    dp.[i,0] <- true
  
  // base case 2 :  sum <> 0 but OPT = empty --> 
  // answer is false
  for j=1 to sum do 
    dp.[0,j] <- false
  
  for i = 1 to n do 
    let v_i = set.[i-1]
    for j = 1 to sum do 
      dp.[i,j] <-
        if j - v_i < 0 then 
          // we can't include i th item in OPT   
          dp.[i-1,j]
        else
          dp.[i-1,j]||dp.[i-1,j-v_i]
  
  let mutable result = 0
  for j=1 to sum do 
    result  <- result+
      ( if dp.[n,j] = true then 
          j
        else 
          0
      )
  result 

(* IO STUFF:
 * ----------- 
 * Reading problem definition and invoke function to 
 * solve it.
 *)

let parseSet (s:string,n:int)  :(int array*int*int) = 
  s
  |> (fun x -> x.Split())
  |> (fun s -> s.[0..n-1])
  |> Array.map (fun s_i -> s_i|> int) 
  |> (fun x -> (x, n, Array.sum x))


let parseCaseData (ns:string)  = 

  let n = ns.Trim() |> int
  let s = System.Console.ReadLine().Trim()
  
  (s,n)
  |> parseSet 

  
let solve_spojMain72() = 
  let T = Console.ReadLine().Trim() |> int
  let mutable n:string = null

  for case_i = 1 to T do 
    n <- System.Console.ReadLine()
    if n <> null then 
      n
      |> parseCaseData 
      |> computeSubsetSum
      |> printfn "%d"

solve_spojMain72()
