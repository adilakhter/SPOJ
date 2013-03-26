(* Problem Definition  
 * ------------------
 * Official Problem spacification is available in the following URI:
 * http://www.spoj.com/problems/COINS/
 * 
 * Algorithmic Technique: This problem can be solved using _dynamic
 * programming_. Instead of using a bottom up DP with OPT table, 
 * we could use a top down
 * approach, which seems to be more effective in this case. Reason: 
 * Bottom up approach generally OPT table, which in this case 
 * would be an array of size 1000000000. Thus, it seemes that the top-
 * down approach would be more space and computation efficient. 
 *)

open System
open System.Collections.Generic 

module Memo = 
  let empty () = new Dictionary<int64,int64>()

  let add k v (memo:Dictionary<int64,int64>) = 
    memo.[k] <- v; memo 

  let tryFind k (memo:Dictionary<int64,int64>) = 
    match memo.TryGetValue(k) with 
    | true, v -> Some(v) 
    | false,_ -> None


let computeMaxDollars (n:int) (memo:Dictionary<int64,int64>)= 
   
  let rec computeMaxDollars' (ni:int64) = 
    if ni = 0L || ni = 1L then // base case 
      ni
    else
      match memo|> Memo.tryFind ni with 
      | Some (nx) -> nx  // found in memo. Returning Result. 
      | None     -> 
        let f  = computeMaxDollars' 
        let nx = 
          (ni/2L, ni/3L, ni/4L)
          |> (fun (x,y,z) -> (f x) + (f y) + (f z)) 
          |> (fun nx -> Math.Max(ni,nx))

        memo|> Memo.add ni nx |> ignore // storing the result in memo
        nx
  computeMaxDollars' (n|>int64) 

    

(*
 * IO STUFF 
 * ------------------
 *  Reading input and printing
 *)

let solve_SPOJCOINS() = 
  let memo = Memo.empty()

  let parseCase () : int option= 
    let s = System.Console.ReadLine()
    if s <> null  then 
      s.Trim()
      |> fun x -> match Int32.TryParse x  with | true, xs -> Some(xs) | false,_ -> None
    else 
      None

  let rec solveCases () = 
    match parseCase() with
    | None -> () 
    | Some(x) -> 
        memo 
        |> computeMaxDollars x
        |> printfn "%d" 
        solveCases () 
  
  solveCases()

solve_SPOJCOINS()
