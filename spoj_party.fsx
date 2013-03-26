open System

let computeOptCost'' (budget, N, (opt:int [,]), (costs:int []), optFunValue) =
  let getcost i = 
    costs.[i-1]
  
  let appendAcc lst (i,k) = 
    (i,k, i|>getcost) |> (fun x -> x::lst)

  let rec computeOptCostRec i k acc: (int*int) list = 
    match k,i with 
    | _ when k>0 && i>0 ->  
      if opt.[i,k] <> opt.[i-1,k] then // this item is included. Hence, updating acc  
        computeOptCostRec  (i-1) (k-costs.[i-1]) ((i,costs.[i-1])|>appendAcc acc) 
      else 
        match acc with 
        | [] -> computeOptCostRec (i-1)  k acc
        | (x,c)::xs when opt.[i,k] = x && c<costs.[i-1]-> computeOptCostRec (i-1)  k acc
        
    | _                  -> 
        printfn "%A" acc
        acc |> List.fold (fun acc (i,k) -> acc+(i|>getcost)) 0
  computeOptCostRec N budget []

   


let computeOptCost' (budget,N,(opt:int [,]),(costs:int[])) = 
    let rec computeOptCost' i k optCost = 
      match k,i with 
      |_ when k>0 && i>0  -> 
        if opt.[i-1,k] <> opt.[i,k] then // i is included in OPT
          computeOptCost' (i-1) (k-costs.[i-1]) (optCost+costs.[i-1])
        else
            computeOptCost' (i-1) k optCost
      | _ -> optCost
    computeOptCost' N budget 0 
  
let computeOptCost (budget,N,(opt:int [,]),optFunValue) = 
  let mutable optCost = 0
  for  c=budget downto 0 do 
    if opt.[N,c] = optFunValue then 
      optCost <- c
    else
      ()
  optCost 

let computeOptFunValue (budget,N,(costs:int []), (funValues:int[])) = 
  let OPT = Array2D.zeroCreate (N+1) (budget+1)
  
  for i = 1 to N do        
    for j = 0 to budget do 
      let c_i = costs.[i-1] //cost for the ith party
      let f_i = funValues.[i-1] // fun value associated with ith party
      
      OPT.[i,j] <-
        match j,c_i with 
        | _ when j<c_i -> OPT.[i-1,j]
        | _            -> Math.Max(OPT.[i-1,j],f_i + OPT.[i-1, j-c_i])
  
  
  (OPT,OPT.[N,budget]) 
  
let computeOptPartySchedule (budget,N,(costs:int[]),(funValues:int[]))  =   
   let OPT, optFunValue = computeOptFunValue (budget,N,costs,funValues)
   let optCost =  (budget,N,OPT,OPT.[N,budget])|>computeOptCost 
   
   // TODO: Experimental Codes 
   printfn "1.debugging-- %d" ((budget,N,OPT,costs,OPT.[N,budget])|>computeOptCost'')
   printfn "2.debugging-- %d" ((budget,N,OPT,costs)|>computeOptCost')
 
   
   // returning final result 
   (optCost, optFunValue) 
   
 


// Example Usage:
(7,2, [|6;5;|],[|1;1;|])|> computeOptPartySchedule

(50,10,[|12;15;16;16;10;21;18;12;17;18;|],[|3;8;9;6;2;9;4;4;8;9|]) |> computeOptPartySchedule
//
(50,10,[|13;19;16;12;10;12;13;15;11;16;|],[|8;10;8;9;2;8;5;5;7;2|]) |> computeOptPartySchedule


//---------------- IO Stuff ------------------- 
let processProblemHeader  () = 
  let s = Console.ReadLine().Split()
  let budget,noOfParties = s.[0]|>int,s.[1]|>int

  match budget,noOfParties with 
  | (0,0) -> None 
  | _     -> Some(budget,noOfParties)

let parsePartyData  n = 
  let costs = Array.create n 0
  let funValues =  Array.create n 0

  for i = 0 to (n-1) do
    System.Console.ReadLine().Split() 
    |> (fun (s:string []) -> (s.[0]|>int,s.[1]|>int))
    |> (fun (x,y) -> costs.[i] <- x; funValues.[i]<-y)
  
  (System.Console.ReadLine()|>ignore) // reading last line that ends this problem instance 
  costs,funValues

let solve_SPOJ_PARTY () = 
  let rec solvePartyAux() = 
    match processProblemHeader() with 
    | None -> () 
    | Some(budget,n) -> 
        
        parsePartyData n
        |> (fun (costs,funValues) -> (budget, n, costs, funValues))
        |> computeOptPartySchedule 
        |> (fun (optCosts,optFun)  -> printfn "%d %d" optCosts optFun)
        solvePartyAux()

  solvePartyAux()

//solve_SPOJ_PARTY ()