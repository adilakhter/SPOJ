(* 
  Problem URI : http://www.spoj.com/problems/MUL/ 
  Date        : 21.01.2013
*)

open System

let initArray len  = Array.create len 0 

//let implode = Array.fold  (fun acc x -> acc*10+x) 0

let implode = Array.fold  (fun acc x -> acc*10+x) 0

let toInt32 (c:char)= c |> int

let toArray (s:string) = s |> (fun x-> x.ToCharArray()) |> Array.map (fun x -> toInt32 x - 48)

(*  
  Computes addition of two number represented as arrays.
  Applicable for large integer addition. 
*)
let add (num1:int[]) (num2:int[]) = 
  let accumulator = initArray num1.Length
  let f x y ((acc:int[]),(i:int),(carry:int)) =
    acc.[i] <- (x+y) % 10 + carry
    acc,i-1, (x+y)/10
  
  // computing addition by using function f
  let (acc, currentIndex, carry) = Array.foldBack2  
                                    (fun x y (acc,index,carry) -> 
                                    printfn "%A" (acc,index,carry)
                                    f x y (acc,index,carry)) 
                                    num1  
                                    num2 
                                    (accumulator,(num1.Length-1),0)   
  // checking whether carry is more than 0 
  // If yes, appending it at the front     
  match carry with
  | 0  -> acc  // returning accumulator as it contains the result
  | _  -> Array.append [|carry|]  acc // appending carry 



(*
  Append zeros infront of array b. It appends zeroes till the length becomes  maxlength.
*)
let appendZeros maxLength (b:int[]) = 
  if b.Length >= maxLength then 
    b // do nothing 
  else
    let (tempB: int[]) = initArray maxLength // creating a temp array
    Array.foldBack (fun elem ((temp:int array),(i:int)) -> 
        temp.[i]<- elem;
        (temp,i-1)
    ) b (tempB,maxLength-1)|> ignore
    tempB

let makeEqualArrays ((a:int array), (b:int array)) =
  if a.Length = b.Length then 
    (a,b)
  else
    let maxLength =  Math.Max(a.Length,  b.Length)
    let a' = a |> appendZeros  maxLength 
    let b' = b |> appendZeros  maxLength 
    (a',b')




let pow10 (n:int) (m:int[])= 
  let (temp, index) = Array.fold  
                          (fun ((acc:int[]),index) x -> acc.[index]<-x;(acc,index+1)) 
                          (initArray (m.Length+n),0) 
                          m 
  for i = index to (m.Length+n-1)  do 
    temp.[i] <- 0
  temp

(*
Given 2 Number n and m, it derives- 
  n = x1*10^p+y1
  m = x2*10^p+y2
  
Then computes multiplication as follows.n*m  is given by-- 

  R = x1*x2*10^2p + x1*y2*10^p +x2*y1*10^p + y1*y2 
    = x1*x2*10^2p + (x1*y2+x2*y1)10^p + y1*y2
    = A*10^2p + B*10^p + C

 It computes A, B and C and then add them together to return the final result. We apply divide and conquer strategy to divide the problem into subproblems and  compute A B and C by recursively invoking this function. 
   
*)
let rec executeNaiveAlgorithm (multiplier':int[]) (multiplicand':int[]) =
  let maxLength =  Math.Max(multiplier'.Length,  multiplicand'.Length)
  let multipler   = multiplier'  |> appendZeros  maxLength 
  let mutiplicand = multiplicand'|> appendZeros  maxLength 
  multipler,mutiplicand



  

(*
  Multiply to large numbers, multiplier and multiplicand.
*)
let multiply (multiplier':string) (multiplicand':string)=
  
  executeNaiveAlgorithm
    (multiplier'  |> toArray) 
    (multiplicand'|> toArray) 

let parseLine() =
  let line = System.Console.ReadLine().Split()
  line.[0].ToCharArray()|> Array.rev, line.[1].ToCharArray()|> Array.rev
  
let solveSpoj31() = 
  let rec solveLines currentLine maxLines =
    if currentLine < maxLines then 
      parseLine()
      |> computeMultiplication 
      |> printfn "%s"
      solveLines (currentLine+1) maxLines
  in
  match Console.ReadLine() |> Int32.TryParse with 
  | (true, i) when i > 0 -> solveLines 0 i 
  | _ -> ()

  

   

     
