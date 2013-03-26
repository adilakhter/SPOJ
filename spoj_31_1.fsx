open System

let initArray len = Array.create len 0   

(*
Adds two numbers of any length and returns the result as an int array.
*)
let add (num1:int[]) (num2:int[]) = 
  let f x y ((acc:int[]),(i:int),(rem:int)) = 
    acc.[i] <- ((x+y)%10) + rem
    (acc, (i-1), ((x+y)/10)) 
  
  let (acc, i, rem) = Array.foldBack2 (fun x y (acc,i,rem) -> f x y (acc,i,rem)) num1 num2 ((initArray num1.Length), (num1.Length-1),0)  
  match rem with 
  | 0             -> acc 
  | _             -> Array.append [|rem|] acc  


let toInt (num:int[])  = Array.fold (fun acc x-> ((acc*10)+x)) 0 num 


let ctoi (c:char)   =  (int)c

let toNumArray (s:string) = 
  s
  |> (fun x -> x.ToCharArray())
  |> Array.map (fun x -> ctoi x - 48)

let eqArray len (a:int array) = 
  if a.Length >= len then 
    a
  else
    let (acc: int[]) = initArray len
    Array.foldBack (fun elem ((acc:int array),(i:int)) -> 
        acc.[i]<- elem;
        (acc,(i-1))
      ) a (acc,(len-1)) |> ignore
    acc

let maxLength num1 num2 = 
  let length (x:string) = x.Length
  Math.Max (num1|>length ,  num2|>length)
 
 
let execKaratsubaAlgorithm mutiplier multiplicand = 
  let rec run num1 num2 = 
    num1, num2 
  in 
    run mutiplier multiplicand   

let multiply num1 num2 = 
  let maxLength = maxLength num1 num2
  let multipler   = num1|> toNumArray |> eqArray maxLength 
  let mutiplicand = num2|> toNumArray |> eqArray maxLength 
  multipler , mutiplicand

let test() = 
  for i = 0 to 1000 do 
    for j = 0 to 1000 do 
      let l = maxLength (i |> string)  (j |> string) 
      
      let i' = i |> string |> toNumArray |> eqArray l
      let j' = j |> string |> toNumArray |> eqArray l  

      let actual = add i' j' |> toInt 

      if (actual <> (i+j)) then
        printfn "%d <> %d" actual (i+j)
      else
        printfn "%d = %d" actual (i+j)
   
  

let explode (s:string) =
  [for c in s -> c]

let implode (xs:char list) = 
  let sb = System.Text.StringBuilder()
  xs |> List.iter (sb.Append >> ignore)
  sb.ToString()

