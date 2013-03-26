(*
Problem Definition  
===================

SPOJ 31. Fast Multiplication
-----------------------------------------
[URL](https://www.spoj.com/problems/MUL/)

###Input
5
4 2
123 43
324 342
0 12
9999 12345

###Output
8
5289
110808
0
123437655
*)

open System

let initResult len = Array.create len 0   

let toString (a:int array)  = 
  let s = System.Text.StringBuilder()
  let b = true; // flag to check trailing 0s 
  let result,b = Array.foldBack 
                    (fun i ((s:System.Text.StringBuilder),(b:bool))  -> 
                      if b = true && i = 0 then 
                        (s.Append(""),b) 
                      else
                        (s.Append(i),false) 
                    ) 
                    a 
                    (s,true)       
  if result.ToString()  = "" then "0" else result.ToString()

let computeMultiplication ((multiplier:char array), (multiplicand:char array)) = 
  let multiply (m:char array) (n:char array) (result:int array)= 
    let mutable reminder = 0 
    for i = 0 to (m.Length-1) do // mutable reminder 
      for j = 0 to (n.Length-1) do  
        let r = ((((int)m.[i])-48)*(((int)n.[j])-48)) + result.[i+j] + reminder
        result.[i+j] <- r%10
        reminder     <- r/10
      result.[i+n.Length] <- result.[i+n.Length]+reminder
      reminder <- 0 
    result

  multiply 
    multiplier 
    multiplicand  
    (initResult (multiplicand.Length+multiplier.Length))  // result array 
  |>toString //Array.fold (fun acc x -> x.ToString()+acc) ""


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
solveSpoj31()


//--------------------------------------------------
let test' m n = 
  let m',n' = m |> string , n|> string 
  let result = computeMultiplication (m'.ToCharArray() |> Array.rev, n'.ToCharArray() |> Array.rev)
  let expected = (m *n) |> string
  String.Compare(result,expected), result,expected 



let test() = 
 for i = 1 to 9999 do 
  for j = 1 to 99999  do
    let r1 = i * j 
    let r2 = computeMultiplication (i.ToString().ToCharArray() |> Array.rev, j.ToString().ToCharArray() |> Array.rev)
    if String.Compare(r1.ToString(), r2) <> 0 then 
      printfn "%d*%d %d<>%s" i j r1 r2
