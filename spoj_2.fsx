open System
let parseLine() =
  let line = System.Console.ReadLine().Split()
  line.[0] |> int, line.[1] |> string 

let rec gcd a b = 
  match b with 
  | 0 -> a 
  | _ -> gcd b (a%b)


let remainder (b:string) (a:int)=
  b.ToCharArray()
  |> Array.fold (fun acc x -> ((acc*10 + (((int)x)-48))%a)) 0


let rec solveLines currentLine maxLines =
  if currentLine < maxLines then 
      let num1,num2 = parseLine()
      match num1,num2  with 
      | 0,_ -> printfn "%s" num2
      | _   -> 
            remainder num2 num1
            |> gcd num1
            |> printfn "%d" 
      solveLines (currentLine+1) maxLines


let solveSpoj2906() = 
  match Console.ReadLine() |> Int32.TryParse with 
  | (true, i) when i > 0 -> solveLines 0 i 
  | _ -> ()

solveSpoj2906()  






