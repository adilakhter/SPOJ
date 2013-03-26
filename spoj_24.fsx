open System
open System.Numerics

let init() = Array.init 158 (fun x -> if x = 0 then 1 else 0)

let minLength (a:int array) : int =
    let rec m' (a:int array) i  =
      match a.[i-1] with 
      | 0 -> m' a (i-1)
      | _ -> i 
    in 
    m' a a.Length
     
let toString' (a:int array)  = 
  let s = System.Text.StringBuilder()
  let mLength = minLength a 
  for i=(mLength-1) downto 0 do 
    s.Append(a.[i])|>ignore
  s.ToString()

let (^*) multiplier (multiplicand:int array)= 
  let rec multiplyOPT (a:int array) b i acc length=
    let l = Math.Min(length,a.Length)
    if i< l then 
      let r = a.[i] * b + acc
      a.[i]  <- r%10
      multiplyOPT a b (i+1) (r/10)  length    
    else
      a
  in
  multiplyOPT multiplicand multiplier 0 0  (multiplicand |>minLength |> (+) 2)  
let factorial n = 
  let rec fac n r = 
     match n with 
     | 0 -> toString' r 
     | _ -> fac (n-1) (n^*r)    
  fac n (init()) 
  

let intFactorial (i : uint64) = 
  let rec f (i:uint64) (acc:uint64) = 
    match i with 
      | 0UL -> acc 
      | i -> f (i-1UL) (acc*i)
  in
    f i 1UL


let computeFactorial (n:int) = 
  if n > 20 then
   factorial n
  else 
    (intFactorial (n |> uint64)).ToString(); 

let solveSpoj24() = 
  let rec solveLines currentLine maxLines =
    if currentLine < maxLines then 
      System.Console.ReadLine() 
      |> int 
      |> computeFactorial 
      |> printfn "%s"
      solveLines (currentLine+1) maxLines
  in
  match Console.ReadLine() |> Int32.TryParse with 
  | (true, i) when i > 0 -> solveLines 0 i 
  | _ -> ()
solveSpoj24() // solving spoj 24
  
//Test 
let testN (n:int): unit =
  let expected = ([1I..System.Numerics.BigInteger(n)] |> List.fold (fun acc x -> x*acc) 1I).ToString()
  let actual = computeFactorial n
  if String.Compare(expected,actual) <> 0  then 
    printfn "%d : %s <> %s" n  expected actual 
  else
    printfn "%d : %s =  %s" n  expected actual 

//
//for i = 1 to 21 do
//  testN i
//
//



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
  result.ToString()

//  let rec multiply' (a:int array) b i acc =
//    if i<a.Length then 
//      let r = a.[i] * b + acc
//      a.[i]  <- r%10
//      multiply' a b (i+1) (r/10)    
//    else
//      a
  // Invoking multiplication method  
  // multiply' multiplicand multiplier 0 0    
  //


  

    

    