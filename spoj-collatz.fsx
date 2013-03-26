let max = 1000000L
let memo = Array.create ((max|>int)+1) 0L

let nextCollatz (x:int64) :int64= 
  match x%2L with 
  | 0L -> x/2L
  | _ -> 3L*x+1L 

let seqLength (n:int64) (length:int64) =
  match n with 
  | _ when n = 1L ->  length+1L
  | _             ->
    if n < max && memo.[n] <> 0 then // memo contains seq length
      memo.[(n|>int)]
    else
      let length = seqLength (nextCollatz n) (length+1)
    
    

  
