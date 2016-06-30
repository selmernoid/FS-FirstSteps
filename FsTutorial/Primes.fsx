#load "../packages/FSharp.Charting.0.90.14/FSharp.Charting.fsx"
open FSharp.Charting
open FSharp.Data
open System

let round (x:float) = int x

let primes max =
    let isPrime x list = list |> Seq.forall (fun i -> x % i <> 0) 
    [for i in [2..max] do if isPrime i [2.. int (Math.Sqrt(float i))] then yield i ]

let list = primes 1000000

let data = (primes 10000000) 
            |> Seq.mapi (fun i x -> x, ((list.Item (Math.Min(i+1, list.Length - 1))) - x)) 
            |> Seq.filter ( fun i -> snd(i) >= 1 )  
            //|> Seq.map (fun i -> fst(i), snd(i)/2)
//let sumList list count = list |> Seq.take count |> Seq.sum
let s f state item = f(state) + f(item)
let dbl a = a, a
let eachOfTuple f x y = f fst x y, f snd x y
let sum = fst( Seq.mapFold (fun state item -> dbl (eachOfTuple s state item)) (0,0) data) 
    


Chart.Point data
Chart.Point sum



let log = [2..10000] 
            |> Seq.map(fun x -> x, Math.Log(float x))
let logCoeff coeff= log |> Seq.map (fun x -> fst(x) * coeff, snd(x))
let logFun f = log |> Seq.map (fun x -> f(fst(x)), snd(x))

Chart.Point log 
Chart.Point (logCoeff 20)
Chart.Point (logFun (fun x -> Math.Pow(float x, Math.PI/10.0)))
Chart.Point (logFun (fun x -> Math.Pow(float x,0.3)))

let pows f = [2..10000] 
            |> Seq.map(fun x -> x, f(float x))
Chart.Point (pows (fun x -> Math.Pow(float x, Math.PI/10.0)))
Chart.Point (pows (fun x -> Math.Pow(float x,0.6)))
