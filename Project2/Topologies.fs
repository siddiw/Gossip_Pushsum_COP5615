module Topologies

open System
open Akka.Actor
open Akka.FSharp


let findFullNeighboursFor (pool:list<IActorRef>, index:int, numNodes:int) =
    let neighbourArray = pool |> List.indexed |> List.filter (fun (i, _) -> i <> index-1) |> List.map snd
    neighbourArray


let findLineNeighboursFor (pool:list<IActorRef>, index:int, numNodes:int) =
    let mutable neighbourArray = []
    printfn "\n pool size in line = %d\n" pool.Length
    // X-axis neighbours
    if index <> 0 then
       neighbourArray <- pool.[index-1] :: neighbourArray 
       //neighbourArray <- pool.[index] :: neighbourArray 
    if index <> numNodes-1 then
        neighbourArray <- pool.[index+1] :: neighbourArray 
        //neighbourArray <- pool.[index+2] :: neighbourArray 
    neighbourArray


let find2DNeighboursFor (pool:list<IActorRef>, index:int, side:int, numNodes:int) =
    let mutable neighbourArray = []
    printfn "\n pool size in find2dneighbours = %d\n" pool.Length
    //printfn "side = %d and incomingindex "
    // X-axis neighbours
    if index % side <> 0 then
       neighbourArray <- pool.[index-1] :: neighbourArray 
    if index % side <> side - 1 then
        neighbourArray <- pool.[index+1] :: neighbourArray 
    // Y-axis neighbours
    if index - side >= 0 then 
        neighbourArray <- pool.[index-side] :: neighbourArray
    if (index + side) <= numNodes - 1 then 
        neighbourArray <- pool.[index+side] :: neighbourArray
    neighbourArray


// to correct
let find3DNeighboursFor (pool:list<IActorRef>, index:int, side:int, sidesquare:int, numNodes:int) =
    let mutable neighbourArray = []
    // X-axis neighbours
    if index % side <> 1 then
       neighbourArray <- pool.[index-1] :: neighbourArray 
    if index % side <> 0 then
        neighbourArray <- pool.[index+1] :: neighbourArray 
    // Y-axis neighbours
    if index % sidesquare > side then 
        neighbourArray <- pool.[index-side] :: neighbourArray
    if sidesquare - (index % sidesquare) > side then 
        neighbourArray <- pool.[index+side] :: neighbourArray
    // Z-axis
    if index > sidesquare then 
        neighbourArray <- pool.[index-sidesquare] :: neighbourArray
    if (numNodes - index) < sidesquare then 
        neighbourArray <- pool.[index+sidesquare] :: neighbourArray
    neighbourArray