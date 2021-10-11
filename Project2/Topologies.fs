module Topologies

open System
open Akka.Actor
open Akka.FSharp

// Full/Mesh Topology
// All nodes are connected to all other nodes.
let findFullNeighboursFor (pool:list<IActorRef>, index:int, numNodes:int) =
    let neighbourArray = pool |> List.indexed |> List.filter (fun (i, _) -> i <> index) |> List.map snd
    neighbourArray


// Line Topology
// Single dimensional line. Max 2 neighbours possible - X-axis : left & right.
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


// 2D Grid Topology
// Simple square topology. Max 4 neighbours possible - X-axis : left & right | Y-axis : top & bottom.
let find2DNeighboursFor (pool:list<IActorRef>, index:int, side:int, numNodes:int, isImproper:bool) =
    let mutable neighbourArray = []
    let mutable neighbourIndexArray = []
    printfn "\n pool size in find2dneighbours = %d\n" pool.Length
    //printfn "side = %d and incomingindex "
    // X-axis neighbours
    if index % side <> 0 then
       neighbourArray <- pool.[index-1] :: neighbourArray
       neighbourIndexArray <- index-1 :: neighbourIndexArray
    if index % side <> side - 1 then
        neighbourArray <- pool.[index+1] :: neighbourArray 
        neighbourIndexArray <- index+1 :: neighbourIndexArray
    // Y-axis neighbours
    if index - side >= 0 then 
        neighbourArray <- pool.[index-side] :: neighbourArray
        neighbourIndexArray <- index-side :: neighbourIndexArray
    if (index + side) <= numNodes - 1 then 
        neighbourArray <- pool.[index+side] :: neighbourArray
        neighbourIndexArray <- index+side :: neighbourIndexArray

    if isImproper then
        let r = System.Random()
        let randomNeighbour =
            pool
            |> List.filter (fun x -> (x <> pool.[index] && not (List.contains x neighbourArray)))
            |> fun y -> y.[r.Next(y.Length - 1)]
        neighbourArray <- randomNeighbour :: neighbourArray

    neighbourArray


// 3D Grid Topology
// Simple cube topology. Max 6 neighbours possible - X-axis : left & right | Y-axis : top & bottom | Z-axis : front & back
let find3DNeighboursFor (pool:list<IActorRef>, index:int, side:int, sidesquare:int, numNodes:int, isImproper:bool) =
    let mutable neighbourArray = []
    let mutable neighbourIndexArray = []
    // X-axis neighbours
    if (index % side) <> 0 then
        //printfn "%d left = %d\n" index (index-1)
        neighbourArray <- pool.[index-1] :: neighbourArray 
        neighbourIndexArray <- index-1 :: neighbourIndexArray
    if (index % side) <> (side - 1) then
        //printfn "%d right = %d\n" index (index+1)
        neighbourArray <- pool.[index+1] :: neighbourArray 
        neighbourIndexArray <- index+1 :: neighbourIndexArray
    // Y-axis neighbours
    if index % sidesquare >= side then 
        //printfn "%d top = %d\n" index (index-side)
        neighbourArray <- pool.[index-side] :: neighbourArray
        neighbourIndexArray <- index-side :: neighbourIndexArray
    if sidesquare - (index % sidesquare) > side then 
        //printfn "%d bottom = %d\n" index (index+side)
        neighbourArray <- pool.[index+side] :: neighbourArray
        neighbourIndexArray <- index+side :: neighbourIndexArray
    // Z-axis
    if index >= sidesquare then 
        //printfn "%d front = %d\n" index (index-sidesquare)
        neighbourArray <- pool.[index-sidesquare] :: neighbourArray
        neighbourIndexArray <- index-sidesquare :: neighbourIndexArray
    if (numNodes - index) > sidesquare then 
        //printfn "%d back = %d\n" index (index+sidesquare)
        neighbourArray <- pool.[index+sidesquare] :: neighbourArray
        neighbourIndexArray <- index+sidesquare :: neighbourIndexArray

    // if the topology is improper, then add one additional random node in addition to regular neighbours
    // TODO - trial
    if isImproper then
        let r = System.Random()
        let randomNeighbour =
            pool
            |> List.filter (fun x -> (x <> pool.[index] && not (List.contains x neighbourArray)))
            |> fun y -> y.[r.Next(y.Length - 1)]
        neighbourArray <- randomNeighbour :: neighbourArray

    neighbourArray