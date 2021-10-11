module Main

open Akka
open System
open Topologies
open Schedulers
open Akka.Actor
open Akka.Actor.Scheduler
open Akka.FSharp
open System.Diagnostics
open System.Collections.Generic
open Akka.Configuration

// Configuration
let configuration = 
    ConfigurationFactory.ParseString(
        @"akka {            
            stdout-loglevel : ERROR
            loglevel : ERROR
            log-dead-letters = 0
            log-dead-letters-during-shutdown = off
        }")

let gossipSystem = ActorSystem.Create("GossipSystem", configuration)
let mutable mainActorRef = null

type MainCommands =
    | Start of (string)
    | Stop of (string)
    | FindNeighbours of (list<IActorRef> * string)
    | BuildTopology of (string)
    | GossipActorStoppedTransmitting of (string)
    | StartAlgorithm of (string)
    | GossipMessage of (int*string)
    | Round
    | IReceivedRumour of (int)
    | IFoundNeighbours of (int)
    | PushsumMessage of (int*float*float)
    | InitiatePushsum
    | PushsumActorConverged of (int*float*float)

//*** Globals ***//
let mutable numNodes = 27000
let mutable topology = "3D"
let mutable algorithm = "pushsum"
let mutable actorsPool = []

let proc = Process.GetCurrentProcess()
let cpuTimeStamp = proc.TotalProcessorTime
let timer = Stopwatch()

let dictionary = new Dictionary<IActorRef, bool>()

// For 3D grid, rounding off the total nodes to next nearest cube
let roundOff3DNodes (numNodes:int) =
    let sides = numNodes |> float |> Math.Cbrt |> ceil |> int
    pown sides 3

// For 2D grid, rounding off the total nodes to next nearest square
let roundOff2DNodes (numNodes:int) =
    let sides = numNodes |> float |> sqrt |> ceil |> int
    pown sides 2



// Each actor (node) will call this function to maintain its own list of neighbours
let findMyNeighbours (pool:list<IActorRef>, topology:string, myActorIndex:int) = 
    let mutable myNeighbours = []
    match topology with 
    | "line" ->
        printfn "Calling findLineNeighboursFor %d\n" myActorIndex
        myNeighbours <- Topologies.findLineNeighboursFor(pool, myActorIndex, numNodes)
    | "2d" ->
        let side = numNodes |> float |> sqrt |> int
        myNeighbours <- Topologies.find2DNeighboursFor(pool, myActorIndex, side, numNodes)
    | "3d" ->
        let side = numNodes |> float |> Math.Cbrt |> int
        let sidesquare = side * side
        myNeighbours <- Topologies.find3DNeighboursFor(pool, myActorIndex, side, sidesquare, numNodes)
    | "full" ->
        myNeighbours <- Topologies.findFullNeighboursFor(pool, myActorIndex, numNodes)
    | _ -> ()
    myNeighbours


let GossipActor (id:int) (mailbox:Actor<_>) =
    printfn "Entered GossipActor %d with name %s\n" id mailbox.Self.Path.Name
    let mutable myNeighboursArray = []
    //let mutable waitingFirst = 1
    let mutable myRumourCount = 0
    let myActorIndex = id
    let mutable isActive = 1
 
    let rec loop () = actor {
        //printfn "\n "
        if isActive = 1 then
            let! (message) = mailbox.Receive()
            //let sender = mailbox.Sender()

            //printfn "\n %d received something from %s\n" id sender.Path.Name
            match message with 
            | FindNeighbours(pool, topology) ->
                myNeighboursArray <- findMyNeighbours(pool, topology, myActorIndex)
                mainActorRef <! IFoundNeighbours(myActorIndex)
            | GossipMessage(fromNode, message) ->
                if myRumourCount = 0 then
                    mainActorRef <! IReceivedRumour(myActorIndex)
                    mailbox.Self <! Round

                myRumourCount <- myRumourCount + 1
                if myRumourCount > 10 then
                    isActive <- 0
                    mainActorRef <! GossipActorStoppedTransmitting(mailbox.Self.Path.Name) 
            | Round(_) ->
                // Randomly select a neighbour
                //printfn "Round of %d\n" id
                //if myRumourCount < 11 then
                    let randomNeighbour = Random().Next(myNeighboursArray.Length)
                    let randomActor = myNeighboursArray.[randomNeighbour]
                    // Send GossipMessage to it
                    randomActor <! GossipMessage(myActorIndex, "rumour")
                    gossipSystem.Scheduler.ScheduleTellOnce(TimeSpan.FromMilliseconds(10.0), mailbox.Self, Round, mailbox.Self)
                
            | _ -> printfn "**********gadbad\n"

            return! loop()
        }
    loop()


let PushsumActor (id:int) (mailbox:Actor<_>) =
    let mutable myNeighboursArray = []
    let mutable myActiveNeighbours = []
    let myActorIndex = id
    let mutable s = id + 1 |> float
    let mutable w = 1 |> float
    let mutable prevRatio = s/w
    let mutable towardsEnd = 0
    let mutable isActive = 1
    let mutable incomingsList = []
    let mutable incomingwList = []
    let mutable s_aggregateTminus1 = 0.0
    let mutable w_aggregateTminus1 = 0.0
    let mutable currRatio = s/w
    let mutable count = 0
 
    let rec loop () = actor {
        if isActive = 1 then    
            let! (message) = mailbox.Receive()

            match message with 
            | FindNeighbours(pool, topology) ->
                myNeighboursArray <- findMyNeighbours(pool, topology, myActorIndex)
                myActiveNeighbours <- myNeighboursArray
                mainActorRef <! IFoundNeighbours(myActorIndex)
                printfn "%d found neighbours \n" myActorIndex

            | Round(_) ->
                
                // Step 2: s <- Σ(incomingsList) and w <- Σ(incomingwList)
                // todo
                s <- s_aggregateTminus1
                w <- w_aggregateTminus1

                // Step 3: Choose a random neighbour
                let randomNeighbour = Random().Next(0, myActiveNeighbours.Length)
                let randomActor = myActiveNeighbours.[randomNeighbour]

                // Step 4: Send the pair ( ½s , ½w ) to randomNeighbour and self
                randomActor <! PushsumMessage(myActorIndex , (s/2.0) , (w/2.0))
                mailbox.Self <! PushsumMessage(myActorIndex , (s/2.0) , (w/2.0))

                currRatio <- s / w
                // Check if converged? only when you had atleast 1 incoming message from a neighbour
                //if count <> 1 then
                if (abs(currRatio - prevRatio)) < (pown 10.0 -10) then 
                    towardsEnd <- towardsEnd + 1
                else 
                    towardsEnd <- 0
                
                if towardsEnd = 15 then 
                    mainActorRef <! PushsumActorConverged(myActorIndex, s,w)
                    isActive <- 0

                prevRatio <- currRatio

                // Reset the aggregate back to 0 after each round
                s_aggregateTminus1 <- 0.0
                w_aggregateTminus1 <- 0.0
                count <- 0
                       
            | PushsumMessage (fromIndex, incomings, incomingw) ->
                count <- count + 1
                incomingsList <- incomings :: incomingsList
                incomingwList <- incomingw :: incomingwList

                s_aggregateTminus1 <- s_aggregateTminus1 + incomings
                w_aggregateTminus1 <- w_aggregateTminus1 + incomingw

            | InitiatePushsum ->
                mailbox.Self <! PushsumMessage(myActorIndex , s , w)
                gossipSystem.Scheduler.ScheduleTellRepeatedly(TimeSpan.FromSeconds(0.0),TimeSpan.FromMilliseconds(50.0), mailbox.Self, Round)
 
            | _ -> ()

            return! loop()
        }
    loop()

let MainActor (mailbox:Actor<_>) =
    printfn "Entered MainActor\n"
    
    let mutable actorsDone = 0
    let mutable actorsThatKnow = 0
    let mutable topologyBuilt = 0

    let rec loop () = 
        actor {
            let! (message) = mailbox.Receive()

            match message with 
            | BuildTopology(_) ->
                printfn "BuildTopology"
                if algorithm = "gossip" then    
                    actorsPool <-
                        [0 .. numNodes-1]
                        |> List.map(fun id -> spawn gossipSystem (sprintf "GossipActor_%d" id) (GossipActor id))
                else
                    actorsPool <- 
                        [0 .. numNodes-1]
                        |> List.map(fun id -> spawn gossipSystem (sprintf "PushsumActor_%d" id) (PushsumActor id))

                actorsPool |> List.iter (fun item -> 
                    item <! FindNeighbours(actorsPool, topology))

            | IFoundNeighbours(index) ->
                topologyBuilt <- topologyBuilt + 1
                if topologyBuilt = numNodes then 
                    timer.Start()
                    mainActorRef <! StartAlgorithm(algorithm)
            | StartAlgorithm(algorithm) ->
                //start algo
                match algorithm with 
                | "gossip" ->
                    // Randomly select a neighbour
                    let randomNeighbour = Random().Next(actorsPool.Length)
                    //let randomActor = actorsPool.[randomNeighbour]
                    let randomActor = actorsPool.[1]

                    // Send GossipMessage to it
                    printfn "sending firstrumour to %d\n" randomNeighbour
                    randomActor <! GossipMessage(0, "theRumour")
                | "pushsum" ->
                    // Initialize all
                    actorsPool |> List.iter (fun item -> 
                        item <! InitiatePushsum)
                | _ -> ()
            | GossipActorStoppedTransmitting(actorName) ->
                actorsDone <- actorsDone + 1
                //printfn "%s Terminated | Total terminated = %d" actorName actorsDone
                (*if actorsDone = numNodes then 
                    printfn "\n SAB TERMINATED\n"
                    timer.Stop()
                    printfn "Total time = %dms" timer.ElapsedMilliseconds
                    Environment.Exit(0) *)
            | IReceivedRumour(actorIndex) ->
                actorsThatKnow <- actorsThatKnow + 1
                printfn "%d knows! Total = %d\n" actorIndex actorsThatKnow
                if actorsThatKnow = numNodes then 
                    timer.Stop()
                    printfn "\nTotal time = %dms | Total Terminated = %d\n" timer.ElapsedMilliseconds actorsDone
                    printfn "\n SABKO PATA CHAL GAYA\n"
                    Environment.Exit(0) 
            | PushsumActorConverged (index, s, w) ->
                actorsDone <- actorsDone + 1
                printfn "id = %d | s = %f | w = %f | s/w = %f | Total terminated = %d"  index s w (s/w) actorsDone
                if actorsDone = numNodes then 
                    printfn "\n SAB CONVERGED\n"
                    timer.Stop()
                    printfn "Total time = %dms" timer.ElapsedMilliseconds
                    Environment.Exit(0)
            | _ -> ()

            return! loop()
        }
    loop()


[<EntryPoint>]
let main argv =
    printfn "\n argv.len = %d" argv.Length

    if (argv.Length <> 3) then printfn "Starting with default values" 
    else 
        topology <-  argv.[1]
        algorithm <- argv.[2]
        if topology = "2D" || topology = "imp2D" then numNodes <-  argv.[0] |> int |> roundOff2DNodes
        else if topology = "3D" || topology = "imp3D"  then numNodes <-  argv.[0] |> int |> roundOff3DNodes
        else numNodes <- argv.[0] |> int
   
    printfn "\n1 - %s 2 - %s 3 - %s" (numNodes.ToString()) topology algorithm

    mainActorRef <- spawn gossipSystem "MainActor" MainActor
    printfn "mainActorRef created\n"
    mainActorRef <! BuildTopology("start")

    gossipSystem.WhenTerminated.Wait()

    0