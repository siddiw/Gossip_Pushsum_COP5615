# Gossip Simulator and Pushsum Implementation 
The goal of this project is to determine the convergence of Gossip-type algorithms through a simulator based on Actor-model written in F# using the Akka.NET framework. Since actors in F# are fully asynchronous, the particular type of Gossip implemented is the so called Asynchronous Gossip. The implementation is for Gossip and PushSum algorithms with a configurable parameter to select one of the different topologies to simulate. Detailed description in Gossip_Project_Description.pdf.

## Project Members ##
* Mandar Palkar (UFID: 2140-6740)
* Siddhi Wadgaonkar (UFID: 9544-2212)

## Requirements: ##
The project was tested with
* dotnet SDK - version 5.0.401
* Akka - version 1.4.25
* Akka.FSharp - version 1.4.25

## Steps to run: ##
Run following commands in your console.

### To Run 'Server'###
* ``cd Server``
* ``dotnet run {nodecount} {topology} {algorithm}`` or ``dotnet run`` 
* for e.g ``dotnet run 1000 line gossip``

### Largest number of Nodes We ran Gossip with
* ``Line - 5000``
* ``Full - 45000``
* ``3D - 45000``
* ``Imp3D - 45000``

### Largest number of Nodes We ran Pushsum with
* ``Line - 1000``
* ``Full - 15000``
* ``3D - 15000``
* ``Imp3D - 15000``



