MWRouting
=========

This is a Scala project that provides an implementation of the Multiplicative Weights Algorithm (MWA) for online learning.

The algorithm, also called the experts algorithm, has many applications, but this project is intrested in particular in applications to game theory and repeated games, including routing games. This implementation is useful in particular for observing trajecories of player strategies, as well as studying the effects of tha algorithm parameters on convergence.

### Repeated game and no regret-learning
In the most general form, we consider a game in which many populations of players interact. On each day, every player chooses an action, and observes a loss (a cost) at the end of the day. The loss of a player may depend on the actions of other players, as well as external factors that do not directly depend on players' actions. In other words, nature may affect the losses of players. Each player seekes to minimize her cumulative loss.

The idea of the MW algorithm is to maintain a probability distribution over the action set of the player, such that on each day, the player draws an action from that distribution, and at the end of that day, after observing the losses, updates the probability distribution by putting more weight on the actions that performed well. The distribution on a given day is called the strategy of the player on that day.

The MW algorithm generates a sequence of strategies that has the following guarantee: its expected cumlative loss is within a small distance of the cumulative loss of the best constant strategy in hindsight. In other words, once the sequence of losses is revealed, we can compute, after the fact, the best constant strategy, and compare it to the performace of the algorithm. The difference in average cumulative loss (called the regret of the algorithm), is guaraneed to have a small upper bound. The upper bound can be made arbitrarily small with an appropriate choice of two important parameters: the number of iterations (or number of days the repeated game is played), and a learning rate which specifies how much one puts weight on past observations.

For a very nice introduction to the MW algorithm, see [Professor Rao's notes](http://www.cs.berkeley.edu/~satishr/cs270/sp13/rough-notes/Lecture4/umesh-experts-lecture.pdf).

For an extensive survey on the MW algorithm and its applications, see [Arora, Hazan, Kale: [The Multiplicative Weights Update Method: A Meta-Algorithm and its Applications](http://www.cs.princeton.edu/~arora/pubs/MWsurvey.pdf)


### Selfish Routing
One particularly interesting application of the MW algorithm is the selfish routing game. In this game, we are given a graph and a set of players, where each player seeks to send one unit of flow from a given source node to a given destination node (sources and destinations may differ from player to player). Each player seeks to minimize the total latency, i.e. the total time it takes to send the flow. The total latency on a path is the sum of edge latencies, and the edge latency depends on the total flow on that edge (including flow of other players). This game can be used to model drivers on a road network, for example. 

On each day, the strategy of a player is an allocation of the unit flow over possible paths. If every player uses the MW algorithm, and if the sequence of learning rates satisfies a mild assumption, we show that the combined strategies of the players converges to the Nash equilibrium of the game. This proves in particular that players can converge to the Nash equilibrium 
- in a decentralized way (no coordination required)
- using a simple to implement algorithm
- with minimal information: they do not need to know the latency functions, only to observe the resulting path latency at the end of each day.

More details on the topic will be posted here soon:
[http://www.eecs.berkeley.edu/~walid/projects/mwrouting]()

A talk that I gave on multiplictive weights algorithms and application to Routing:
[http://www.eecs.berkeley.edu/~walid/talks/experts-talk.pdf]()


-------------------------------------------------------------------------------
### Setup: running the code
First, you should install the Scala Simple Build Tool, or `sbt`. If you are on Mac and have `brew`, you can simply `brew install sbt`. For other options see [http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html]((http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html)).

You can then build `MWRouting` by running

```
sbt compile
```
The first time you run `sbt`, it make take a couple of minutes since it will fetch the dependencies.

Then you can either run the project from commandline

```
sbt run
```
or run it from your favorite IDE. For example to open in [Eclipse with the Scala plugin](http://scala-ide.org/download/sdk.html):
1. `sbt eclipse` to generate the project files
2. In Eclipse: file>import>general>existing projects and select the root directory where you cloned MWRouting.




------------------------------------------------------------------------------
### Implementation overview
The main classes are:
- `Game`: this is an abstract class, that represents the game in which players interact. This class provides two important methods:
  - `update`, which updates the state of the game given the previous state and the strategies of the `MWAlgorithm` instances playing the game.
  - `loss`, which specifies the loss of each expert (action), given the current game state.
- `MWAlgorithm`: the class that stores the set of  `experts` (actions), and the logic for updating the strategy given a vector of losses. Other parameters of the class include 
  - an instance of the `LearningRate` class, which specifies a sequence of learning rates (a decreasing sequence of learning rates means that the algorithm will be less agressive in its updates as time increases. This helps guarantee convergence of the algorithm)
  - an instance of the `UpdateRule` class, which specifies which multiplicative update rule should be used. Several rules are implemented in the `UpdateRule.scala` file.
- `MWCoordinator`: this class takes an instance of `Game` and an array of `MWAlgorithm`, and coordinates the algorithms with the game. This consists in iteratively applying the following steps:
  - update the game state using the previous strategies of the algorithms.
  - get the losses from the game given the current game state
  - update the strategies of each algorithm instance given the current losses
Finally, `MWCoordinator` defines a number of streams that contain the sequence of strategies and game states. The most important ones are:
- `strategiesStream`
- `lossStream`
- `gameStateStream`

Note about the use of `Stream`s: defining the sequences of states as `Stream`s allows us to easily express each sequence as functional transformations of other sequences. This does not incur any significant increase in time complexity, since each Stream is immutable and is not recomputed. Also Stream's are evaluated lazily, so theses Streams are only evaluated when needed.


------------------------------------------------------------------------------
### Example: RoutingGame
To implement a routing game, we create a class `RoutingGame` that `extends Game`. The description of the game is contained in `network`, an instance of `LatencyNetowrk`, which contains in particular
- the topology of the graph
- the edge latency functions
- the commodities (source-sink pairs and flow demands)
- and methods for computing the flows and latencies.

The RoutingGame class then specifies
- a `State` type that extends `GameType` (as required by the abstract class `Game`). The `State` type is simply a case class that stores the pathFlows and pathLatencies.
- the `update` method. This calls the appropriate methods in `network`


------------------------------------------------------------------------------
### Creating and running an instance of the RoutingGame
Here we walk through an example, given in `Main.scala`.

To create an instance of the routing game, we first need to create
- a Graph instance
- the corresponding latencies, stored in a `HashMap[Int, LatencyFunction]`
this can be done using a helper function as follows:

```
val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(adj)
```

where `adj` is an adjacency map, that specifies, for each node, its neighboring nodes, and the latency on the edge connecting it to that neighbor. 
The following example describes a parallel network:

```
val adj: Map[Int, List[(Int, LatencyFunction)]] = 
  Map(1->Nil,
      0->List(
        (1, SLF(x => 2 + 2 * x)), 
        (1, SLF(x => x * x)), 
        (1, SLF(x => 2 * (x + 1) * (x + 1) - 1))
        ))
```
here node `0` is connected to node `1` by multiple edges. The first edge has latency `SLF(x=>3*x).until(50).then(SLF(x=>3*x+4))`. Here `SLF` is an alias for `StaticLatencyFunction`, and as its name suggests, creates a latency function that is constant in time.

A note on latency functions: an instance `lat` of `LatencyFunction` can be applied to a `Double`, so `lat(f)` will return the latency for a flow value `f`. Lateny functions can depend not only on flow, but also on time (see the class `TimeDependentLatencyFunction` for details)

Once we have a `graph` and its `latencies`, we need to choose some additional parameters:
- `val updateRule = ExponentialUpdate()` specified the update rule
- `val learningRate = HarmonicLearningRate(1.)` specifies that we use a sequence of learning rates that decreases as 1/t
- `val commodity = Commodity(0, 1, flowDemand, epsilon, updateRule, graph.findLooplessPaths(0, 1))` describes the commodity. Here 
  - the first two fields specify the source and the sink. 
  - `flowDemand` is the flow that we need to send from the source to the sink (an instance of `FlowDemand`, which behaves essentially as a `Stream[Double]`). 
  - `graph.findLooplessPaths(0, 1)` returns all the paths that connect the source to the sink. One could potentially restrict the set of paths that are offered to the players by manually giving a set of paths.

Finally, a helper class, `RoutingGameSim`, takes care of creating the game, an instance of MWAlgorithm and an instance of MWCoordinator

```
val sim = new RoutingGameSim(graph, latencyFunctions, Array(commodity), randomizedStart)
```

then `sim.runFor(T)` will run the game for `T` iterations, and generate a number of plots:
- the trajectory of the algorithm strategy in the simplex
- path latencies
- path flows

running the simulation with different learning rate sequences and update rules shows the impact on convergence.





-------------------------------------------------------------------------------
#### Dependencies
- `breeze` for linear algebra and visualization [https://github.com/scalanlp/breeze/wiki/Breeze-Linear-Algebra]()