package mw

import scala.collection.immutable.{Map, Stream}
import breeze.linalg._
import util.Visualizer
import routing._

object main {
  def main(args: Array[String]): Unit = {
//    Simulations.launchParallelRoutingGame()
//    Simulations.launchNoisyLatencyParallelRoutingGame()
//    Simulations.launchAdaptiveParallelRoutingGame()
//    Simulations.launchTimeVaryingParallelRoutingGame()
//    Simulations.launchRoutingGame()
//    Simulations.launchNoisyRoutingGame()
    Simulations.launchECCsim3()
//    Simulations.launchDBLoadBalancing()
//    Simulations.launchNoRegretSocialRouting()
//    Simulations.launchNoRegretSocialTwoLinkRouting()
//    Simulations.launchNoRegretSocialParallelRouting()
//    Simulations.launchLLFParallelRouting()
//    Simulations.launchTollRouting()
//    Simulations.launchOptimalConstantTollRouting()
//    Simulations.launchTollParallelRouting()
//    Simulations.launchZeroSumGame()
//    Simulations.launchZeroSumGameAdversarial()
  }
}


object Simulations {
  val adjacencyMap: Map[Int, List[(Int, LatencyFunction)]] = Map(
    0 -> List((1, SLF(x=>x*x+2.5)), (4, SLF(x=>x/2))),
    1 -> List(),
    2 -> List((3, SLF(x=>x+1.0)), (4, SLF(x=>.5))),
    3 -> List((8, SLF(x=>1.0))),
    4 -> List((5, SLF(x=>3*x*x)), (6, SLF(x=>x*x*x))),
    5 -> List((1, SLF(x=>x/3)), (3, SLF(x=>x/4))), 
    6 -> List((1, SLF(x=>x*x/2)), (3, SLF(x=>x))),
    7 -> List((2, SLF(x=>x*x/2))),
    8 -> List()
    )
  
  val factor = .7
  val adjacencyMap2: Map[Int, List[(Int, LatencyFunction)]] = Map(
    0 -> List((1, SLF(x=>x*x+2.5)*factor), (4, SLF(x=>x/2)*factor)),
    1 -> List(),
    2 -> List((3, SLF(x=>x+1.0)*factor), (4, SLF(x=>.5)*factor)),
    3 -> List(),
    4 -> List((5, SLF(x=>3*x*x)*factor), (6, SLF(x=>x*x*x)*factor)),
    5 -> List((1, SLF(x=>x/3)*factor), (3, SLF(x=>x/4)*factor)), 
    6 -> List((1, SLF(x=>x*x/2)*factor), (3, SLF(x=>x)*factor))
    )
    
  val adjacencyMap3: Map[Int, List[(Int, LatencyFunction)]] = Map(
    0 -> List((1, SLF(x=>x+2.0)), (4, SLF(x=>x/2)), (5, SLF(x => x))),
    1 -> List(),
    2 -> List((3, SLF(x=>x+1.0)), (4, SLF(x=>.5))),
    3 -> List(),
    4 -> List((5, SLF(x=>3*x)), (3, SLF(x=>x))),
    5 -> List((1, SLF(x=>x/3)), (3, SLF(x=>x/4)))
    )
    
  val parallelAdjacencyMap: Map[Int, List[(Int, LatencyFunction)]] = 
    Map(1 -> Nil, 
        0 -> List(
          (1, SLF(x=>3*x)),
          (1, SLF(x=>x*x)),
          (1, SLF(x=>2*(x+1)*(x+1)))
          )
        )
    
  
  def launchParallelRoutingGame() {
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(parallelAdjacencyMap)
    val flowDemand = ConstantFlowDemand(2.0)
    val updateRule = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
    val learningRate = 
      HarmonicLearningRate(1.0)
//      ConstantLearningRate(2.)
    val randomizedStart = true
    val T = 100
    val commodity = Commodity(0, 1, flowDemand, learningRate, updateRule, graph.findSimplePaths(0, 1))
    val sim = new RoutingGameSim(graph, latencyFunctions, Array(commodity), randomizedStart)
    sim.runFor(T)
    val maxFlow = commodity.demand()
    Visualizer("Latency Functions").plotLatencies(latencyFunctions.values.toArray, (0, maxFlow), "f", "l(f)", 300).exportToPdf("latency_functions")
  }
  
  
  def launchNoisyLatencyParallelRoutingGame() {
    val sigma = .1
    val adj: Map[Int, List[(Int, LatencyFunction)]] = 
      Map(1->Nil,
          0->List(
            (1, SLF(x => 2 + 2 * x)+GaussianNoise(sigma)), 
            (1, SLF(x => x * x)+GaussianNoise(sigma)), 
            (1, SLF(x => 2 * (x + 1) * (x + 1) - 1)+GaussianNoise(sigma))
          ))
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(adj)
    val flowDemand = ConstantFlowDemand(2.0).until(100).then(ConstantFlowDemand(3.0).until(100))
    val updateRule = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
    val learningRate = 
//      HarmonicLearningRate(1.)
      ConstantLearningRate(10.0)
    val randomizedStart = true
    val T = 200
    val commodity = Commodity(0, 1, flowDemand, learningRate, updateRule, graph.findSimplePaths(0, 1))
    val sim = new RoutingGameSim(graph, latencyFunctions, Array(commodity), randomizedStart)
    sim.runFor(T)
    val maxFlow = commodity.demand()
    Visualizer("Latency Functions").plotLatencies(latencyFunctions.values.toArray, (0, maxFlow), "f", "l(f)", 300)
  }
  
  def launchAdaptiveParallelRoutingGame() {
    val sigma = .0
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(parallelAdjacencyMap)
    val flowDemand = 
      ConstantFlowDemand(1.0).until(100)
      .then(ConstantFlowDemand(3.0).until(100))
      .then(ConstantFlowDemand(1.0))
//      + GaussianNoise(sigma)
    val updateRule = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
    val learningRate = 
//      HarmonicLearningRate(1.)
      SimpleAdaptiveRoutingLearningRate(5.0, 0)
    val randomizedStart = false
    val T = 400
    val commodity = Commodity(0, 1, flowDemand, learningRate, updateRule, graph.findSimplePaths(0, 1))
    val sim = new RoutingGameSim(graph, latencyFunctions, Array(commodity), randomizedStart)
    sim.runFor(T)
    Visualizer("Latency Functions").plotLatencies(latencyFunctions.values.toArray, (0, 3), "f", "l(f)", 300)
  }
  
  def launchTimeVaryingParallelRoutingGame() {
    val adj: Map[Int, List[(Int, LatencyFunction)]] = 
      Map(1->Nil,
          0->List(
          (1, SLF(x=>3*x).until(50).then(SLF(x=>3*x+4))), 
          (1, SLF(x => 2 + 2 * x)), 
          (1, SLF(x => x * x)), 
          (1, SLF(x => 2 * (x + 1) * (x + 1) - 1))
          ))
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(adj)
    val flowDemand = ConstantFlowDemand(2.0).until(100).then(ConstantFlowDemand(3.0))
    val updateRule = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
    val learningRate = 
//      HarmonicLearningRate(10.0)
      ConstantLearningRate(10.0)
    val randomizedStart = true
    val T = 200
    val commodity = Commodity(0, 1, flowDemand, learningRate, updateRule, graph.findSimplePaths(0, 1))
    val sim = new RoutingGameSim(graph, latencyFunctions, Array(commodity), randomizedStart)
    sim.runFor(T)
    val maxFlow = commodity.demand()
    Visualizer("Latency Functions").plotLatencies(latencyFunctions.values.toArray, (0, maxFlow), "f", "l(f)", 300)
  }
  
  def launchECCsim() {
    val ECCadj: Map[Int, List[(Int, LatencyFunction)]] = Map(
      0 -> List((2, SLF(x=>.1))),
      1 -> List((2, SLF(x=>.5*x)), (3, SLF(x=>x))),
      2 -> List((3, SLF(x=>.5*x)), (4, SLF(x=>x*x))),
      3 -> List((4, SLF(x=>.5*x))),
      4 -> List()
      )
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(ECCadj)
    val T = 100
    val randomizedStart = false
    val updateRule1 = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
//      PolyUpdate(.5)
    val updateRule2 = 
      ExponentialUpdate()
    val eps1 = 
      PolyLearningRate(.5, .6)
//      HarmonicLearningRate(.5)
//      ConstantLearningRate(4.)
    val eps2 =
      PolyLearningRate(.2, 2.)
//      ConstantLearningRate(4.)
    val commodities = Array(
        Commodity(0, 4, ConstantFlowDemand(1.0), eps1, updateRule1, graph.findSimplePaths(0, 4)), 
        Commodity(1, 4, ConstantFlowDemand(1.0), eps2, updateRule2, graph.findSimplePaths(1, 4))
        )
    
    val sim = new RoutingGameSim(graph, latencyFunctions, commodities, randomizedStart)
    sim.runFor(T)
  }
  
  def launchECCsim2() {
    val ECCadj: Map[Int, List[(Int, LatencyFunction)]] = Map(
      0 -> List((1, SLF(x=>.2)), (1, SLF(x=>x))),
      1 -> List((2, SLF(x=>.6)), (2, SLF(x=>x))),
      2 -> List((3, SLF(x=>.1)), (3, SLF(x=>x))),
      3 -> List()
      )
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(ECCadj)
    val T = 200
    val randomizedStart = false
    val updateRule1 = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
//      PolyUpdate(.5)
    val updateRule2 = 
      ExponentialUpdate()
    val eps1 = 
      PolyLearningRate(.6, .15)
//      HarmonicLearningRate(.5)
//      ConstantLearningRate(4.)
    val eps2 =
      PolyLearningRate(.2, 2.)
//      ConstantLearningRate(4.)
    val commodities = Array(
        Commodity(0, 3, ConstantFlowDemand(1.0), eps1, updateRule1, graph.findSimplePaths(0, 3)) 
        )
    
    val sim = new RoutingGameSim(graph, latencyFunctions, commodities, randomizedStart)
    sim.runFor(T)
  }
  
  def launchECCsim3() {
    val ECCadj: Map[Int, List[(Int, LatencyFunction)]] = Map(
      0 -> List((1, SLF(x=>.35)), (1, SLF(x=>.4))),
      1 -> List((2, SLF(x=>.6)), (2, SLF(x=>.55))),
      2 -> List((3, SLF(x=>.1)), (3, SLF(x=>.2))),
      3 -> List()
      )
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(ECCadj)
    val T = 2500
    val randomizedStart = false
    val updateRule1 = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
//      PolyUpdate(.5)
    val updateRule2 = 
      ExponentialUpdate()
    val eps1 = 
      PolyLearningRate(.6, .15)
//      HarmonicLearningRate(.5)
//      ConstantLearningRate(4.)
    val eps2 =
      PolyLearningRate(.2, 2.)
//      ConstantLearningRate(4.)
    val commodities = Array(
        Commodity(0, 3, ConstantFlowDemand(1.0), eps1, updateRule1, graph.findSimplePaths(0, 3)) 
        )
    
    val sim = new RoutingGameSim(graph, latencyFunctions, commodities, randomizedStart)
    sim.runFor(T)
  }
  
  def launchRoutingGame() { 
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(adjacencyMap3)
    val T = 50
    val randomizedStart = false
    val updateRule = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
//      PolyUpdate(.5)
    val eps1 = 
      HarmonicLearningRate(.5)
//      ConstantLearningRate(.6)
//    val eps2 = 
//      RootLearningRate(1.0)
//      ConstantLearningRate(1./.8)
    val commodities = Array(
        Commodity(0, 1, ConstantFlowDemand(1.0), eps1, updateRule, graph.findSimplePaths(0, 1)), 
        Commodity(2, 3, ConstantFlowDemand(1.0), eps1, updateRule, graph.findSimplePaths(2, 3)) 
//        Commodity(7, 8, ConstantFlowDemand(1.0), eps, updateRule, graph.findSimplePaths(7, 8))
        )
    
    val sim = new RoutingGameSim(graph, latencyFunctions, commodities, randomizedStart)
    sim.runFor(T)
  }

    
  def launchNoisyRoutingGame() {
    val sigma = 0.05
    val noiseFunction = GaussianNoise(sigma)
    val adj: Map[Int, List[(Int, LatencyFunction)]] = Map(
      0 -> List((1, SLF(x=>x*x+2.5)*factor), (4, SLF(x=>x/2)*factor)),
      1 -> List(),
      2 -> List((3, SLF(x=>x+1.0)*factor), (4, SLF(x=>.5)*factor)),
      3 -> List(),
      4 -> List((5, SLF(x=>3*x*x)*factor), (6, SLF(x=>x*x*x)*factor)),
      5 -> List((1, SLF(x=>x/3)*factor), (3, SLF(x=>x/4)*factor)), 
      6 -> List((1, SLF(x=>x*x/2)*factor), (3, SLF(x=>x)*factor))
    )
      
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(adj)
    val flowDemand = ConstantFlowDemand(1.0)
    val updateRule = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
    val rates1 = PolyLearningRate(0.5, 1.)
    val rates2 = PolyLearningRate(0.1, 2.)
    val randomizedStart = false
    val T = 50
    val commodities = Array(
        Commodity(0, 1, ConstantFlowDemand(1.0), rates1, updateRule, graph.findSimplePaths(0, 1)), 
        Commodity(2, 3, ConstantFlowDemand(1.0), rates2, updateRule, graph.findSimplePaths(2, 3)) 
        )
    
    val eqsim = new RoutingGameSim(graph, latencyFunctions, commodities, randomizedStart)
    val eqFlows = eqsim.coordinator.gameStateStream.map(_.pathFlows).apply(500)
    
    val sim = new StochasticRoutingGameSim(graph, latencyFunctions, commodities, randomizedStart, noiseFunction)
    sim.runFor(60, 100, eqFlows)
  }
  
  
  def launchDBLoadBalancing() {
    val T = 100
    val adj: Map[Int, List[(Int, LatencyFunction)]] = 
      Map(1->Nil, 
          0->List(
            (1, SLF(x => 80+30*Math.exp(x*Math.abs(Math.sin(6.28*x/10))/300))),
            (1, SLF(x => 60*Math.exp(x*Math.abs(Math.sin(6.28*x/10))/50)))
            )
          )
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(adj)
    val demand = ConstantFlowDemand(75)
    val randomizedStart = true
    val learningRate = 
      HarmonicLearningRate(20.9)
    val updateRule = 
      ExponentialUpdate()
//      PolyUpdate(.5)
//    FollowTheMeanUpdate()
    val commodity = Commodity(0, 1, demand, learningRate, updateRule, graph.findSimplePaths(0, 1))
    val sim = new RoutingGameSim(graph, latencyFunctions, Array(commodity), randomizedStart)
    sim.runFor(T)
    Visualizer("Latency Functions").plotLatencies(latencyFunctions.values.toArray, (0, demand()), "f", "l(f)", 300)
  }
  
  def launchNoRegretSocialParallelRouting() {
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(parallelAdjacencyMap)
    val T = 200
    val nonCompliantDemand = ConstantFlowDemand(1.5)
    val compliantDemand = ConstantFlowDemand(.5)
    val randomizedStart = true
    val updateRule = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
    val learningRate = HarmonicLearningRate(1.0)

    val nonCompliantCommodities = Array(Commodity(0, 1, nonCompliantDemand, learningRate, updateRule, graph.findSimplePaths(0, 1)))
    val compliantCommodities = Array(Commodity(0, 1, compliantDemand, learningRate, updateRule, graph.findSimplePaths(0, 1)))
    
    val sim = new NoRegretSocialRoutingGameSim(graph, latencyFunctions, nonCompliantCommodities, compliantCommodities, randomizedStart)
    sim.runFor(T)
  }
  
  def launchLLFParallelRouting() {
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(parallelAdjacencyMap)
    val T = 200
    val nonCompliantDemand = ConstantFlowDemand(1.5)
    val compliantDemand = ConstantFlowDemand(.5)
    val randomizedStart = false
    val updateRule = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
    val learningRate = HarmonicLearningRate(1.0)

    val nonCompliantCommodities = Array(Commodity(0, 1, nonCompliantDemand, learningRate, updateRule, graph.findSimplePaths(0, 1)))
    val compliantCommodities = Array(Commodity(0, 1, compliantDemand, learningRate, updateRule, graph.findSimplePaths(0, 1)))
    val sim = new LLFRoutingGameSim(graph, latencyFunctions, nonCompliantCommodities, compliantCommodities, randomizedStart)
    val vis = Visualizer("Latency Functions").plotLatencies(latencyFunctions.values.toArray, (0, 2), "f", "l(f)", 300)
    sim.runFor(T)
  }
  
  def launchNoRegretSocialRouting() {
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(adjacencyMap)
    val T = 100
    val randomizedStart = true
    val updateRule = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
    val learningRate = HarmonicLearningRate(1.0)
    
    val nonCompliantCommodities = Array(
        Commodity(0, 1, ConstantFlowDemand(.8), learningRate, updateRule, graph.findSimplePaths(0, 1)),
        Commodity(2, 3, ConstantFlowDemand(.8), learningRate, updateRule, graph.findSimplePaths(2, 3)),
        Commodity(7, 8, ConstantFlowDemand(.8), learningRate, updateRule, graph.findSimplePaths(7, 8)))
    
    val compliantCommodities = Array(
        Commodity(0, 1, ConstantFlowDemand(.2), learningRate, updateRule, graph.findSimplePaths(0, 1)),
        Commodity(2, 3, ConstantFlowDemand(.2), learningRate, updateRule, graph.findSimplePaths(2, 3)),
        Commodity(7, 8, ConstantFlowDemand(.2), learningRate, updateRule, graph.findSimplePaths(7, 8)))
    
    val sim = new NoRegretSocialRoutingGameSim(graph, latencyFunctions, nonCompliantCommodities, compliantCommodities, randomizedStart)
    sim.runFor(T)
  }
  
  def launchNoRegretSocialTwoLinkRouting() {
    val adjacencyMap: Map[Int, List[(Int, LatencyFunction)]] = 
    Map(1 -> Nil, 
        0 -> List(
          (1, SLF(x=>1)),
          (1, SLF(x=>x))
          )
        )
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(adjacencyMap)
    val T = 100
    val randomizedStart = false
    val updateRule = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
    val learningRate = HarmonicLearningRate(1.0)
    
    val nonCompliantCommodities = 
      Array(Commodity(0, 1, ConstantFlowDemand(1.0), learningRate, updateRule, graph.findSimplePaths(0, 1)))
    val compliantCommodities = 
      Array(Commodity(0, 1, ConstantFlowDemand(1.0), learningRate, updateRule, graph.findSimplePaths(0, 1)))
    
    val sim = new NoRegretSocialRoutingGameSim(graph, latencyFunctions, nonCompliantCommodities, compliantCommodities, randomizedStart)
    sim.runFor(T)
  }
  
  
  def launchTollRouting() {
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(adjacencyMap)
    val T = 300
    val tollDelay = 3
    val tollInterval = 3
    val randomizedStart = false
    val updateRule = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
    val learningRate = 
//      ConstantLearningRate(.1)
      HarmonicLearningRate(3.0)
    
    val commodities = Array(
        Commodity(0, 1, ConstantFlowDemand(1.0), learningRate, updateRule, graph.findSimplePaths(0, 1)),
        Commodity(2, 3, ConstantFlowDemand(1.0), learningRate, updateRule, graph.findSimplePaths(2, 3)),
        Commodity(7, 8, ConstantFlowDemand(1.0), learningRate, updateRule, graph.findSimplePaths(7, 8)))
    
    val sim = new TollRoutingGameSim(graph, latencyFunctions, commodities, tollDelay, tollInterval, randomizedStart)
    sim.runFor(T)
  }
  
  def launchOptimalConstantTollRouting() {
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(adjacencyMap)
    val T = 100
    val randomizedStart = true
    val updateRule = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
    val learningRate = 
      ConstantLearningRate(1.0)
//      HarmonicLearningRate(1.)
    
    val commodities = Array(
        Commodity(0, 1, ConstantFlowDemand(1.0), learningRate, updateRule, graph.findSimplePaths(0, 1)),
        Commodity(2, 3, ConstantFlowDemand(1.0), learningRate, updateRule, graph.findSimplePaths(2, 3)),
        Commodity(7, 8, ConstantFlowDemand(1.0), learningRate, updateRule, graph.findSimplePaths(7, 8)))
    
    val sim = new OptimalConstantTollRoutingGameSim(graph, latencyFunctions, commodities, randomizedStart)
    sim.runFor(T)
  }
  
  def launchTollParallelRouting() {
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(parallelAdjacencyMap)
    val T = 500
    val tollInterval = 1 // the tolls need to remain constant for a few days at a time
    val tollDelay = 50 // the tolls are announced a few intervals in advance
    val randomizedStart = true
    val updateRule = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
    val learningRate = 
      ConstantLearningRate(.1)
//      HarmonicLearningRate(10.)
    
    val commodities = Array(
        Commodity(0, 1, ConstantFlowDemand(2.0), learningRate, updateRule, graph.findSimplePaths(0, 1)))
    
    Visualizer("Latency Functions").plotLatencies(latencyFunctions.values.toArray, (0, 2), "f", "l(f)", 300)
    val sim = new TollRoutingGameSim(graph, latencyFunctions, commodities, tollDelay, tollInterval, randomizedStart)
    sim.runFor(T)
  }
  
  // Zero Sum Games
  val payoffMatrix = DenseMatrix((.5, 1.0, 0.0), (0.0, .5, 1.0), (1.0, 0.0, .5), (.5, 0.0, 0.0))
  def launchZeroSumGame() {
    val T = 400
    val randomizedStart = true
    val eps = 
      ConstantLearningRate(.5)
//      HarmonicLearningRate(10.0)

    val sim = new ZeroSumGameSim(payoffMatrix, eps, ExponentialUpdate(), randomizedStart)
    sim.runFor(T)
  }
 
  def launchZeroSumGameAdversarial() {
    val T = 200
    val randomizedStart = true
    val updateRule = ExponentialUpdate()
    val eps = HarmonicLearningRate(10.0)
    
    val sim = new ZeroSumGameAdversarialSim(payoffMatrix, eps, updateRule, randomizedStart)
    sim.runFor(T)
  }

  
}
