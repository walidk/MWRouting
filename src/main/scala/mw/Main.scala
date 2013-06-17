package mw

import scala.collection.immutable.Map
import breeze.linalg._
import util.Visualizer
import routing._

object main {
  def main(args: Array[String]): Unit = {
    Simulations.launchParallelRoutingGame()
//    Simulations.launchRoutingGame()
//    Simulations.launchDBLoadBalancing()
//    Simulations.launchStackelbergRouting()
//    Simulations.launchStackelbergParallelRouting()
//    Simulations.launchZeroSumGame()
//    Simulations.launchZeroSumGameAdversarial()
  }
}


object Simulations {
  
  // Here SLF is a shorthand for StaticLatencyFunction
  def launchParallelRoutingGame() {
    val adj: Map[Int, List[(Int, LatencyFunction)]] = 
      Map(1->Nil,
          0->List(
          (1, SLF(x=>3*x).until(50).then(SLF(x=>3*x+4))), 
          (1, SLF(x => 2 + 2 * x)), 
          (1, SLF(x => x * x)), 
          (1, SLF(x => 2 * (x + 1) * (x + 1) - 1))))
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(adj)
    val totalFlow = 2.
    val updateRule = 
//      ExponentialUpdate()
      FollowTheMeanUpdate()
    val epsilon = 
//      HarmonicLearningRate(1.)
      ConstantLearningRate(.1)
    val randomizedStart = true
    val T = 200
    
    val commodity = Commodity(0, 1, ConstantFlowDemand(totalFlow), epsilon, updateRule, graph.findLooplessPathsAsEdgeId(0, 1))
    val sim = new ParallelRoutingGameSim(graph, latencyFunctions, Array(commodity), randomizedStart)
    sim.runFor(T)
  }
  
  
  def launchRoutingGame() { 
    val adj: Map[Int, List[(Int, LatencyFunction)]] = Map(
      0 -> List((1, SLF(x => x*x+2.5)), (4, SLF(x => x / 2))),
      1 -> List(),
      2 -> List((3, SLF(x => x+1.)), (4, SLF(x => .5))),
      3 -> List((8, SLF(x=>1.))),
      4 -> List((5, SLF(x => 3 * x * x)), (6, SLF(x => x*x*x))),
      5 -> List((1, SLF(x => x / 3)), (3, SLF(x => x/4))), 
      6 -> List((1, SLF(x => x*x / 2)), (3, SLF(x => x))),
      7 -> List((2, SLF(x => x*x / 2))),
      8 -> List()
      )
    val (graph, latencyFunctions) = DirectedGraph.graphAndLatenciesFromAdjMap(adj)
    
    val T = 100
    
    val randomizedStart = true
    val updateRule = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
//      PolyUpdate(.5)
    val eps = HarmonicLearningRate(.1)
//      ConstantLearningRate(.1)
    val commodities = Array(
        Commodity(0, 1, ConstantFlowDemand(1.), eps, updateRule, graph.findLooplessPathsAsEdgeId(0, 1)), 
        Commodity(2, 3, ConstantFlowDemand(1.), eps, updateRule, graph.findLooplessPathsAsEdgeId(2, 3)), 
        Commodity(7, 8, ConstantFlowDemand(1.), eps, updateRule, graph.findLooplessPathsAsEdgeId(7, 8)))
    
      
    val sim = new RoutingGameSim(graph, latencyFunctions, commodities, randomizedStart)
    sim.runFor(T)
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
    val totalFlow = 75
    val randomizedStart = true
    val epsilon = 
      HarmonicLearningRate(20.)
    val updateRule = 
      ExponentialUpdate()
//      PolyUpdate(.5)
//    FollowTheMeanUpdate()
      
    val commodity = Array(Commodity(0, 1, ConstantFlowDemand(totalFlow), epsilon, updateRule, graph.findLooplessPathsAsEdgeId(0, 1)))
    val sim = new ParallelRoutingGameSim(graph, latencyFunctions, commodity, randomizedStart)
    sim.runFor(T)
  }
  
  def launchStackelbergParallelRouting() {
    // The map is of the form (nodeId -> List((neighborId1, lat1, dLat1), (neighborId2, lat2, dLat2), ...))
    // lat1 is the latency function of the edge (nodeId, neighborId1), and dLat1 is its derivative  
    val adj: Map[Int, List[(Int, LatencyFunction, LatencyFunction)]] = 
      Map(1 -> Nil, 
          0 -> List(
            (1, SLF(x=>3*x), SLF(x=>3.)),
            (1, SLF(x=>x*x), SLF(x=>2*x)),
            (1, SLF(x=>2*(x+1)*(x+1)), SLF(x=>4*(x+1)))
            )
          )
    
    val (graph, latencyFunctions, latencyDerivatives) = DirectedGraph.graphAndLatenciesAndDerivativesFromAdjMap(adj)
    val T = 100
    val nonCompliantDemand = ConstantFlowDemand(1.5)
    val compliantDemand = ConstantFlowDemand(.5)
    val randomizedStart = true
    val updateRule = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
    val epsilon = HarmonicLearningRate(1.)

    val nonCompliantCommodities = Array(Commodity(0, 1, nonCompliantDemand, epsilon, updateRule, graph.findLooplessPathsAsEdgeId(0, 1)))
    val compliantCommodities = Array(Commodity(0, 1, compliantDemand, epsilon, updateRule, graph.findLooplessPathsAsEdgeId(0, 1)))
    
    val sim = new StackelbergRoutingGameSim(graph, latencyFunctions, latencyDerivatives, nonCompliantCommodities, compliantCommodities, randomizedStart)
    sim.runFor(T)
  }
  
  
  def launchStackelbergRouting() {
    val adj: Map[Int, List[(Int, LatencyFunction, LatencyFunction)]] = Map(
      0 -> List((1, SLF(x=>x*x+2.5), SLF(x=>2*x)), (4, SLF(x=>x/2), SLF(x=>.5))),
      1 -> List(),
      2 -> List((3, SLF(x=>x+1.), SLF(x=>1.)), (4, SLF(x=>.5), SLF(x=>0.))),
      3 -> List((8, SLF(x=>1.), SLF(x=>0.))),
      4 -> List((5, SLF(x=>3*x*x), SLF(x=>6*x)), (6, SLF(x=>x*x*x), SLF(x=>3*x*x))),
      5 -> List((1, SLF(x=>x/3), SLF(x=>1./3)), (3, SLF(x=>x/4), SLF(x=>1./4))), 
      6 -> List((1, SLF(x=>x*x/2), SLF(x=>x)), (3, SLF(x=>x), SLF(x=>1.))),
      7 -> List((2, SLF(x=>x*x/2), SLF(x=>x))),
      8 -> List()
      )
    
    val (graph, latencyFunctions, latencyDerivatives) = DirectedGraph.graphAndLatenciesAndDerivativesFromAdjMap(adj)
    val T = 100
    val randomizedStart = true
    val updateRule = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
    val epsilon = HarmonicLearningRate(10.)
    
    val nonCompliantCommodities = Array(
        Commodity(0, 1, ConstantFlowDemand(.8), epsilon, updateRule, graph.findLooplessPathsAsEdgeId(0, 1)),
        Commodity(2, 3, ConstantFlowDemand(.8), epsilon, updateRule, graph.findLooplessPathsAsEdgeId(2, 3)),
        Commodity(7, 8, ConstantFlowDemand(.8), epsilon, updateRule, graph.findLooplessPathsAsEdgeId(7, 8)))
    
    val compliantCommodities = Array(
        Commodity(0, 1, ConstantFlowDemand(.2), epsilon, updateRule, graph.findLooplessPathsAsEdgeId(0, 1)),
        Commodity(2, 3, ConstantFlowDemand(.2), epsilon, updateRule, graph.findLooplessPathsAsEdgeId(2, 3)),
        Commodity(7, 8, ConstantFlowDemand(.2), epsilon, updateRule, graph.findLooplessPathsAsEdgeId(7, 8)))
    
    val sim = new StackelbergRoutingGameSim(graph, latencyFunctions, latencyDerivatives, nonCompliantCommodities, compliantCommodities, randomizedStart)
    sim.runFor(T)
  }
  
  
    // Zero Sum Games
  val payoffMatrix = DenseMatrix((.5, 1., 0.), (0., .5, 1.), (1., 0., .5), (.5, 0., 0.))
  def launchZeroSumGame() {
    val T = 400
    val randomizedStart = true
    val eps = 
      ConstantLearningRate(.5)
//      HarmonicLearningRate(10.)

    val sim = new ZeroSumGameSim(payoffMatrix, eps, ExponentialUpdate(), randomizedStart)
    sim.runFor(T)
  }

  def launchZeroSumGameAdversarial() {
    val T = 200
    val randomizedStart = true
    val updateRule = ExponentialUpdate()
    val eps = HarmonicLearningRate(10.)
    
    val sim = new ZeroSumGameAdversarialSim(payoffMatrix, eps, updateRule, randomizedStart)
    sim.runFor(T)
  }

  
}
