package mw

import scala.collection.immutable.Map
import breeze.linalg._
import util.Visualizer

object main {
  def main(args: Array[String]): Unit = {
//    Simulations.launchDBLoadBalancing()
//    Simulations.launchRoutingGame()
    Simulations.launchStackelbergRouting()
//    Simulations.launchParallelRoutingGame()
//    Simulations.launchZeroSumGame()
//    Simulations.launchZeroSumGameAdversarial()
  }
}


object Simulations {
  def launchParallelRoutingGame() {
    val latencies: Array[Double => Double] = 
      Array(
          x => 3 * x, 
          x => 2 + 2 * x, 
          x => x * x, 
          x => 2 * (x + 1) * (x + 1) - 1)
    val totalFlow = 2.
    val updateRule = ExponentialUpdate()
    val randomizedStart = true
    
    val sim = new ParallelRoutingSim(latencies, totalFlow, updateRule, randomizedStart)
    sim.runFor(100)
  }
  
  def launchRoutingGame() { 
    val adj: Map[Int, List[(Int, Double => Double)]] = Map(
      0 -> List((1, x => x*x+2.5), (4, x => x / 2)),
      1 -> List(),
      2 -> List((3, x => x+1.), (4, x => .5)),
      3 -> List((8, x=>1)),
      4 -> List((5, x => 3 * x * x), (6, x => x*x*x)),
      5 -> List((1, x => x / 3), (3, x => x/4)), 
      6 -> List((1, x => x*x / 2), (3, x => x)),
      7 -> List((2, x => x*x / 2)),
      8 -> List()
      )
      
    val T = 100
    val sourceSinks = Array((0, 1), (2, 3), (7, 8))
    val totalFlows = Array(1., 1., 1.)
    val randomizedStart = true
    val updateRule = 
//      ExponentialUpdate()
//      FollowTheMeanUpdate()
      PolyUpdate(.5)
    val eps = (t:Int)=>
//      .1
//      .5
//      .8
      1./(10+t)
//      1./math.sqrt(t)
    
    
    val sim = new RoutingGameSim(adj, sourceSinks, totalFlows, updateRule, randomizedStart)
    sim.algorithms(0).epsilon = eps
    sim.algorithms(1).epsilon = eps
    sim.algorithms(2).epsilon = eps
    sim.runFor(T)
  }

  
  // Zero Sum Games
  val payoffMatrix = DenseMatrix((.5, 1., 0.), (0., .5, 1.), (1., 0., .5), (.5, 0., 0.))
  def launchZeroSumGame() {
    val T = 400
    val eps: Int=>Double = t =>
      .5
//      1./(10+t)
    val randomizedStart = true
    
    val sim = new ZeroSumGameSim(payoffMatrix, ExponentialUpdate(), randomizedStart)
    sim.algorithms(0).epsilon = eps
    sim.algorithms(1).epsilon = eps
    sim.runFor(T)
  }

  def launchZeroSumGameAdversarial() {
    val T = 200
    val randomizedStart = true
    val updateRule = ExponentialUpdate()
    val eps: Int=>Double = t =>1./(10+t)
    
    val sim = new ZeroSumGameAdversarialSim(payoffMatrix, updateRule, randomizedStart)
    sim.algorithms(0).epsilon = eps
    sim.runFor(T)
  }
  
  def launchDBLoadBalancing() {
    val T = 100
    val latencies: Array[Double => Double] = 
      Array(
          x => 80+30*Math.exp(x*Math.abs(Math.sin(6.28*x/10))/300),
          x => 60*Math.exp(x*Math.abs(Math.sin(6.28*x/10))/50))
//          x => Math.exp(-x/1000),
//          x => 1 - x/1000)
//          x => 30*Math.exp(x/50),
//          x => 60*Math.exp(x/150))
//          x => 2 + 15 * Math.pow(1.28,x*3))
    val totalFlow = 75
    val randomizedStart = true
    val eps: Int => Double = t=> .5 / (10 + t)
    
    val sim1 = new ParallelRoutingSim(latencies, totalFlow, ExponentialUpdate(), randomizedStart)
    sim1.algorithms.foreach(_.epsilon = eps)
    sim1.runFor(T)
    
    val sim2 = new ParallelRoutingSim(latencies, totalFlow, PolyUpdate(.5), randomizedStart)
    sim2.runFor(T)
    
    val sim4 = new ParallelRoutingSim(latencies, totalFlow, FollowTheMeanUpdate(), randomizedStart)
    sim4.algorithms.foreach(_.epsilon = eps)
    sim4.runFor(T)
  }
  
  def launchStackelbergRouting() {
    val adj: Map[Int, List[(Int, Double => Double, Double => Double)]] = 
      Map(0 -> List(
          (1, x=>3*x, x=>3),
          (1, x=>x*x, x=>2*x),
          (1, x=>2*(x+1)*(x+1), x=>4*(x+1))),
          1 -> List())
    
    val T = 100
    val sourceSinks = Array((0, 1))
    val nonCompliantFlows = Array(1.5)
    val compliantFlows = Array(.5)
    val randomizedStart = true
    val updateRule = 
      ExponentialUpdate()
//      FollowTheMeanUpdate()
//      PolyUpdate(.5)
    val eps = (t:Int)=>.1
    
    val sim = new StackelbergRoutingGameSim(adj, sourceSinks, nonCompliantFlows, compliantFlows, updateRule, randomizedStart)
//    sim.algorithms(1).epsilon = eps
    sim.runFor(T)
  }
  
  
}
