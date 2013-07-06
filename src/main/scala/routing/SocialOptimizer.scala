package routing

import breeze.linalg.DenseVector
import breeze.optimize._
import scala.collection.mutable.HashMap


object SocialOptimizer{
  def apply(network: LatencyNetwork) = new SocialOptimizer(network)
  def apply(
      graph: DirectedGraph, 
      latencyFunctions: HashMap[Int, LatencyFunction],
      ncCommodities: Array[Commodity], 
      cCommodities: Array[Commodity]) = {
    val commodities = for(
        Commodity(source, sink, demand, epsilon, updateRule, paths) <- ncCommodities;
        Commodity(cSource, cSink, cDemand, _, _, _) <- cCommodities;
        if cSource == source && cSink == sink)
      yield Commodity(source, sink, demand+cDemand, epsilon, updateRule, paths)
    println(commodities(0).demand())
    val network = new LatencyNetwork(graph, latencyFunctions, commodities)
    new SocialOptimizer(network)
  } 
}

class SocialOptimizer(network: LatencyNetwork) {
  private lazy val solution = solve()
  lazy val optimalStrategy = unpack(solution)
  lazy val optimalCost = cost(solution)
  
  private val cardinalities = network.commodities.map(_.paths.length)
  private val indexPairs = new Array[(Int, Int)](cardinalities.length)
  var start = 0
  for(i <- cardinalities.indices) {
    val end = start+cardinalities(i)
    indexPairs(i) = (start, end)
    start = end
  }
  
  private def pack(strategies: Array[DenseVector[Double]]) = 
    DenseVector.vertcat(strategies: _*)
 
  private def unpack(strategyVector: DenseVector[Double]): Array[DenseVector[Double]] = {
    for((start, end) <- indexPairs)
      yield strategyVector.slice(start, end)
  }
  
  def cost(strategyVector: DenseVector[Double]): Double = {
    val strategies = unpack(strategyVector)
    val pathFlows = network.pathFlowsFromStrategies(strategies)
    network.socialCostFromPathFlows(pathFlows)
  }

  private def solve(): DenseVector[Double] = {
    val differentiableCost = new ApproximateGradientFunction(cost, epsilon = 1.0E-4)

    def simplexNormal(n: Int) = DenseVector.ones[Double](n) / math.sqrt(n)
    def simplexCenter(n: Int) = DenseVector.ones[Double](n) / n.doubleValue
    
    def projectOnSimplex(v: DenseVector[Double]): DenseVector[Double] = {
      val n = v.length
      val normal = simplexNormal(n)
      val center = simplexCenter(n)
      val projected = v - normal * ((v-center).dot(normal))
      projected / projected.norm(1)
    }
    
    def project(vec: DenseVector[Double]): DenseVector[Double] = {
      pack(unpack(vec).map(projectOnSimplex))
    }

    val initialStrategies = cardinalities.map(simplexCenter)
    val solver = new SPG(optTol = 1.0E-5, projection = project)

    val solution = solver.minimize(differentiableCost, pack(initialStrategies))
    solution
  }
}