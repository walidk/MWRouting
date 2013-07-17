package routing

import breeze.linalg.DenseVector
import breeze.optimize._
import scala.collection.mutable.HashMap

class SocialOptimizer(network: LatencyNetwork) {
  private lazy val solution = solve()
  lazy val optimalStrategy = unpack(solution)
  lazy val optimalCost = cost(solution)
  lazy val optimalTotalPathFlows = computeOptTotalPathFlows()
  
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
  
  private def cost(strategyVector: DenseVector[Double]): Double = {
    val strategies = unpack(strategyVector)
    val pathFlows = network.pathFlowsFromStrategies(strategies)
    network.socialCostFromPathFlows(pathFlows)
  }

  private def solve(): DenseVector[Double] = {
    val differentiableCost = new ApproximateGradientFunction(cost, epsilon = 1.0E-4)

    def simplexNormal(n: Int) = DenseVector.ones[Double](n) / math.sqrt(n)
    def simplexCenter(n: Int) = DenseVector.ones[Double](n) / n.doubleValue
    
    def projectOnSimplex(v: DenseVector[Double]): DenseVector[Double] = {
      val w = v.toArray.sorted.reverse
      var opt = 0.
      var sum = 0.
      for(i <- w.indices){
        sum+=w(i)
        if(w(i)>(sum-1)/(i+1))
          opt = (sum-1)/(i+1)
      }
      
      val projected = v.map(x=>math.max(x - opt, 0))
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
  
  private def computeOptTotalPathFlows() = {
    val nbCommodities = network.commodities.length/2
    val optPathFlows = network.pathFlowsFromStrategies(optimalStrategy)
    val ncFlows = optPathFlows.take(nbCommodities)
    val cFlows = optPathFlows.takeRight(nbCommodities)
    val totalPathFlows = for((cFlow, ncFlow) <- cFlows.zip(ncFlows)) yield cFlow + ncFlow
    totalPathFlows
  }
  
  
  // LLF
  lazy val LLFStrategy = computeLLFStrategy()
  
  private def computeLLFStrategy() = {
    // First, we compute the path flows for all commodities
    // Only the second half of the commodities are compliant
    val nbCommodities = network.commodities.length/2
    val optPathFlows = network.pathFlowsFromStrategies(optimalStrategy)
    val optLatencies = network.pathLatenciesFromPathFlows(optPathFlows)
    val cLatencies = optLatencies.takeRight(nbCommodities)
    val cCommodities = network.commodities.takeRight(nbCommodities)
    
    val llfStrategy = new Array[DenseVector[Double]](nbCommodities)
    for(i <- llfStrategy.indices) {
      val cDemand = cCommodities(i).demand()
      val paths = cCommodities(i).paths
      val orderedPathIndices = paths.indices.sortBy(-cLatencies(i)(_))
      llfStrategy(i) = DenseVector.zeros[Double](paths.length)
      var allocated = 0.
      var j = 0
      while(allocated < cDemand && j < paths.length) {
        val pathId = orderedPathIndices(j)
        llfStrategy(i)(pathId) = math.min(optimalTotalPathFlows(i)(pathId), cDemand - allocated)
        allocated += llfStrategy(i)(pathId)
        j += 1
      }
      llfStrategy(i) /= llfStrategy(i).norm(1)
    }
    llfStrategy
  }
  
  // Scale
  lazy val scaleStrategy = computeScaleStrategy()
  
  private def computeScaleStrategy() = {
    // First, we compute the path flows for all commodities
    // Only the second half of the commodities are compliant
    val nbCommodities = network.commodities.length/2
    val scaleStrategy = optimalTotalPathFlows.map(flows => flows/flows.norm(1))
    scaleStrategy
  }
}