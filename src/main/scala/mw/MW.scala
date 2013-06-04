package mw
import breeze.linalg._

trait Nature {
  def update(id: Int, strategy: DenseVector[Double])
}

abstract class Expert[N<:Nature](val nature: N) {
  def nextLoss(): Double
}


// Multiplicative Weights algorithm
// Type parameter N must extend Nature, and experts (actions) are of type Expert[N]
// (the experts share an instance of Nature)
// nature keeps extra information specific to the particular game being played.
// for example, if nature is a ZeroSumGame, it provides methods to compute best
// column response, and it keeps track of the average row and column strategies.
// In the max congestion game, nature would provide a best response method that computes 
// the shortest path, etc.
abstract class MWAlgorithm[N<:Nature](id: Int, epsilon: Int=>Double, experts: List[Expert[N]], val nature: N, val randomizedStart: Boolean = false) {
  val nbExperts = experts.length
  
  def uniformStartegy = DenseVector.fill[Double](nbExperts){1./nbExperts}
  def randomStrategy = {
    val s = DenseVector.rand(nbExperts, new scala.util.Random())
    s/s.norm(1)
  }
  val strategy:DenseVector[Double] = if(randomizedStart) randomStrategy else uniformStartegy
  
  var round = 0
  nature.update(id, strategy)
  
  def updateRule(losses: DenseVector[Double]): DenseVector[Double]
  
  def next() {
    round += 1
    val losses = new DenseVector[Double](experts.map(_.nextLoss).toArray)
    val weights = updateRule(losses)
    strategy :*= weights
    strategy :/= strategy.norm(1)
    
    nature.update(id, strategy)
  }
}


class ExponentialMWAlgorithm[N<:Nature](id: Int, epsilon: Int=>Double, experts: List[Expert[N]], nature: N, randomizedStart: Boolean = false) 
  extends MWAlgorithm[N](id, epsilon, experts, nature, randomizedStart) {
  def updateRule(losses: DenseVector[Double]) = losses.map(loss => math.exp(-epsilon(round)*loss))
}

class PolyMWAlgorithm[N<:Nature](pow: Double, id: Int, epsilon: Int=>Double, experts: List[Expert[N]], nature: N, randomizedStart: Boolean = false) 
  extends MWAlgorithm[N](id, epsilon, experts, nature, randomizedStart) {
  def updateRule(losses: DenseVector[Double]) = losses.map(loss => epsilon(round)/math.pow(loss, pow))
}

class MultilinearMWAlgorithm[N<:Nature](id: Int, epsilon: Int=>Double, experts: List[Expert[N]], nature: N, randomizedStart: Boolean = false) 
  extends MWAlgorithm[N](id, epsilon, experts, nature, randomizedStart) {
  def updateRule(losses: DenseVector[Double]) = {
    val avgLoss = sum(strategy :* losses)
    losses.map(loss => 1+epsilon(round)*(avgLoss - loss))
  }
}