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
class MWAlgorithm[N<:Nature](id: Int, epsilon: Int=>Double, experts: List[Expert[N]], val nature: N, val randomize: Boolean = false) {
  val support = experts.length
  
  def uniformStartegy = DenseVector.fill[Double](support){1./support}
  def randomStrategy = {
    val s = DenseVector.rand(support, new scala.util.Random())
    s/s.norm(1)
  }
  val strategy:DenseVector[Double] = if(randomize) randomStrategy else uniformStartegy
  
  var round = 0
  
  def next() {
    round += 1
    nature.update(id, strategy)
    val weights = new DenseVector[Double](experts.map(_.nextLoss).map(l => math.exp(-epsilon(round)*l)).toArray)
    strategy :*= weights
    strategy :/= strategy.norm(1)
  }
}