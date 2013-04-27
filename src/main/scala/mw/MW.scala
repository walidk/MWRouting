package mw
import breeze.linalg._

trait Nature {
  def update(id: Int, strategy: DenseVector[Double])
}

abstract class Expert[N<:Nature](val nature: N) {
  def nextLoss(): Double
}


// Multiplicative Weights algorithm
// Type parameter N must extend Nature, and actions (experts) are of type Action[N]
// (the experts share an instance of Nature)
// nature keeps extra information specific to the particular game being played.
// for example, if nature is a ZeroSumGame, it provides methods to compute best
// column response, and it keeps track of the average row and column strategies.
// In the routing game, it would provide a best response method that computes the
// shortest path, etc.
class MWAlgorithm[N<:Nature](id: Int, epsilon: Int=>Double, actions: List[Expert[N]], val nature: N) {
  val support = actions.length
  val strategy:DenseVector[Double] = DenseVector.fill[Double](support){1./support}
  var round = 0
  def next() {
    round += 1
    nature.update(id, strategy)
    val weights = new DenseVector[Double](actions.map(_.nextLoss).map(math.pow(1-epsilon(round), _)).toArray)
    strategy :*= weights
    strategy :/= strategy.norm(1)
  }
}