package mw
import breeze.linalg._
import breeze.numerics._


class InvalidArgumentException(message: String) extends Exception(message)

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
class MWAlgorithm[N<:Nature](id: Int, epsilon: Double, actions: List[Expert[N]], val nature: N) {
  if(epsilon >= 1 || epsilon <= 0)
    throw new InvalidArgumentException("epsilon should be in (0, 1)")
  
  val support = actions.length
  val strategy:DenseVector[Double] = DenseVector.fill[Double](support){1./support}
  
  def next() {
    nature.update(id, strategy)
    val weights = new DenseVector[Double](actions.map(_.nextLoss).map(math.pow(1-epsilon, _)).toArray)
    strategy :*= weights
    strategy :/= strategy.norm(1)
  }
}