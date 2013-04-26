package mw
import util.linAlgebra._

class InvalidArgumentException(message: String) extends Exception(message)

trait Nature {
  def update(strategy: Array[Double])
}

abstract class Action[N<:Nature](val nature: N) {
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
class MWAlgorithm[N<:Nature](epsilon: Double, actions: List[Action[N]], val nature: N) {
  if(epsilon >= 1 || epsilon <= 0)
    throw new InvalidArgumentException("epsilon should be in (0, 1)")
  
  val support = actions.length
  var strategy = Array.fill(support)(1./support)
  
  def normalize() {
    val norm = strategy.sum
    for(i<- 0 to support-1)
      strategy(i)/=norm
  }
  
  def next() {
    nature.update(strategy)
    val weights = actions.map(_.nextLoss).map(math.pow(1-epsilon, _)).toArray
    strategy = strategy.dotMult(weights).arrayValue()
    normalize()
  }
}