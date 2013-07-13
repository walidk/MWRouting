package mw
import breeze.linalg._

class Expert

class MWAlgorithm(
    val epsilon: LearningRate,
    val experts: Array[Expert],
    val updateRule: UpdateRule) {
  
  def initialStrategy(randomizeStart: Boolean) =
    if(randomizeStart) randomStrategy else uniformStrategy
  
  val nextWeights = updateRule.nextWeights
  
  def uniformStrategy = {
    val s = DenseVector(experts.map(_=>1.))
    s/s.norm(1)
  }
  
  def randomStrategy = {
    val rand = new scala.util.Random()
    val s = DenseVector(experts.map(_=>rand.nextDouble))
    s/s.norm(1)
  }
  
  def nextStrategy(
      strategy: DenseVector[Double], 
      losses: DenseVector[Double], 
      epsilon: Double
      ): DenseVector[Double] = {
    val weights = nextWeights(strategy, losses, epsilon)
    val next = (strategy :* weights)
      .map(math.max(_, 0.00001)) // to avoid getting stuck in a False Nash
    
    next :/ next.norm(1)
  }
}