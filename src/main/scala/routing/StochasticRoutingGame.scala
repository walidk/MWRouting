package routing

import breeze.linalg._
import util.Visualizer
import mw._
import scala.collection.mutable.HashMap

class StochasticRoutingGame(network: LatencyNetwork, noiseFunction: LatencyFunction) extends Game {
  case class NetworkState(
      pathFlows: Array[DenseVector[Double]], 
      pathLatencies: Array[DenseVector[Double]],
      stochasticPathLatencies: Array[DenseVector[Double]],
      potentialValue: Double) extends GameState
      
  type State = NetworkState
  
  def initialState(strategies: Array[DenseVector[Double]]): State = {
    update(null, strategies)
  }
  
  def update(state: State, strategies: Array[DenseVector[Double]]): State = {
	val pathFlows = network.pathFlowsFromStrategies(strategies)
    val pathLatencies = network.pathLatenciesFromPathFlows(pathFlows)
    val potentialValue = network.rosenthalCostFromPathFlows(pathFlows)
    val stochasticPathLatencies = pathLatencies.map(latencies => latencies.map(_+noiseFunction(0)))
    NetworkState(pathFlows, pathLatencies, stochasticPathLatencies, potentialValue)
  }
  
  def loss(state: State)(expert: Expert): Double = expert match {
    case RoutingExpert(groupId, pathId) => getNoisyLatency(state)(groupId)(pathId)
  }
  
  protected def getLatency(state: State)(groupId: Int)(pathId: Int) = 
    state.pathLatencies(groupId)(pathId)
  
  protected def getRosenthalCost(state: State) =
    network.rosenthalCostFromPathFlows(state.pathFlows)
    
  protected def getNoisyLatency(state: State)(groupId: Int)(pathId: Int) = 
    state.stochasticPathLatencies(groupId)(pathId)
  
}

class StochasticRoutingGameSim(
  graph: DirectedGraph,
  latencyFunctions: HashMap[Int, LatencyFunction],
  commodities: Array[Commodity],
  randomizedStart: Boolean,
  noiseFunction: LatencyFunction) extends RoutingGameSimBase(graph) {
  
  private val network = new LatencyNetwork(graph, latencyFunctions, commodities)
  private val game = new StochasticRoutingGame(network, noiseFunction)
  
  def runFor(T: Int, nbSim: Int, eqFlows: Stream[Array[DenseVector[Double]]]) {
    
    def sumStreams(ass: Stream[Array[DenseVector[Double]]], bss: Stream[Array[DenseVector[Double]]]): Stream[Array[DenseVector[Double]]] = {
      ass zip bss map ({case(as, bs) => as zip bs map {case(a, b) => a+b}})
    }
    
    def distance(ass: Stream[Array[DenseVector[Double]]], bss: Stream[Array[DenseVector[Double]]]): Stream[Array[DenseVector[Double]]] = {
      ass zip bss map ({case(as, bs) => {as zip bs map {case(a, b) => DenseVector((a-b).norm(2))}}})
      }
    
    def scaleStreams(lambda: Double, ass: Stream[Array[DenseVector[Double]]]): Stream[Array[DenseVector[Double]]] = {
      ass map (as => as map (a => a*lambda))
    }
    
    val algorithms = MWAlgorithmsFromCommodities[RoutingExpert](commodities)
    val legend = commodities.map(_.paths.map(pathToString))
    
    var coordinator = new MWCoordinator[StochasticRoutingGame](game, algorithms, randomizedStart)
    var flows = coordinator.gameStateStream.map(_.pathFlows)
    val regrets = coordinator.regretsStream
    val maxRegrets = coordinator.maxRegretsStream
    val losses = coordinator.lossStream // these are the noisy losses
    val distances = distance(flows, eqFlows)
    val potentials = coordinator.gameStateStream.map(s => Array(DenseVector(s.potentialValue)))
    
    // One realization
//    Visualizer("Path Flows").plotLineGroups(flows.take(T), "t", "f(t)", legend).exportToPdf("out/flows")
//    Visualizer("Path Latencies").plotLineGroups(losses.take(T), "t", "l(t)", legend).exportToPdf("out/latencies")
    Visualizer("Instantaneous Regrets").plotLineGroups(regrets.take(T), "t", "Instantaneous Regrets", legend).exportToPdf("out/regrets")
//    Visualizer("Variances").plotLineGroups(distances.take(T), "t", "Distance to optimum", legend).exportToPdf("out/vars")
    Visualizer("Potentials").plotLineGroups(potentials.take(T), "t", "Potentials", legend).exportToPdf("out/potentials")
    
    exportToCSV("out/flows.csv", flows.take(T), Array("f11", "f12", "f13", "f21", "f22", "f23"))
    exportToCSV("out/latencies.csv", losses.take(T), Array("l11", "l12", "l13", "l21", "l22", "l23"))
    exportToCSV("out/regrets.csv", regrets.take(T), Array("r11", "r12", "r13", "r21", "r22", "r23"))
    exportToCSV("out/distances.csv", distances.take(T), Array("v1", "v2"))
    exportToCSV("out/potentials.csv", potentials.take(T), Array("p"))
    
    var cFlows = flows
    var cVariance = distances
    var cLatencies = losses
    var cRegrets = regrets
    var cMaxRegrets = maxRegrets
    var cPotentials = potentials
    
    for(i <- 2 to nbSim) {
      coordinator = new MWCoordinator[StochasticRoutingGame](game, algorithms, randomizedStart)
      flows = coordinator.gameStateStream.map(_.pathFlows)
      cFlows = sumStreams(cFlows, flows)
      cLatencies = sumStreams(cLatencies, coordinator.lossStream)
      cRegrets = sumStreams(cRegrets, coordinator.regretsStream)
      cMaxRegrets = sumStreams(cMaxRegrets, coordinator.maxRegretsStream)
      cVariance = sumStreams(cVariance, distance(flows, eqFlows))
      cPotentials = sumStreams(cPotentials, coordinator.gameStateStream.map(s => Array(DenseVector(s.potentialValue))))
    }
    
    cFlows = scaleStreams(1./nbSim, cFlows)
    cLatencies = scaleStreams(1./nbSim, cLatencies)
    cRegrets = scaleStreams(1./nbSim, cRegrets)
    cMaxRegrets = scaleStreams(1./nbSim, cMaxRegrets)
    cVariance = scaleStreams(1./nbSim, cVariance)
    cPotentials = scaleStreams(1./nbSim, cPotentials)
    
    // Expected
//    Visualizer("Expected Path Flows").plotLineGroups(cFlows.take(T), "t", "f(t)", legend).exportToPdf("out/expflows")
//    Visualizer("Expected Path Latencies").plotLineGroups(cLatencies.take(T), "t", "l(t)", legend).exportToPdf("out/explatencies")
//    Visualizer("Expected Instantaneous Regrets").plotLineGroups(cRegrets.take(T), "t", "Expected Instantaneous Regrets", legend).exportToPdf("out/expregrets")
    Visualizer("Expected Max Regrets").plotLineGroups(cMaxRegrets.take(T), "t", "Expected Max Regrets", legend).exportToPdf("out/expmaxregrets")
//    Visualizer("Variances").plotLineGroups(cVariance.take(T), "t", "Variances", legend).exportToPdf("out/expvars")
    Visualizer("Expected Potentials").plotLineGroups(cPotentials.take(T), "t", "Expected Potentials", legend).exportToPdf("out/exppotentials")
    
    exportToCSV("out/expflows.csv", cFlows.take(T), Array("f11", "f12", "f13", "f21", "f22", "f23"))
    exportToCSV("out/explatencies.csv", cLatencies.take(T), Array("l11", "l12", "l13", "l21", "l22", "l23"))
    exportToCSV("out/expregrets.csv", cRegrets.take(T), Array("r11", "r12", "r13", "r21", "r22", "r23"))
    exportToCSV("out/expdistances.csv", cVariance.take(T), Array("v1", "v2"))
    exportToCSV("out/expmaxregrets.csv", cMaxRegrets.take(T), Array("r1", "r2"))
    exportToCSV("out/exppotentials.csv", cPotentials.take(T), Array("p"))
  }
  
}