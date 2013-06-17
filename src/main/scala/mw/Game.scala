package mw
import breeze.linalg.DenseVector

abstract class GameState 

/** Abstract class that represents a Game, i.e. everything that is external to 
 *  the experts algorithm.
 *  
 *  An extending `class G extends Game` can interact with `MWAlgorithm`s
 *  through an instance of a `MWCoordinator[N]`. The coordinator will run each
 *  algorithm to get the current strategies, and update the state of game 
 *  using those strategies. It is recommended that extending classes be 
 *  immutable, and the state of game be entirely captured in the type State.
 *  
 *  See [[mw.RoutingGame]] for an example of an extending class
 */
abstract class Game {
  type State<:GameState
  
  /** @param strategies: the initial strategies of the MWAlgorithms 
   *  interacting with this instance of game, returns the initial state of 
   *  game
   *  @return the initial state of this instance  
   */
  def initialState(strategies: Array[DenseVector[Double]]): State
  
  /** @param state: the current state of game 
   *  @param strategies: the current strategies of the MWAlgorithms
   *  @return the updated state of game 
   */
  def update(state: State, strategies: Array[DenseVector[Double]]): State
  
  /** @param state: the current state of game 
   *  @param expert: an `Expert` (action) of one MWAlgorithms interacting 
   *  with game
   *  @return the loss of the expert given the state
   */
  def loss(state: State)(expert: Expert): Double
}
