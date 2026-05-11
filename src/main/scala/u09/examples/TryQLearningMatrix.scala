package scala.u09.examples

import u09.model.QMatrix

object TryQLearningMatrix extends App :

  import u09.model.QMatrix.Move.*
  import u09.model.QMatrix.*

  val rl: QMatrix.Facade = Facade(
    width = 5,
    height = 5,
    initial = (0,0),
    terminal = {case _=>false},
    reward = { case ((1,0),_) => 10; case ((3,0),_) => 5; case _ => 0},
    jumps = { case ((1,0),_) => (1,4); case ((3,0),_) => (3,2) },
    gamma = 0.9,
    alpha = 0.5,
    epsilon = 0.3,
    v0 = 1
  )

  val q0 = rl.qFunction
  println(rl.show(q0.vFunction,"%2.2f"))
  val q1 = rl.makeLearningInstance().learn(10000,100,q0)
  println(rl.show(q1.vFunction,"%2.2f"))
  println(rl.show(s => q1.bestPolicy(s).toString,"%7s"))

// Path with fixed obstacles
object Task1 extends App:

  import u09.model.QMatrix.Move.*
  import u09.model.QMatrix.*

  val blocked: Set[(Int, Int)] = Set(
    (1, 0), (1, 1), (1, 2), (1, 3), // seconda colonna (x=1) tranne l'ultima posizione (y=4)
    (2, 3), (3, 3), (4, 3)          // penultima riga (y=3) tranne la prima posizione (x=0) e (1,3) già incluso
  )

  def nextState(s: (Int, Int), a: Move): (Int, Int) = a match
    case UP => (s._1, (s._2 - 1) max 0)
    case DOWN => (s._1, (s._2 + 1) min 4)
    case LEFT => ((s._1 - 1) max 0, s._2)
    case RIGHT => ((s._1 + 1) min 4, s._2)

  val rl: QMatrix.Facade = Facade(
    width = 5,
    height = 5,
    initial = (4, 4),
    terminal = {
      //case (0, 0) => true
      case _ => false
    },
    reward = {
      //case (s, a) if !blocked.contains(s) && nextState(s, a) == (0, 0) => 10
      case ((0, 0), _) => 10
      case (s, a) if blocked.contains(nextState(s, a)) => -100
      case _ => 0
    },
    jumps = {
      case ((0, 0), _) => (4, 4)
      case (s, a) if blocked.contains(nextState(s, a)) => s
    },
    gamma = 0.9,
    alpha = 0.5,
    epsilon = 0.3,
    v0 = 1
  )

  val q0 = rl.qFunction
  println(rl.show(s => if blocked.contains(s) then 0.0 else q0.vFunction(s), "%2.2f"))
  val q1 = rl.makeLearningInstance().learn(10000, 100, q0)
  println(rl.show(s => if blocked.contains(s) then 0.0 else q1.vFunction(s), "%2.2f"))
  println(rl.show(s => if blocked.contains(s) then "X" else q1.bestPolicy(s).toString, "%7s"))

// Path with movable obstacles
object Task2 extends App:
  import u09.model.QMatrix.Move.*
  import u09.model.QMatrix.*

  // TODO