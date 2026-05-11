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

  /*val blocked: Set[(Int, Int)] = Set(
    (1, 0), (1, 1), (1, 2), (1, 3),
    (2, 3), (3, 3), (4, 3)
  )*/

  val path = Set(
    (4, 4), (4, 3),
    (3, 3), (2, 3),
    (2, 2), (2, 1),
    (1, 1),
    (1, 0),
    (0, 0)
  )

  private val blocked: Set[(Int, Int)] =
    (for
      x <- 0 until 5
      y <- 0 until 5
      if !path.contains((x, y))
    yield (x, y)).toSet

  val rl: QMatrix.FacadeWithFixedObstacles = FacadeWithFixedObstacles(
    width = 5,
    height = 5,
    initial = (4, 4),
    terminal = {
      case _ => false
    },
    reward = {
      case ((0, 0), _) => 10
      case _ => 0
    },
    jumps = {
      case ((0, 0), _) => (4, 4)
    },
    obstacles = blocked,
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
  import u09.model.QMatrix.*
  import u09.model.QMatrix.Move.*
  import u09.model.QRLImpl
