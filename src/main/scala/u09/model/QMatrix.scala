package scala.u09.model

object QMatrix:

  type Node = (Int, Int)

  enum Move:
    case LEFT, RIGHT, UP, DOWN
    override def toString = Map(LEFT -> "<", RIGHT -> ">", UP -> "^", DOWN -> "v")(this)

  import Move.*

  sealed abstract class AbstractFacade(
                     width: Int,
                     height: Int,
                     initial: Node,
                     terminal: PartialFunction[Node, Boolean],
                     reward: PartialFunction[(Node, Move), Double],
                     jumps: PartialFunction[(Node, Move), Node],
                     gamma: Double,
                     alpha: Double,
                     epsilon: Double = 0.0,
                     v0: Double) extends QRLImpl:
    type State = Node
    type Action = Move

    protected def calculateN2(s: (Int, Int), a: Move): (Int, Int) = (s, a) match
        case ((n1, n2), UP) => (n1, (n2 - 1) max 0)
        case ((n1, n2), DOWN) => (n1, (n2 + 1) min (height - 1))
        case ((n1, n2), LEFT) => ((n1 - 1) max 0, n2)
        case ((n1, n2), RIGHT) => ((n1 + 1) min (width - 1), n2)
        case _ => ???

    def qEnvironment(): Environment = (s: Node, a: Move) =>
      // applies direction, without escaping borders
      val n2: Node = calculateN2(s, a)
      // computes rewards, and possibly a jump
      (reward.apply((s, a)), jumps.orElse[(Node, Move), Node](_ => n2)(s, a))

    def qFunction = QFunction(Move.values.toSet, v0, terminal)

    def qSystem = QSystem(environment = qEnvironment(), initial, terminal)

    def makeLearningInstance() = QLearning(qSystem, gamma, alpha, epsilon, qFunction)

    def show[E](v: Node => E, formatString: String): String =
      (for
        row <- 0 until width
        col <- 0 until height
      yield formatString.format(v((col, row))) + (if (col == height - 1) "\n" else "\t"))
        .mkString("")

  case class Facade(
                     width: Int,
                     height: Int,
                     initial: Node,
                     terminal: PartialFunction[Node, Boolean],
                     reward: PartialFunction[(Node, Move), Double],
                     jumps: PartialFunction[(Node, Move), Node],
                     gamma: Double,
                     alpha: Double,
                     epsilon: Double = 0.0,
                     v0: Double)
    extends AbstractFacade(width, height, initial, terminal, reward, jumps, gamma, alpha, epsilon, v0)

  case class FacadeWithFixedObstacles(width: Int,
                                      height: Int,
                                      initial: Node,
                                      terminal: PartialFunction[Node, Boolean],
                                      reward: PartialFunction[(Node, Move), Double],
                                      jumps: PartialFunction[(Node, Move), Node],
                                      obstacles: Set[Node] = Set.empty,
                                      gamma: Double,
                                      alpha: Double,
                                      epsilon: Double = 0.0,
                                      v0: Double)
    extends AbstractFacade(width, height, initial, terminal, reward, jumps, gamma, alpha, epsilon, v0):

    override def qEnvironment(): Environment = (s: Node, a: Move) =>
      val n2: Node = calculateN2(s, a)
      if (obstacles.contains(n2))
        (-100.0, s)
      else
        (reward.applyOrElse((s, a), _ => 0.0), jumps.orElse[(Node, Move), Node](_ => n2)(s, a))