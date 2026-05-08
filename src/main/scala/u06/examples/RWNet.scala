package scala.u06.examples

import u06.modelling.PetriNet

object RWNet:

  enum ReadWrite:
    case P1, P2, R1, R2, W1, W2, P5

  export ReadWrite.*
  export u06.modelling.PetriNet.*
  export u06.modelling.SystemAnalysis.*
  export u06.utils.MSet

  def rwNet = PetriNet[ReadWrite](
    MSet(P1) ~~> MSet(P2),
    //read
    MSet(P2) ~~> MSet(R1),
    MSet(R1, P5) ~~> MSet(R2, P5),
    MSet(R2) ~~> MSet(P1),
    //write
    MSet(P2) ~~> MSet(W1),
    MSet(W1, P5) ~~> MSet(W2) ^^^ MSet(R2),
    MSet(W2) ~~> MSet(P1, P5)
  ).toSystem

@main def mainRWNet() =
  import RWNet.*
  println(rwNet.paths(MSet(P1, P5),6).toList.mkString("\n"))