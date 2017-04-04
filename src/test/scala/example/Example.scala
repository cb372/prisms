package example

import monocle._

@Prisms
sealed trait X

case class Y() extends X
case object Z extends X

object Example extends App {
  val prism = X.prisms.Y
  println(prism.getOption(Y()))
  println(prism.getOption(Z))
}

