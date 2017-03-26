package example

import monocle._

@Prisms
sealed trait X

class Y extends X
class Z extends X

object Example {
  //val foo = DefMacro.printSubclasses[X]
}

