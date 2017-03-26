package monocle

import scala.reflect.macros.blackbox
import scala.language.experimental.macros

class Prisms extends scala.annotation.StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro PrismsImpl.prismsAnnotationMacro
}

class PrismsImpl(val c: blackbox.Context) {
  import c.universe._

  def prismsAnnotationMacro(annottees: c.Tree*): c.Tree = {
    annottees match {
      case (traitDef @ q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }")
        :: Nil if mods.hasFlag(Flag.SEALED) =>

        val tpe = c.typecheck(Ident(tpname), mode = c.TYPEmode, silent = true, withMacrosDisabled = true)

        /*
        Comment out the following two lines and the macro expands fine (but doesn't do anything interesting).

        Leave them in and you get weirdness ...

        `knownDirectSubclasses` works (so long SI-7046!), so it prints "Subclasses: Set(class Y, class Z)",
        but then the macro fails to expand:

        ---
[error] /Users/chris/code/prisms/src/test/scala/example/Example.scala:5: macro annotation could not be expanded (the most common reason for that is that you need to enable the macro paradise plugin; another possibility is that you try to use macro annotation in the same compilation run that defines it)
[error] @Prisms
[error]  ^
[error] one error found
        ---

        Similar code works fine in the def macro below.
        Maybe the problem is calling `knownDirectSubclasses` on the result of a call to `c.typecheck`?
         */
        val subs = tpe.symbol.asClass.knownDirectSubclasses
        println(s"Subclasses: $subs")

        traitDef

      case _ => c.abort(c.enclosingPosition, "sorry")
    }
  }

}

object DefMacro {

  def printSubclasses[A]: Int = macro impl[A]

  def impl[A: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._

    val tpe = weakTypeOf[A]

    val subs = tpe.typeSymbol.asClass.knownDirectSubclasses
    println(s"Def macro: $subs")

    q"42"
  }

}
