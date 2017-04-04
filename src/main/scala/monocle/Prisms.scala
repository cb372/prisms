package monocle

import scala.reflect.macros.{blackbox,whitebox}
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

        val name = tpname.toTermName

        q"""
        $traitDef

        object $name {
          val prisms = _root_.monocle.DefMacros.genPrisms[$tpname]
        }
        """

      case _ => c.abort(c.enclosingPosition, "sorry")
    }
  }

}

trait Marker

trait Prism[A, B] {
  def getOption(a: A): Option[B]
}

import scala.language.dynamics

object DefMacros {

  class DynamicPrisms[A] extends Marker with Dynamic {
    def selectDynamic(name: String): Any = macro _root_.monocle.DefMacrosImpl.genPrism_impl[A]
  }

  def genPrisms[A]: DynamicPrisms[A] = macro DefMacrosImpl.genPrisms_impl[A]

}

class DefMacrosImpl(val c: whitebox.Context) {
  import c.universe._

  def genPrism_impl[A: c.WeakTypeTag](name: c.Expr[String]): c.Tree = {
    val A = weakTypeOf[A]

    val subclassName = name.tree match {
      case Literal(Constant(str: String)) => str
      case _ => c.abort(c.enclosingPosition, "nope")
    }

    val subs = A.typeSymbol.asClass.knownDirectSubclasses
    subs.find(_.name.toString == subclassName) match {
      case Some(subclass) =>
        val B = subclass.info.typeSymbol
        q"""
        new Prism[$A, $B] {
          def getOption(a: $A): Option[$B] =
            if (a.isInstanceOf[$B]) Some(a.asInstanceOf[$B]) else None
        }
        """
      case None =>
        c.abort(c.enclosingPosition, s"$subclassName is not a direct subclass of $A")

    }

  }

  def genPrisms_impl[A: c.WeakTypeTag]: c.Tree = {
    val tpe = weakTypeOf[A]
    q"""
    new _root_.monocle.DefMacros.DynamicPrisms[$tpe]
    """
  }

}
