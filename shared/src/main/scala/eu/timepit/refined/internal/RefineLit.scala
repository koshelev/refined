package eu.timepit.refined
package internal

import shapeless.tag.@@

import scala.reflect.macros.blackbox

/**
 * Helper class that allows the type `T` to be inferred from calls like
 * `[[refineLit]][P](t)`.
 */
final class RefineLit[P] {
  def apply[T](t: T)(implicit p: Predicate[P, T]): T @@ P = macro RefineLit.macroImpl[P, T]
}

object RefineLit {
  def macroImpl[P: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context)(t: c.Expr[T])(p: c.Expr[Predicate[P, T]]): c.Expr[T @@ P] = {
    import c.universe._

    def refineExpr: Option[String] => c.Expr[T @@ P] = {
      case None => c.Expr(q"$t.asInstanceOf[${weakTypeOf[T @@ P]}]")
      case Some(err) => c.abort(c.enclosingPosition, err)
    }

    def literal: T = t.tree match {
      case Literal(Constant(value)) => value.asInstanceOf[T]
      case _ => c.abort(c.enclosingPosition,
        "compile-time refinement only works with literals or constant predicates")
    }

    MacroUtils.eval(c)(p) match {
      case pp: ConstPredicate[P, T] => refineExpr(pp.constValidate)
      case pp: Predicate[P, T] => refineExpr(pp.validate(literal))
    }
  }
}
