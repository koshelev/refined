package eu.timepit.refined
package internal

import cats.data.Xor

/**
 * Helper class that allows the type `T` to be inferred from calls like
 * `[[refineV]][P](t)`. See [[http://tpolecat.github.io/2015/07/30/infer.html]]
 * for a detailed explanation of this trick.
 */
final class RefineAux[P, F[_, _]] {

  def apply[T](t: T)(implicit p: Predicate[P, T], w: Wrapper[F]): Xor[String, F[T, P]] =
    p.validate(t) match {
      case None => Xor.right(w.wrap(t))
      case Some(s) => Xor.left(s)
    }
}
