package eu.timepit.refined

object TestUtils {
  def consistent[P, T](p: Predicate1[P, T]): T => Boolean =
    t => p.isValid(t) == p.validate(t).isEmpty
}
