package eu.timepit.refined

import shapeless.tag.@@

import scala.util.{ Failure, Success, Try }

/**
 * Type class for validating values of type `T` according to a type-level
 * predicate `P`. The semantics of `P` are defined by the instance(s) of
 * this type class for `P`.
 */
trait Predicate[P, T] extends Serializable { self =>

  /** Checks if `t` satisfies the predicate `P`. */
  def isValid(t: T): Boolean

  /** Returns a string representation of this [[Predicate]] using `t`. */
  def show(t: T): String

  /**
   * Returns `None` if `t` satisfies the predicate `P`, or an error message
   * contained in `Some` otherwise.
   */
  def validate(t: T): Option[String] =
    if (isValid(t)) None else Some(s"Predicate failed: ${show(t)}.")

  /**
   * Returns `t` tagged with the predicate `P` on the right if it satisfies
   * it, or an error message on the left otherwise.
   */
  final def refine(t: T): Either[String, T @@ P] =
    validate(t) match {
      case None => Right(t.asInstanceOf[T @@ P])
      case Some(s) => Left(s)
    }

  /** Checks if `t` does not satisfy the predicate `P`. */
  final def notValid(t: T): Boolean =
    !isValid(t)

  /**
   * Returns the result of [[isValid]] in a `List`. Can be overridden to
   * accumulate the results of sub-predicates.
   */
  def accumulateIsValid(t: T): List[Boolean] =
    List(isValid(t))

  /**
   * Returns the result of [[show]] in a `List`. Can be overridden to
   * accumulate the string representations of sub-predicates.
   */
  def accumulateShow(t: T): List[String] =
    List(show(t))

  private[refined] def contramap[U](f: U => T): Predicate[P, U] =
    new Predicate[P, U] {
      def isValid(u: U): Boolean = self.isValid(f(u))
      def show(u: U): String = self.show(f(u))
      override def validate(u: U): Option[String] = self.validate(f(u))
      override def accumulateIsValid(u: U): List[Boolean] = self.accumulateIsValid(f(u))
      override def accumulateShow(u: U): List[String] = self.accumulateShow(f(u))
    }
}

object Predicate {

  def apply[P, T](implicit p: Predicate[P, T]): Predicate[P, T] = p

  /** Constructs a [[Predicate]] from its parameters. */
  def instance[P, T](isValidF: T => Boolean, showF: T => String): Predicate[P, T] =
    new Predicate[P, T] {
      def isValid(t: T): Boolean = isValidF(t)
      def show(t: T): String = showF(t)
    }

  /**
   * Constructs a [[Predicate]] from the partial function `pf`. All `T`s for
   * which `pf` throws an exception are considered invalid according to `P`.
   */
  def fromPartial[P, T, U](pf: T => U, showT: T => String): Predicate[P, T] =
    new Predicate[P, T] {
      def isValid(t: T): Boolean = Try(pf(t)).isSuccess
      def show(t: T): String = showT(t)

      override def validate(t: T): Option[String] =
        Try(pf(t)) match {
          case Success(_) => None
          case Failure(ex) => Some(s"Predicate ${show(t)} failed: ${ex.getMessage}")
        }
    }
}

trait ConstPredicate[P, T] extends Predicate[P, T] {

  def constIsValid: Boolean

  def constShow: String

  def constValidate: Option[String] =
    if (constIsValid) None else Some(s"Predicate failed: $constShow.")

  override def isValid(t: T): Boolean = constIsValid

  override def show(t: T): String = constShow
}

object ConstPredicate {

  def instance[P, T](isValidV: Boolean, showV: String): ConstPredicate[P, T] =
    new ConstPredicate[P, T] {
      override val constIsValid: Boolean = isValidV
      override val constShow: String = showV
    }

  def valid[P, T]: ConstPredicate[P, T] =
    instance(isValidV = true, "true")

  def invalid[P, T]: ConstPredicate[P, T] =
    instance(isValidV = false, "false")
}
