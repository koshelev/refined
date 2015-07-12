package eu.timepit.refined

import shapeless.tag.@@

import scala.util.{ Failure, Success, Try }

sealed trait Predicate[P, T] extends Serializable

trait Predicate0[P, T] extends Predicate[P, T] {

  def isValid: Boolean

  def show: String
}

object Predicate0 {

  def apply[P, T](implicit p: Predicate0[P, T]): Predicate0[P, T] = p

  def instance[P, T](validateT: Boolean, showT: String): Predicate0[P, T] =
    new Predicate0[P, T] {
      def isValid: Boolean = validateT
      def show: String = showT
    }

  def alwaysValid[P, T]: Predicate0[P, T] =
    instance(validateT = true, "true")

  def alwaysInvalid[P, T]: Predicate0[P, T] =
    instance(validateT = false, "false")
}

/**
 * Type class for validating values of type `T` according to a type-level
 * predicate `P`. The semantics of `P` are defined by the instance(s) of
 * this type class for `P`.
 */
trait Predicate1[P, T] extends Predicate[P, T] { self =>

  /** Checks if `t` satisfies the predicate `P`. */
  def isValid(t: T): Boolean

  /** Returns a string representation of this [[Predicate1]] using `t`. */
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

  private[refined] def contramap[U](f: U => T): Predicate1[P, U] =
    new Predicate1[P, U] {
      def isValid(u: U): Boolean = self.isValid(f(u))
      def show(u: U): String = self.show(f(u))
      override def validate(u: U): Option[String] = self.validate(f(u))
      override def accumulateIsValid(u: U): List[Boolean] = self.accumulateIsValid(f(u))
      override def accumulateShow(u: U): List[String] = self.accumulateShow(f(u))
    }
}

object Predicate1 {

  def apply[P, T](implicit p: Predicate1[P, T]): Predicate1[P, T] = p

  /** Constructs a [[Predicate1]] from its parameters. */
  def instance[P, T](validateT: T => Boolean, showT: T => String): Predicate1[P, T] =
    new Predicate1[P, T] {
      def isValid(t: T): Boolean = validateT(t)
      def show(t: T): String = showT(t)
    }

  /**
   * Constructs a [[Predicate1]] from the partial function `pf`. All `T`s for
   * which `pf` throws an exception are considered invalid according to `P`.
   */
  def fromPartial[P, T, U](pf: T => U, showT: T => String): Predicate1[P, T] =
    new Predicate1[P, T] {
      def isValid(t: T): Boolean = Try(pf(t)).isSuccess
      def show(t: T): String = showT(t)

      override def validate(t: T): Option[String] =
        Try(pf(t)) match {
          case Success(_) => None
          case Failure(ex) => Some(s"Predicate ${show(t)} failed: ${ex.getMessage}")
        }
    }

  def fromPredicate0[P, T](p0: Predicate0[P, T]): Predicate1[P, T] =
    instance(_ => p0.isValid, _ => p0.show)

  /** Returns a [[Predicate1]] that ignores its inputs and always yields `true`. */
  def alwaysValid[P, T]: Predicate1[P, T] =
    fromPredicate0(Predicate0.alwaysValid)

  /** Returns a [[Predicate1]] that ignores its inputs and always yields `false`. */
  def alwaysInvalid[P, T]: Predicate1[P, T] =
    fromPredicate0(Predicate0.alwaysInvalid)
}
