package eu.timepit.refined

import eu.timepit.refined.boolean.Or
import eu.timepit.refined.char._

object char extends CharPredicates {

  /** Predicate that checks if a `Char` is a digit. */
  trait Digit

  /** Predicate that checks if a `Char` is a letter. */
  trait Letter

  /** Predicate that checks if a `Char` is a lower case character. */
  trait LowerCase

  /** Predicate that checks if a `Char` is an upper case character. */
  trait UpperCase

  /** Predicate that checks if a `Char` is white space. */
  trait Whitespace

  /** Predicate that checks if a `Char` is a letter or digit. */
  type LetterOrDigit = Letter Or Digit
}

private[refined] trait CharPredicates {

  implicit val digitPredicate: Predicate1[Digit, Char] =
    Predicate1.instance(_.isDigit, t => s"isDigit('$t')")

  implicit val letterPredicate: Predicate1[Letter, Char] =
    Predicate1.instance(_.isLetter, t => s"isLetter('$t')")

  implicit val lowerCasePredicate: Predicate1[LowerCase, Char] =
    Predicate1.instance(_.isLower, t => s"isLower('$t')")

  implicit val upperCasePredicate: Predicate1[UpperCase, Char] =
    Predicate1.instance(_.isUpper, t => s"isUpper('$t')")

  implicit val whitespacePredicate: Predicate1[Whitespace, Char] =
    Predicate1.instance(_.isWhitespace, t => s"isWhitespace('$t')")
}
