package eu.timepit.refined

import eu.timepit.refined.TestUtils._
import eu.timepit.refined.boolean._
import eu.timepit.refined.char.{ Digit, Letter, UpperCase, Whitespace }
import eu.timepit.refined.numeric.{ Greater, Less }
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._
import shapeless.{ ::, HNil }

class BooleanPredicateSpec extends Properties("BooleanPredicate") {

  type FF[Op[_, _]] = False Op False
  type FT[Op[_, _]] = False Op True
  type TF[Op[_, _]] = True Op False
  type TT[Op[_, _]] = True Op True

  property("True.isValid") = secure {
    Predicate0[True, Unit].isValid
  }

  property("True.show") = secure {
    Predicate0[True, Unit].show ?= "true"
  }

  property("Not[True].isValid") = secure {
    Predicate1[Not[True], Unit].notValid(())
  }

  property("Not[True].show") = secure {
    Predicate1[Not[True], Unit].show(()) ?= "!true"
  }

  property("Not.consistent") = secure {
    consistent(Predicate1[Not[True], Unit])(()) &&
      consistent(Predicate1[Not[False], Unit])(())
  }

  property("False.isValid") = secure {
    Predicate1[False, Unit].notValid(())
  }

  property("False.show") = secure {
    Predicate1[False, Unit].show(()) ?= "false"
  }

  property("Not[False].isValid") = secure {
    Predicate1[Not[False], Unit].isValid(())
  }

  property("And.isValid") = secure {
    Predicate1[FF[And], Unit].notValid(()) &&
      Predicate1[FT[And], Unit].notValid(()) &&
      Predicate1[TF[And], Unit].notValid(()) &&
      Predicate1[TT[And], Unit].isValid(())
  }

  property("And.consistent") = secure {
    consistent(Predicate1[FF[And], Unit])(()) &&
      consistent(Predicate1[FT[And], Unit])(()) &&
      consistent(Predicate1[TF[And], Unit])(()) &&
      consistent(Predicate1[TT[And], Unit])(())
  }

  property("And.show") = secure {
    Predicate1[TF[And], Unit].show(()) ?= "(true && false)"
  }

  property("Or.isValid") = secure {
    Predicate1[FF[Or], Unit].notValid(()) &&
      Predicate1[FT[Or], Unit].isValid(()) &&
      Predicate1[TF[Or], Unit].isValid(()) &&
      Predicate1[TT[Or], Unit].isValid(())
  }

  property("Or.consistent") = secure {
    consistent(Predicate1[FF[Or], Unit])(()) &&
      consistent(Predicate1[FT[Or], Unit])(()) &&
      consistent(Predicate1[TF[Or], Unit])(()) &&
      consistent(Predicate1[TT[Or], Unit])(())
  }

  property("Or.show") = secure {
    Predicate1[TF[Or], Unit].show(()) ?= "(true || false)"
  }

  property("Xor.isValid") = secure {
    Predicate1[FF[Xor], Unit].notValid(()) &&
      Predicate1[FT[Xor], Unit].isValid(()) &&
      Predicate1[TF[Xor], Unit].isValid(()) &&
      Predicate1[TT[Xor], Unit].notValid(())
  }

  property("Xor.consistent") = secure {
    consistent(Predicate1[FF[Xor], Unit])(()) &&
      consistent(Predicate1[FT[Xor], Unit])(()) &&
      consistent(Predicate1[TF[Xor], Unit])(()) &&
      consistent(Predicate1[TT[Xor], Unit])(())
  }

  property("Xor.show") = secure {
    Predicate1[TF[Xor], Unit].show(()) ?= "(true ^ false)"
  }

  property("AllOf[Greater[_0] :: Less[_10] :: HNil].isValid") = forAll { (i: Int) =>
    Predicate1[AllOf[Greater[_0] :: Less[_10] :: HNil], Int].isValid(i) ?=
      (i > 0 && i < 10)
  }

  property("AllOf[Greater[_0] :: Less[_10] :: HNil].show") = secure {
    Predicate1[AllOf[Greater[_0] :: Less[_10] :: HNil], Int].show(5) ?=
      "((5 > 0) && ((5 < 10) && true))"
  }

  property("AnyOf[Digit :: Letter :: Whitespace :: HNil].isValid") = forAll { (c: Char) =>
    Predicate1[AnyOf[Digit :: Letter :: Whitespace :: HNil], Char].isValid(c) ?=
      (c.isDigit || c.isLetter || c.isWhitespace)
  }

  property("AnyOf[Digit :: Letter :: Whitespace :: HNil].show") = secure {
    Predicate1[AnyOf[Digit :: Letter :: Whitespace :: HNil], Char].show('c') ?=
      "(isDigit('c') || (isLetter('c') || (isWhitespace('c') || false)))"
  }

  property("OneOf[Digit :: Letter :: UpperCase :: HNil].isValid") = forAll { (c: Char) =>
    Predicate1[OneOf[Digit :: Letter :: UpperCase :: HNil], Char].isValid(c) ?=
      List(c.isDigit, c.isLetter, c.isUpper).count(identity) == 1
  }

  property("OneOf[Digit :: Letter :: UpperCase :: HNil].show") = secure {
    Predicate1[OneOf[Digit :: Letter :: UpperCase :: HNil], Char].show('c') ?=
      "oneOf(isDigit('c'), isLetter('c'), isUpper('c'), false)"
  }

  property("OneOf[_].consistent") = forAll {
    consistent(Predicate1[OneOf[Digit :: Letter :: UpperCase :: HNil], Char])
  }

  property("OneOf[_].contramap(identity).accumulateIsValid") = forAll { (c: Char) =>
    val p = Predicate1[OneOf[Digit :: Letter :: UpperCase :: HNil], Char]
    p.contramap(identity[Char]).accumulateIsValid(c) ?= p.accumulateIsValid(c)
  }

  property("OneOf[_].contramap(identity).accumulateShow") = secure {
    val p = Predicate1[OneOf[Digit :: Letter :: UpperCase :: HNil], Char]
    p.contramap(identity[Char]).accumulateShow('c') ?= p.accumulateShow('c')
  }
}
