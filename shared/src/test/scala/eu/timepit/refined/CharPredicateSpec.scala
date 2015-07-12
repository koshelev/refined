package eu.timepit.refined

import eu.timepit.refined.char._
import org.scalacheck.Prop._
import org.scalacheck.Properties

class CharPredicateSpec extends Properties("CharPredicate") {

  property("Digit.isValid") = forAll { (c: Char) =>
    Predicate1[Digit, Char].isValid(c) ?= c.isDigit
  }

  property("Digit.show") = secure {
    Predicate1[Digit, Char].show('c') ?= "isDigit('c')"
  }

  property("Letter.isValid") = forAll { (c: Char) =>
    Predicate1[Letter, Char].isValid(c) ?= c.isLetter
  }

  property("Letter.show") = secure {
    Predicate1[Letter, Char].show('c') ?= "isLetter('c')"
  }

  property("LowerCase.isValid") = forAll { (c: Char) =>
    Predicate1[LowerCase, Char].isValid(c) ?= c.isLower
  }

  property("LowerCase.show") = secure {
    Predicate1[LowerCase, Char].show('c') ?= "isLower('c')"
  }

  property("UpperCase.isValid") = forAll { (c: Char) =>
    Predicate1[UpperCase, Char].isValid(c) ?= c.isUpper
  }

  property("UpperCase.show") = secure {
    Predicate1[UpperCase, Char].show('c') ?= "isUpper('c')"
  }
}
