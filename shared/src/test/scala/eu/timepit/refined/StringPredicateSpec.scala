package eu.timepit.refined

import eu.timepit.refined.boolean._
import eu.timepit.refined.char._
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._

class StringPredicateSpec extends Properties("StringPredicate") {

  property("Empty.isValid") = forAll { (s: String) =>
    Predicate1[Empty, String].isValid(s) ?= s.isEmpty
  }

  property("Empty.show") = secure {
    Predicate1[Empty, String].show("test") ?= "isEmpty(test)"
  }

  property("NonEmpty.isValid") = forAll { (s: String) =>
    Predicate1[NonEmpty, String].isValid(s) ?= s.nonEmpty
  }

  property("NonEmpty.show") = secure {
    Predicate1[NonEmpty, String].show("test") ?= "!isEmpty(test)"
  }

  property("Forall[LowerCase].isValid") = forAll { (s: String) =>
    Predicate1[Forall[LowerCase], String].isValid(s) ?= s.forall(_.isLower)
  }

  property("Forall[LowerCase].show") = secure {
    Predicate1[Forall[LowerCase], String].show("abc") ?=
      "(isLower('a') && isLower('b') && isLower('c'))"
  }

  property("Forall[UpperCase].isValid") = forAll { (s: String) =>
    Predicate1[Forall[UpperCase], String].isValid(s) ?= s.forall(_.isUpper)
  }

  property("Forall[UpperCase].show") = secure {
    Predicate1[Forall[UpperCase], String].show("abc") ?=
      "(isUpper('a') && isUpper('b') && isUpper('c'))"
  }

  property("Head[Letter].isValid") = forAll { (s: String) =>
    Predicate1[Head[Letter], String].isValid(s) ?= s.headOption.fold(false)(_.isLetter)
  }

  property("Last[Letter].show") = secure {
    Predicate1[Last[Letter], String].show("abc0") ?= "isLetter('0')"
  }

  property("Size.isValid") = forAll { (s: String) =>
    Predicate1[Size[LessEqual[_10]], String].isValid(s) ?= (s.length <= 10)
  }

  property("Size.validate") = forAll { (s: String) =>
    Predicate1[Size[LessEqual[_10]], String].validate(s).isEmpty ?= (s.length <= 10)
  }

  property("Size.show") = secure {
    type P = Size[Greater[_5] And LessEqual[_10]]
    Predicate1[P, String].show("test") ?= "((4 > 5) && !(4 > 10))"
  }

  property("Count[LowerCase, Greater[_2]].isValid") = forAll { (s: String) =>
    Predicate1[Count[LowerCase, Greater[_2]], String].isValid(s) ?= (s.count(_.isLower) > 2)
  }

  property("MinSize[_5].isValid") = forAll { (s: String) =>
    Predicate1[MinSize[_5], String].isValid(s) ?= (s.length >= 5)
  }

  property("MatchesRegex[R].isValid") = forAll { (s: String) =>
    Predicate1[MatchesRegex[W.`".{2,10}"`.T], String].isValid(s) ?= s.matches(".{2,10}")
  }

  property("MatchesRegex[R].show") = secure {
    Predicate1[MatchesRegex[W.`".{2,10}"`.T], String].show("Hello") ?=
      """"Hello".matches(".{2,10}")"""
  }

  property("EndsWith[S].isValid") = secure {
    val s = "abcd"
    Predicate1[EndsWith[W.`"cd"`.T], String].isValid(s) ?= s.endsWith("cd")
  }

  property("EndsWith[S].show") = secure {
    val s = "abcd"
    Predicate1[EndsWith[W.`"cd"`.T], String].show(s) ?= """"abcd".endsWith("cd")"""
  }

  property("StartsWith[S].isValid") = secure {
    val s = "abcd"
    Predicate1[StartsWith[W.`"ab"`.T], String].isValid(s) ?= s.startsWith("ab")
  }

  property("StartsWith[S].show") = secure {
    val s = "abcd"
    Predicate1[StartsWith[W.`"ab"`.T], String].show(s) ?= """"abcd".startsWith("ab")"""
  }

  property("Regex.isValid") = secure {
    Predicate1[Regex, String].isValid(".*")
  }

  property("Regex.notValid") = secure {
    Predicate1[Regex, String].notValid("(a|b")
  }

  property("Regex.show") = secure {
    Predicate1[Regex, String].show("(a|b)") ?= """isValidRegex("(a|b)")"""
  }

  property("Regex.validate success") = secure {
    Predicate1[Regex, String].validate("(a|b)") ?= None
  }

  property("Regex.validate failure") = secure {
    Predicate1[Regex, String].validate("(a|b") ?=
      Some(
        """Predicate isValidRegex("(a|b") failed: Unclosed group near index 4
          |(a|b
          |    ^""".stripMargin)
  }

  property("Uri.validate success") = secure {
    Predicate1[Uri, String].validate("/a/b/c") ?= None
  }

  property("Uri.validate failure") = secure {
    Predicate1[Uri, String].validate(" /a/b/c") ?=
      Some("Predicate isValidUri(\" /a/b/c\") failed: Illegal character in path at index 0:  /a/b/c")
  }

  property("Url.validate success") = secure {
    Predicate1[Url, String].validate("http://example.com") ?= None
  }

  property("Url.validate failure") = secure {
    Predicate1[Url, String].validate("htp://example.com") ?=
      Some("Predicate isValidUrl(\"htp://example.com\") failed: unknown protocol: htp")
  }
}
