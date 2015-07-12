package eu.timepit.refined

import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._

class NumericPredicateSpec extends Properties("NumericPredicate") {

  property("Less.isValid") = forAll { (d: Double) =>
    Predicate1[Less[W.`1.0`.T], Double].isValid(d) ?= (d < 1.0)
  }

  property("Less.show") = secure {
    Predicate1[Less[W.`1.1`.T], Double].show(0.1) ?= "(0.1 < 1.1)"
  }

  property("Greater.isValid") = forAll { (d: Double) =>
    Predicate1[Greater[W.`1.0`.T], Double].isValid(d) ?= (d > 1.0)
  }

  property("Greater.show") = secure {
    Predicate1[Greater[W.`1.1`.T], Double].show(0.1) ?= "(0.1 > 1.1)"
  }

  property("Less.Nat ~= Less") = forAll { (i: Int) =>
    Predicate1[Less[_5], Int].validate(i) ?= Predicate1[Less[W.`5`.T], Int].validate(i)
  }

  property("Greater.Nat ~= Greater") = forAll { (i: Int) =>
    Predicate1[Greater[_5], Int].validate(i) ?= Predicate1[Greater[W.`5`.T], Int].validate(i)
  }

  property("Less.Nat.isValid") = forAll { (i: Int) =>
    Predicate1[Less[_5], Int].isValid(i) ?= (i < 5)
  }

  property("Less.Nat.show") = secure {
    Predicate1[Less[_5], Int].show(0) ?= "(0 < 5)"
  }

  property("LessEqual.Nat.isValid") = forAll { (i: Int) =>
    Predicate1[LessEqual[_5], Int].isValid(i) ?= (i <= 5)
  }

  property("LessEqual.Nat.show") = secure {
    Predicate1[LessEqual[_5], Int].show(0) ?= "!(0 > 5)"
  }

  property("Greater.Nat.isValid") = forAll { (i: Int) =>
    Predicate1[Greater[_5], Int].isValid(i) ?= (i > 5)
  }

  property("Greater.Nat.show") = secure {
    Predicate1[Greater[_5], Int].show(0) ?= "(0 > 5)"
  }

  property("GreaterEqual.Nat.isValid") = forAll { (i: Int) =>
    Predicate1[GreaterEqual[_5], Int].isValid(i) ?= (i >= 5)
  }

  property("GreaterEqual.Nat.show") = secure {
    Predicate1[GreaterEqual[_5], Int].show(0) ?= "!(0 < 5)"
  }

  property("Interval[_0, _1].isValid") = forAll { (d: Double) =>
    Predicate1[Interval[_0, _1], Double].isValid(d) ?= (d >= 0.0 && d <= 1.0)
  }

  property("Equal.Nat.isValid") = forAll { (i: Int) =>
    Predicate1[Equal[_5], Int].isValid(i) ?= (i == 5)
  }

  property("Equal.Nat.show") = secure {
    Predicate1[Equal[_5], Int].show(0) ?= "(0 == 5)"
  }

  property("Equal.Nat ~= Equal") = forAll { (i: Int) =>
    Predicate1[Equal[_1], Int].validate(i) ?= Predicate1[Equal[W.`1`.T], Int].validate(i)
  }
}
