package eu.timepit.refined

import eu.timepit.refined.generic._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.test.illTyped

class GenericPredicateSpec extends Properties("GenericPredicate") {

  property("Equal[_].isValid") = secure {
    Predicate1[Equal[W.`1.4`.T], Double].isValid(1.4)
  }

  property("Equal[_].notValid") = secure {
    Predicate1[Equal[W.`1.4`.T], Double].notValid(2.4)
  }

  property("Equal[_].show") = secure {
    Predicate1[Equal[W.`1.4`.T], Double].show(0.4) ?= "(0.4 == 1.4)"
  }

  property("Equal[object.type].isValid") = secure {
    object Foo
    Predicate1[Equal[Foo.type], Any].isValid(Foo)
  }

  property("Equal[Symbol].isValid") = secure {
    Predicate1[Equal[W.`'foo`.T], Symbol].isValid('foo)
  }

  property("Equal[Symbol].notValid") = secure {
    Predicate1[Equal[W.`'foo`.T], Symbol].notValid('bar)
  }

  property("Subtype.isValid") = secure {
    Predicate0[Subtype[AnyVal], Int].isValid
  }

  property("Subtype.notValid") = secure {
    illTyped("Predicate0[Subtype[Int], AnyVal]", ".*could not find implicit value.*")
    true
  }

  property("Supertype.isValid") = secure {
    Predicate0[Supertype[List[Int]], Seq[Int]].isValid
  }

  property("Supertype.notValid") = secure {
    illTyped("Predicate0[Supertype[Seq[Int]], List[Int]]", ".*could not find implicit value.*")
    true
  }
}
