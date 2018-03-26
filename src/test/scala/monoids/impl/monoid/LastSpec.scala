package monoids.impl.monoid

import org.scalatest._
import org.scalatest.prop.PropertyChecks

class LastSpec extends FlatSpec with Matchers with PropertyChecks {

  // mappend - appends two Monoids
  // <> - alias for mappend
  // append - appends two Sums (the same thing with different types)
  // mconcat - concatenates a list of Monoids
  // concat - concatenates a list of Lasts (the same thing with different types)

  // you can access the encapsulated Maybe only if you have a Last, Monoid is not sufficient.

  private def findLastLast[A, MB <: Maybe[A]](lsts: List[Last[A, MB]]): Last[A, MB] = {

    def findFirstLast[A, MB <: Maybe[A]](fsts: List[Last[A, MB]]): Last[A, MB] =
      if (fsts.isEmpty) Last(Nothingg.asInstanceOf[MB])
      else if (fsts.head.mb.isJust) fsts.head
      else findFirstLast(fsts.tail)

    findFirstLast(lsts.reverse)
  }

  "Last(Nothingg) <> Last(Nothingg)" should "be equal to Last(Nothingg)" in {
    Last(Nothingg) mappend Last(Nothingg) shouldEqual Last(Nothingg)
    Last(Nothingg) <> Last(Nothingg) shouldEqual Last(Nothingg)
  }

  "left identity: Last(Nothingg) <> Last(Just(5))" should "be equal to Last(Just(5))" in {
    Last(Nothingg) mappend Last(Just(5)) shouldEqual Last(Just(5))
    Last(Nothingg) <> Last(Just(5)) shouldEqual Last(Just(5))
    Last(Nothingg) mappend Last(Just("whooop")) shouldEqual Last(Just("whooop"))
    Last(Nothingg) <> Last(Just("whooop")) shouldEqual Last(Just("whooop"))
  }

  "PROPERTY: left identity" should "hold for Monoid Last[T, Maybe[T]]" in {
    forAll { (lst: Last[Int, Maybe[Int]]) =>
      Last(Nothingg) mappend lst shouldEqual lst
      Last(Nothingg) <> lst shouldEqual lst
    }
    forAll { (lst: Last[String, Maybe[String]]) =>
      Last(Nothingg) mappend lst shouldEqual lst
      Last(Nothingg) <> lst shouldEqual lst
    }
  }

  "right identity: Last(Just(5)) <> Last(Nothingg)" should "be equal to Last(Just(5))" in {
    Last(Just(5)) mappend Last(Nothingg) shouldEqual Last(Just(5))
    Last(Just(5)) <> Last(Nothingg) shouldEqual Last(Just(5))
    Last(Just("whooop")) mappend Last(Nothingg) shouldEqual Last(Just("whooop"))
    Last(Just("whooop")) <> Last(Nothingg) shouldEqual Last(Just("whooop"))
  }

  "PROPERTY: right identity" should "hold for Monoid Last[T, Maybe[T]]" in {
    forAll { (lst: Last[Int, Maybe[Int]]) =>
      lst mappend Last(Nothingg) shouldEqual lst
      lst <> Last(Nothingg) shouldEqual lst
    }
    forAll { (lst: Last[String, Maybe[String]]) =>
      lst mappend Last(Nothingg) shouldEqual lst
      lst <> Last(Nothingg) shouldEqual lst
    }
  }

  "associativity: (Last(Just(3)) <> Last(Just(5))) <> Last(Just(10))" should "Last(Just(3)) <> (Last(Just(5)) <> Last(Just(10)))" in {
    (Last(Just(3)) mappend Last(Just(5))) mappend Last(Just(10)) shouldEqual (Last(Just(3)) mappend (Last(Just(5)) mappend Last(Just(10))))
    (Last(Just(3)) <> Last(Just(5))) <> Last(Just(10)) shouldEqual Last(Just(3)) <> (Last(Just(5)) <> Last(Just(10)))
    (Last(Just("x")) mappend Last(Just("y"))) mappend Last(Just("z")) shouldEqual (Last(Just("x")) mappend (Last(Just("y")) mappend Last(Just("z"))))
    (Last(Just("x")) <> Last(Just("y"))) <> Last(Just("z")) shouldEqual Last(Just("x")) <> (Last(Just("y")) <> Last(Just("z")))
  }

  "PROPERTY: associativity" should "hold for Monoid Last[T, Maybe[T]]" in {
    forAll { (lst1: Last[Int, Maybe[Int]], lst2: Last[Int, Maybe[Int]], lst3: Last[Int, Maybe[Int]]) =>
      (lst1 mappend lst2) mappend lst3 shouldEqual (lst1 mappend (lst2 mappend lst3))
      (lst1 <> lst2) <> lst3 shouldEqual lst1 <> (lst2 <> lst3)
    }
    forAll { (lst1: Last[String, Maybe[String]], lst2: Last[String, Maybe[String]], lst3: Last[String, Maybe[String]]) =>
      (lst1 mappend lst2) mappend lst3 shouldEqual (lst1 mappend (lst2 mappend lst3))
      (lst1 <> lst2) <> lst3 shouldEqual lst1 <> (lst2 <> lst3)
    }
  }

  "mconcat" should "return the Last of a List of Last objects (as a Monoid)" in {
    Last(Nothingg).mconcat(List(Just(1), Just(2), Just(3), Just(4), Just(5)).map(Last(_))) shouldEqual Last(Just(5))
    Last(Nothingg).mconcat(List(Just(1), Just(2), Just(3), Nothingg, Just(5)).map(Last(_))) shouldEqual Last(Just(5))
    Last(Nothingg).mconcat(List(Just(1), Just(2), Just(3), Just(4), Nothingg).map(Last(_))) shouldEqual Last(Just(4))
    Last(Nothingg).mconcat(List(Just("v"), Just("w"), Just("x"), Just("y"), Just("z")).map(Last(_))) shouldEqual Last(Just("z"))
    Last(Nothingg).mconcat(List(Just("v"), Just("w"), Just("x"), Nothingg, Just("z")).map(Last(_))) shouldEqual Last(Just("z"))
    Last(Nothingg).mconcat(List(Just("v"), Just("w"), Just("x"), Just("y"), Nothingg).map(Last(_))) shouldEqual Last(Just("y"))
  }

  "PROPERTY: mconcat" should "return the first Last of a List of Last objects (as a Monoid)" in {
    forAll { (lsts: List[Last[Int, Maybe[Int]]]) =>
      Last(Nothingg).mconcat(lsts) shouldEqual findLastLast(lsts)
    }
    forAll { (lsts: List[Last[String, Maybe[String]]]) =>
      Last(Nothingg).mconcat(lsts) shouldEqual findLastLast(lsts)
    }
  }

  "concat" should "return the first Last of a List of Last objects (as a Last)" in {
    import Last.concat
    concat(List(Just(1), Just(2), Just(3), Just(4), Just(5)).map(Last(_))) shouldEqual Last(Just(5))
    concat(List(Just(1), Just(2), Just(3), Nothingg, Just(5)).map(Last(_))) shouldEqual Last(Just(5))
    concat(List(Just(1), Just(2), Just(3), Just(4), Nothingg).map(Last(_))) shouldEqual Last(Just(4))
    concat(List(Just("v"), Just("w"), Just("x"), Just("y"), Just("z")).map(Last(_))) shouldEqual Last(Just("z"))
    concat(List(Just("v"), Just("w"), Just("x"), Nothingg, Just("z")).map(Last(_))) shouldEqual Last(Just("z"))
    concat(List(Just("v"), Just("w"), Just("x"), Just("y"), Nothingg).map(Last(_))) shouldEqual Last(Just("y"))
  }

  "PROPERTY: concat" should "return the first Last of a List of Last objects (as a Last)" in {
    import Last.concat
    forAll { (lsts: List[Last[Int, Maybe[Int]]]) =>
      concat(lsts) shouldEqual findLastLast(lsts)
    }
    forAll { (lsts: List[Last[String, Maybe[String]]]) =>
      concat(lsts) shouldEqual findLastLast(lsts)
    }
  }
}
