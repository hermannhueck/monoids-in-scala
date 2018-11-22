package monoids.inheritance.impl.monoid

import org.scalatest._
import org.scalatest.prop.PropertyChecks

class FirstSpec extends FlatSpec with Matchers with PropertyChecks {

  // mappend - appends two Monoids
  // <> - alias for mappend
  // append - appends two Sums (the same thing with different types)
  // mconcat - concatenates a list of Monoids
  // concat - concatenates a list of Firsts (the same thing with different types)

  // you can access the encapsulated Maybe only if you have a First, Monoid is not sufficient.

  private def findFirstFirst[A, MB <: Maybe[A]](fsts: List[First[A, MB]]): First[A, MB] =
    if (fsts.isEmpty) First(Nothingg.asInstanceOf[MB])
    else if (fsts.head.mb.isJust) fsts.head
    else findFirstFirst(fsts.tail)

  "First(Nothingg) <> First(Nothingg)" should "be equal to First(Nothingg)" in {
    First(Nothingg) mappend First(Nothingg) shouldEqual First(Nothingg)
    First(Nothingg) <> First(Nothingg) shouldEqual First(Nothingg)
  }

  "left identity: First(Nothingg) <> First(Just(5))" should "be equal to First(Just(5))" in {
    First(Nothingg) mappend First(Just(5)) shouldEqual First(Just(5))
    First(Nothingg) <> First(Just(5)) shouldEqual First(Just(5))
    First(Nothingg) mappend First(Just("whooop")) shouldEqual First(Just("whooop"))
    First(Nothingg) <> First(Just("whooop")) shouldEqual First(Just("whooop"))
  }

  "PROPERTY: left identity" should "hold for Monoid First[T, Maybe[T]]" in {
    forAll { (fst: First[Int, Maybe[Int]]) =>
      First(Nothingg) mappend fst shouldEqual fst
      First(Nothingg) <> fst shouldEqual fst
    }
    forAll { (fst: First[String, Maybe[String]]) =>
      First(Nothingg) mappend fst shouldEqual fst
      First(Nothingg) <> fst shouldEqual fst
    }
  }

  "right identity: First(Just(5)) <> First(Nothingg)" should "be equal to First(Just(5))" in {
    First(Just(5)) mappend First(Nothingg) shouldEqual First(Just(5))
    First(Just(5)) <> First(Nothingg) shouldEqual First(Just(5))
    First(Just("whooop")) mappend First(Nothingg) shouldEqual First(Just("whooop"))
    First(Just("whooop")) <> First(Nothingg) shouldEqual First(Just("whooop"))
  }

  "PROPERTY: right identity" should "hold for Monoid First[T, Maybe[T]]" in {
    forAll { (fst: First[Int, Maybe[Int]]) =>
      fst mappend First(Nothingg) shouldEqual fst
      fst <> First(Nothingg) shouldEqual fst
    }
    forAll { (fst: First[String, Maybe[String]]) =>
      fst mappend First(Nothingg) shouldEqual fst
      fst <> First(Nothingg) shouldEqual fst
    }
  }

  "associativity: (First(Just(3)) <> First(Just(5))) <> First(Just(10))" should "First(Just(3)) <> (First(Just(5)) <> First(Just(10)))" in {
    (First(Just(3)) mappend First(Just(5))) mappend First(Just(10)) shouldEqual (First(Just(3)) mappend (First(Just(5)) mappend  First(Just(10))))
    (First(Just(3)) <> First(Just(5))) <> First(Just(10)) shouldEqual First(Just(3)) <> (First(Just(5)) <> First(Just(10)))
    (First(Just("x")) mappend First(Just("y"))) mappend First(Just("z")) shouldEqual (First(Just("x")) mappend (First(Just("y")) mappend  First(Just("z"))))
    (First(Just("x")) <> First(Just("y"))) <> First(Just("z")) shouldEqual First(Just("x")) <> (First(Just("y")) <> First(Just("z")))
  }

  "PROPERTY: associativity" should "hold for Monoid First[T, Maybe[T]]" in {
    forAll { (fst1: First[Int, Maybe[Int]], fst2: First[Int, Maybe[Int]], fst3: First[Int, Maybe[Int]]) =>
      (fst1 mappend fst2) mappend fst3 shouldEqual (fst1 mappend (fst2 mappend fst3))
      (fst1 <> fst2) <> fst3 shouldEqual fst1 <> (fst2 <> fst3)
    }
    forAll { (fst1: First[String, Maybe[String]], fst2: First[String, Maybe[String]], fst3: First[String, Maybe[String]]) =>
      (fst1 mappend fst2) mappend fst3 shouldEqual (fst1 mappend (fst2 mappend fst3))
      (fst1 <> fst2) <> fst3 shouldEqual fst1 <> (fst2 <> fst3)
    }
  }

  "mconcat" should "return the First of a List of First objects (as a Monoid)" in {
    First(Nothingg).mconcat(List(Just(1), Just(2), Just(3), Just(4), Just(5)).map(First(_))) shouldEqual First(Just(1))
    First(Nothingg).mconcat(List(Just(1), Nothingg, Just(3), Just(4), Just(5)).map(First(_))) shouldEqual First(Just(1))
    First(Nothingg).mconcat(List(Nothingg, Just(2), Just(3), Just(4), Just(5)).map(First(_))) shouldEqual First(Just(2))
    First(Nothingg).mconcat(List(Just("v"), Just("w"), Just("x"), Just("y"), Just("z")).map(First(_))) shouldEqual First(Just("v"))
    First(Nothingg).mconcat(List(Just("v"), Nothingg, Just("x"), Just("y"), Just("z")).map(First(_))) shouldEqual First(Just("v"))
    First(Nothingg).mconcat(List(Nothingg, Just("w"), Just("x"), Just("y"), Just("z")).map(First(_))) shouldEqual First(Just("w"))
  }

  "PROPERTY: mconcat" should "return the first First of a List of First objects (as a Monoid)" in {
    forAll { (fsts: List[First[Int, Maybe[Int]]]) =>
      First(Nothingg).mconcat(fsts) shouldEqual findFirstFirst(fsts)
    }
    forAll { (fsts: List[First[String, Maybe[String]]]) =>
      First(Nothingg).mconcat(fsts) shouldEqual findFirstFirst(fsts)
    }
  }

  "concat" should "return the first First of a List of First objects (as a First)" in {
    import First.concat
    concat(List(Just(1), Just(2), Just(3), Just(4), Just(5)).map(First(_))) shouldEqual First(Just(1))
    concat(List(Just(1), Nothingg, Just(3), Just(4), Just(5)).map(First(_))) shouldEqual First(Just(1))
    concat(List(Nothingg, Just(2), Just(3), Just(4), Just(5)).map(First(_))) shouldEqual First(Just(2))
    concat(List(Just("v"), Just("w"), Just("x"), Just("y"), Just("z")).map(First(_))) shouldEqual First(Just("v"))
    concat(List(Just("v"), Nothingg, Just("x"), Just("y"), Just("z")).map(First(_))) shouldEqual First(Just("v"))
    concat(List(Nothingg, Just("w"), Just("x"), Just("y"), Just("z")).map(First(_))) shouldEqual First(Just("w"))
  }

  "PROPERTY: concat" should "return the first First of a List of First objects (as a First)" in {
    import First.concat
    forAll { (fsts: List[First[Int, Maybe[Int]]]) =>
      concat(fsts) shouldEqual findFirstFirst(fsts)
    }
    forAll { (fsts: List[First[String, Maybe[String]]]) =>
      concat(fsts) shouldEqual findFirstFirst(fsts)
    }
  }
}
