package monoids.inheritance.impl.monoid

import org.scalatest._
import org.scalatest.prop.PropertyChecks

class AnyySpec extends FlatSpec with Matchers with PropertyChecks {

  // mappend - appends two Monoids
  // <> - alias for mappend
  // append - appends two Anyys (the same thing with different types)
  // mconcat - concatenates a list of Monoids
  // concat - concatenates a list of Anyys (the same thing with different types)

  // you can access the encapsulated value only if you have a Anyy, Monoid is not sufficient.

  "Anyy(false) <> Anyy(false)" should "be equal to Anyy(false)" in {
    Anyy(false) mappend Anyy(false) shouldEqual Anyy(false)
    Anyy(false) <> Anyy(false) shouldEqual Anyy(false)
  }

  "left identity: Anyy(false) <> Anyy(x)" should "be equal to Anyy(x)" in {
    Anyy(false) mappend Anyy(false) shouldEqual Anyy(false)
    Anyy(false) mappend Anyy(true) shouldEqual Anyy(true)
    Anyy(false) <> Anyy(false) shouldEqual Anyy(false)
    Anyy(false) <> Anyy(true) shouldEqual Anyy(true)
  }

  "PROPERTY: left identity" should "hold for Monoid Anyy" in {
    forAll { (anyy: Anyy) =>
      Anyy(false) mappend anyy shouldEqual anyy
      Anyy(false) <> anyy shouldEqual anyy
    }
  }

  "right identity: Anyy(x) <> Anyy(false)" should "be equal to Anyy(x)" in {
    Anyy(false) mappend Anyy(false) shouldEqual Anyy(false)
    Anyy(true) mappend Anyy(false) shouldEqual Anyy(true)
    Anyy(false) <> Anyy(false) shouldEqual Anyy(false)
    Anyy(true) <> Anyy(false) shouldEqual Anyy(true)
  }

  "PROPERTY: right identity" should "hold for Monoid Anyy" in {
    forAll { (anyy: Anyy) =>
      anyy mappend Anyy(false) shouldEqual anyy
      anyy <> Anyy(false) shouldEqual anyy
    }
  }

  "associativity: (Anyy(false) <> Anyy(true)) <> Anyy(false)" should "Anyy(false) <> (Anyy(true) <> Anyy(false))" in {
    (Anyy(false) mappend Anyy(true)) mappend Anyy(false) shouldEqual (Anyy(false) mappend (Anyy(true) mappend  Anyy(false)))
    (Anyy(false) <> Anyy(true)) <> Anyy(false) shouldEqual Anyy(false) <> (Anyy(true) <> Anyy(false))
  }

  "PROPERTY: associativity" should "hold for Monoid Anyy" in {
    forAll { (anyy1: Anyy, anyy2: Anyy, anyy3: Anyy) =>
      (anyy1 mappend anyy2) mappend anyy3 shouldEqual (anyy1 mappend (anyy2 mappend anyy3))
      (anyy1 <> anyy2) <> anyy3 shouldEqual anyy1 <> (anyy2 <> anyy3)
    }
  }

  "mconcat" should "return the Anyy of a List of Anyy objects (as a Monoid)" in {
    val anyys = List(true, false, false,true, true).map(Anyy(_))
    Anyy(false).mconcat(anyys) shouldEqual Anyy(true)
  }

  "PROPERTY: mconcat" should "return the Anyy of a List of Anyy objects (as a Monoid)" in {
    forAll { (list: List[Boolean]) =>
      val anyys = list.map(Anyy(_))
      Anyy(false).mconcat(anyys).asInstanceOf[Anyy].value shouldEqual list.contains(true)
    }
  }

  "concat" should "return the Anyy of a List of Anyy objects (as a Anyy)" in {
    import Anyy.concat
    val anyys = List(true, false, false,true, true).map(Anyy(_))
    concat(anyys).value shouldEqual true
  }

  "PROPERTY: concat" should "return the Anyy of a List of Anyy objects (as a Anyy)" in {
    import Anyy.concat
    forAll { (list: List[Boolean]) =>
      val anyys = list.map(Anyy(_))
      concat(anyys).value shouldEqual list.contains(true)
    }
  }
}
