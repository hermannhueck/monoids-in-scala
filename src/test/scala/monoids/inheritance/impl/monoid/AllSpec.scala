package monoids.inheritance.impl.monoid

import org.scalatest._
import org.scalatest.prop.PropertyChecks

class AllSpec extends FlatSpec with Matchers with PropertyChecks {

  // mappend - appends two Monoids
  // <> - alias for mappend
  // append - appends two Alls (the same thing with different types)
  // mconcat - concatenates a list of Monoids
  // concat - concatenates a list of Alls (the same thing with different types)

  // you can access the encapsulated value only if you have a All, Monoid is not sufficient.

  "All(true) <> All(true)" should "be equal to All(true)" in {
    All(true) mappend All(true) shouldEqual All(true)
    All(true) <> All(true) shouldEqual All(true)
  }

  "left identity: All(true) <> All(x)" should "be equal to All(x)" in {
    All(true) mappend All(true) shouldEqual All(true)
    All(true) mappend All(false) shouldEqual All(false)
    All(true) <> All(true) shouldEqual All(true)
    All(true) <> All(false) shouldEqual All(false)
  }

  "PROPERTY: left identity" should "hold for Monoid All" in {
    forAll { (all: All) =>
      All(true) mappend all shouldEqual all
      All(true) <> all shouldEqual all
    }
  }

  "right identity: All(x) <> All(true)" should "be equal to All(x)" in {
    All(true) mappend All(true) shouldEqual All(true)
    All(false) mappend All(true) shouldEqual All(false)
    All(true) <> All(true) shouldEqual All(true)
    All(false) <> All(true) shouldEqual All(false)
  }

  "PROPERTY: right identity" should "hold for Monoid All" in {
    forAll { (all: All) =>
      all mappend All(true) shouldEqual all
      all <> All(true) shouldEqual all
    }
  }

  "associativity: (All(true) <> All(false)) <> All(true)" should "All(true) <> (All(false) <> All(true))" in {
    (All(true) mappend All(false)) mappend All(true) shouldEqual (All(true) mappend (All(false) mappend  All(true)))
    (All(true) <> All(false)) <> All(true) shouldEqual All(true) <> (All(false) <> All(true))
  }

  "PROPERTY: associativity" should "hold for Monoid All" in {
    forAll { (all1: All, all2: All, all3: All) =>
      (all1 mappend all2) mappend all3 shouldEqual (all1 mappend (all2 mappend all3))
      (all1 <> all2) <> all3 shouldEqual all1 <> (all2 <> all3)
    }
  }

  "mconcat" should "return the All of a List of All objects (as a Monoid)" in {
    val alls = List(true, false, false,true, true).map(All(_))
    All(true).mconcat(alls) shouldEqual All(false)
  }

  "PROPERTY: mconcat" should "return the All of a List of All objects (as a Monoid)" in {
    forAll { (list: List[Boolean]) =>
      val alls = list.map(All(_))
      All(true).mconcat(alls).asInstanceOf[All].value shouldEqual list.forall(_ == true)
    }
  }

  "concat" should "return the All of a List of All objects (as a All)" in {
    import All.concat
    val alls = List(true, false, false,true, true).map(All(_))
    concat(alls).value shouldEqual false
  }

  "PROPERTY: concat" should "return the All of a List of All objects (as a All)" in {
    import All.concat
    forAll { (list: List[Boolean]) =>
      val alls = list.map(All(_))
      concat(alls).value shouldEqual list.forall(_ == true)
    }
  }
}
