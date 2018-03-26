package monoids.impl.monoid

import org.scalatest._
import org.scalatest.prop.PropertyChecks

class SumSpec extends FlatSpec with Matchers with PropertyChecks {

  // mappend - appends two Monoids
  // <> - alias for mappend
  // append - appends two Sums (the same thing with different types)
  // mconcat - concatenates a list of Monoids
  // concat - concatenates a list of Sums (the same thing with different types)

  // you can access the encapsulated value only if you have a Sum, Monoid is not sufficient.

  "Sum(0) <> Sum(0)" should "be equal to Sum(0)" in {
    Sum(0) mappend Sum(0) shouldEqual Sum(0)
    Sum(0) <> Sum(0) shouldEqual Sum(0)
  }

  "left identity: Sum(0) <> Sum(5)" should "be equal to Sum(5)" in {
    Sum(0) mappend Sum(5) shouldEqual Sum(5)
    Sum(0) <> Sum(5) shouldEqual Sum(5)
  }

  "PROPERTY: left identity" should "hold for Monoid Sum" in {
    forAll { (sum: Sum) =>
      Sum(0) mappend sum shouldEqual sum
      Sum(0) <> sum shouldEqual sum
    }
  }

  "right identity: Sum(5) <> Sum(0)" should "be equal to Sum(5)" in {
    Sum(5) mappend Sum(0) shouldEqual Sum(5)
    Sum(5) <> Sum(0) shouldEqual Sum(5)
  }

  "PROPERTY: right identity" should "hold for Monoid Sum" in {
    forAll { (sum: Sum) =>
      sum mappend Sum(0) shouldEqual sum
      sum <> Sum(0) shouldEqual sum
    }
  }

  "associativity: (Sum(3) <> Sum(5)) <> Sum(10)" should "Sum(3) <> (Sum(5) <> Sum(10))" in {
    (Sum(3) mappend Sum(5)) mappend Sum(10) shouldEqual (Sum(3) mappend (Sum(5) mappend  Sum(10)))
    (Sum(3) <> Sum(5)) <> Sum(10) shouldEqual Sum(3) <> (Sum(5) <> Sum(10))
  }

  "PROPERTY: associativity" should "hold for Monoid Sum" in {
    forAll { (sum1: Sum, sum2: Sum, sum3: Sum) =>
      (sum1 mappend sum2) mappend sum3 shouldEqual (sum1 mappend (sum2 mappend sum3))
      (sum1 <> sum2) <> sum3 shouldEqual sum1 <> (sum2 <> sum3)
    }
  }

  "mconcat" should "return the Sum of a List of Sum objects (as a Monoid)" in {
    val sums = List(1, 2, 3, 4, 5).map(Sum(_))
    Sum(0).mconcat(sums) shouldEqual Sum(15)
  }

  "PROPERTY: mconcat" should "return the Sum of a List of Sum objects (as a Monoid)" in {
    forAll { (list: List[Int]) =>
      val sums = list.map(Sum(_))
      Sum(0).mconcat(sums).asInstanceOf[Sum].value shouldEqual list.sum
    }
  }

  "concat" should "return the Sum of a List of Sum objects (as a Sum)" in {
    import Sum.concat
    val sums = List(1, 2, 3, 4, 5).map(Sum(_))
    concat(sums).value shouldEqual 15
  }

  "PROPERTY: concat" should "return the Sum of a List of Sum objects (as a Sum)" in {
    import Sum.concat
    forAll { (list: List[Int]) =>
      val sums = list.map(Sum(_))
      concat(sums).value shouldEqual list.sum
    }
  }
}
