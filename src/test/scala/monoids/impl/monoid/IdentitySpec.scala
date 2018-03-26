package monoids.impl.monoid

import org.scalatest._
import org.scalatest.prop.PropertyChecks

class IdentitySpec extends FlatSpec with Matchers with PropertyChecks {

  "Identity(Monoid.empty) <> Identity(Monoid.empty)" should "be equal to Identity(Monoid.empty)" in {
    Identity(Sum.empty) mappend Identity(Sum.empty) shouldEqual Identity(Sum.empty)
    Identity(Sum.empty) <> Identity(Sum.empty) shouldEqual Identity(Sum.empty)
  }

  "left identity: Identity(Monoid.empty) <> Identity(aMonoid)" should "be equal to Identity(aMonoid)" in {
    Identity(Sum.empty) mappend Identity(Sum(5)) shouldEqual Identity(Sum(5))
    Identity(Sum.empty) <> Identity(Sum(5)) shouldEqual Identity(Sum(5))
    Identity(Product.empty) <> Identity(Product(5)) shouldEqual Identity(Product(5))
    Identity(All.empty) <> Identity(All(true)) shouldEqual Identity(All(true))
    Identity(All.empty) <> Identity(All(false)) shouldEqual Identity(All(false))
    Identity(Anyy.empty) <> Identity(Anyy(true)) shouldEqual Identity(Anyy(true))
    Identity(Anyy.empty) <> Identity(Anyy(false)) shouldEqual Identity(Anyy(false))
    Identity(CList.empty) <> Identity(CList(5, 6, 7)) shouldEqual Identity(CList(5, 6, 7))
    Identity(CList.empty) <> Identity(CList("Scala", "is", "fun")) shouldEqual Identity(CList("Scala", "is", "fun"))
    Identity(First.empty) <> Identity(First(Nothingg)) shouldEqual Identity(First(Nothingg))
    Identity(First.empty) <> Identity(First(Just("bla"))) shouldEqual Identity(First(Just("bla")))
    Identity(Last.empty) <> Identity(Last(Nothingg)) shouldEqual Identity(Last(Nothingg))
    Identity(Last.empty) <> Identity(Last(Just("bla"))) shouldEqual Identity(Last(Just("bla")))
  }

  "PROPERTY: left identity" should "hold for Monoid Identity[T, Monoid[T]]" in {
    forAll { (idm: Identity[Int, Sum]) =>
      Identity(Sum.empty) mappend idm shouldEqual idm
      Identity(Sum.empty) <> idm shouldEqual idm
    }
  }

  "right identity: Identity(aMonoid) <> Identity(Monoid.empty)" should "be equal to Identity(aMonoid)" in {
    Identity(Sum(5)) mappend Identity(Sum.empty) shouldEqual Identity(Sum(5))
    Identity(Sum(5)) <> Identity(Sum.empty) shouldEqual Identity(Sum(5))
    Identity(Product(5)) <> Identity(Product.empty) shouldEqual Identity(Product(5))
    Identity(All(true)) <> Identity(All.empty) shouldEqual Identity(All(true))
    Identity(All(false)) <> Identity(All.empty) shouldEqual Identity(All(false))
    Identity(Anyy(true)) <> Identity(Anyy.empty) shouldEqual Identity(Anyy(true))
    Identity(Anyy(false)) <> Identity(Anyy.empty) shouldEqual Identity(Anyy(false))
    Identity(CList(5, 6, 7)) <> Identity(CList.empty) shouldEqual Identity(CList(5, 6, 7))
    Identity(CList("Scala", "is", "fun")) <> Identity(CList.empty) shouldEqual Identity(CList("Scala", "is", "fun"))
    Identity(First(Nothingg)) <> Identity(First.empty) shouldEqual Identity(First(Nothingg))
    Identity(First(Just("bla"))) <> Identity(First.empty) shouldEqual Identity(First(Just("bla")))
    Identity(Last(Nothingg)) <> Identity(Last.empty) shouldEqual Identity(Last(Nothingg))
    Identity(Last(Just("bla"))) <> Identity(Last.empty) shouldEqual Identity(Last(Just("bla")))
  }

  "PROPERTY: right identity" should "hold for Monoid Identity[T, Monoid[T]]" in {
    forAll { (idm: Identity[Int, Sum]) =>
      idm mappend Identity(Sum.empty) shouldEqual idm
      idm <> Identity(Sum.empty) shouldEqual idm
    }
  }

  "associativity: (Identity(aMonoid) <> Identity(bMonoid)) <> Identity(cMonoid)" should "Identity(aMonoid) <> (Identity(bMonoid) <> Identity(cMonoid))" in {
    (Identity(Sum(3)) mappend Identity(Sum(5))) mappend Identity(Sum(10)) shouldEqual (Identity(Sum(3)) mappend (Identity(Sum(5)) mappend Identity(Sum(10))))
    (Identity(Sum(3)) <> Identity(Sum(5))) <> Identity(Sum(10)) shouldEqual Identity(Sum(3)) <> (Identity(Sum(5)) <> Identity(Sum(10)))
    (Identity(Sum(3)) <> Identity(Sum.empty)) <> Identity(Sum(10)) shouldEqual Identity(Sum(3)) <> (Identity(Sum.empty) <> Identity(Sum(10)))
    (Identity(Product(3)) <> Identity(Product(5))) <> Identity(Product(10)) shouldEqual Identity(Product(3)) <> (Identity(Product(5)) <> Identity(Product(10)))
    (Identity(All(true)) <> Identity(All(false))) <> Identity(All(true)) shouldEqual Identity(All(true)) <> (Identity(All(false)) <> Identity(All(true)))
    (Identity(Anyy(true)) <> Identity(Anyy(false))) <> Identity(Anyy(true)) shouldEqual Identity(Anyy(true)) <> (Identity(Anyy(false)) <> Identity(Anyy(true)))
    (Identity(CList(1,2,3)) <> Identity(CList(4,5,6))) <> Identity(CList(7,8,9)) shouldEqual Identity(CList(1,2,3)) <> (Identity(CList(4,5,6)) <> Identity(CList(7,8,9)))
    (Identity(First(Just(3))) <> Identity(First(Just(5)))) <> Identity(First(Just(10))) shouldEqual Identity(First(Just(3))) <> (Identity(First(Just(5))) <> Identity(First(Just(10))))
    (Identity(First(Just(3))) <> Identity(First.empty)) <> Identity(First(Just(10))) shouldEqual Identity(First(Just(3))) <> (Identity(First.empty) <> Identity(First(Just(10))))
    (Identity(Last(Just(3))) <> Identity(Last(Just(5)))) <> Identity(Last(Just(10))) shouldEqual Identity(Last(Just(3))) <> (Identity(Last(Just(5))) <> Identity(Last(Just(10))))
    (Identity(Last(Just(3))) <> Identity(Last.empty)) <> Identity(Last(Just(10))) shouldEqual Identity(Last(Just(3))) <> (Identity(Last.empty) <> Identity(Last(Just(10))))
  }

  "PROPERTY: associativity" should "hold for Monoid Identity[T, Monoid[T]]" in {
    forAll { (idm1: Identity[Int, Sum], idm2: Identity[Int, Sum], idm3: Identity[Int, Sum]) =>
      (idm1 <> idm2) <> idm3 shouldEqual idm1 <> (idm2 <> idm3)
    }
    forAll { (idm1: Identity[Int, Product], idm2: Identity[Int, Product], idm3: Identity[Int, Product]) =>
      (idm1 <> idm2) <> idm3 shouldEqual idm1 <> (idm2 <> idm3)
    }
    forAll { (idm1: Identity[Boolean, All], idm2: Identity[Boolean, All], idm3: Identity[Boolean, All]) =>
      (idm1 <> idm2) <> idm3 shouldEqual idm1 <> (idm2 <> idm3)
    }
    forAll { (idm1: Identity[Boolean, Anyy], idm2: Identity[Boolean, Anyy], idm3: Identity[Boolean, Anyy]) =>
      (idm1 <> idm2) <> idm3 shouldEqual idm1 <> (idm2 <> idm3)
    }
    forAll { (idm1: Identity[Int, CList[Int]], idm2: Identity[Int, CList[Int]], idm3: Identity[Int, CList[Int]]) =>
      (idm1 <> idm2) <> idm3 shouldEqual idm1 <> (idm2 <> idm3)
    }
    forAll { (idm1: Identity[String, CList[String]], idm2: Identity[String, CList[String]], idm3: Identity[String, CList[String]]) =>
      (idm1 <> idm2) <> idm3 shouldEqual idm1 <> (idm2 <> idm3)
    }
  }

  "mconcat" should "return the concatenated result of a List of Identity[...] objects (as a Monoid)" in {
    Identity(Sum.empty).mconcat(List(Identity(Sum(1)), Identity(Sum(2)), Identity(Sum(3)), Identity(Sum(4)), Identity(Sum(5)))) shouldEqual Identity(Sum(15))
    Identity(Sum.empty).mconcat(List(Identity(Sum(1)), Identity(Sum.empty), Identity(Sum(3)), Identity(Sum(4)), Identity(Sum(5)))) shouldEqual Identity(Sum(13))
    Identity(Sum.empty).mconcat(List(Identity(Sum.empty), Identity(Sum(2)), Identity(Sum(3)), Identity(Sum(4)), Identity(Sum(5)))) shouldEqual Identity(Sum(14))
  }

  "PROPERTY: mconcat" should "return the concatenated result of a List of Identity[...] objects (as a Monoid)" in {
    forAll { (idms: List[Identity[Int, Sum]]) =>
      Identity(Sum.empty).mconcat(idms) shouldEqual idms.fold(Identity(Sum.empty))(_ <> _)
    }
    forAll { (idms: List[Identity[Int, Product]]) =>
      Identity(Product.empty).mconcat(idms) shouldEqual idms.fold(Identity(Product.empty))(_ <> _)
    }
    forAll { (idms: List[Identity[Int, CList[Int]]]) =>
      Identity(CList.empty).mconcat(idms) shouldEqual idms.fold(Identity(CList.empty))(_ <> _)
    }
    forAll { (idms: List[Identity[String, CList[String]]]) =>
      Identity(CList.empty).mconcat(idms) shouldEqual idms.fold(Identity(CList.empty))(_ <> _)
    }
  }

  "concat" should "return the concatenated result of a List of Identity[...] objects (as a Identity[T, Monoid[T]])" in {
    def concat(ms: List[Identity[Int, Sum]]) = Identity(Sum.empty).concat(ms)
    concat(List(Identity(Sum(1)), Identity(Sum(2)), Identity(Sum(3)), Identity(Sum(4)), Identity(Sum(5)))) shouldEqual Identity(Sum(15))
    concat(List(Identity(Sum(1)), Identity(Sum.empty), Identity(Sum(3)), Identity(Sum(4)), Identity(Sum(5)))) shouldEqual Identity(Sum(13))
    concat(List(Identity(Sum.empty), Identity(Sum(2)), Identity(Sum(3)), Identity(Sum(4)), Identity(Sum(5)))) shouldEqual Identity(Sum(14))
  }

  "PROPERTY: concat" should "return the concatenated result of a List of Identity[...] objects (as a Identity[T, Monoid[T]])" in {
    forAll { (idms: List[Identity[Int, Sum]]) =>
      Identity(Sum.empty).concat(idms) shouldEqual idms.fold(Identity(Sum.empty))(_ <> _)
    }
    forAll { (idms: List[Identity[Int, Product]]) =>
      Identity(Product.empty).concat(idms) shouldEqual idms.fold(Identity(Product.empty))(_ <> _)
    }
    forAll { (idms: List[Identity[Int, CList[Int]]]) =>
      Identity(CList.empty).concat(idms) shouldEqual idms.fold(Identity(CList.empty))(_ <> _)
    }
    forAll { (idms: List[Identity[String, CList[String]]]) =>
      Identity(CList.empty).concat(idms) shouldEqual idms.fold(Identity(CList.empty))(_ <> _)
    }
  }
}
