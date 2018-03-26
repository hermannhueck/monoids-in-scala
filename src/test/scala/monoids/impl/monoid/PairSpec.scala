package monoids.impl.monoid

import org.scalatest._
import org.scalatest.prop.PropertyChecks

class PairSpec extends FlatSpec with Matchers with PropertyChecks {

  "Pair(mempty1, mempty2) <> Pair(mempty1, mempty2)" should "be equal to Pair(mempty1, mempty2)" in {
    Pair(Sum.empty, CList.empty) mappend Pair(Sum.empty, CList.empty) shouldEqual Pair(Sum.empty, CList.empty)
    Pair(Sum.empty, CList.empty) <> Pair(Sum.empty, CList.empty) shouldEqual Pair(Sum.empty, CList.empty)
  }

  "left identity: Pair(mempty1, mempty2) <> Pair(monoid1, monoid2)" should "be equal to Pair(monoid1, monoid2)" in {
    Pair(Sum.empty, Product.empty) mappend Pair(Sum(5), Product(7)) shouldEqual Pair(Sum(5), Product(7))
    Pair(Sum.empty, Product.empty) <> Pair(Sum(5), Product(7)) shouldEqual Pair(Sum(5), Product(7))
    Pair(Product.empty, All.empty) <> Pair(Product(5), All(true)) shouldEqual Pair(Product(5), All(true))
    Pair(All.empty, Anyy.empty) <> Pair(All(true), Anyy(true)) shouldEqual Pair(All(true), Anyy(true))
    Pair(All.empty, CList.empty) <> Pair(All(false), CList(1, 2, 3)) shouldEqual Pair(All(false), CList(1, 2, 3))
    Pair(Anyy.empty, CList.empty) <> Pair(Anyy(true), CList("Scala", "is", "fun")) shouldEqual Pair(Anyy(true), CList("Scala", "is", "fun"))
    Pair(Anyy.empty, First.empty) <> Pair(Anyy(false), First(Just("bla"))) shouldEqual Pair(Anyy(false), First(Just("bla")))
    Pair(CList.empty, Last.empty) <> Pair(CList(5, 6, 7), Last(Nothingg)) shouldEqual Pair(CList(5, 6, 7), Last(Nothingg))
  }

  "PROPERTY: left identity" should "hold for Monoid Pair[T1, Monoid1[T1], T2, Monoid2[T2]]" in {
    forAll { (pm: Pair[Int, Sum, String, CList[String]]) =>
      Pair(Sum.empty, CList.empty) mappend pm shouldEqual pm
      Pair(Sum.empty, CList.empty) <> pm shouldEqual pm
    }
    forAll { (pm: Pair[Boolean, Anyy, String, CList[String]]) =>
      Pair(Anyy.empty, CList.empty) mappend pm shouldEqual pm
      Pair(Anyy.empty, CList.empty) <> pm shouldEqual pm
    }
  }

  "right identity: Pair(monoid1, monoid2) <> Pair(mempty1, mempty2)" should "be equal to Pair(monoid1, monoid2)" in {
    Pair(Sum(5), Product(7)) mappend Pair(Sum.empty, Product.empty) shouldEqual Pair(Sum(5), Product(7))
    Pair(Sum(5), Product(7)) <> Pair(Sum.empty, Product.empty) shouldEqual Pair(Sum(5), Product(7))
    Pair(Product(5), All(true)) <> Pair(Product.empty, All.empty) shouldEqual Pair(Product(5), All(true))
    Pair(All(true), Anyy(true)) <> Pair(All.empty, Anyy.empty) shouldEqual Pair(All(true), Anyy(true))
    Pair(All(false), CList(1, 2, 3)) <> Pair(All.empty, CList.empty) shouldEqual Pair(All(false), CList(1, 2, 3))
    Pair(Anyy(true), CList("Scala", "is", "fun")) <> Pair(Anyy.empty, CList.empty) shouldEqual Pair(Anyy(true), CList("Scala", "is", "fun"))
    Pair(Anyy(false), First(Just("bla"))) <> Pair(Anyy.empty, First.empty) shouldEqual Pair(Anyy(false), First(Just("bla")))
    Pair(CList(5, 6, 7), Last(Nothingg)) <> Pair(CList.empty, Last.empty) shouldEqual Pair(CList(5, 6, 7), Last(Nothingg))
  }

  "PROPERTY: right identity" should "hold for Monoid Pair[T1, Monoid1[T1], T2, Monoid2[T2]]" in {
    forAll { (pm: Pair[Int, Sum, String, CList[String]]) =>
      pm mappend Pair(Sum.empty, CList.empty) shouldEqual pm
      pm <> Pair(Sum.empty, CList.empty) shouldEqual pm
    }
    forAll { (pm: Pair[Boolean, Anyy, String, CList[String]]) =>
      pm mappend Pair(Anyy.empty, CList.empty) shouldEqual pm
      pm <> Pair(Anyy.empty, CList.empty) shouldEqual pm
    }
  }

  "associativity: (Pair(x1, x2) <> Pair(y1, y2)) <> Pair(z1, z2)" should "Pair(x1, x2) <> (Pair(y1, y2) <> Pair(z1, z2))" in {
    (Pair(Sum(3), All(false)) mappend Pair(Sum(5), All(true))) mappend Pair(Sum(10), All(false)) shouldEqual
      (Pair(Sum(3), All(false)) mappend (Pair(Sum(5), All(true)) mappend Pair(Sum(10), All(false))))
    (Pair(Sum(3), Product(7)) <> Pair(Sum(5), Product(9))) <> Pair(Sum(10), Product(14)) shouldEqual
      Pair(Sum(3), Product(7)) <> (Pair(Sum(5), Product(9)) <> Pair(Sum(10), Product(14)))
    (Pair(Sum(3), Product(7)) <> Pair(Sum.empty, Product.empty)) <> Pair(Sum(10), Product(14)) shouldEqual
      Pair(Sum(3), Product(7)) <> (Pair(Sum.empty, Product.empty) <> Pair(Sum(10), Product(14)))
    (Pair(Anyy(true), First(Just(3))) <> Pair(Anyy(false), First(Just(5)))) <> Pair(Anyy(true), First(Just(10))) shouldEqual
      Pair(Anyy(true), First(Just(3))) <> (Pair(Anyy(false), First(Just(5))) <> Pair(Anyy(true), First(Just(10))))
    (Pair(CList(1,2,3), Last(Just(3))) <> Pair(CList(4,5,6), Last.empty)) <> Pair(CList(7,8,9), Last(Just(10))) shouldEqual
      Pair(CList(1,2,3), Last(Just(3))) <> (Pair(CList(4,5,6), Last.empty) <> Pair(CList(7,8,9), Last(Just(10))))
  }

  "PROPERTY: associativity" should "hold for Monoid Monoid Pair[T1, Monoid1[T1], T2, Monoid2[T2]]" in {
    forAll { (pm1: Pair[Int, Sum, String, CList[String]], pm2: Pair[Int, Sum, String, CList[String]], pm3: Pair[Int, Sum, String, CList[String]]) =>
      (pm1 <> pm2) <> pm3 shouldEqual pm1 <> (pm2 <> pm3)
    }
    forAll { (pm1: Pair[Boolean, All, Int, CList[Int]], pm2: Pair[Boolean, All, Int, CList[Int]], pm3: Pair[Boolean, All, Int, CList[Int]]) =>
      (pm1 <> pm2) <> pm3 shouldEqual pm1 <> (pm2 <> pm3)
    }
    forAll { (pm1: Pair[String, CList[String], Int, Product], pm2: Pair[String, CList[String], Int, Product], pm3: Pair[String, CList[String], Int, Product]) =>
      (pm1 <> pm2) <> pm3 shouldEqual pm1 <> (pm2 <> pm3)
    }
  }

  "mconcat" should "return the concatenated result of a List of Monoid Pair[...] objects (as a Monoid)" in {
    Pair(Sum.empty, Product.empty).mconcat(List(Pair(Sum(2), Product(2)), Pair(Sum(3), Product(3)), Pair(Sum(4), Product(4)))) shouldEqual Pair(Sum(9), Product(24))
    Pair(Sum.empty, Product.empty).mconcat(List(Pair(Sum.empty, Product.empty), Pair(Sum(3), Product(3)), Pair(Sum(4), Product(4)))) shouldEqual Pair(Sum(7), Product(12))
    Pair(Sum.empty, Product.empty).mconcat(List(Pair(Sum(2), Product(2)), Pair(Sum.empty, Product.empty), Pair(Sum(4), Product(4)))) shouldEqual Pair(Sum(6), Product(8))
    Pair(Sum.empty, Product.empty).mconcat(List(Pair(Sum(2), Product(2)), Pair(Sum(3), Product(3)), Pair(Sum.empty, Product.empty))) shouldEqual Pair(Sum(5), Product(6))
  }

  "PROPERTY: mconcat" should "return the concatenated result of a List of Pair[...] objects (as a Monoid)" in {
    forAll { (pms: List[Pair[Int, Sum, String, CList[String]]]) =>
      Pair(Sum.empty, CList.empty).mconcat(pms) shouldEqual pms.fold(Pair(Sum.empty, CList.empty))(_ <> _)
    }
    forAll { (pms: List[Pair[Boolean, All, Int, CList[Int]]]) =>
      Pair(All.empty, CList.empty).mconcat(pms) shouldEqual pms.fold(Pair(All.empty, CList.empty))(_ <> _)
    }
    forAll { (pms: List[Pair[String, CList[String], Int, Product]]) =>
      Pair(CList.empty, Product.empty).mconcat(pms) shouldEqual pms.fold(Pair(CList.empty, Product.empty))(_ <> _)
    }
  }

  "concat" should "return the concatenated result of a List of Pair[...] objects (as a Monoid Pair[T1, Monoid1[T1], T2, Monoid2[T2]])" in {
    def concat(ms: List[Pair[Int, Sum, String, CList[String]]]) = Pair(Sum.empty, CList.empty).concat(ms)
    concat(List(Pair(Sum(1), CList("Scala")), Pair(Sum(2), CList("is")), Pair(Sum(3), CList("fun")))) shouldEqual Pair(Sum(6), CList("Scala", "is", "fun"))
    concat(List(Pair(Sum.empty, CList("Scala")), Pair(Sum(2), CList.empty), Pair(Sum(3), CList("fun")))) shouldEqual Pair(Sum(5), CList("Scala", "fun"))
    concat(List(Pair(Sum(1), CList.empty), Pair(Sum.empty, CList("is")), Pair(Sum(3), CList("fun")))) shouldEqual Pair(Sum(4), CList("is", "fun"))
  }

  "PROPERTY: concat" should "return the concatenated result of a List of Pair[...] objects (as a Monoid Pair[T1, Monoid1[T1], T2, Monoid2[T2]])" in {
    forAll { (pms: List[Pair[Int, Sum, String, CList[String]]]) =>
      Pair(Sum.empty, CList.empty).concat(pms) shouldEqual pms.fold(Pair(Sum.empty, CList.empty))(_ <> _)
    }
    forAll { (pms: List[Pair[Boolean, All, Int, CList[Int]]]) =>
      Pair(All.empty, CList.empty).concat(pms) shouldEqual pms.fold(Pair(All.empty, CList.empty))(_ <> _)
    }
    forAll { (pms: List[Pair[String, CList[String], Int, Product]]) =>
      Pair(CList.empty, Product.empty).concat(pms) shouldEqual pms.fold(Pair(CList.empty, Product.empty))(_ <> _)
    }
  }
}
