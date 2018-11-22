package monoids.inheritance.impl.monoid

import org.scalatest._
import org.scalatest.prop.PropertyChecks

class MonoMaybeSpec extends FlatSpec with Matchers with PropertyChecks {

  // mappend - appends two Monoids
  // <> - alias for mappend
  // append - appends two Sums (the same thing with different types)
  // mconcat - concatenates a list of Monoids
  // concat - concatenates a list of MonoMaybes (the same thing with different types)

  "MonoNothing <> MonoNothing" should "be equal to MonoNothing" in {
    MonoNothing mappend MonoNothing shouldEqual MonoNothing
    MonoNothing <> MonoNothing shouldEqual MonoNothing
  }

  "left identity: MonoNothing <> MonoJust(aMonoid)" should "be equal to MonoJust(aMonoid)" in {
    MonoNothing mappend MonoJust(Sum(5)) shouldEqual MonoJust(Sum(5))
    MonoNothing <> MonoJust(Sum(5)) shouldEqual MonoJust(Sum(5))
    MonoNothing <> MonoJust(Product(5)) shouldEqual MonoJust(Product(5))
    MonoNothing <> MonoJust(All(true)) shouldEqual MonoJust(All(true))
    MonoNothing <> MonoJust(All(false)) shouldEqual MonoJust(All(false))
    MonoNothing <> MonoJust(Anyy(true)) shouldEqual MonoJust(Anyy(true))
    MonoNothing <> MonoJust(Anyy(false)) shouldEqual MonoJust(Anyy(false))
    MonoNothing <> MonoJust(CList(5, 6, 7)) shouldEqual MonoJust(CList(5, 6, 7))
    MonoNothing <> MonoJust(CList("Scala", "is", "fun")) shouldEqual MonoJust(CList("Scala", "is", "fun"))
    MonoNothing <> MonoJust(First(Nothingg)) shouldEqual MonoJust(First(Nothingg))
    MonoNothing <> MonoJust(First(Just("bla"))) shouldEqual MonoJust(First(Just("bla")))
    MonoNothing <> MonoJust(Last(Nothingg)) shouldEqual MonoJust(Last(Nothingg))
    MonoNothing <> MonoJust(Last(Just("bla"))) shouldEqual MonoJust(Last(Just("bla")))
  }

  "PROPERTY: left identity" should "hold for Monoid MonoMaybe[T, Monoid[T]]" in {
    forAll { (mm: MonoMaybe[Int, Sum]) =>
      MonoNothing mappend mm shouldEqual mm
      MonoNothing <> mm shouldEqual mm
    }
  }

  "right identity: MonoJust(aMonoid) <> MonoNothing" should "be equal to MonoJust(aMonoid)" in {
    MonoJust(Sum(5)) mappend MonoNothing shouldEqual MonoJust(Sum(5))
    MonoJust(Sum(5)) <> MonoNothing shouldEqual MonoJust(Sum(5))
    MonoJust(Product(5)) <> MonoNothing shouldEqual MonoJust(Product(5))
    MonoJust(All(true)) <> MonoNothing shouldEqual MonoJust(All(true))
    MonoJust(All(false)) <> MonoNothing shouldEqual MonoJust(All(false))
    MonoJust(Anyy(true)) <> MonoNothing shouldEqual MonoJust(Anyy(true))
    MonoJust(Anyy(false)) <> MonoNothing shouldEqual MonoJust(Anyy(false))
    MonoJust(CList(5, 6, 7)) <> MonoNothing shouldEqual MonoJust(CList(5, 6, 7))
    MonoJust(CList("Scala", "is", "fun")) <> MonoNothing shouldEqual MonoJust(CList("Scala", "is", "fun"))
    MonoJust(First(Nothingg)) <> MonoNothing shouldEqual MonoJust(First(Nothingg))
    MonoJust(First(Just("bla"))) <> MonoNothing shouldEqual MonoJust(First(Just("bla")))
    MonoJust(Last(Nothingg)) <> MonoNothing shouldEqual MonoJust(Last(Nothingg))
    MonoJust(Last(Just("bla"))) <> MonoNothing shouldEqual MonoJust(Last(Just("bla")))
  }

  "PROPERTY: right identity" should "hold for Monoid MonoMaybe[T, Monoid[T]]" in {
    forAll { (mm: MonoMaybe[Int, Sum]) =>
      mm mappend MonoNothing shouldEqual mm
      mm <> MonoNothing shouldEqual mm
    }
  }

  "associativity: (MonoJust(aMonoid) <> MonoJust(bMonoid)) <> MonoJust(cMonoid)" should "MonoJust(aMonoid) <> (MonoJust(bMonoid) <> MonoJust(cMonoid))" in {
    (MonoJust(Sum(3)) mappend MonoJust(Sum(5))) mappend MonoJust(Sum(10)) shouldEqual (MonoJust(Sum(3)) mappend (MonoJust(Sum(5)) mappend MonoJust(Sum(10))))
    (MonoJust(Sum(3)) <> MonoJust(Sum(5))) <> MonoJust(Sum(10)) shouldEqual MonoJust(Sum(3)) <> (MonoJust(Sum(5)) <> MonoJust(Sum(10)))
    (MonoJust(Sum(3)) <> MonoNothing) <> MonoJust(Sum(10)) shouldEqual MonoJust(Sum(3)) <> (MonoNothing <> MonoJust(Sum(10)))
    (MonoJust(Product(3)) <> MonoJust(Product(5))) <> MonoJust(Product(10)) shouldEqual MonoJust(Product(3)) <> (MonoJust(Product(5)) <> MonoJust(Product(10)))
    (MonoJust(All(true)) <> MonoJust(All(false))) <> MonoJust(All(true)) shouldEqual MonoJust(All(true)) <> (MonoJust(All(false)) <> MonoJust(All(true)))
    (MonoJust(Anyy(true)) <> MonoJust(Anyy(false))) <> MonoJust(Anyy(true)) shouldEqual MonoJust(Anyy(true)) <> (MonoJust(Anyy(false)) <> MonoJust(Anyy(true)))
    (MonoJust(CList(1,2,3)) <> MonoJust(CList(4,5,6))) <> MonoJust(CList(7,8,9)) shouldEqual MonoJust(CList(1,2,3)) <> (MonoJust(CList(4,5,6)) <> MonoJust(CList(7,8,9)))
    (MonoJust(First(Just(3))) <> MonoJust(First(Just(5)))) <> MonoJust(First(Just(10))) shouldEqual MonoJust(First(Just(3))) <> (MonoJust(First(Just(5))) <> MonoJust(First(Just(10))))
    (MonoJust(First(Just(3))) <> MonoNothing) <> MonoJust(First(Just(10))) shouldEqual MonoJust(First(Just(3))) <> (MonoNothing <> MonoJust(First(Just(10))))
    (MonoJust(Last(Just(3))) <> MonoJust(Last(Just(5)))) <> MonoJust(Last(Just(10))) shouldEqual MonoJust(Last(Just(3))) <> (MonoJust(Last(Just(5))) <> MonoJust(Last(Just(10))))
    (MonoJust(Last(Just(3))) <> MonoNothing) <> MonoJust(Last(Just(10))) shouldEqual MonoJust(Last(Just(3))) <> (MonoNothing <> MonoJust(Last(Just(10))))
  }

  "PROPERTY: associativity" should "hold for Monoid MonoMaybe[T, Monoid[T]]" in {
    forAll { (mm1: MonoMaybe[Int, Sum], mm2: MonoMaybe[Int, Sum], mm3: MonoMaybe[Int, Sum]) =>
      (mm1 <> mm2) <> mm3 shouldEqual mm1 <> (mm2 <> mm3)
    }
    forAll { (mm1: MonoMaybe[Int, Product], mm2: MonoMaybe[Int, Product], mm3: MonoMaybe[Int, Product]) =>
      (mm1 <> mm2) <> mm3 shouldEqual mm1 <> (mm2 <> mm3)
    }
    forAll { (mm1: MonoMaybe[Boolean, All], mm2: MonoMaybe[Boolean, All], mm3: MonoMaybe[Boolean, All]) =>
      (mm1 <> mm2) <> mm3 shouldEqual mm1 <> (mm2 <> mm3)
    }
    forAll { (mm1: MonoMaybe[Boolean, Anyy], mm2: MonoMaybe[Boolean, Anyy], mm3: MonoMaybe[Boolean, Anyy]) =>
      (mm1 <> mm2) <> mm3 shouldEqual mm1 <> (mm2 <> mm3)
    }
    forAll { (mm1: MonoMaybe[Int, CList[Int]], mm2: MonoMaybe[Int, CList[Int]], mm3: MonoMaybe[Int, CList[Int]]) =>
      (mm1 <> mm2) <> mm3 shouldEqual mm1 <> (mm2 <> mm3)
    }
    forAll { (mm1: MonoMaybe[String, CList[String]], mm2: MonoMaybe[String, CList[String]], mm3: MonoMaybe[String, CList[String]]) =>
      (mm1 <> mm2) <> mm3 shouldEqual mm1 <> (mm2 <> mm3)
    }
  }

  "mconcat" should "return the concatenated result of a List of MonoMaybe[...] objects (as a Monoid)" in {
    MonoNothing.mconcat(List(MonoJust(Sum(1)), MonoJust(Sum(2)), MonoJust(Sum(3)), MonoJust(Sum(4)), MonoJust(Sum(5)))) shouldEqual MonoJust(Sum(15))
    MonoNothing.mconcat(List(MonoJust(Sum(1)), MonoNothing, MonoJust(Sum(3)), MonoJust(Sum(4)), MonoJust(Sum(5)))) shouldEqual MonoJust(Sum(13))
    MonoNothing.mconcat(List(MonoNothing, MonoJust(Sum(2)), MonoJust(Sum(3)), MonoJust(Sum(4)), MonoJust(Sum(5)))) shouldEqual MonoJust(Sum(14))
  }

  "PROPERTY: mconcat" should "return the concatenated result of a List of MonoMaybe[...] objects (as a Monoid)" in {
    forAll { (mms: List[MonoMaybe[Int, Sum]]) =>
      MonoNothing.mconcat(mms) shouldEqual mms.fold(MonoNothing)(_ <> _)
    }
    forAll { (mms: List[MonoMaybe[Int, Product]]) =>
      MonoNothing.mconcat(mms) shouldEqual mms.fold(MonoNothing)(_ <> _)
    }
    forAll { (mms: List[MonoMaybe[Int, CList[Int]]]) =>
      MonoNothing.mconcat(mms) shouldEqual mms.fold(MonoNothing)(_ <> _)
    }
    forAll { (mms: List[MonoMaybe[String, CList[String]]]) =>
      MonoNothing.mconcat(mms) shouldEqual mms.fold(MonoNothing)(_ <> _)
    }
  }

  "concat" should "return the concatenated result of a List of MonoMaybe[T, Monoid[T]] objects (as a MonoMaybe[T, Monoid[T]])" in {
    import MonoMaybe.concat
    concat(List(MonoJust(Sum(1)), MonoJust(Sum(2)), MonoJust(Sum(3)), MonoJust(Sum(4)), MonoJust(Sum(5)))) shouldEqual MonoJust(Sum(15))
    concat(List(MonoJust(Sum(1)), MonoNothing, MonoJust(Sum(3)), MonoJust(Sum(4)), MonoJust(Sum(5)))) shouldEqual MonoJust(Sum(13))
    concat(List(MonoNothing, MonoJust(Sum(2)), MonoJust(Sum(3)), MonoJust(Sum(4)), MonoJust(Sum(5)))) shouldEqual MonoJust(Sum(14))
  }

  "PROPERTY: concat" should "return the concatenated result of a List of MonoMaybe[T, Monoid[T]] objects (as a MonoMaybe[T, Monoid[T]])" in {
    import MonoMaybe.concat
    forAll { (mms: List[MonoMaybe[Int, Sum]]) =>
      concat(mms) shouldEqual mms.fold(MonoNothing)(_ <> _)
    }
    forAll { (mms: List[MonoMaybe[Int, Product]]) =>
      concat(mms) shouldEqual mms.fold(MonoNothing)(_ <> _)
    }
    forAll { (mms: List[MonoMaybe[Int, CList[Int]]]) =>
      concat(mms) shouldEqual mms.fold(MonoNothing)(_ <> _)
    }
    forAll { (mms: List[MonoMaybe[String, CList[String]]]) =>
      concat(mms) shouldEqual mms.fold(MonoNothing)(_ <> _)
    }
  }
}
