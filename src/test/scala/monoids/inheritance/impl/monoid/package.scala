package monoids.inheritance.impl

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalactic.Equality
import monoids.inheritance.categories.Monoid

package object monoid {

  implicit def arbitraryCList[T](implicit at: Arbitrary[T]): Arbitrary[CList[T]] = Arbitrary {
    arbitrary[List[T]].map(list => CList.fromList(list))
  }

  implicit def arbitraryMaybe[T](implicit at: Arbitrary[T]): Arbitrary[Maybe[T]] = Arbitrary {
    arbitrary[Option[T]].map(Maybe.fromOption)
    // Gen.oneOf(Gen.const(Nothingg), arbitrary[T].map(Just(_))) // alternative impl
  }

  implicit val arbitrarySum: Arbitrary[Sum] = Arbitrary {
    arbitrary[Int].map(Sum(_))
  }

  implicit val arbitraryProduct: Arbitrary[Product] = Arbitrary {
    arbitrary[Int].map(Product(_))
  }

  implicit val arbitraryAll: Arbitrary[All] = Arbitrary {
    Gen.oneOf(false, true).map(All(_))
  }

  implicit val arbitraryAnyy: Arbitrary[Anyy] = Arbitrary {
    Gen.oneOf(false, true).map(Anyy(_))
  }

  implicit def arbitraryFirst[T](implicit at: Arbitrary[T]): Arbitrary[First[T, Maybe[T]]] = Arbitrary {
    arbitrary[Maybe[T]].map(First(_))
  }

  implicit def arbitraryLast[T](implicit at: Arbitrary[T]): Arbitrary[Last[T, Maybe[T]]] = Arbitrary {
    Arbitrary.arbitrary[Maybe[T]].map(Last(_))
  }

  implicit def arbitraryMonoMaybe[T, MO](implicit amo: Arbitrary[MO], ev: MO <:< Monoid[T]): Arbitrary[MonoMaybe[T, MO]] = Arbitrary {
    arbitrary[Option[MO]].map(monoid => MonoMaybe.fromOption[T, MO](monoid))
  }

  implicit def arbitraryIdentity[T, MO](implicit amo: Arbitrary[MO], ev: MO <:< Monoid[T]): Arbitrary[Identity[T, MO]] = Arbitrary {
    arbitrary[MO].map(Identity[T, MO])
  }

  implicit def arbitraryPair[T1, MO1, T2, MO2](implicit amo1: Arbitrary[MO1], amo2: Arbitrary[MO2],
                                               ev1: MO1 <:< Monoid[T1], ev2: MO2 <:< Monoid[T2]): Arbitrary[Pair[T1, MO1, T2, MO2]] = Arbitrary {
    for {
      monoid1 <- arbitrary[MO1]
      monoid2 <- arbitrary[MO2]
    } yield Pair(monoid1, monoid2)
  }

  implicit val eqFirstNothing: Equality[First[Nothing, Nothingg.type]] = new Equality[First[Nothing, Nothingg.type]] {
    override def areEqual(a: First[Nothing, Nothingg.type], b: Any): Boolean = a == b
  }

  implicit val eqLastNothing: Equality[Last[Nothing, Nothingg.type]] = new Equality[Last[Nothing, Nothingg.type]] {
    override def areEqual(a: Last[Nothing, Nothingg.type], b: Any): Boolean = a == b
  }
}
