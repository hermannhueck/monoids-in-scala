package monoids.typeclasses.sumprod

import monoids.typeclasses.monoid.Monoid

case class Sum(value: Int)

object Sum {

  implicit val sumMonoid: Monoid[Sum] = new Monoid[Sum] {
    override val empty: Sum = new Sum(0)
    override def combine(lhs: Sum, rhs: Sum): Sum =
      Sum(lhs.value + rhs.value)
  }
}
