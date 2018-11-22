package monoids.typeclasses.bool

import monoids.typeclasses.monoid.Monoid

case class All(value: Boolean) extends AnyVal

object All {

  implicit val allMonoid: Monoid[All] = new Monoid[All] {
    override val empty: All = new All(true)
    override def combine(lhs: All, rhs: All): All =
      All(lhs.value && rhs.value)
  }
}
