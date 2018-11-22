package monoids.typeclasses.bool

import monoids.typeclasses.monoid.Monoid

case class Anyy(value: Boolean) extends AnyVal

object Anyy {

  implicit val anyMonoid: Monoid[Anyy] = new Monoid[Anyy] {
    override val empty: Anyy = new Anyy(false)
    override def combine(lhs: Anyy, rhs: Anyy): Anyy =
      Anyy(lhs.value || rhs.value)
  }
}
