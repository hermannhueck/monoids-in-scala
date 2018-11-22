package monoids.typeclasses.minmax

import monoids.typeclasses.monoid.Monoid

sealed trait Min
case object PosInf extends Min
final case class Minimum(value: Int) extends Min

object Min {

  def none: Min = PosInf
  def apply(x: Int): Min = Minimum(x)
  def apply(x: Int, y: Int): Min = Minimum(x max y)
  def apply(x: Min): Min = x
  def apply(x: Min, y: Min): Min = (x, y) match {
    case (PosInf, y)                => y
    case (x, PosInf)                => x
    case ((Minimum(x)), Minimum(y)) => Minimum(x max y)
  }

  implicit val minMonoid: Monoid[Min] = new Monoid[Min] {
    override val empty: Min = PosInf
    override def combine(lhs: Min, rhs: Min): Min = Min(lhs, rhs)
  }
}
