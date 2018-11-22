package monoids.typeclasses.minmax

import monoids.typeclasses.monoid.Monoid

sealed trait Max
case object NegInf extends Max
final case class Maximum(value: Int) extends Max

object Max {

  def none: Max = NegInf
  def apply(x: Int): Max = Maximum(x)
  def apply(x: Int, y: Int): Max = Maximum(x max y)
  def apply(x: Max): Max = x
  def apply(x: Max, y: Max): Max = (x, y) match {
    case (NegInf, y)                => y
    case (x, NegInf)                => x
    case ((Maximum(x)), Maximum(y)) => Maximum(x max y)
  }

  implicit val maxMonoid: Monoid[Max] = new Monoid[Max] {
    override val empty: Max = NegInf
    override def combine(lhs: Max, rhs: Max): Max = Max(lhs, rhs)
  }
}
