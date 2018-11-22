package monoids.inheritance.impl.monoid

import monoids.inheritance.categories.Monoid

/**
  * Anyy is a Monoid implementation.
  *
  * It implements two Monoid functions:
  * - mempty = Anyy(false): gives us the neutral element of disjunction
  *     def mempty[B >: A]:  Monoid[B]
  * - mappend: appends/merges two Anyys into one by combining them with ||
  *     def mappend[B >: A](other: Monoid[B]): Monoid[B]
  *
  * With the above functions you get <> and mconcat for free (is implemented in the trait Monoid):
  *     def <>   ... is just an alias for mappend
  *     def mconcat[B >: A](ms: List[Monoid[B]]): Monoid[B]
  *
  * concat is just the same as mconcat with the types changed to Anyy instead of Monoid of Boolean
  *     def concat(anys: List[Anyy]): Anyy
  */
object Anyy {

  def empty: Anyy = Anyy(false)

  def concat(anys: List[Anyy]): Anyy = Anyy(false).mconcat(anys)
}

case class Anyy(value: Boolean) extends Monoid[Boolean] {

  override def mempty[B >: Boolean]: Anyy = Anyy(false)

  override def mappend[B >: Boolean](that: Monoid[B]): Anyy = Anyy(this.value || that.asInstanceOf[Anyy].value)
  // alias for mappend overrides super with a more specific return type
  override def <>[B >: Boolean](that: Monoid[B]): Anyy = this mappend that

  // overrides super method, but gives a more specific return type than Monoid
  override def mconcat[B >: Boolean](ms: List[Monoid[B]]): Anyy = super.mconcat(ms).asInstanceOf[Anyy]
}
