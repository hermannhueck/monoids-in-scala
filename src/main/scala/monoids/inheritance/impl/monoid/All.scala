package monoids.inheritance.impl.monoid

import monoids.inheritance.categories.Monoid

/**
  * All is a Monoid implementation.
  *
  * It implements two Monoid functions:
  * - mempty = All(true): gives us the neutral element of conjunction
  *     def mempty[B >: A]: Monoid[B]
  * - mappend: appends/merges two Alls into one by combining them with &&
  *     def mappend[B >: A](other: Monoid[B]): Monoid[B]
  *
  * With the above functions you get <> and mconcat for free (is implemented in the trait Monoid):
  *     def <>   ... is just an alias for mappend
  *     def mconcat[B >: A](ms: List[Monoid[B]]): Monoid[B]
  *
  * concat is just the same as mconcat with the types changed to All instead of Monoid of Boolean
  *     def concat(alls: List[All]): All
  */
object All {

  def empty: All = All(true)

  def concat(alls: List[All]): All = All(true).mconcat(alls)
}

case class All(value: Boolean) extends Monoid[Boolean] {

  override def mempty[B >: Boolean]: All = All(true)

  override def mappend[B >: Boolean](that: Monoid[B]): All = All(this.value && that.asInstanceOf[All].value)
  // alias for mappend overrides super with a more specific return type
  override def <>[B >: Boolean](that: Monoid[B]): All = this mappend that

  // overrides super method, but gives a more specific return type than Monoid
  override def mconcat[B >: Boolean](ms: List[Monoid[B]]): All = super.mconcat(ms).asInstanceOf[All]
}
