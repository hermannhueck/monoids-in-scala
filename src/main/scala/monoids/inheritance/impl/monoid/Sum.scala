package monoids.inheritance.impl.monoid

import monoids.inheritance.categories.Monoid

/**
  * Sum is a Monoid implementation.
  *
  * It implements two Monoid functions:
  * - mempty = Sum(0): gives us the neutral element of addition
  *     def mempty[B >: A]: Monoid[B]
  * - mappend: appends/merges two Sums into one by adding them
  *     def mappend[B >: A](other: Monoid[B]): Monoid[B]
  *
  * With the above functions you get <> and mconcat for free (is implemented in the trait Monoid):
  *     def <>   ... is just an alias for mappend
  *     def mconcat[B >: A](ms: List[Monoid[B]]): Monoid[B]
  *
  * concat is just the same as mconcat with the types changed to Sum instead of Monoid of Integer
  *     def concat(sums: List[Sum]): Sum
  */
object Sum {

  def empty: Sum = Sum(0)

  def concat(sums: List[Sum]): Sum = Sum(0).mconcat(sums)
}

case class Sum(value: Int) extends Monoid[Int] {

  override def mempty[B >: Int]: Sum = Sum(0)

  override def mappend[B >: Int](that: Monoid[B]): Sum = Sum(this.value + that.asInstanceOf[Sum].value)
  // alias for mappend overrides super with a more specific return type
  override def <>[B >: Int](that: Monoid[B]): Sum = this mappend that

  // overrides super method, but gives a more specific return type than Monoid
  override def mconcat[B >: Int](ms: List[Monoid[B]]): Sum = super.mconcat(ms).asInstanceOf[Sum]
}
