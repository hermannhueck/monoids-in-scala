package monoids.impl.monoid

import monoids.categories.Monoid

/**
  * Product is a Monoid implementation.
  *
  * It implements two Monoid functions:
  * - mempty = Product(1): gives us the neutral element of multiplication
  *     def mempty[B >: A]:  Monoid[B]
  * - mappend: appends/merges two Products into one by multiplying them
  *     def mappend[B >: A](other: Monoid[B]): Monoid[B]
  *
  * With the above functions you get <> and mconcat for free (is implemented in the trait Monoid):
  *     def <>   ... is just an alias for mappend
  *     def mconcat[B >: A](ms: List[Monoid[B]]): Monoid[B]
  *
  * concat is just the same as mconcat with the types changed to Product instead of Monoid of Integer
  *     def concat(sums: List[Product]): Product
  */
object Product {

  def empty: Product = Product(1)

  def concat(products: List[Product]): Product = Product(1).mconcat(products)
}

case class Product(value: Int) extends Monoid[Int] {

  override def mempty[B >: Int]: Product = Product(1)

  override def mappend[B >: Int](that: Monoid[B]): Product = Product(this.value * that.asInstanceOf[Product].value)
  // alias for mappend overrides super with a more specific return type
  override def <>[B >: Int](that: Monoid[B]): Product = this mappend that

  // overrides super method, but gives a more specific return type than Monoid
  override def mconcat[B >: Int](ms: List[Monoid[B]]): Product = super.mconcat(ms).asInstanceOf[Product]
}
