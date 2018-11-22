package monoids.typeclasses.sumprod

import monoids.typeclasses.monoid.Monoid

case class Product(value: Int)

object Product {

  implicit val productMonoid: Monoid[Product] = new Monoid[Product] {
    override val empty: Product = new Product(1)
    override def combine(lhs: Product, rhs: Product): Product =
      Product(lhs.value * rhs.value)
  }
}