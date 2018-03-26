package monoids.impl.monoid

import org.scalatest._
import org.scalatest.prop.PropertyChecks

class ProductSpec extends FlatSpec with Matchers with PropertyChecks {

  // mappend - appends two Monoids
  // <> - alias for mappend
  // append - appends two Products (the same thing with different types)
  // mconcat - concatenates a list of Monoids
  // concat - concatenates a list of Products (the same thing with different types)

  // you can access the encapsulated value only if you have a Product, Monoid is not sufficient.

  "Product(1) <> Product(1)" should "be equal to Product(1)" in {
    Product(1) mappend Product(1) shouldEqual Product(1)
    Product(1) <> Product(1) shouldEqual Product(1)
  }

  "left identity: Product(1) <> Product(5)" should "be equal to Product(5)" in {
    Product(1) mappend Product(5) shouldEqual Product(5)
    Product(1) <> Product(5) shouldEqual Product(5)
  }

  "PROPERTY: left identity" should "hold for Monoid Product" in {
    forAll { (product: Product) =>
      Product(1) mappend product shouldEqual product
      Product(1) <> product shouldEqual product
    }
  }

  "right identity: Product(5) <> Product(1)" should "be equal to Product(5)" in {
    Product(5) mappend Product(1) shouldEqual Product(5)
    Product(5) <> Product(1) shouldEqual Product(5)
  }

  "PROPERTY: right identity" should "hold for Monoid Product" in {
    forAll { (product: Product) =>
      product mappend Product(1) shouldEqual product
      product <> Product(1) shouldEqual product
    }
  }

  "associativity: (Product(3) <> Product(5)) <> Product(10)" should "Product(3) <> (Product(5) <> Product(10))" in {
    (Product(3) mappend Product(5)) mappend Product(10) shouldEqual (Product(3) mappend (Product(5) mappend  Product(10)))
    (Product(3) <> Product(5)) <> Product(10) shouldEqual Product(3) <> (Product(5) <> Product(10))
  }

  "PROPERTY: associativity" should "hold for Monoid Product" in {
    forAll { (product1: Product, product2: Product, product3: Product) =>
      (product1 mappend product2) mappend product3 shouldEqual (product1 mappend (product2 mappend product3))
      (product1 <> product2) <> product3 shouldEqual product1 <> (product2 <> product3)
    }
  }

  "mconcat" should "return the Product of a List of Product objects (as a Monoid)" in {
    val products = List(1, 2, 3, 4, 5).map(Product(_))
    Product(1).mconcat(products) shouldEqual Product(120)
  }

  "PROPERTY: mconcat" should "return the Product of a List of Product objects (as a Monoid)" in {
    forAll { (list: List[Int]) =>
      val products = list.map(Product(_))
      Product(1).mconcat(products).asInstanceOf[Product].value shouldEqual list.product
    }
  }

  "concat" should "return the Product of a List of Product objects (as a Product)" in {
    import Product.concat
    val products = List(1, 2, 3, 4, 5).map(Product(_))
    concat(products).value shouldEqual 120
  }

  "PROPERTY: concat" should "return the Product of a List of Product objects (as a Product)" in {
    import Product.concat
    forAll { (list: List[Int]) =>
      val products = list.map(Product(_))
      concat(products).value shouldEqual list.product
    }
  }
}
