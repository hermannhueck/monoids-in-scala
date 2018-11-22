package monoids.typeclasses

import monoids.typeclasses.monoid.Monoid

object Main extends App {

  import Monoid.MonoidOps

  {
    println("\n----- Monoids")

    val list = Monoid[List[Int]].combine(List(1, 2, 3), List(4, 5, 6))
    val str = Monoid[String].empty
    val str2 = Monoid[String].combine("Hello, ", "World!")

    println(list)
    println(str)
    println(str2)
  }

  {
    println("\n----- combining with |+| or <>")

    val list = List(1) |+| List(2, 3)
    val str = "Hello" |+| ", " |+| "World!"
    val str2 = "Hello" <> ", " <> "World!"

    println(list)
    println(str)
    println(str2)
  }

  {
    println("\n----- specific folds")

    def sum(xs: Seq[Int]): Int = xs.fold(0)(_ + _)
    def product(xs: Seq[Int]): Int = xs.fold(1)(_ * _)
    def concat(xs: Seq[String]): String = xs.fold("")(_ + _)

    println(sum(1 to 5))
    println(product(1 to 5))
    println(concat(('a' to 'e').map(_.toString)))
  }

  {
    println("\n----- generic fold")

    import Monoid.fold
    import monoids.typeclasses.sumprod.{Product, Sum}

    def sum(xs: Seq[Int]): Int = fold(xs.map(Sum(_))).value
    def product(xs: Seq[Int]): Int = fold(xs.map(Product(_))).value
    def concat(xs: Seq[String]): String = fold(xs)

    println(sum(1 to 5))
    println(product(1 to 5))
    println(concat(('a' to 'e').map(_.toString)))

    implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
      override val empty = 0
      override def combine(x: Int, y: Int): Int = x + y
    }

    val result = fold(Seq(1 -> "a", 2 -> "b", 3 -> "c"))

    println(result)
  }

  {
    println("\n----- boolean Monoids")

    import Monoid.fold
    import monoids.typeclasses.bool.{All, Anyy}

    def all(xs: Seq[Boolean]): Boolean = fold(xs.map(All(_))).value
    def any(xs: Seq[Boolean]): Boolean = fold(xs.map(Anyy(_))).value

    val booleans = (1 to 5).toList.map(_ % 2 == 0)

    println(all(booleans))
    println(any(booleans))
  }

  {
    println("\n----- Monoid Max")

    import Monoid.{fold, foldMap}
    import monoids.typeclasses.minmax.Max

    def max1(xs: Seq[Int]): Max =
      xs.map(Max(_)).fold(Max.none)(Max(_, _))

    def max2(xs: Seq[Int]): Max =
      fold(xs.map(Max(_))) // using generic Monoid.fold

    def max3(xs: Seq[Int]): Max =
      foldMap(xs)(Max(_))

    println(max1(1 to 5))
    println(max1(Seq.empty[Int]))

    println(max2(1 to 5))
    println(max2(Seq.empty[Int]))

    println(max3(1 to 5))
    println(max3(Seq.empty[Int]))
  }

  {
    println("\n----- Monoid Min")

    import Monoid.fold
    import monoids.typeclasses.minmax.Min

    def min1(xs: Seq[Int]): Min =
      xs.map(Min(_)).fold(Min.none)(Min(_, _))

    def min2(xs: Seq[Int]): Min =
      fold(xs.map(Min(_))) // using generic Monoid.fold

    println(min1(1 to 5))
    println(min1(Seq.empty[Int]))

    println(min2(1 to 5))
    println(min2(Seq.empty[Int]))
  }

  {
    println("\n----- implicit Monoid combination")

    import scala.concurrent.duration._
    import scala.concurrent.{Await, Future}
    import scala.concurrent.ExecutionContext.Implicits.global

    case class Config(one: String, two: String)

    val m1: Config => Future[Map[Int, (String, Option[String])]] =
      (config: Config) =>
        Future {
          Map(
            1 -> ("one", Option(config.one)),
            4 -> ("four", Option("44-4-4-4-44"))
          )
        }

    val m2: Config => Future[Map[Int, (String, Option[String])]] =
      (config: Config) =>
        Future {
          Map(
            1 -> ("eins", Option(s"${config.one}!")),
            2 -> ("zwei", Option(config.two)),
            3 -> ("drei", Option.empty[String]),
            4 -> ("vier", Option.empty[String])
          )
        }

    val m3: Config => Future[Map[Int, (String, Option[String])]] =
      m1 |+| m2

    val future: Future[Map[Int, (String, Option[String])]] =
      m3.apply(Config("11111-", "22222-"))

    val map: Map[Int, (String, Option[String])] = Await.result(future, 3.seconds)

    println(future)
    println(map)
    map foreach println
  }

  println("\n-----")
}
