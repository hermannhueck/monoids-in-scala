package monoids.typeclasses.monoid

import scala.concurrent.{ExecutionContext, Future}

trait Monoid[A] extends java.io.Serializable {
  def empty: A
  def combine(lhs: A, rhs: A): A
}

object Monoid {

  def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]

  implicit class MonoidOps[A](self: A) {
    def |+|(other: A)(implicit m: Monoid[A]): A = m.combine(self, other) // infix combine operator of Cats
    def <>(other: A)(implicit m: Monoid[A]): A = m.combine(self, other) // Haskells infix combine operator
  }

  // generic fold for Monoids
  def fold[A: Monoid](xs: Seq[A]): A =
    xs.fold(Monoid[A].empty)(Monoid[A].combine)

  // maps the Seq to a Seq of Monoids and then folds the elems
  def foldMap[A, B: Monoid](xs: Seq[A])(f: A => B): B =
    xs.map(f).fold(Monoid[B].empty)(Monoid[B].combine)

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    override val empty: String = ""
    override def combine(lhs: String, rhs: String): String = lhs + rhs
  }

  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override val empty: List[A] = List()
    override def combine(xs: List[A], ys: List[A]): List[A] = xs ++ ys
  }

  implicit def optionMonoid[A: Monoid]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override val empty: Option[A] = Option.empty[A]
    override def combine(lhs: Option[A], rhs: Option[A]): Option[A] = (lhs, rhs) match {
      case (None, opt) => opt
      case (opt, None) => opt
      case (Some(x), Some(y)) => Some(x |+| y)
    }
  }

  implicit def functionMonoid[A, B: Monoid]: Monoid[A => B] = new Monoid[A => B] {
    override val empty: A => B = _ => Monoid[B].empty
    override def combine(f: A => B, g: A => B): A => B =
      input => Monoid[B].combine(f(input), g(input))
  }

  implicit def futureMonoid[A: Monoid](implicit ec: ExecutionContext): Monoid[Future[A]] =
    new Monoid[Future[A]] {
      override val empty: Future[A] = Future.successful(Monoid[A].empty)
      override def combine(lhs: Future[A], rhs: Future[A]): Future[A] =
        for {
          (x, y) ← lhs.zip(rhs)
        } yield Monoid[A].combine(x, y)
    }

  implicit def mapMonoid[A, B: Monoid]: Monoid[Map[A, B]] =

    new Monoid[Map[A, B]] {

      override val empty: Map[A, B] = Map()

      override def combine(lhs: Map[A, B], rhs: Map[A, B]): Map[A, B] = {
        lhs.foldLeft(rhs) {
          case (acc, (k, v)) =>
            acc.updated(
              k,
              acc.get(k).map(vv => Monoid[B].combine(v, vv)).getOrElse(v))
        }
      }
    }

  implicit def tuple2Monoid[A: Monoid, B: Monoid]: Monoid[(A, B)] =

    new Monoid[(A, B)] {

      override val empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)

      override def combine(lhs: (A, B), rhs: (A, B)): (A, B) =
        (lhs._1 |+| rhs._1, lhs._2 |+| rhs._2)
    }

  implicit def tuple3Monoid[A: Monoid, B: Monoid, C: Monoid]: Monoid[(A, B, C)] =

    new Monoid[(A, B, C)] {

      override val empty: (A, B, C) =
        (Monoid[A].empty, Monoid[B].empty, Monoid[C].empty)

      override def combine(lhs: (A, B, C), rhs: (A, B, C)): (A, B, C) =
        (lhs._1 |+| rhs._1,
          lhs._2 |+| rhs._2,
          lhs._3 |+| rhs._3)
    }

  implicit def tuple4Monoid[A: Monoid, B: Monoid, C: Monoid, D: Monoid]: Monoid[(A, B, C, D)] =

    new Monoid[(A, B, C, D)] {

      override val empty: (A, B, C, D) =
        (Monoid[A].empty, Monoid[B].empty, Monoid[C].empty, Monoid[D].empty)

      override def combine(lhs: (A, B, C, D), rhs: (A, B, C, D)): (A, B, C, D) =
        (lhs._1 |+| rhs._1,
          lhs._2 |+| rhs._2,
          lhs._3 |+| rhs._3,
          lhs._4 |+| rhs._4)
    }

  implicit def tuple5Monoid[A: Monoid, B: Monoid, C: Monoid, D: Monoid, E: Monoid]: Monoid[(A, B, C, D, E)] =

    new Monoid[(A, B, C, D, E)] {

      override val empty: (A, B, C, D, E) =
        (Monoid[A].empty, Monoid[B].empty, Monoid[C].empty, Monoid[D].empty, Monoid[E].empty)

      override def combine(lhs: (A, B, C, D, E), rhs: (A, B, C, D, E)): (A, B, C, D, E) =
        (lhs._1 |+| rhs._1,
          lhs._2 |+| rhs._2,
          lhs._3 |+| rhs._3,
          lhs._4 |+| rhs._4,
          lhs._5 |+| rhs._5)
    }
}
