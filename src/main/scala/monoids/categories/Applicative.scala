package monoids.categories

trait Applicative[A] extends Functor[A] {

  def pure(a: A): Applicative[A]

  def ap[B](appF: Applicative[A => B]): Applicative[B]
  def <*>[B](appF: Applicative[A => B]): Applicative[B] = ap(appF)
}
