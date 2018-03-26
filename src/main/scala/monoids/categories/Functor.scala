package monoids.categories

trait Functor[A] {

  def fmap[B](f: A => B): Functor[B]
}
