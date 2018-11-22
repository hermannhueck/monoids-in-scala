package monoids.inheritance.categories

trait Monad[A] extends Applicative[A] {

  def unit(a: A): Monad[A] = pure(a).asInstanceOf[Monad[A]]
  def returnn(a: A) = unit(a)

  def flatMap[B](f: A => Monad[B]): Monad[B]
  def bind[B](f: A => Monad[B]): Monad[B] = flatMap(f)
  def >>=[B](f: A => Monad[B]): Monad[B] = flatMap(f)
}
