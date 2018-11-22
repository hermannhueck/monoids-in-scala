package monoids.inheritance.categories

trait Monoid[+A] extends Semigroup[A] {

  // ----- abstract methods to define in any Monoid instance

  // empty Monoid element
  def mempty[B >: A]: Monoid[B] // Haskell
  def empty[B >: A]: Monoid[B] = mempty // Cats


  // ----- implementations based on the abstract methods

  // append/merge/combine a list of Monoids into one
  def mconcat[B >: A](ms: List[Monoid[B]]): Monoid[B] = ms.fold(mempty)(_ <> _)
}
