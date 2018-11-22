package monoids.inheritance.categories

trait Semigroup[+A] {

  // append (or merge or condense or combine) two Monoids: this and that
  def mappend[B >: A](that: Monoid[B]): Monoid[B] // Haskell
  def <>[B >: A](that: Monoid[B]): Monoid[B] = this.mappend(that) // Haskell
  def combine[B >: A](that: Monoid[B]): Monoid[B] = this.mappend(that) // Cats
}
