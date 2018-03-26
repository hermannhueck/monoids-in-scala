package monoids.impl.monoid

import monoids.categories.Monoid

/**
  * First is a Monoid implementation.
  * 
  * It's append operation combines to First objects each containing a Maybe of some type.
  * If the 1st First contains a Just it returns the 1st First, otherwise the 2nd First.
  *
  * It implements two Monoid functions:
  * - mempty = First(Nothing): gives us the neutral element
  * def mempty[B >: MB]: Monoid[B]
  * - mappend: appends/merges/combines two Firsts into one
  * def mappend[B >: A](that: Monoid[B]): Monoid[B]
  *
  * With the above functions you get <> and mconcat for free (is implemented in the trait Monoid):
  * def <>   ... is just an alias for mappend
  * def mconcat[B >: A](ms: List[Monoid[B]]): Monoid[B]
  *
  * concat is just the same as mconcat with the types changed to First instead of Monoid[Maybe[A]]
  * def concat(fsts: List[First]): First
  */
object First {

  def empty[A, MB >: Maybe[A], B >: MB]: First[B, MB] = First(Nothingg).asInstanceOf[First[B, MB]]

  def concat[A, MB >: Maybe[A], B >: MB](fsts: List[First[B, MB]]): First[B, MB] = First(Nothingg).mconcat(fsts)
}

case class First[+A, +MB](mb: MB)(implicit ev: MB <:< Maybe[A]) extends Monoid[MB] {

  override def mempty[B >: MB]: First[B, MB] = First(Nothingg).asInstanceOf[First[B, MB]]

  override def mappend[B >: MB](that: Monoid[B]): First[A, MB] =
    (this, that.asInstanceOf[First[A, MB]]) match {
      case (First(fst), First(snd)) => First {
        if (fst.isJust) fst else snd
      }
    }

  // alias for mappend overrides super with a more specific return type
  override def <>[B >: MB](that: Monoid[B]): First[A, MB] = this mappend that

  // overrides super method, but gives a more specific return type than Monoid
  override def mconcat[B >: MB](ms: List[Monoid[B]]): First[B, MB] = super.mconcat(ms).asInstanceOf[First[B, MB]]
}
