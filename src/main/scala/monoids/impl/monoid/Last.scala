package monoids.impl.monoid

import monoids.categories.Monoid

/**
  * Last is a Monoid implementation.
  *
  * It's append operation combines to Last objects each containing a Maybe of some type.
  * If the 2nd Last contains a Just it returns the 2nd Last, otherwise the 1st Last.
  *
  * It implements two Monoid functions:
  * - mempty = Last(Nothing): gives us the neutral element
  * def mempty[B >: MB]: Monoid[B]
  * - mappend: appends/merges/combines two Lasts into one
  * def mappend[B >: A](that: Monoid[B]): Monoid[B]
  *
  * With the above functions you get <> and mconcat for free (is implemented in the trait Monoid):
  * def <>   ... is just an alias for mappend
  * def mconcat[B >: A](ms: List[Monoid[B]]): Monoid[B]
  *
  * concat is just the same as mconcat with the types changed to Last instead of Monoid[Maybe[A]]
  * def concat(lsts: List[Last]): Last
  */
object Last {

  def empty[A, MB >: Maybe[A], B >: MB]: Last[B, MB] = Last(Nothingg).asInstanceOf[Last[B, MB]]

  def concat[A, MB >: Maybe[A], B >: MB](lsts: List[Last[B, MB]]): Last[B, MB] = Last(Nothingg).mconcat(lsts)
}

case class Last[+A, +MB](mb: MB)(implicit ev: MB <:< Maybe[A]) extends Monoid[MB] {

  override def mempty[B >: MB]: Last[A, MB] = Last(Nothingg.asInstanceOf[MB])

  override def mappend[B >: MB](that: Monoid[B]): Last[A, MB] =
    (this, that.asInstanceOf[Last[A, MB]]) match {
      case (Last(fst), Last(snd)) => Last {
        if (snd.isJust) snd else fst
      }
    }

  // alias for mappend overrides super with a more specific return type
  override def <>[B >: MB](that: Monoid[B]): Last[A, MB] = this mappend that

  // overrides super method, but gives a more specific return type than Monoid
  override def mconcat[B >: MB](ms: List[Monoid[B]]): Last[B, MB] = super.mconcat(ms).asInstanceOf[Last[B, MB]]
}
