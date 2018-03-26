package monoids.impl.monoid

import monoids.categories.Monoid

/**
  * Pair is a Monoid implementation.
  *
  * Pair is a Monoid that encapsulates two other Monoids of possibly different types
  * It's append operation combines the encapsulated Monoids and returns
  * the encapsulated result as a new Pair.
  *
  * It implements two Monoid functions:
  * - mempty = Pair(Monoid1.empty, Monoid2.empty): gives us the neutral element
  * def mempty[B >: MB]: Monoid[B]
  * - mappend: appends/merges/combines two Pairs into one
  * def mappend[B >: A](that: Monoid[B]): Monoid[B]
  *
  * With the above functions you get <> and mconcat for free (is implemented in the trait Monoid):
  * def <>   ... is just an alias for mappend
  * def mconcat[B >: A](ms: List[Monoid[B]]): Monoid[B]
  *
  * concat is just the same as mconcat with the types changed to Pair instead of Monoid[A]
  * def concat[...](mms: List[Pair[...]]): Pair[...]
  */
case class Pair[+A1, +MO1, +A2, +MO2](monoid1: MO1, monoid2: MO2)(implicit ev1: MO1 <:< Monoid[A1], ev2: MO2 <:< Monoid[A2]) extends Monoid[(MO1, MO2)] {

  override def mempty[B >: (MO1, MO2)]: Pair[A1, MO1, A2, MO2] = Pair(monoid1.mempty.asInstanceOf[MO1], monoid2.mempty.asInstanceOf[MO2])

  override def mappend[B >: (MO1, MO2)](that: Monoid[(B)]): Pair[A1, MO1, A2, MO2] =
    (this, that.asInstanceOf[Pair[A1, MO1, A2, MO2]]) match {
      case (Pair(x1, x2), Pair(y1, y2)) => Pair((x1 mappend y1).asInstanceOf[MO1], (x2 mappend y2).asInstanceOf[MO2]) // invokes mappend for the encapsulated Monoids
    }

  // alias for mappend overrides super with a more specific return type
  override def <>[B >: (MO1, MO2)](that: Monoid[B]): Pair[A1, MO1, A2, MO2] = this mappend that

  // overrides super method, but gives a more specific return type than Monoid
  override def mconcat[B >: (MO1, MO2)](ms: List[Monoid[B]]): Pair[A1, MO1, A2, MO2] = super.mconcat(ms).asInstanceOf[Pair[A1, MO1, A2, MO2]]

  def concat[AA1 >: A1, B1 >: MO1, AA2 >: A2, B2 >: MO2](ms: List[Pair[AA1, B1, AA2, B2]]): Pair[A1, B1, A2, B2] = mconcat(ms)

  def _1: MO1 = monoid1
  def _2: MO2 = monoid2

  def fst: MO1 = monoid1
  def snd: MO2 = monoid2
}
