package monoids.inheritance.impl.monoid

import monoids.inheritance.categories.Monoid

/**
  * Identity is a Monoid implementation.
  *
  * Identity is a Monoid that encapsulates another Monoid
  * It's append operation combines the encapsulated Monoids and returns
  * the encapsulated result as a new Identity.
  *
  * It implements two Monoid functions:
  * - mempty = Identity(Monoid.empty): gives us the neutral element
  * def mempty[B >: MB]: Monoid[B]
  * - mappend: appends/merges/combines two Identitys into one
  * def mappend[B >: A](that: Monoid[B]): Monoid[B]
  *
  * With the above functions you get <> and mconcat for free (is implemented in the trait Monoid):
  * def <>   ... is just an alias for mappend
  * def mconcat[B >: A](ms: List[Monoid[B]]): Monoid[B]
  *
  * concat is just the same as mconcat with the types changed to Identity instead of Monoid[A]
  * def concat[...](mms: List[Identity[...]]): Identity[...]
  */
case class Identity[+A, +MO](monoid: MO)(implicit ev: MO <:< Monoid[A]) extends Monoid[MO] {

  override def mempty[B >: MO]: Identity[A, B] = Identity(monoid.mempty.asInstanceOf[MO])

  override def mappend[B >: MO](that: Monoid[B]): Identity[A, MO] =
    (this, that.asInstanceOf[Identity[A, MO]]) match {
      case (Identity(x), Identity(y)) => Identity((x mappend y).asInstanceOf[MO]) // invokes mappend for the encapsulated Monoids
    }

  // alias for mappend overrides super with a more specific return type
  override def <>[B >: MO](that: Monoid[B]): Identity[A, MO] = this mappend that

  // overrides super method, but gives a more specific return type than Monoid
  override def mconcat[B >: MO](ms: List[Monoid[B]]): Identity[A, B] = super.mconcat(ms).asInstanceOf[Identity[A, B]]

  def concat[AA >: A, B >: MO](ms: List[Identity[AA, B]]): Identity[A, B] = mconcat(ms)

  def value: MO = monoid
}
