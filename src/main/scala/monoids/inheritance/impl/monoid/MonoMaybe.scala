package monoids.inheritance.impl.monoid

import monoids.inheritance.categories.Monoid

/**
  * MonoMaybe is a Monoid implementation.
  *
  * MonoMaybe is a Maybe that implements Monoid and encapsulates another Monoid
  * It's append operation combines the encapsulated Monoids and returns
  * the encapsulated result as a new MonoMaybe.
  *
  * It implements two Monoid functions:
  * - mempty = MonoMaybe(Nothing): gives us the neutral element
  * def mempty[B >: MB]: Monoid[B]
  * - mappend: appends/merges/combines two MonoMaybes into one
  * def mappend[B >: A](that: Monoid[B]): Monoid[B]
  *
  * With the above functions you get <> and mconcat for free (is implemented in the trait Monoid):
  * def <>   ... is just an alias for mappend
  * def mconcat[B >: A](ms: List[Monoid[B]]): Monoid[B]
  *
  * concat is just the same as mconcat with the types changed to MonoMaybe instead of Monoid[A]
  * def concat[...](mms: List[MonoMaybe[...]]): MonoMaybe[...]
  */
object MonoMaybe {

  def fromOption[A, MO](o: Option[MO])(implicit ev: MO <:< Monoid[A]): MonoMaybe[A, MO] = o match {
    case None => MonoNothing
    case Some(value) => MonoJust(value)
  }

  def empty[A, B >: A, MO >: Monoid[B]]: MonoMaybe[B, MO] = MonoNothing

  def concat[A, B >: A, MO >: Monoid[B]](ms: List[Monoid[B]]): MonoMaybe[A, B] = MonoNothing.mconcat(ms)
}

abstract class MonoMaybe[+A, +MO](implicit ev: MO <:< Monoid[A]) extends Monoid[MO] with Maybe[MO] {

  override def mempty[B >: MO]: MonoMaybe[A, B] = MonoNothing

  override def mappend[B >: MO](that: Monoid[B]): MonoMaybe[A, MO] =
    (this, that.asInstanceOf[MonoMaybe[A, MO]]) match {
      case (MonoJust(x), MonoJust(y)) => MonoJust((x mappend y).asInstanceOf[MO]) // invokes mappend for the encapsulated Monoids
      case (MonoJust(x), MonoNothing) => MonoJust(x)
      case (MonoNothing, MonoJust(y)) => MonoJust(y)
      case (_, _) => MonoNothing
    }

  // alias for mappend overrides super with a more specific return type
  override def <>[B >: MO](that: Monoid[B]): MonoMaybe[A, MO] = this mappend that

  // overrides super method, but gives a more specific return type than Monoid
  override def mconcat[B >: MO](ms: List[Monoid[B]]): MonoMaybe[A, B] = super.mconcat(ms).asInstanceOf[MonoMaybe[A, B]]
}

case object MonoNothing extends MonoMaybe[Nothing, Nothing] {

  override def value = throw new NoSuchElementException("get value of MonoNothing")

  override def isJust: Boolean = false
}

final case class MonoJust[+A, +MO](value: MO)(implicit ev: MO <:< Monoid[A]) extends MonoMaybe[A, MO] {

  override def isJust: Boolean = true
}
