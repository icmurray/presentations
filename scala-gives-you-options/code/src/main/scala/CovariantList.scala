sealed trait MyList[+T] {
  def head: Option[T]
  def cons[TT >: T](t: TT): MyList[TT]

  // synonym for cons
  def ::[TT >: T](t: TT) = cons(t)
}

case object MyNil extends MyList[Nothing] {
  def head = None
  def cons[T](t: T) = Cons[T](t, MyNil)
}

case class Cons[T](t: T, tail: MyList[T]) extends MyList[T] {
  def head = Some(t)
  def cons[TT >: T](t: TT) = Cons[TT](t, this)
}
