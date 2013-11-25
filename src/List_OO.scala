sealed trait List[+A] {
  def head: A
  def tail: List[A]
  def isEmpty: Boolean 
  override def toString = "[ " + asString_internal(this) + "]"
  private def asString_internal[B >: A](l: List[B]): String  =
    l match {
      case Cons(head,tail) => head.toString + " " + asString_internal(tail)
      case _ => ""
    }
}

case object Nil extends List[Nothing] {
  override def isEmpty = true
  override def head: Nothing =
    throw new NoSuchElementException("head of empty list")
  override def tail: List[Nothing] =
    throw new UnsupportedOperationException("tail of empty list")
}

case class Cons[+A] (hd: A, tl: List[A]) extends List[A] {
  override def isEmpty = false
  override def head : A = hd
  override def tail : List[A] = tl
}

object List {
  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
   
}
