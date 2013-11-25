sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A] (head: A, tail: List[A]) extends List[A] 

object List {
  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  private def asString_internal[A](l: List[A]): String = 
    l match {
      case Nil => ""
      case Cons(head,tail) => head.toString + " " + asString_internal(tail)
    }

  def toString[A](l: List[A]): String = 
    "[ " + asString_internal(l) + "]"

}
