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

  def append[B](a1: List[B], a2: List[B]): List[B] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldleft[That](z: That)(f: (That, A) => That): That =
    foldLeft_internal(this, z)(f)

  @annotation.tailrec
  private def foldLeft_internal[B, That](l: List[B], z: That)(f: (That, B) => That): That = 
    l match { 
      case Nil => z
      case Cons(h,t) => foldLeft_internal(t, f(z,h))(f)
    }

  def reverse: List[A] = reverse_internal(this)

  private def reverse_internal[B](l: List[B]) = foldLeft_internal(l, List[B]())((acc,h) => Cons(h,acc))

  def foldRight[That](z: That)(f: (A,That) => That): That = 
    foldRight_internal(this, z)(f)

  private def foldRight_internal[B, That](l: List[B], z: That)(f: (B,That) => That): That = 
    foldLeft_internal(reverse_internal(l), z)((b,a) => f(a,b))

  def concat[B](l: List[List[B]]): List[B] =
    foldRight_internal(l, Nil:List[B])(append)

  def unit[B >: A](a: B): List[B] = 
    Cons(a, Nil)
  
  private def unit_internal[B](a:B): List[B] = 
    Cons(a, Nil)

  def flatMap[That](f: A => List[That]): List[That] =
    concat(foldRight(Nil:List[List[That]])((h,t) => Cons(f(h),t)))

  private def flatMap_internal[B, That](la: List[B])(f: B => List[That]): List[That] =
    concat(foldRight_internal(la, Nil:List[List[That]])((h,t) => Cons(f(h),t))) 

  def apply[That](f: List[A => That]): List[That] =
    flatMap_internal(f)(t1 => flatMap((t2) => unit_internal(t1(t2))))

  def map[That](f: A => That): List[That] =
    apply(unit_internal(f))

  def fold[B >: A](z: B)(op: (B, B) ⇒ B):  B= 
    foldRight(z)(op)

  def filter(f: A => Boolean): List[A] = 
    foldRight(Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)
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
   
  def test = {
    val a = List(1,2)
    val b = List(3,4)
    for {
      t1 <- a
      t2 <- b
    } yield ((t1,t2))
  }

  def test1 = {
    val a = List(1,2)
    val b = List(3,4)
    a.flatMap {
      t1 => b.map {
        t2 => (t1,t2)
      }
    }
  }
}
