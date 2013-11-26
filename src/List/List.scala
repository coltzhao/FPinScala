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

/////////////

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  
  def sum_new(l: List[Int]) = 
    foldRight(l, 0)((x,y) => x + y)
  
  def product_new(l: List[Double]) = 
    foldRight(l, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`, see sidebar


  def tail[A](l: List[A]): List[A] = 
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_,t) => t
    }

  def setHead[A](l: List[A])(h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_,t) => Cons(h,t)
  }

  def drop[A](l: List[A], n: Int): List[A] = 
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1) 
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f) 
      case _ => l
    }

  def init[A](l: List[A]): List[A] = 
    l match { 
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }

  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((_,acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match { 
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def sum2(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def product2(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = 
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  def concat[A](l: List[List[A]]): List[A] = 
    foldRight(l, Nil:List[A])(append)

  def map_origin[A, B](la: List[A])(f: A => B): List[B] =
    foldRight(la, Nil:List[B])((h,t) => Cons(f(h), t))

////
  def unit[A](a: A): List[A] = 
    Cons(a, Nil)

  def flatMap[A, B](la: List[A])(f: A => List[B]): List[B] =
    concat(foldRight(la, Nil:List[List[B]])((h,t) => Cons(f(h),t)))

  def join[A](lla: List[List[A]]): List[A] = 
    flatMap(lla)(la => la)

  def compose[A,B,C](f: A => List[B], g: B => List[C]): A => List[C] =
    a => flatMap(f(a))(g)

  def apply[A,B](la: List[A])(f: List[A => B]): List[B] =
    flatMap(f)(t1 => flatMap(la)(t2 => unit(t1(t2))))

  def map2[A,B,C](la: List[A], lb: List[B])(f: (A, B) => C): List[C] =
    apply(lb)(apply(la)(unit(f.curried)))

  def map[A, B](la: List[A])(f: A => B): List[B] =
    apply(la)(unit(f))

  def fold[A](l: List[A])(z: A)(op: (A, A) ⇒ A): A = 
    foldRight(l, z)(op)

  def filter[A](l: List[A])(f: A => Boolean): List[A] = 
    foldRight(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)
}
