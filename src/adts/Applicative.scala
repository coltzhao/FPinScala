import scala.language.higherKinds
import scala.language.implicitConversions

abstract class Functor[F[_], A](value: F[A]) {
  def map[B](f: A => B): F[B]
}

abstract class Applicative[F[_], A](o: F[A]) extends Functor[F, A](o) {
  def unit[C](a: C): F[C]
  def ap[B](f: F[A => B]): F[B] 

  override def map[B](f: A => B): F[B] =
    ap(unit(f))
}

implicit def Option2Applicative[A](o: Option[A]): Applicative[Option, A] = new Applicative[Option, A](o) {
  
  def unit[C](a: C): Option[C] = Some(a)
  def ap[B](f: Option[A => B]): Option[B] = {
    f.flatMap {
      t1 => o.flatMap {
        t2 => unit(t1(t2))
      }
    }
  }
}

object test {
  def oneVarFunc: Int => Int = {
    _ + 1
  }

  def twoVarFunc: (Int, Int) => Int = {
    _ + _
  }

  val x1: Option[Int] = Some(1)

  val x2: Option[Int] = Some(2)

  val x3: Option[Int] = None

  //functor
  def test1 = 
    x1.map(oneVarFunc)

  //Applicative Functor
  def test2 = {
    x2.ap(x1.map(twoVarFunc.curried))
  }

  def test2_1 = {
    x3.ap(x2.map(twoVarFunc.curried))
  }

  //Monad
  def test3 = {
    for {
      r1 <- x1
      r2 <- x2
    } yield {
      if(r1 == 1) {
        r1 * r2
      } else {
        r1 + r2
      }
    }
  }

//  trait Future[+T] {
//    def zip[U](that: Future[U]): Future[(T, U)] = ???
//  }

  def compose[F[_], A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = ???

// unlimit stream
// multi dimensional array

}

trait Semigroup[A] {
  def op(a: A, b: A): A
}

trait Monoid[A] extends Semigroup[A] {
  val zero: A
}

trait Foldable[F[_]]  {
  def foldMap[A, M: Monoid](t: F[A], f: A => M): M

/*
  def fold[M: Monoid](t: F[M]): M // also called reduce with variance
  def foldRight[A, B](t: F[A], z: => B, f: (A, B) => B): B 
  def foldLeft[A, B](t: F[A], z: B, f: (B, A) => B): B
  def foldr1[A, B](t: F[A], f: (A, => A) => A): Option[A]
  def foldl1[A, B](t: F[A], f: (A, A) => A): Option[A]
*/
}


