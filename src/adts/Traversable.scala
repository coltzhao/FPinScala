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

trait Functor[F[_]] {
  def map[A,B](t: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F]{
  def unit[A](a: => A): F[A]
  def ap[A,B](fa: F[A])(fab: F[A => B]): F[B]

  override def map[A,B](t: F[A])(f: A => B): F[B] = ap(t)(unit(f))
  def zip[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      ap(fb)(map(fa)(f.curried))
}

type Const[A, B] = A

implicit def monoidApplicative[M](m: Monoid[M]) =
  new Applicative[({ type f[x] = Const[M, x] })#f] {
    def unit[A](a: => A): M = m.zero
    override def ap[A,B](m1: M)(m2: M): M = m.op(m1, m2)
  }

import scala.Predef.identity

trait Traversable[T[_]] extends Functor[T] with Foldable[T] {
  def traverse[F[_]: Applicative, A, B](f: A => F[B], t: T[A]): F[T[B]] // = sequence(map(t)(f))
  def sequence[F[_]: Applicative, A](tfa: T[F[A]]): F[T[A]] = traverse(identity[F[A]], tfa)
  // The above 2 are equivelant

// underneath are deduced
//  def mapM[M[_]: Monad, A, B](f: A => M[B], t: T[A]): M[T[B]] = ???
//  def sequenceM[M[_]: Monad](tmb: T[M[B]]): M[T[B]] = ???

  type Id[A] = A

  val Identity = new Applicative[Id] {
    def unit[A](a: => A) = a
    def ap[A,B](a: A)(f: A => B): B = f(a)
  }

  override def map[A, B](k: T[A])(f: A => B) = traverse[Id, A, B](f, k)(Identity)
  override def foldMap[A, M](as: T[A], f: A => M)(implicit m: Monoid[M]): M =
    traverse[({type f[x] = Const[M,x]})#f,A,Nothing](f, as)(monoidApplicative(m))

}

import scala.language.higherKinds

val OptionApplicatable = new Applicative[Option] {
  def unit[A](a: => A) = Some(a)
  def ap[A,B](a: Option[A])(f: Option[A => B]): Option[B] = 
    f.flatMap {
      t1 => a.flatMap {
        t2 => unit(t1(t2))
      }
    }
}

val ListTraversable = new Traversable[List] {
  def traverse[F[_], A, B](f: A => F[B], t: List[A])(implicit m: Applicative[F]): F[List[B]] =
    t.foldRight(m.unit(List[B]()))((a, fbs) => m.zip(f(a),fbs)(_ :: _))
}

object test {

  val x1 = List(1,2,3,4)

  val x2 = List(Option(1), Option(2), Option(3))
  
  val x3 = List(Option(1), Option(2), Option(3), Option(null))

  def f1(a: Int): Option[Int] = Some(a) 

  val r1 = ListTraversable.traverse(f1, x1)(OptionApplicatable)

  val r2 = ListTraversable.sequence(x2)(OptionApplicatable)

  val r3 = ListTraversable.sequence(x3)(OptionApplicatable)
}

