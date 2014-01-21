import scala.math.abs
import scala.language.higherKinds
import scala.language.implicitConversions

abstract class Lazy[T] {
  def v: T
}
  
object Lazy {
  def apply[T](t: => T): Lazy[T] =  new Lazy[T] {
    def v = t
  }
}

abstract class LazyTuple[A,B]{
  def _1: A
  def _2: B
}

object LazyTuple {
  def apply[A, B](a: => A, b: => B): LazyTuple[A, B] = new LazyTuple[A, B] {
    def _1 = a
    def _2 = b
  }

}

def lazyTuple[A, B](a: => A, b: => B): LazyTuple[A, B] = new LazyTuple[A, B] {
  def _1 = a
  def _2 = b
}

def lazyed[T](t: => T): Lazy[T] =  new Lazy[T] {
  def v = t
}

trait Category[~>[_, _]]  { 
  self =>

  // minimum set
  def id[A]: A ~> A
  def compose[A, B, C](f: B ~> C, g: A ~> B): (A ~> C)

  def andThen[A, B, C](f: A ~> B, g: B ~> C): (A ~> C) = 
    compose(g, f)
}

trait Arrow[~>[_, _]] extends Category[~>] {
  self =>

  //minimum set
  def arr[A, B](f: A => B): A ~> B
  def first[A,B,C](f: A ~> B): ((A, C) ~> (B, C))

  def second[A,B,C](f: A ~> B): ((C, A) ~> (C, B)) = {
    def swap[X, Y] = arr[(X, Y), (Y, X)] {
      case (x, y)  => (y, x)
    }

    compose(swap, compose(first[A, B, C](f), swap))
  }
  def split[A,B,C,D](f: A ~> B, g: C ~> D)
    : ((A, C) ~> (B, D)) = //parallelComposition
    compose(second[C, D, B](g), first[A, B, C](f))
  def combine[A,B,C](f: A ~> B, g: A ~> C)
    : (A ~> (B,C)) = //fanoutComposition
    compose(split(f, g), arr((b: A) => (b, b)))
}

trait ArrowLoop[~>[_, _]] extends Arrow[~>] {
  def loop[A, B, C](f: LazyTuple[A,C] ~> LazyTuple[B,C] ): A ~> B 
}

val FuncArrowLoop = new ArrowLoop[Function1] {
  def arr[A, B](f: A => B): Function1[A, B] = f
  def first[A,B,C](f: A => B): (((A, C)) => (B, C)) =
    (ac: (A, C)) => (f(ac._1), ac._2)

  def id[A]: A => A = (a: A) => a
  def compose[A, B, C](f: B => C, g: A => B): (A => C) =
    (a: A) => f(g(a))

  def loop[A, B, C](f: LazyTuple[A,C] => LazyTuple[B,C] ): A => B = 
    (a: A) => new { val bc: LazyTuple[B,C] = f(lazyTuple(a, bc._2)) }.bc._1
}

val squareRoot = FuncArrowLoop.loop[(Double, Double), Double, Double => Double] {
  ac => lazyTuple[Double, Double => Double](
    ac._2(ac._1._1), 
    { x:Double => {
      val ret: Double = x - (((x * x) - ac._1._2) / (2 * x))
      (abs(ret - x) < 0.0001) match {
        case true => ret
        case false => ac._2(ret)
      }
    }}
  )
}

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def ap[A,B](la: F[A])(f: F[A => B]): F[B] 

  override def map[A, B](la: F[A])(f: A => B): F[B] =
    ap(la)(unit(f))

  def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] =
    ap(fb)(map(fa)(f.curried))

}

trait Monad[F[_]] extends Applicative[F] {
  def unit[A](a: A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  override def ap[A,B](la: F[A])(f: F[A => B]): F[B] =
    flatMap(f)(t1 => flatMap(la)(t2 => unit(t1(t2))))
  override def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
}

trait MonadFix[F[_]] extends Monad[F] {
  def mfix[A](f: Lazy[A => F[A]]): F[A]
}

def fix[A](f: Lazy[A => A]): A = ??? // resulr is where f(x) = x 

