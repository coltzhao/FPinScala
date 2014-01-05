// type scala, into the shell
// type :paste, into paste mode
// paste the following code amd you can play with it.

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

trait ArrowChoice[~>[_, _]] extends Arrow[~>] {
  def left[A,B,C](f: A ~> B): (Either[A, C] ~> Either[B,C])
  def right[A,B,C](f: A ~> B): (Either[C, A] ~> Either[C,B])
  def multiplex[A,B,C,D](f: A ~> B, g: C ~> D)
    : (Either[A,C] ~> Either[B,D])
  def merge[A,B,C](f: A ~> C, g: B ~> C)
    : (Either[A,B] ~> C)
}

trait ArrowLoop[~>[_, _]] extends Arrow[~>] {
  def loop[B, C, D](f: ((B,D) ~> (C,D)) ): (B ~> C) // ???
}

trait ArrowApply[~>[_, _]] extends Arrow[~>] {
  def app[B, C]: (B ~> C, B) ~> C
}

trait Functor[F[_]] {
  def map[A,B](t: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F]{
  def unit[A](a: => A): F[A]
  def ap[A,B](fa: F[A])(fab: F[A => B]): F[B]

  override def map[A,B](t: F[A])(f: A => B): F[B] = ap(t)(unit(f))
}

trait Monad[F[_]] extends Applicative[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  override def ap[A,B](la: F[A])(f: F[A => B]): F[B] =
    flatMap(f)(t1 => flatMap(la)(t2 => unit(t1(t2))))
  override def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
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

val OptionMonad = new Monad[Option] {
  def unit[A](a: => A) = Some(a)
  def flatMap[A,B](a: Option[A])(f: A => Option[B]): Option[B] =
    a.flatMap(f)
}

val FuncArrow = new Arrow[Function1] {
  def arr[A, B](f: A => B): Function1[A, B] = f
  def first[A,B,C](f: A => B): (((A, C)) => (B, C)) =
    (ac: (A, C)) => (f(ac._1), ac._2)

  def id[A]: A => A = (a: A) => a
  def compose[A, B, C](f: B => C, g: A => B): (A => C) =
    (a: A) => f(g(a))
}

case class Kleisli[M[_], A, B](run: A => M[B]) extends  AnyRef  

trait KleisliArrow[M[_]] extends Arrow[({type λ[α, β] = Kleisli[M, α, β]})#λ] {
  implicit def m: Monad[M] 
  def arr[A, B](f: A => B): Kleisli[M, A, B] = Kleisli((a: A) => m.unit(f(a)))
  def first[A,B,C](f: Kleisli[M,A,B]): Kleisli[M, (A,C), (B,C)] =
    Kleisli((ac: (A, C)) => m.flatMap(f.run(ac._1)){(b: B) => m.unit((b, ac._2))})

  def id[A]: Kleisli[M, A, A] = Kleisli((a: A) => m.unit(a))
  def compose[A, B, C](f: Kleisli[M,B,C], g: Kleisli[M,A,B]): Kleisli[M, A, C] = 
    Kleisli((a: A) => m.flatMap(g.run(a))(f.run))
}

val OptionArrow = new KleisliArrow[Option] {
  implicit def m = OptionMonad
}


