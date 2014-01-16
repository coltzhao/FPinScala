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
  def loop[B, C, D](f: ((B,D) ~> (C,D)) ): (B ~> C) // ???
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
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  override def ap[A,B](la: F[A])(f: F[A => B]): F[B] =
    flatMap(f)(t1 => flatMap(la)(t2 => unit(t1(t2))))
  override def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
}

trait MonadFix[F[_]] extends Monad[F] {
  def mfix[A](f: A => m[A]): m[A]
}
