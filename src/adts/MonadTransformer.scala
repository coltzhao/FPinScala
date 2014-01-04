// case class XT[M[_], X[_], A](value: M[X[A]])(implicit M: Monad[M])(implicit X: Monad[M])

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Applictive[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def ap[A,B](la: F[A])(f: F[A => B]): F[B] 

  override def map[A, B](la: F[A])(f: A => B): F[B] =
    ap(la)(unit(f))
}

trait Monad[F[_]] extends Applictive[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  override def ap[A,B](la: F[A])(f: F[A => B]): F[B] =
    flatMap(f)(t1 => flatMap(la)(t2 => unit(t1(t2))))
  override def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
}

case class OptionT[M[_],A](value: M[Option[A]]) {
  self =>

  def unit(a: A)(implicit m: Monad[M]): OptionT[M, A] = 
    new OptionT[M, A](m.unit(Some(a)))
  def flatMap[B](f: A => OptionT[M, B])(implicit m: Monad[M])
    : OptionT[M, B] = new OptionT[M, B](
    m.flatMap(self.value) {
      case None => m.unit(None)
      case Some(a) => f(a).value
}) }

