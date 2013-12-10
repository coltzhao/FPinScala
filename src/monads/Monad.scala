// id function:
// def id[A](a: A): A = a
// compose function:
// def compose[A,B,C](f: B => C, g: A => B): A => C =
//   a => f(g(a))

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}
// Functor Law
// identity: map(x)(id) == x
// composition: map(a)(compose(f, g)) == map(map(a,g), f)

trait Applictive[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def ap[A,B](la: F[A])(f: F[A => B]): F[B] 

  override def map[A, B](la: F[A])(f: A => B): F[B] =
    ap(la)(unit(f))
}
// Applicative Law
// identity: ap(a, unit(id)) == a 
// composition: ap(ap(a, g), f) == ap(a, ap(g, ap(f, unit(compose))))
// homomorphism: ap(unit(a), unit(f)) == unit(f(a))
// interchange: ap(unit(a), f) == ap(f, unit(f => f(x)))

trait Monad[F[_]] extends Applictive[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  override def ap[A,B](la: F[A])(f: F[A => B]): F[B] =
    flatMap(f)(t1 => flatMap(la)(t2 => unit(t1(t2))))
  override def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
}
// Monad Law
// left identity: f(a) == flatmap(unit(a), f)
// right identity: a == flatMap(a, x => unit(x))
// associativity: flatMap(a, x => flatMap(f(x), g)) == flatMap(flatMap(a, f), g)
