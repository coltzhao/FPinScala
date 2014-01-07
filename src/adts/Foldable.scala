trait Semigroup[A] {
    def op(a: A, b: A): A
}

trait Monoid[A] extends Semigroup[A] {
    val zero: A
}

trait Foldable[F[_]]  {
  def foldMap[A,B](fa: F[A], f: A => B)(implicit m: Monoid[B]): B

/*
  def fold[M: Monoid](t: F[M]): M // also called reduce with variance
  def foldRight[A, B](t: F[A], z: => B, f: (A, B) => B): B 
  def foldLeft[A, B](t: F[A], z: B, f: (B, A) => B): B
  def foldr1[A, B](t: F[A], f: (A, => A) => A): Option[A]
  def foldl1[A, B](t: F[A], f: (A, A) => A): Option[A]
*/
}

val IntMonoid = new Monoid[Int] {
  def op(a: Int, b: Int): Int = a * b 
  val zero: Int = 1
}

val ListFodable = new Foldable[List] {
  def foldMap[A, B](t: List[A], f: A => B)(implicit m: Monoid[B]): B = 
    t.foldRight(m.zero)((a,b) => m.op(f(a), b))
}

object test {

  val x1 = List(1,2,3,4)

  val r1 = ListFodable.foldMap(x1, (x: Int) => x)(IntMonoid)
}
