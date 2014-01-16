trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(la => la)

  override def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
}


trait Comonad[F[_]] extends Functor[F] {
  def extract[A](a: F[A]): A
  def extend[A,B](la: F[A])(f: F[A] => B): F[B] 
//    = map(duplicate(la))(f)
  def duplicate[A](ma: F[A]): F[F[A]] = 
    extend(ma)(mma => mma)
  
  override def map[A,B](fa: F[A])(f: A => B): F[B] = 
    extend(fa)((w: F[A]) => f(extract(w)))
}

import scala.language.higherKinds 

val StreamComonad = new Comonad[Stream] {
  def extract[A](a: Stream[A]): A = a.head
  override def duplicate[A](ma: Stream[A]): Stream[Stream[A]] = {
    def rec(remain: Stream[A], last: Stream[A]):Stream[Stream[A]] = {
      val cur = last :+ remain.head 
      cur #:: rec(remain.tail, cur)  
    }
    Stream(ma.head) #:: rec(ma.tail, Stream(ma.head))
  }
  def extend[A,B](la: Stream[A])(f: Stream[A] => B): Stream[B] = {
      duplicate(la).map(f)
  }
}

object test {
  def average(i: Stream[Int]): Double = {
    val res = i.foldLeft((0,0d))( (b,a) => (b._1 + a, b._2 + 1) )
    res._1.toDouble / res._2.toDouble
  }

  val fibs:  Stream[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }

  val test1 = StreamComonad.extend[Int, Double](fibs)(average)

}

