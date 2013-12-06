sealed trait Option[+A]
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Try[+T] 
case class Failure[+T](exception: Throwable) extends Try[T]
case class Success[+T](value: T) extends Try[T]

sealed trait Either[+E,+A] 
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

import scala.util.{Try, Failure, Success}

object test {
  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty) 
      throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length

  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length

  def mean_option(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def mean_try(xs: Seq[Double]): Try[Double] =
    if (xs.isEmpty) Failure(new ArithmeticException("mean of empty list!")) 
    else Success(xs.sum / xs.length)

  def mean_either(xs: Seq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("Why gave me a empty Seq and ask me to get mean?") 
    else Right(xs.sum / xs.length)

  def test = for {
    x <- mean_option(Seq(1,2,3,4,5))
    y <- mean_option(Seq(4,5))
    z <- mean_option(Seq())
  } yield (x+y+z)

  def test1 = for {
    x <- mean_either(Seq(1,2,3,4,5)).right
    y <- mean_either(Seq(4,5)).right
    z <- mean_either(Seq()).right
  } yield (x+y+z)

  def test2 = for {
    x <- mean_either(Seq(1,2,3,4,5)).left
    y <- mean_either(Seq(4,5)).left
    z <- mean_either(Seq()).left
  } yield (x+y+z)
}
