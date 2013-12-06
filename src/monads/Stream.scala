sealed abstract class Stream[+A]
object empty extends Stream[Nothing]
sealed abstract class cons[+A] extends Stream[A]

import  scala.collection.immutable.Stream._

object test {
  val test1 = List(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).map(_ * 3) // iterate 3 times

  val test2 = false && { println("!!"); true } 

  val test3 = true || { println("!!"); false } 

  val input = List(1,2)

  val test4 =  if (input.isEmpty) sys.error("empty input") else input

  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse
    
  def test0 = if2(false, sys.error("fail"), 3)

  def maybeTwice(b: Boolean, i: => Int) = if (b) i+i else 0

  val x = maybeTwice(true, { println("hi"); 1+41 })

  def maybeTwice2(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j+j else 0
  }

  val test5 = Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0) // iterate 1 time

//def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A]

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  val fibsViaUnfold: Stream[Int] = cons(0, 
    unfold((0,1)) { case (f0,f1) => Some((f1,(f1,f0+f1))) })

  def test6 = fibsViaUnfold.take(5).toList

  def test = {
    val a = Stream(1,2)
    val b = Stream(3,4)
    for {
      t1 <- a
      t2 <- b
    } yield ((t1,t2))
  }
}
