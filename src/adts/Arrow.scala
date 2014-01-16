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
  def right[A,B,C](f: A ~> B): (Either[C, A] ~> Either[C,B]) = {
    def swap[X,Y] = arr[Either[X, Y], Either[Y, X]] {
        case Left(x) => Right(x)
        case Right(y) => Left(y)
    }
    compose(swap, compose(left[A, B, C](f), swap))
  }
  def multiplex[A,B,C,D](f: A ~> B, g: C ~> D)
    : (Either[A,C] ~> Either[B,D]) = 
    compose(right[C, D, B](g), left[A, B, C](f))
  def merge[A,B,C](f: A ~> C, g: B ~> C)
    : (Either[A,B] ~> C) = {
    def utag[D](v: Either[D, D]): D = v match {
      case Left(x) => x
      case Right(y) => y
    }
    compose(arr[Either[C,C],C](utag[C]), multiplex(f,g))
  }
}

trait ArrowApply[~>[_, _]] extends Arrow[~>] {
  def app[B, C]: (B ~> C, B) ~> C
}

abstract class Functor[F[_], A](value: F[A]) {
  def map_: [B](f: A => B): F[B]
}

abstract class Applicative[F[_], A](o: F[A]) extends Functor[F, A](o) {
  def unit[C](a: C): F[C]
  def ap_: [B](f: F[A => B]): F[B] 

  override def map_: [B](f: A => B): F[B] =
    ap_:(unit(f))
}

implicit def Stream2Applicative[A](o: Stream[A]): Applicative[Stream, A] = new Applicative[Stream, A](o) {
  
  def unit[C](a: C): Stream[C] = Stream.continually(a)
  def ap_: [B](f: Stream[A => B]): Stream[B] = {
    o.zip(f).map { x => (x._2(x._1)) }
  }
}

implicit def Option2Applicative[A](o: Option[A]): Applicative[Option, A] = new Applicative[Option, A](o) {
  
  def unit[C](a: C): Option[C] = Some(a)
  def ap_: [B](f: Option[A => B]): Option[B] = {
    f.flatMap {
      t1 => o.flatMap {
        t2 => unit(t1(t2))
      }
    }
  }
}

import scala.language.higherKinds

val FuncArrow = new Arrow[Function1] {
  def arr[A, B](f: A => B): Function1[A, B] = f
  def first[A,B,C](f: A => B): (((A, C)) => (B, C)) =
    (ac: (A, C)) => (f(ac._1), ac._2)

  def id[A]: A => A = (a: A) => a
  def compose[A, B, C](f: B => C, g: A => B): (A => C) =
    (a: A) => f(g(a))
}

object simpleTest {
  def twoVarFunc(x: (Int,Int)): Int = {
    x._1 + x._2
  }
 
  def eleven (x: Any): Int = {11} 

  def twelve (x: Any): Int = {12}

  val test = FuncArrow.andThen(
                FuncArrow.combine(
                  FuncArrow.arr(eleven),  
                  FuncArrow.arr(twelve)
                ), 
                FuncArrow.arr[(Int, Int), Int](twoVarFunc)
              )

  import scala.util._
  val res = test.apply(new Random(System.currentTimeMillis).nextDouble)
}


object test {
  def twoVarFunc: ((Stream[Int], Stream[Int])) => Stream[Int] = n => {
    (n._1).zip(n._2).map { n  => {
      ( n._1 < n._2 ) match { 
        case true => n._1 
        case false => n._2
      }
    }}
  }

  def twoVarFunc2: ((Option[Int], Option[Int])) => Option[Int] = n => {
    n._1 match { 
      case Some(a) => {
        n._2 match { 
          case Some(b) => {
            ( a < b ) match { 
              case true => Some(a)
              case false => Some(b)
            }
          }
          case None => None
        }
      }
      case None => None
    }
  }

  def threeVarFunc: (Int, Int, Int) => Int = {
    _ + _ + _
  }

  val x1: Stream[Int] = {
    def loop(v: Int): Stream[Int] = v #:: loop(v + 1000)
      loop(0)
  }

  val x2: Stream[Int] = 0 #:: 1 #:: x2.zip(x2.tail).map { n => n._1 + n._2 }


  val x3: Stream[Int] = {
    def loop(v: Int): Stream[Int] = v #:: loop(v * 2)
      loop(1)
  }

  val x4 = Option(1)
  val x5 = Option(2)
  val x6 = Option(3)

  val test1 =  ((threeVarFunc.curried map_: x1) ap_: x2) ap_: x3

  val test2 = FuncArrow.andThen(
                FuncArrow.combine[Stream[Int], Stream[Int], Stream[Int]](
                  FuncArrow.arr( (x: Any) => { x1 } ), 
                  FuncArrow.arr( (x: Any) => { x3 } )
                ), 
                FuncArrow.arr[(Stream[Int], Stream[Int]), Stream[Int]](twoVarFunc)
              )(Stream(0))

  val test3 = for {
    r1 <- x1
    r2 <- x2
    r3 <- x3
  } yield { (r1,r2,r3) }

  val test4 = ((threeVarFunc.curried map_: x4) ap_: x5) ap_: x6

  val test5 = FuncArrow.andThen(
                FuncArrow.combine[Option[Int], Option[Int], Option[Int]](
                  FuncArrow.arr( (x: Any) => { x5 } ), 
                  FuncArrow.arr( (x: Any) => { x6 } )
                ), 
                FuncArrow.arr[(Option[Int], Option[Int]), Option[Int]](twoVarFunc2)
              )(Option(0))

  val test6  =  {
    for {
      r1 <- x4
      r2 <- x5
    } yield {
      if(r1 == 1) {
        r1 * r2
      } else {
        r1 + r2
      }
    }
  }
}
