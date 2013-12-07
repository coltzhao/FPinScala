trait Future[+T]
when complete return a Try[T]


import scala.concurrent._
import ExecutionContext.Implicits.global

object test {

  def xFut: Future[Int] = future {
    Thread.sleep(10000);
    println("x happened");
    10  
  }.flatMap(i => Future.successful(i + 1)) 

  def yFut(a: Int) : Future[Int] = future {
    println("y begin")
    Thread.sleep(6000);
    println("y happened " + a); 
    20  
  }   

  def zFut(a: Int): Future[Int] = future {
    println("z begin")
    Thread.sleep(5000);
    println("z hapenned " + a); 
    30  
  }  

  def af: Int => Future[(Int, Int)] = a => {
    val yf = yFut(a)
    val zf = zFut(a)

    for {
      y <- yf
      z <- zf
    } yield ((y,z))
  }

  def test = {
    val xf = xFut
 
    val result: Future[(Int, (Int, Int))] = for {
      x <- xf
      a <- af(x)
    } yield (x, a)

    Thread.sleep(20000)
    println("\nThe end")
  }

  val p = promise[Int]
  val f = p.future

  def producer = {
    println("Begin producer")
    Thread.sleep(3000)
    val r = 100 //produce an Int
    p success r
    println("end producer")
  }

  def consumer = {
    println("Begin consumer")
    f onSuccess {
      case r => println("receive product: " + r.toString)
    }
    println("end consumer")
  }

  def test2 = {
    consumer
    producer
  } 
}
