sealed trait ST[S,A] { self =>
  def run(s: S): (A,S)
  def map[B](f: A => B): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }
  def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S,A](f: S => (A,S) ) = {
    new ST[S,A] {
      def run(s: S): (A,S)  = f(s)
    }
  }

  def unit[S, A](a: A): ST[S, A] =
    ST(s => (a, s))
  def get[S]: ST[S, S] = ST(s => (s, s))
  def set[S](s: S): ST[S, Unit] = ST(_ => ((), s))
  def modify[S](f: S => S): ST[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

trait RNG {
  def nextInt: (Int, RNG)
}

case class Simple(seed: Long) extends RNG{
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL 
    val nextRNG = Simple(newSeed) 
    val n = (newSeed >>> 16).toInt 
    (n, nextRNG) 
  }
}

object test {
  def rollDie: Int = {
    val rng = new scala.util.Random
    rng.nextInt(6)
  }
  
  type Rand[A] = ST[RNG, A]

  val int: Rand[Int] = ST(_.nextInt)

  def test1 = int.run(Simple(1))

  val posInt = int.map[Int]{ i:Int => if (i < 0) -(i + 1) else i }

  def test2 = {
    int.run(Simple(25214903928L))
    posInt.run(Simple(25214903928L))
  }

  def positiveLessThan(n: Int): Rand[Int] = posInt.flatMap {
    i => {
      val mod = i % n
      if (i + (n-1) - mod > 0) ST.unit(mod) else positiveLessThan(n)
    }
  }

  def test3 = positiveLessThan(6).run(Simple(1))

  def test4 = 
    for {
      x <- int
      y <- int
      z <- posInt
    } yield ((x , y , z))
}
