object test {
  def oneVarFunc: Int => Int = {
    _ + 1
  }

  def twoVarFunc: (Int, Int) => Int = {
    _ + _
  }

  def optionap[A,B](a: Option[A])(f: Option[A => B]): Option[B] =
    f.flatMap(t1 => a.flatMap(t2 => Some(t1(t2))))

  val x1 = Some(1)

  val x2 = Some(2)

  val x3 = None

  //functor
  def test1 = 
    x1.map(oneVarFunc)

  //Applicative Functor
  def test2 = {
    optionap(x2)(x1.map(twoVarFunc.curried))
  }

  def test2_1 = {
    optionap(x3)(x2.map(twoVarFunc.curried))
  }

  //Monad
  def test3 = {
    for {
      r1 <- x1
      r2 <- x2
    } yield {
      if(r1 == 1) {
        r1 * r2
      } else {
        r1 + r2
      }
    }
  }
}
