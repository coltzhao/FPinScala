trait Semigroup[A] {
    def op(a: A, b: A): A
}

trait Monoid[A] extends Semigroup[A] {
    val zero: A
}

trait Foldable[F[_]]  {
    def foldMap[A, M: Monoid](t: F[A], f: A => M): M

    /*
      def fold[M: Monoid](t: F[M]): M // also called reduce with variance
        def foldRight[A, B](t: F[A], z: => B, f: (A, B) => B): B 
          def foldLeft[A, B](t: F[A], z: B, f: (B, A) => B): B
            def foldr1[A, B](t: F[A], f: (A, => A) => A): Option[A]
              def foldl1[A, B](t: F[A], f: (A, A) => A): Option[A]
              */
}

