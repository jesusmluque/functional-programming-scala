

object Example4 {

  //Examples: Typeclass and syntax, function composition and effects
  //Typeclass
  trait Monad[F[_]] {
    def pure[A](a: A):F[A]
    def map[A,B](fa: F[A])(f: A => B):F[B]
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
    def kcompose[A,B,C](f: A => F[B], g: B => F[C]):A => F[C] =
      a => flatMap(f(a))(g)
  }
  implicit class monadOps[F[_]: Monad,A](fa: F[A]) {
    def flatMap[B](f: A => F[B]): F[B] = implicitly[Monad[F]].flatMap(fa)(f)
    def map[B](f: A => B): F[B] = implicitly[Monad[F]].map(fa)(f)

  }
  implicit class monadKleisliOps[F[_]: Monad, A, B](f: A => F[B]) {
    def >=>[C](g: B => F[C]):A => F[C] =
      implicitly[Monad[F]].kcompose(f,g)
  }
  object Monad {
    def pure[F[_]: Monad, A](a: A):F[A] = implicitly[Monad[F]].pure(a)
  }
  trait Eq[A] {
    def eq(a: A, b: A):Boolean
    def gt(a: A, b: A):Boolean
    def lt(a: A, b: A):Boolean
  }
  implicit class EqOps[A: Eq](a: A) {
    def ===(b:A):Boolean = implicitly[Eq[A]].eq(a,b)
    def >(b:A):Boolean = implicitly[Eq[A]].gt(a,b)
    def <(b:A):Boolean = implicitly[Eq[A]].lt(a,b)
  }
  trait Console[F[_]] {
    def putStrLn(str: String): F[Unit]
    def getStrLn: F[String]
  }

  def putStrLn[F[_]: Console](str: String) = implicitly[Console[F]].putStrLn(str)
  def getStrLn[F[_]: Console] = implicitly[Console[F]].getStrLn


  //Functions
  def qsort[F[_]: Monad: Console, A: Eq](l: List[A]):F[List[A]] = {
    if (l.length < 2)
      Monad.pure(l)
    else {
      val pivot = l(l.length / 2)
      for {
        left <- qsort(l.filter(_ < pivot))
        right <- qsort(l.filter(_ > pivot))
        _ <- putStrLn(s"recursion con pivote ${pivot}")
      } yield left ++ List(pivot) ++ right

    }
  }
  def search[F[_]: Monad: Console, A: Eq](elem: A)(l: List[A]):F[Boolean] = {

    for {
      _ <- putStrLn("hola")
      res <-  if (l.length < 1)
                Monad.pure(false)
              else if (l.length < 2 && l.head === elem)
                Monad.pure(true)
              else if (l.length < 2 && !(l.head === elem))
                Monad.pure(false)
              else if (l(l.length / 2) < elem)
                search(elem)(l.splitAt(l.length / 2)._2)
              else if (l(l.length / 2) > elem)
                search(elem)(l.splitAt(l.length / 2)._1)
              else if (l(l.length / 2) === elem)
                Monad.pure(true)
              else
                Monad.pure(false)
      _ <- putStrLn(s"el resultado es: ${res}")
    } yield res
    }


  //Function Composition
  def existElement[F[_]: Monad: Console, A: Eq](elem: A, l: List[A]):F[Boolean] = for {
    res <- (qsort[F, A] _ >=> search[F, A](elem)).apply(l)
    _   <- putStrLn(s"Resultado = ${res}")
  } yield res


  //Integer typeclass instance
  implicit val eqInt = new Eq[Int] {
    override def eq(a: Int, b: Int) = a == b

    override def gt(a: Int, b: Int) = a > b

    override def lt(a: Int, b: Int) = a < b
  }
  case class IO[A](performEff: () => A)
  object IO {
    def pure[A](a: A): IO[A] = IO(() => a)
    implicit val ioMonad = new Monad[IO] {
      override def pure[A](a: A): IO[A] = IO.pure(a)

      override def map[A, B](fa: IO[A])(f: A => B): IO[B] = IO(() => f(fa.performEff()))

      override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = IO(() => f(fa.performEff()).performEff())
    }
    implicit val consoleIO = new Console[IO] {
      override def putStrLn(str: String): IO[Unit] = IO(() => println(str))

      override def getStrLn: IO[String] = IO(() => readLine())
    }

  }

  qsort[IO, Int](List(5,3,4,2,1,6,8,7,9)).performEff()
  existElement[IO, Int](11, List(5,3,4,2,1,6,8,7,9)).performEff()
  existElement[IO, Int](4, List(5,3,4,2,1,6,8,7,9)).performEff()


  case object identity {
    def apply[A](a:A) = a
  }
  identity("hola")
  identity(1)


}