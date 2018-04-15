import scalaz.{Monad, OptionT, Scalaz, \/}

object mtl {
  import Scalaz._
  trait MiMonadError[F[_], E] {
    def monad: Monad[F]

    def raiseError[A](e: E):F[A]
    def handleErrorWith[A](fa: F[A])(f: A => F[A]):F[A]
  }

  trait MiMonadState[F[_], S] {
    def monad: Monad[F]

    def get: F[S]
    def set(s: S): F[Unit]
  }

  def program[F[_]](implicit F0: MiMonadError[F, String],
                    F1: MiMonadState[F, Int]): F[Int] = {
    implicit val monad = F0.monad
    monad.bind(F1.get) { i =>
      F0.raiseError[Int]("fail")
    }
  }

  type Error[A] =  \/[String, A]
  type OptionTEither[A] = OptionT[Error, A]

  implicit val optionTEither = new MiMonadError[OptionTEither, String] {
    override def raiseError[A](e: String): OptionTEither[A] = e.left[A].liftM[OptionT]

    override def handleErrorWith[A](fa: OptionTEither[A])(f: (A) => OptionTEither[A]): OptionTEither[A] = fa.flatMap(f)

    override def monad: Monad[OptionTEither] = Monad[OptionTEither]
  }
  //implicit def miMonadErrorIsAMonad[F[_,_]: MiMonadError, E]:Monad[F] = implicitly[MiMonadError[F, E]].monad

  program[OptionTEither]

}