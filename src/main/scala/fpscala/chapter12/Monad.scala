package fpscala.chapter12

import scala.concurrent.Future

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](fa: F[A])(f: A => F[B]):F[B] = join(map(fa)(f))
  def join[A](ffa:F[F[A]]):F[A] = flatMap(ffa)(fa => fa)
  def compose[A,B,C](f1: A => F[B], f2: B => F[C]):(A => F[C]) = (a:A) => {
    flatMap(f1(a))(f2)
  }
}

object Implicits {

  /**
    * Implicit class to apply the monad methods without implicitly
    * With the Ops classes, as a convention, you can apply implicitly monad method in
    * directly in the F object
    * @param fa
    * @param ev$1
    * @tparam F
    * @tparam A
    */
  implicit class monadOps[F[_]: Monad,A](fa: F[A]) {
    def flatMap[B](f: A => F[B]): F[B] = implicitly[Monad[F]].flatMap(fa)(f)
    def map[B](f: A => B): F[B] = implicitly[Monad[F]].map(fa)(f)
    def unit[A](a: A):F[A] = implicitly[Monad[F]].unit(a)
    def >>=[B](f: A => B): F[B] = implicitly[Monad[F]].map(fa)(f)
    def bind[B](f: A => B): F[B] = implicitly[Monad[F]].map(fa)(f)
  }

}