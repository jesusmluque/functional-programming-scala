package fpscala.chapter11

import fpscala.chapter12.Functor

/**
  * Created by jesus on 9/05/18.
  */
trait Comonad[F[_]] extends Functor[F]{
  def extract[A](a: F[A]):A
  def duplicate[A](a: F[A]):F[F[A]] = extend ((a:F[A]) => a) (a)
  def extend[A, B](g: F[A] => B)(fa: F[A]):F[B] = {
    val f1:F[F[A]] => F[B] = map(_)(g)
    (f1 compose duplicate)(fa)
  }
  def coCompose[A, B, C](g: F[A] => B, h: F[B] => C):F[A] => C = (a:F[A]) => h(extend(g)(a))

  override def map[A, B](fa: F[A])(f: (A) => B) = {
    val g:F[A] => B = f compose extract
    extend(g)(fa)
  }
}

object ImplicitsCo {
  implicit class comonadOpts[F[_]: Comonad, A](fa: F[A]) {
    def duplicate:F[F[A]] = implicitly[Comonad[F]].duplicate(fa)
    def extend[B](g: F[A] => B):F[B] = implicitly[Comonad[F]].extend(g)(fa)
    def =>>[B](g: F[A] => B):F[B] = implicitly[Comonad[F]].extend(g)(fa)
  }
  implicit class coKleisliOpts[F[_]: Comonad, A, B](c: F[A] => B) {
    def <<=(fa: F[A]):F[B] = implicitly[Comonad[F]].extend(c)(fa)
    def =<=[C](h: F[B] => C):F[A] => C = implicitly[Comonad[F]].coCompose(c, h)
  }
}
