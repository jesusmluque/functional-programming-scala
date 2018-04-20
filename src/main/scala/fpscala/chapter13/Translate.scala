package fpscala.chapter13

/**
  * Created by jesus on 29/01/17.
  */

trait Translate[F[_], G[_]] {
  def apply[A](f: F[A]):G[A]

}

object Translate {
  type ~>[F[_], G[_]] = Translate[F,G]
}