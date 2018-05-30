package fpscala.chapter12

/**
  * Created by jesus on 11/05/18.
  */
trait ProFunctor[F[-_, +_]] {
  def dimap[A, B, C, D](f: A => B, g: C => D)(fab: F[B,C]):F[A,D]
  def lmap[A, B, C, D](f: A => B)(fab: F[B,C]):F[A,C] = dimap(f, (c:C) => c)(fab)
  def rmap[A, B, C, D](g: C => D)(fab: F[B,C]):F[B,D] = dimap((b:B) => b, g)(fab)
}

object ImplicitsPro {
  implicit class proFunctorOpts[F[-_,+_]: ProFunctor, B, C](fab: F[B,C]) {
    def dimap[A, D](f: A => B, g: C => D):F[A,D] = implicitly[ProFunctor[F]].dimap(f, g)(fab)
    def lmap[A, D](f: A => B):F[A,C] = implicitly[ProFunctor[F]].lmap(f)(fab)
    def rmap[A, D](g: C => D):F[B,D] = implicitly[ProFunctor[F]].rmap(g)(fab)
  }
}
