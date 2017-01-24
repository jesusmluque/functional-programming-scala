package fpscala.chapter12

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](fa: F[A])(f: A => F[B]):F[B] = join(map(fa)(f))
  def join[A](ffa:F[F[A]]):F[A] = flatMap(ffa)(fa => fa)
  def compose[A,B,C](f1: A => F[B], f2: B => F[C]):(A => F[C]) = (a:A) => {
    flatMap(f1(a))(f2)
  }
  
}