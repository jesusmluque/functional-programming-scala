package fpscala.chapter13

/**
  * Created by jesus on 16/04/18.
  */
case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
