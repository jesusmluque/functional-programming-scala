package fpscala.chapter13

/**
  * Created by jesus on 16/04/18.
  */
case class FlatMap[F[_], A, B](sub: Free[F, A], f: A => Free[F, B]) extends Free[F,B]
