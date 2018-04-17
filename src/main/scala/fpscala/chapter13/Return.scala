package fpscala.chapter13

/**
  * Created by jesus on 16/04/18.
  */
case class Return[F[_], A](a: A) extends Free[F,A]
