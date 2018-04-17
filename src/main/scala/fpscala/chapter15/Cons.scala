package fpscala.chapter15

/**
  * Created by jesus on 17/04/18.
  */
case class Cons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]
