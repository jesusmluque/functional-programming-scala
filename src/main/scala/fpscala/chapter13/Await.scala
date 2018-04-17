package fpscala.chapter13

/**
  * Created by jesus on 17/04/18.
  */
case class Await[I, O](
  recv: Option[I] => Process[I, O]
) extends Process[I, O]
