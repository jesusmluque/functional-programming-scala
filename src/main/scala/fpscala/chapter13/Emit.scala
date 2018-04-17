package fpscala.chapter13

/**
  * Created by jesus on 17/04/18.
  */
case class Emit[I, O](
  head: O,
  tail: Process[I, O] = Halt[I, O]()
) extends Process[I, O]
