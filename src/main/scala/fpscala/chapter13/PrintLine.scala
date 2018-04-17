package fpscala.chapter13

/**
  * Created by jesus on 16/04/18.
  */
case class PrintLine(line: String) extends Console[Unit] {
  override def toThunk = () => println(line)
}
