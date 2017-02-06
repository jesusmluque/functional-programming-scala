package fpscala.chapter12

/**
  * Created by jesus on 29/01/17.
  */
sealed trait Console[A] {
  def toThunk: () => A
}

case object ReadLine extends Console[Option[String]] {
  override def toThunk = () => run
  def run: Option[String] =
    Some(readLine())
}
case class PrintLine(line: String) extends Console[Unit] {
  override def toThunk = () => println(line)
}
object Console {
  type ConsoleIO[A] = Free[Console, A]
  def readLn: ConsoleIO[Option[String]] =
    Suspend(ReadLine)
  def printLn(line: String):ConsoleIO[Unit] =
    Suspend(PrintLine(line))
}



