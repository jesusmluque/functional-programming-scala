package fpscala.chapter13

/**
  * Created by jesus on 29/01/17.
  */
trait Console[A] {
  def toThunk: () => A
}

object Console {
  type ConsoleIO[A] = Free[Console, A]
  def readLn: ConsoleIO[Option[String]] =
    Suspend(ReadLine)
  def printLn(line: String):ConsoleIO[Unit] =
    Suspend(PrintLine(line))
}