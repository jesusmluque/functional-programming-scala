package fpscala.chapter13

/**
  * Created by jesus on 16/04/18.
  */
case object ReadLine extends Console[Option[String]] {
  override def toThunk = () => run
  def run: Option[String] =
    Some(readLine())
}
