import scalaz._

object ScalazTest {
  import Scalaz._
  //-----Functors and Lists-------------------------
  val p = Functor[List].lift((_:Int) + 1 )
  p(List(1,2,3,4,5))
  List(1,2,3,4,5) as "x"
  List(1,2,3,4,5).fpair

  List(1,2,3,4,5) map ((_:Int) + (_:Int)).curried

  List(1,2,3,4,5) <*> (List(2,4) <*> List(((_:Int) + (_:Int)).curried))

  //---------------Applicative style with Scalaz------------------------
  List("ha","ham","heh") <*> (List("!", "?", ".") <*> List(((_:String) + (_:String)).curried))
  (List("ha","ham","heh") |@| List("!", "?", ".") ) {_ + _}


  ("bien".successNel[String] |@| "bien 2".successNel[String]) {_ + _}
  "bien".successNel[String] <*> (" bien 2".successNel[String] <*> ((_:String) + (_:String)).curried.successNel)

  //-------------------Salaz Monad Transformers-------------------------
  type Error[A] =  \/[String, A]
  type OptionTEither[A] = OptionT[Error, A]
  //type OptionTEither[A] = OptionT[({type l[x] = \/[String, x]})#l, A]
  val f2 = (a:Int) => (a + 1).point[OptionTEither]
  val p2 = for {
    a <- 1.point[OptionTEither]
    c <- 2.point[Error].liftM[OptionT]
    b <- f2(a + c)
  } yield b
  p2.run

//---------------------Scalaz Kleisli Arrows-----------------------------
  val f = Kleisli[Option, Int, Int]((a) => Some(a + 1)) <=< Kleisli[Option, Int, Int]((b:Int) => Some(b * 2))
  f(4)

  val k = Kleisli[OptionTEither, Int, Int]((a:Int) => (a + 1).point[OptionTEither])
  val k2 = Kleisli[OptionTEither, Int, Int]((a:Int) => (a * a).point[OptionTEither])
  val k5 = Kleisli[OptionTEither, Int, Int]((a:Int) => (a * 2).point[OptionTEither])
  val k3 = k <=< k2 // kcompose
  val k4 = k >=> k2 // kandThen
  s"compose: ${k3(4).run}"
  s"andThen: ${k4(4).run}"
  s"""associativity law: k . (k2 . k5) = (k . k2) . k5.
     | ${val ktotal = k <=< (k2 <=< k5);ktotal(4).run} =
     | ${val ktotal = (k <=< k2) <=< k5; ktotal(4).run}
   """.stripMargin

  1 + 2 + 3 |> {_ * 6}

  4 |> k <=< k2 <=< k3 run


  //--------------Natural Transformation------------------------------
  trait F[+A]
  case class Bien[A](a:A) extends F[A]
  case object Mal extends F[Nothing]

  implicit val applicativeFInstance = new Monad[F] {
    def point[A](a: => A): F[A] = Bien(a)
    override def ap[A,B](fa: => F[A])(f: => F[A => B]): F[B] = fa match {
      case Bien(a) => f match {
        case Bien(g) => Bien(g(a))
        case Mal => Mal
      }
      case Mal => Mal
    }

    override def bind[A, B](fa: F[A])(f: (A) => F[B]) = fa match {
      case Bien(a) => f(a)
      case Mal => Mal
    }
  }

  val fun = Kleisli[F, Int, Int]((a:Int) => Bien(a + 1))
  val fun2 = Kleisli[F, Int, Int]((a:Int) => Bien(a * 2))
  val fun3 = Kleisli[F, Int, Int]((a:Int) => Bien(a - 1))
  val fun4 = Kleisli[F, Int, Int]((_:Int) => Mal)

  //Interpreter with Natural Transformation
  def inter = new (F ~> OptionTEither) {
    override def apply[A](fa: F[A]) = fa match {
      case Bien(a) => a.point[OptionTEither]
      case Mal => "error".left[A].liftM[OptionT]
    }
  }
  fun(4)
  inter(2 |> fun >=> fun2 >=> fun3).run
  inter(fun2(2)).run

  

}