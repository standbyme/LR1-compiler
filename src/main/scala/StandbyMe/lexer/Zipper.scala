package StandbyMe.lexer

class Zipper[T](arg_past: List[T], arg_now: T, arg_future: List[T]) {
  val past:List[T] = arg_past
  val now:T = arg_now
  val future:List[T] = arg_future
  lazy val forward: Option[Zipper[T]] =
    if (future.isEmpty) None
    else Some(new Zipper[T](now :: past, future.head, future.tail))
}

object Zipper {
  def apply[T](list: List[T]): Zipper[T] = {
    if (list.isEmpty) throw new Exception("Zipper get a empty list")
    else new Zipper(Nil, list.head, list.tail)
  }
}
