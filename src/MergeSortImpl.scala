import scala.annotation.tailrec

object MergeSortImpl {
  
  def msort[T <% Ordered[T]](xs: List[T]): List[T] = {

    @tailrec
    def merge(res: List[T], xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (_, Nil) => res.reverse ::: xs
      case (Nil, _) => res.reverse ::: ys
      case (x :: xs1, y :: ys1) =>
        if (x < y) merge(x :: res, xs1, ys)
        else merge(y :: res, xs, ys1)
    }

    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs splitAt n
      merge(Nil, msort(ys), msort(zs))
    }
  }
  
  def main(args: Array[String]): Unit = {
    val list = List(23,52,152,2,45,25,2)
    println(msort(list))
  }
}