package xs.utils

import scala.math.{ceil, log, pow}

object ParallelOperationN {
  def apply[T](xs: Seq[T], N:Int, func: Seq[T] => T): T = {
    require(xs.nonEmpty)
    if(xs.length == 1) {
      xs.head
    } else if(xs.length <= N) {
      func(xs)
    } else {
      val level = ceil(log(xs.length) / log(N)).round.toInt
      val groupSize = ceil(pow(xs.length, 1.0 / level)).round.toInt
      val results = xs.grouped(groupSize).map(func).toSeq
      apply(results, N, func)
    }
  }
}