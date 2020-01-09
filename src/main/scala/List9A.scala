import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

import math.log
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object List9 extends App {
  private val GLOBAL_THREAD_LIMIT =
    math.pow(2, (log(Runtime.getRuntime().availableProcessors()) / log(2)).floor.toInt).toInt

  private lazy implicit val executionContext =
    ExecutionContext.fromExecutorService(
      Executors.newFixedThreadPool(GLOBAL_THREAD_LIMIT)
    )

  var normal = 0.0
  var paralell = 0.0
  var sorted = 0.0

  def mergeSort[T](inputList: List[T])(implicit ev$1: T => Ordered[T]): List[T] = {
    val n = inputList.length / 2
    if (n == 0) inputList
    else {
      val (left, right) = inputList.splitAt(n)
      merge(mergeSort(left), mergeSort(right)).reverse
    }
  }

  @scala.annotation.tailrec
  def merge[T](fl: List[T], sl: List[T], acc: List[T] = List())(implicit ev$1: T => Ordered[T]): List[T] = {
    (fl, sl) match {
      case (Nil, _) => sl.reverse ::: acc
      case (_, Nil) => fl.reverse ::: acc
      case (fh :: ft, sh :: st) =>
        if (fh.compare(sh) < 0) merge(ft, sl, fh :: acc)
        else merge(fl, st, sh :: acc)
    }
  }


  def mergeSortConc[T](inputList: List[T], threads: Int)(implicit ev$1: T => Ordered[T]): List[T] = {
    val l = (log(GLOBAL_THREAD_LIMIT - 1) / log(2)).floor.asInstanceOf[Int]
    if (threads <= l) {
      var x = inputList.splitAt(inputList.length / 2)
      val result = Future[List[T]] {
        mergeSortConc(x._2, threads + 1)
      }
      x = (mergeSortConc(x._1, threads + 1), Await.result(result, 10.second))
      merge(x._1, x._2).reverse
    } else mergeSort(inputList) //dziel
  }

  def mergeSortConc[T](inputList: List[T])(implicit ev$1: T => Ordered[T]): List[T] =
    mergeSortConc(inputList, 0)

  def test() = {
    for (i <- 1 to 20)
      mergeSort(Seq.fill(1000000)(util.Random.nextInt(100000)).toList)
    
    for (i <- 1 to 100) {
      val list = Seq.fill(1000000)(util.Random.nextInt(100000)).toList
      var x = System.nanoTime()
      println(s"Sorting list: $i")
      mergeSort(list)
      normal += (System.nanoTime() - x) * 0.000000001
      x = System.nanoTime()
      mergeSortConc(list)
      paralell += (System.nanoTime() - x) * 0.000000001
      x = System.nanoTime()
      list.sorted
      sorted += (System.nanoTime() - x) * 0.000000001
    }
    normal /= 100
    paralell /= 100
    sorted /= 100
    println(s"Normal: $normal")
    println(s"Paralell: $paralell")
    println(s"Sorted: $sorted")
    println("Performance gain: " + ((normal / paralell) * 100 - 100).toInt + "%")
  }

  test()
  System.exit(0)
}