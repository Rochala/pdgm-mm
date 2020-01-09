import java.util.concurrent.Executors

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.math.log

object MatrixMult extends App{
  private val GLOBAL_THREAD_LIMIT = Runtime.getRuntime().availableProcessors()

  private lazy implicit val executionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(GLOBAL_THREAD_LIMIT))

  def mulMatrixConc(m1: Array[Array[Int]], m2: Array[Array[Int]]): Array[Array[Int]] = {
    if (m1(0).length != m2.length) throw new IllegalArgumentException("Cant multiplicate those 2 matrices")
    val x: Array[Array[Int]] = Array.ofDim[Int](m1.length, m2(0).length)
    if (m1.length < 50) {
      mulPartMatrix(m1, m2, x, 0, m1.length - 1)
      x
    } else {
      var workingSize: Int = 0
      val division: Int = m1.length / 4
      val futures: Array[Future[Unit]] = Array.ofDim(3)
      for (i <- 0 to 2) {
        futures(i) = createFuture(m1, m2, x, workingSize, workingSize + division)
        workingSize += division
      }
      val result = for { // w celu oczekiwania na 3 naraz
        r1 <- futures(0)
        r2 <- futures(1)
        r3 <- futures(2)
      } yield (r1, r2, r3)
      mulPartMatrix(m1, m2, x, workingSize, m1.length - 1)
      Await.ready(result, 15.second)
      x
    }
  }

  def multMatrix(m1: Array[Array[Int]], m2: Array[Array[Int]]):Array[Array[Int]] = {
    val x: Array[Array[Int]] = Array.ofDim[Int](m1.length, m2(0).length)
    mulPartMatrix(m1,m2,x,0,arr2(0).length-1)
    x
  }

  def createFuture(m1: Array[Array[Int]], m2: Array[Array[Int]], result: Array[Array[Int]], start:Int, end: Int): Future[Unit] = {
    Future{
      println ("starting thread")
      mulPartMatrix(m1,m2,result,start,end)
    } //tworzy nowy futur ktory zwraca unit i uzupelnia result o wymnozone wartosci
  }

  def mulPartMatrix(m1: Array[Array[Int]], m2: Array[Array[Int]], result: Array[Array[Int]], start:Int, end:Int): Unit = {
    for (i <- start to end) {
      for (j <- m2(0).indices) {
        var x = 0
        for ( l <- m2(0).indices) {
          x += m1(i)(l) * m2(l)(j)
        }
        result(i)(j) = x
    }
  }
    }

  var time = 0.0
  var normal = 0.0
  var paralell = 0.0
  val size = 500
  val arr1 = Array.ofDim[Int](size,size)
  val arr2 = Array.ofDim[Int](size,size)
  for (i <- 1 to 20) {
    println(s"Calculating $i")
    for (i <- 0 until size; j <- 0 until size) {
      arr1(i)(j) = util.Random.nextInt(10)
    }
    for (i <- 0 until size; j <- 0 until size) {
      arr2(i)(j) = util.Random.nextInt(10)
    }
    time = System.nanoTime()
    multMatrix(arr1, arr2)
    normal += (System.nanoTime() - time) * 0.000000001
    time = System.nanoTime()
    mulMatrixConc(arr1, arr2)
    paralell += (System.nanoTime() - time) * 0.000000001
  }
  normal /= 100
  paralell /= 100
  println(s"Normal $normal")
  println(s"Paralell $paralell")

  System.exit(0)
}
