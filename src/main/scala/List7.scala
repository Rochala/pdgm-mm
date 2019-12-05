import scala.collection.mutable.LinkedHashSet

object List7 extends App {
  def repeatList[A](inputList: List[A], repeatCounter: List[Int]): List[A] = {
    if (repeatCounter.exists(y => y < 0))
      throw new IllegalArgumentException("U cant repeat 0 times") //tak sobie zalozylem / gdyby jednak Pan mial na mysli inaczej to wystarczy zmienic tutaj na <= i 3 case minimalnie edytowac
    if (inputList.length > repeatCounter.length) 
      throw new IllegalArgumentException("Input list cant be longer than repeat counter list")
      (inputList, repeatCounter) match {
      case (Nil, _)  => Nil
      case (_, Nil) => Nil
      case (h :: t, hr :: tr) =>
        h :: (if (hr == 1) repeatList(t, tr)
              else repeatList(inputList, hr - 1 :: tr))
    }
  }

  def repeatListWithoutDuplicates[A](inputSet: LinkedHashSet[A],repeatCounter: List[Int]): List[A] = {
    if (repeatCounter.exists(y => y < 0))
      throw new IllegalArgumentException("U cant repeat 0 times") //tak sobie zalozylem / gdyby jednak Pan mial na mysli inaczej to wystarczy zmienic tutaj na <= i 3 case minimalnie edytowac
    if (inputSet.size > repeatCounter.length) 
      throw new IllegalArgumentException("Input list cant be longer than")
      if (inputSet.isEmpty) Nil
      else if (repeatCounter.isEmpty) Nil
    else inputSet.head :: (if (repeatCounter.head == 1)repeatListWithoutDuplicates(inputSet.tail,repeatCounter.tail)
                        else repeatListWithoutDuplicates(inputSet, repeatCounter.head - 1 :: repeatCounter.tail))
  }

  println(repeatList(List(1, 2, 3), List(2, 4, 6)))
  println(
    repeatListWithoutDuplicates(
      LinkedHashSet(2, 3, 4, 6, 8),
      List(2, 2, 2, 2, 2)
    )
  )
}
