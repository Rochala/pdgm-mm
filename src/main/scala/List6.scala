object List6 extends App {
  def eachNElement[A](list: LazyList[A], n: Int, length: Int): LazyList[A] = {
    if (list.isEmpty) throw new IllegalArgumentException("Empty List")
    if (n < 1)
      throw new IllegalArgumentException(
        "Skip index has either zero or negative number"
      )
    if (length < 1)
      throw new IllegalArgumentException(
        "Length has either zero or negative number"
      )
    def helper(remList: LazyList[A], lengthCounter: Int): LazyList[A] = {
      (remList, lengthCounter) match {
        case (LazyList(), _) => LazyList()
        case (h #:: t, i) => {
          if (i == length) LazyList()
          else {
            if (i % n == 0) h #:: helper(t, i + 1)
            else helper(t, i + 1)
          }
        }
      }
    }
    helper(list, 0)
  }

  def lEquation(
      list1: LazyList[Double],
      list2: LazyList[Double],
      operation: Char
  ): LazyList[Double] = {
    def add(
        remList1: LazyList[Double],
        remList2: LazyList[Double]
    ): LazyList[Double] = {
      (remList1, remList2) match {
        case (LazyList(), l2)       => l2
        case (l1, LazyList())       => l1
        case (h1 #:: t1, h2 #:: t2) => (h1 + h2) #:: add(t1, t2)
      }
    }
    def sub(
        remList1: LazyList[Double],
        remList2: LazyList[Double]
    ): LazyList[Double] = {
      (remList1, remList2) match {
        case (LazyList(), l2)       => l2
        case (l1, LazyList())       => l1
        case (h1 #:: t1, h2 #:: t2) => (h1 + h2) #:: add(t1, t2)
      }
    }
    def mul(
        remList1: LazyList[Double],
        remList2: LazyList[Double]
    ): LazyList[Double] = {
      (remList1, remList2) match {
        case (LazyList(), l2)       => l2
        case (l1, LazyList())       => l1
        case (h1 #:: t1, h2 #:: t2) => (h1 + h2) #:: add(t1, t2)
      }
    }
    def div(
        remList1: LazyList[Double],
        remList2: LazyList[Double]
    ): LazyList[Double] = {
      (remList1, remList2) match {
        case (LazyList(), l2) => l2
        case (l1, LazyList()) => l1
        case (h1 #:: t1, h2 #:: t2) =>
          (h1 / (if (h2 == 0.0)
                   throw new IllegalArgumentException("Dividing by 0")
                 else h2)) #:: lEquation(t1, t2, operation)
      }
    }
    operation match {
      case '+' => add(list1, list2)
      case '-' => sub(list1, list2)
      case '*' => mul(list1, list2)
      case '/' => div(list1, list2)
      case _ =>
        throw new UnsupportedOperationException("Only basic math implemented")
    }
  }

  println(eachNElement(LazyList(2, 3, 4, 5, 6, 7), 2, 6).force)
  println(lEquation(LazyList(2, 3, 4), LazyList(3, 4), '-').force)
  println(lEquation(LazyList(2, 3, 4), LazyList(3, 4), '/').force)
  println(lEquation(LazyList(2, 3, 4), LazyList(3, 4), '*').force)
  println(lEquation(LazyList(2, 3, 4), LazyList(3, 4), '+').force)

}
