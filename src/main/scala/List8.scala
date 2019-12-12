trait Debug {
  def debugName(): String
  def debugVar(): Unit
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"

  def debugName(): String = "Class name: " + getClass().getName()
  def debugVar(): Unit = {
    def helper(list: Array[(String, String, String)]): Unit = {
      if (!list.isEmpty) {
        println(
          "Field: " + list.head._1 + " => " + list.head._2 + ", " + list.head._3
        )
        helper(list.tail)
      }
    }
    helper(fieldArray())
  }

  def fieldArray(): Array[(String, String, String)] = {
    getClass().getDeclaredFields().map { field =>
      field.setAccessible(true)
      val result = (
        field.getName,
        field.getType.getSimpleName(),
        field.get(this).toString()
      )
      field.setAccessible(false)
      result
    }
  }

}
object Lista8 extends App {
  var p: Point = new Point(3, 4);
  println(p.debugName());
  p.debugVar()
}
