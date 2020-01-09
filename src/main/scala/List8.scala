import java.lang.reflect.AnnotatedType
trait Debug {
  def debugName(): String
  def debugVar(): Unit
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"

  def this() {
    this(0,0)
    this.a = "default"
  }

  def debugName(): String = "Class name: " + getClass().getName()
  def debugVar(): Unit = {
    def helper(array: Array[(String, Class[_], Object)]): Unit = {
      if (!array.isEmpty) {
        println(
          "Field: " + array.head._1 + " => " + array.head._2.getSimpleName + ", " + array.head._3
        )
        helper(array.tail)
      }
    }
    helper(fieldArray())
  }

  def fieldArray(): Array[(String, Class[_], Object)] = {
    getClass().getDeclaredFields().map { field =>
      field.setAccessible(true)
      val result = (
        field.getName,
        field.getType(),
        field.get(this)
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
