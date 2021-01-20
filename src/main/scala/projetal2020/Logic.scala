package projetal2020

case class Point(x: Int, y: Int)

case class State(position: Point, direction: Char)

trait Action {
  def apply(code: Char): Unit
}

class MoveRight extends Action {
  override def apply(code: Char): Unit = {
    println(code)
  }
}

class MoveLeft extends Action {
  override def apply(code: Char): Unit = {
    println(code)
  }
}

class MoveFoward extends Action {
  override def apply(code: Char): Unit = {
    println(code)
  }
}
