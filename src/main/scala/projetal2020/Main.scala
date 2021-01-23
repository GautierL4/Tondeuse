package projetal2020

object Main extends App {
  println("Ici le programme principal")
  val point = Point(1, 2)
  val state = State(point, "A")
  val actionFoward = MoveFoward("A")
  val actionRight = MoveRight("D")
  val actionLeft = MoveLeft("G")
  val instructions: List[Action] =
    List[Action](actionFoward, actionFoward, actionRight, actionLeft)
  val tondeuse = new Tondeuse(state, instructions)
  val newState: State = tondeuse.computeInstructions()
  println(newState.direction)
  println(newState.position.x)
  println(newState.position.y)
  // Le code suivant ne compilera pas.
  // var tmp = null;
  // var tmp2 = if (tmp == 1) "yes" else 1

  // println(s"tmp: $tmp, tmp2: $tmp2")
}
