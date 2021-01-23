package projetal2020

object Main extends App {
  println("Ici le programme principal")
  val environment = Point(5, 5)
  val point = Point(1, 2)
  val state = State(point, Direction.N)
  val instructions: List[Action.Value] =
    List[Action.Value](
      Action.A,
      Action.A,
      Action.D,
      Action.D,
      Action.G
    )
  val tondeuse = new Tondeuse(state, instructions)
  val newState: State = tondeuse.computeInstructions(environment)
  println(newState.direction)
  println(newState.position.x)
  println(newState.position.y)
  val resultList: List[TondeuseResult] = List[TondeuseResult](
    TondeuseResult(
      tondeuse.start,
      tondeuse.instructions,
      tondeuse.computeInstructions(environment)
    )
  )
  val result = ResultTemplate(environment, resultList)
  val output: OutputHandler = new OutputHandler(
    "out/output.json"
  )
  output.write(result)
  // Le code suivant ne compilera pas.
  // var tmp = null;
  // var tmp2 = if (tmp == 1) "yes" else 1

  // println(s"tmp: $tmp, tmp2: $tmp2")
}
