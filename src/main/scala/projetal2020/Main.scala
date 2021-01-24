package projetal2020

object Main extends App {

  val inputHandler: InputHandler = new InputHandler(
    "in/input.txt"
  )

  val inputs: List[String] = inputHandler.readFile()
  val environment: Point = inputHandler.getEnvironment(inputs(0))
  val tondeuses: List[Tondeuse] = inputHandler.getTondeuses(inputs, environment)

  /*
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
   */

  val logicHandler: LogicHandler = new LogicHandler(environment, tondeuses)
  val resultList: List[TondeuseResult] = logicHandler.computeTondeusesResult()

  val result = ResultTemplate(environment, resultList)
  val output: OutputHandler = new OutputHandler(
    "out/output.json"
  )
  output.write(result)
}
