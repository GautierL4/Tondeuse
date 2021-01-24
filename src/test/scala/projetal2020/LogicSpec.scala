package projetal2020

import org.scalatest.funsuite.AnyFunSuite

class LogicSpec extends AnyFunSuite {

  test("Tondeuse result with one tondeuse") {
    val environment: Point = Point(5, 5)
    val tondeuseState: State = State(Point(1, 2), Direction.create('N'))
    val instructions: List[Action.Value] = List(
      Action.G,
      Action.A,
      Action.G,
      Action.A,
      Action.G,
      Action.A,
      Action.G,
      Action.A,
      Action.A
    )
    val tondeuse: Tondeuse = new Tondeuse(tondeuseState, instructions)
    val tondeuses: List[Tondeuse] = List(tondeuse)
    val logicHandler: LogicHandler = new LogicHandler(environment, tondeuses)

    val firstResult: TondeuseResult = TondeuseResult(
      tondeuseState,
      instructions,
      State(Point(1, 3), Direction.N)
    )
    val expectedResults: List[TondeuseResult] = List(firstResult)
    assert(logicHandler.computeTondeusesResult() == expectedResults)
  }

  test("Tondeuse result with two tondeuse") {
    val environment: Point = Point(5, 5)
    val tondeuseState: State = State(Point(1, 2), Direction.create('N'))
    val instructions: List[Action.Value] = List(
      Action.G,
      Action.A,
      Action.G,
      Action.A,
      Action.G,
      Action.A,
      Action.G,
      Action.A,
      Action.A
    )

    val secTondeuseState: State = State(Point(3, 3), Direction.create('E'))
    val secInstructions: List[Action.Value] = List(
      Action.A,
      Action.A,
      Action.D,
      Action.A,
      Action.A,
      Action.D,
      Action.A,
      Action.D,
      Action.D,
      Action.A
    )

    val tondeuse: Tondeuse = new Tondeuse(tondeuseState, instructions)
    val secTondeuse: Tondeuse = new Tondeuse(secTondeuseState, secInstructions)
    val tondeuses: List[Tondeuse] = List(tondeuse, secTondeuse)
    val logicHandler: LogicHandler = new LogicHandler(environment, tondeuses)

    val firstResult: TondeuseResult = TondeuseResult(
      tondeuseState,
      instructions,
      State(Point(1, 3), Direction.N)
    )
    val secondResult: TondeuseResult = TondeuseResult(
      secTondeuseState,
      secInstructions,
      State(Point(5, 1), Direction.E)
    )
    val expectedResults: List[TondeuseResult] = List(firstResult, secondResult)
    assert(logicHandler.computeTondeusesResult() == expectedResults)
  }
}
