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
}
