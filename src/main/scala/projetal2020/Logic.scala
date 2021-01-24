package projetal2020

object Direction extends Enumeration {
  val N, E, W, S = Value

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def create(code: Char): Direction.Value =
    code match {
      case 'N' => N
      case 'E' => E
      case 'W' => W
      case 'S' => S
      case _ =>
        throw new DonneesIncorectesException(
          "La direction demandé n'est pas pris en charge"
        )
    }

}

object Action extends Enumeration {
  val A, D, G = Value

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def create(code: Char): Action.Value =
    code match {
      case 'A' => A
      case 'D' => D
      case 'G' => G
      case _ =>
        throw new DonneesIncorectesException(
          "L'action demandé n'est pas pris en charge"
        )
    }

}

case class Point(x: Int, y: Int)

case class State(position: Point, direction: Direction.Value)

case class TondeuseResult(
    debut: State,
    instructions: List[Action.Value],
    fin: State
)

class LogicHandler(environment: Point, tondeuses: List[Tondeuse]) {

  def computeTondeusesResult(): List[TondeuseResult] = {
    def helper(
        remainingTondeuse: List[Tondeuse],
        environment: Point,
        output: List[TondeuseResult]
    ): List[TondeuseResult] =
      remainingTondeuse match {
        case head :: tail =>
          helper(
            tail,
            environment,
            output :+ computeTondeuseResult(head, environment)
          )
        case _ => output
      }
    helper(tondeuses, environment, List())
  }

  def computeTondeuseResult(
      tondeuse: Tondeuse,
      environment: Point
  ): TondeuseResult = {
    TondeuseResult(
      tondeuse.start,
      tondeuse.instructions,
      tondeuse.computeInstructions(environment)
    )
  }
}

class Tondeuse(val start: State, val instructions: List[Action.Value]) {

  def computeInstructions(environment: Point): State = {
    def computeInstruction(
        data: List[Action.Value],
        state: State,
        environment: Point
    ): State =
      data match {
        case Action.A :: rest =>
          computeInstruction(rest, moveForward(state, environment), environment)
        case Action.D :: rest =>
          computeInstruction(rest, moveRight(state), environment)
        case Action.G :: rest =>
          computeInstruction(rest, moveLeft(state), environment)
        case Nil => state
        case _   => state
      }
    computeInstruction(instructions, start, environment)
  }

  def moveForward(initState: State, environment: Point): State = {
    def checkMove(initState: State, environment: Point): Point =
      initState.direction match {
        case Direction.N =>
          if (initState.position.y == environment.y) initState.position
          else Point(initState.position.x, initState.position.y + 1)
        case Direction.S =>
          if (initState.position.y == 0) initState.position
          else Point(initState.position.x, initState.position.y - 1)
        case Direction.E =>
          if (initState.position.x == environment.x) initState.position
          else Point(initState.position.x + 1, initState.position.y)
        case Direction.W =>
          if (initState.position.x == 0) initState.position
          else Point(initState.position.x - 1, initState.position.y)
      }
    State(
      checkMove(initState, environment),
      initState.direction
    )
  }

  def moveRight(initState: State): State = {
    def defineDirection(currDirection: Direction.Value): Direction.Value =
      currDirection match {
        case Direction.N => Direction.E
        case Direction.E => Direction.S
        case Direction.S => Direction.W
        case Direction.W => Direction.N
      }
    State(
      Point(initState.position.x, initState.position.y),
      defineDirection(initState.direction)
    )
  }

  def moveLeft(initState: State): State = {
    def defineDirection(currDirection: Direction.Value): Direction.Value =
      currDirection match {
        case Direction.N => Direction.W
        case Direction.W => Direction.S
        case Direction.S => Direction.E
        case Direction.E => Direction.N
      }
    State(
      Point(initState.position.x, initState.position.y),
      defineDirection(initState.direction)
    )
  }
}
