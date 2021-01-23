package projetal2020

object Direction extends Enumeration {
  val N, E, W, S = Value

  def isValid(code: Char): Boolean =
    code match {
      case 'N' | 'E' | 'W' | 'S' => true
      case _                     => false
    }

}

object Action extends Enumeration {
  val A, D, G = Value

  def isValid(code: Char): Boolean =
    code match {
      case 'A' | 'D' | 'G' => true
      case _               => false
    }

}

case class Point(x: Int, y: Int)

case class State(position: Point, direction: Direction.Value)

case class TondeuseResult(
    debut: State,
    instructions: List[Action.Value],
    fin: State
)

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
