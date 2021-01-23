package projetal2020

case class Point(x: Int, y: Int)

case class State(position: Point, direction: String)

trait Action {
  def code: String
}

case class MoveRight(code: String) extends Action

case class MoveLeft(code: String) extends Action

case class MoveForward(code: String) extends Action

class Tondeuse(start: State, instructions: List[Action]) {
  def computeInstructions(environment: Point): State = {
    def computeInstruction(
        data: List[Action],
        state: State,
        environment: Point
    ): State =
      data match {
        case MoveForward(_) :: rest =>
          computeInstruction(rest, moveForward(state, environment), environment)
        case MoveRight(_) :: rest =>
          computeInstruction(rest, moveRight(state), environment)
        case MoveLeft(_) :: rest =>
          computeInstruction(rest, moveLeft(state), environment)
        case Nil => state
        case _   => state
      }
    computeInstruction(instructions, start, environment)
  }

  def moveForward(initState: State, environment: Point): State = {
    def checkMove(initState: State, environment: Point): Point =
      initState.direction match {
        case "N" =>
          if (initState.position.y == environment.y) initState.position
          else Point(initState.position.x, initState.position.y + 1)
        case "S" =>
          if (initState.position.y == 0) initState.position
          else Point(initState.position.x, initState.position.y - 1)
        case "E" =>
          if (initState.position.x == environment.x) initState.position
          else Point(initState.position.x + 1, initState.position.y)
        case "W" =>
          if (initState.position.x == 0) initState.position
          else Point(initState.position.x - 1, initState.position.y)
      }
    State(
      checkMove(initState, environment),
      initState.direction
    )
  }

  def moveRight(initState: State): State = {
    def defineDirection(currDirection: String): String =
      currDirection match {
        case "N" => "E"
        case "E" => "S"
        case "S" => "W"
        case "W" => "N"
        case _   => "E"
      }
    State(
      Point(initState.position.x, initState.position.y),
      defineDirection(initState.direction)
    )
  }

  def moveLeft(initState: State): State = {
    def defineDirection(currDirection: String): String =
      currDirection match {
        case "N" => "W"
        case "W" => "S"
        case "S" => "E"
        case "E" => "N"
        case _   => "W"
      }
    State(
      Point(initState.position.x, initState.position.y),
      defineDirection(initState.direction)
    )
  }
}
