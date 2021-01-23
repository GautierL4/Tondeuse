package projetal2020

case class Point(x: Int, y: Int)

case class State(position: Point, direction: String)

trait Action {
  def code: String
}

case class MoveRight(code: String) extends Action

case class MoveLeft(code: String) extends Action

case class MoveFoward(code: String) extends Action

class Tondeuse(start: State, instructions: List[Action]) {
  def computeInstructions(): State = {
    def computeInstruction(data: List[Action], state: State): State =
      data match {
        case MoveFoward(_) :: rest =>
          computeInstruction(rest, moveFoward(state))
        case MoveRight(_) :: rest => computeInstruction(rest, moveRight(state))
        case MoveLeft(_) :: rest  => computeInstruction(rest, moveLeft(state))
        case Nil                  => state
        case _                    => state
      }
    computeInstruction(instructions, start)
  }

  def moveFoward(initState: State): State = {
    State(
      Point(initState.position.x + 1, initState.position.y),
      initState.direction
    )
  }

  def moveRight(initState: State): State = {
    State(Point(initState.position.x, initState.position.y), "E")
  }

  def moveLeft(initState: State): State = {
    State(Point(initState.position.x, initState.position.y), "W")
  }
}
