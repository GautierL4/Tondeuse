package projetal2020

import scala.io.Source

object Main extends App {

  val data_from_file = Source
    .fromFile("/home/azote/Documents/projets_esgi/input_example.txt")
    .getLines()
    .toList

  // 5 5 ==> dimension du terrain
  val xmax: Int = data_from_file(0).split(" ")(0).toInt
  val ymax: Int = data_from_file(0).split(" ")(1).toInt
  val xmin: Int = 0
  val ymin: Int = 0
  println(s"Taille du terrain: ${xmax.toString} x ${ymax.toString}")

  // 1 2 N ==> orientation
  val orientation_x: Int = data_from_file(1).split(" ")(0).toInt // récup 1
  val orientation_y: Int = data_from_file(1).split(" ")(1).toInt // récup 2
  val orientation_NEWS: String = data_from_file(1).split(" ")(2) // récup "N"

  println(
    s"Orientation: ${orientation_x.toString} , ${orientation_y.toString} , ${orientation_NEWS}"
  )
  val point = Point(orientation_x, orientation_y)
  //val state = State(point, orientation_NEWS)

  val environment = Point(5, 5)
  val oldPoint = Point(1, 2)
  val oldState = State(oldPoint, Direction.N)
  val instructions: List[Action.Value] =
    List[Action.Value](
      Action.Forward,
      Action.Forward,
      Action.Right,
      Action.Right,
      Action.Left
    )

  val tondeuse = new Tondeuse(oldState, instructions)
  val newState: State = tondeuse.computeInstructions(environment)
  println(newState.direction)
  println(newState.position.x)
  println(newState.position.y)

}
