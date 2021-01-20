package projetal2020

object Main extends App {
  println("Ici le programme principal")
  val point = Point(1, 2)
  val state = State(point, 'A')
  // Le code suivant ne compilera pas.
  // var tmp = null;
  // var tmp2 = if (tmp == 1) "yes" else 1

  // println(s"tmp: $tmp, tmp2: $tmp2")
}
