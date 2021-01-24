package projetal2020

import better.files._

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
class InputHandler(filePath: String) {

  def readFile(): List[String] = {
    val lines = File(filePath).lines.toList
    if (lines.length % 2 == 0) {
      throw new DonneesIncorectesException(
        "Nombre de ligne incorrect : Données de tondeuse possiblement incomplète"
      )
    } else if (lines.length < 2) {
      throw new DonneesIncorectesException(
        "Données de la première tondeuse incomplète"
      )
    } else if (lines.length < 1) {
      throw new DonneesIncorectesException(
        "Aucune tondeuse"
      )
    } else {
      lines
    }
  }

  def parseInt(s: String): Int = {
    try {
      s.toInt
    } catch {
      case _: Exception =>
        throw new DonneesIncorectesException(
          "Une de vos valeurs est incorrect (Valeurs numérique attendu)"
        )
    }
  }

  def getEnvironment(firstLine: String): Point = {
    Point(
      parseInt(firstLine.split(" ")(0)),
      parseInt(firstLine.split(" ")(1))
    )
  }

  def getTondeuses(data: List[String]): List[Tondeuse] = {
    def readLine(
        remainingElement: List[String],
        output: List[Tondeuse],
        index: Int
    ): List[Tondeuse] =
      remainingElement match {
        case head :: tail if (index % 2 != 0 && index != 0) =>
          readLine(tail, output :+ getTondeuse(head, tail(0)), index + 1)
        case _ :: tail => readLine(tail, output, index + 1)
        case _         => output
      }
    readLine(data, List(), 0)
  }

  def getTondeuse(stateLine: String, instructionsLine: String): Tondeuse = {
    new Tondeuse(
      State(
        Point(
          parseInt(stateLine.split(" ")(0)),
          parseInt(stateLine.split(" ")(1))
        ),
        validDirection(stateLine.split(" ")(2)(0))
      ),
      extractInstructions(instructionsLine)
    )
  }

  def validDirection(direction: Char): Direction.Value = {
    if (Direction.isValid(direction)) Direction.create(direction)
    else
      throw new DonneesIncorectesException(
        "La direction initial d'une tondeuse n'est pas valide"
      )
  }

  def extractInstructions(instructionsLine: String): List[Action.Value] = {
    try {
      val chars = instructionsLine.toList
      def helper(
          remainingInstructions: List[Char],
          output: List[Action.Value]
      ): List[Action.Value] =
        remainingInstructions match {
          case head :: tail => helper(tail, output :+ validInstruction(head))
          case _            => output
        }
      helper(chars, List())
    } catch {
      case _: Exception =>
        throw new DonneesIncorectesException(
          "Une des instructions fournis à une des toundeuses n'est pas valide"
        )
    }
  }

  def validInstruction(instruction: Char): Action.Value = {
    if (Action.isValid(instruction)) Action.create(instruction)
    else
      throw new DonneesIncorectesException(
        "Une des instruction d'une tondeuse n'est pas valide"
      )
  }

}
