package projetal2020

import better.files._

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
class InputHandler(filePath: String) {

  def getFile(): List[String] = {
    try {
      File(filePath).lines.toList
    } catch {
      case _: Exception =>
        throw new DonneesIncorectesException(
          "Impossible de récupérer le contenu du fichier, vérifier le format ou l'emplacement du fichier"
        )
    }
  }

  def checkFormat(lineCount: Int): Boolean = {
    if (lineCount % 2 == 0) {
      throw new DonneesIncorectesException(
        "Nombre de ligne incorrect : Données de tondeuse possiblement incomplète"
      )
    } else if (lineCount < 2) {
      throw new DonneesIncorectesException(
        "Données de la première tondeuse incomplète"
      )
    } else if (lineCount < 1) {
      throw new DonneesIncorectesException(
        "Aucune tondeuse"
      )
    } else {
      true
    }
  }

  def readFile(): List[String] = {
    val lines: List[String] = getFile()
    if (checkFormat(lines.length)) {
      lines
    } else {
      throw new DonneesIncorectesException(
        "Le format du fichier n'est pas correct"
      )
    }
  }

  def parseInt(s: String): Int = {
    try {
      val int: Int = s.toInt
      if (int > 0) int else throw new DonneesIncorectesException("")
    } catch {
      case _: Exception =>
        throw new DonneesIncorectesException(
          "Une de vos valeurs est incorrect (Valeurs numérique positive attendu)"
        )
    }
  }

  def getEnvironment(firstLine: String): Point = {
    try {
      Point(
        parseInt(splitSeparator(0, firstLine)),
        parseInt(splitSeparator(1, firstLine))
      )
    } catch {
      case _: Exception =>
        throw new DonneesIncorectesException(
          "Le format de la ligne environnement n'est pas correct"
        )
    }

  }

  def getTondeuses(data: List[String], environnement: Point): List[Tondeuse] = {
    def readLine(
        remainingElement: List[String],
        output: List[Tondeuse],
        index: Int
    ): List[Tondeuse] =
      remainingElement match {
        case head :: tail if (index % 2 != 0 && index != 0) =>
          readLine(
            tail,
            output :+ getTondeuse(head, tail(0), environnement),
            index + 1
          )
        case _ :: tail => readLine(tail, output, index + 1)
        case _         => output
      }
    readLine(data, List(), 0)
  }

  def getTondeuseState(stateLine: String, environnement: Point): State = {
    State(
      getTondeuseStatePoint(stateLine, environnement),
      validDirection(splitSeparator(2, stateLine)(0))
    )
  }

  def splitSeparator(valueIndex: Int, string: String): String = {
    try {
      string.split(" ")(valueIndex)
    } catch {
      case _: Exception =>
        throw new DonneesIncorectesException(
          "Le format d'une des lignes n'est pas respecté (nombre de valeurs incorrect)"
        )
    }
  }

  def getTondeuseStatePoint(stateLine: String, environnement: Point): Point = {
    Point(
      checkValidPosition(
        parseInt(splitSeparator(0, stateLine)),
        environnement.x
      ),
      checkValidPosition(
        parseInt(splitSeparator(1, stateLine)),
        environnement.y
      )
    )
  }

  def checkValidPosition(position: Int, limit: Int): Int = {
    if (position <= limit) position
    else
      throw new DonneesIncorectesException(
        "Le point de départ d'une tondeuse est invalide"
      )
  }

  def getTondeuse(
      stateLine: String,
      instructionsLine: String,
      environnement: Point
  ): Tondeuse = {
    new Tondeuse(
      getTondeuseState(stateLine, environnement),
      extractInstructions(instructionsLine)
    )
  }

  def validDirection(direction: Char): Direction.Value = {
    Direction.create(direction)
  }

  def extractInstructions(instructionsLine: String): List[Action.Value] = {
    def helper(
        remainingInstructions: List[Char],
        output: List[Action.Value]
    ): List[Action.Value] =
      remainingInstructions match {
        case head :: tail => helper(tail, output :+ validInstruction(head))
        case _            => output
      }
    helper(instructionsLine.toList, List())
  }

  def validInstruction(instruction: Char): Action.Value = {
    Action.create(instruction)
  }

}
