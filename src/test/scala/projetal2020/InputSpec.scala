package projetal2020

import org.scalatest.funsuite.AnyFunSuite

class InputSpec extends AnyFunSuite {

  val inputHandler: InputHandler = new InputHandler("")

  test("Should fail reading environment(String found)") {
    assertThrows[DonneesIncorectesException](inputHandler.getEnvironment("N T"))
  }

  test("Should fail reading environment(Bad Format)") {
    assertThrows[DonneesIncorectesException](inputHandler.getEnvironment("5T"))
  }

  test("Should fail reading environment(One String found)") {
    assertThrows[DonneesIncorectesException](inputHandler.getEnvironment("5 T"))
  }

  test("Should fail reading environment(Bad Format Int)") {
    assertThrows[DonneesIncorectesException](inputHandler.getEnvironment("56"))
  }

  test("Should fail reading tondeuse(Bad State Format)") {
    assertThrows[DonneesIncorectesException](
      inputHandler
        .getTondeuse("55N", "AGAGAGA", inputHandler.getEnvironment("5 5"))
    )
  }

  test("Should fail reading tondeuse(Bad State point)") {
    assertThrows[DonneesIncorectesException](
      inputHandler
        .getTondeuse("5 B N", "AGAGAGA", inputHandler.getEnvironment("5 5"))
    )
  }

  test("Should fail reading tondeuse(Bad State direction)") {
    assertThrows[DonneesIncorectesException](
      inputHandler
        .getTondeuse("5 5 G", "AGAGAGA", inputHandler.getEnvironment("5 5"))
    )
  }

  test("Should fail reading tondeuse(Bad instruction format") {
    assertThrows[DonneesIncorectesException](
      inputHandler
        .getTondeuse("5 5 N", "A G A GA", inputHandler.getEnvironment("5 5"))
    )
  }

  test("Should fail reading tondeuse(Bad instruction code)") {
    assertThrows[DonneesIncorectesException](
      inputHandler
        .getTondeuse("5 5 N", "AGAP", inputHandler.getEnvironment("5 5"))
    )
  }

  test("invalid file format(missing tondeuse data)") {
    assertThrows[DonneesIncorectesException](
      inputHandler.checkFormat(6)
    )
  }

  test("invalid file format(incomplete first tondeuse)") {
    assertThrows[DonneesIncorectesException](inputHandler.checkFormat(2))
  }

  test("invalid file format(no tondeuse)") {
    assertThrows[DonneesIncorectesException](
      inputHandler.checkFormat(1)
    )
  }

  test("can't read file") {
    assertThrows[DonneesIncorectesException](
      inputHandler.getFile()
    )
  }

  test("can't parse string to int") {
    assertThrows[DonneesIncorectesException](inputHandler.parseInt("S"))
  }

  test("Should fail reading tondeuse(Bad start state first case)") {
    assertThrows[DonneesIncorectesException](
      inputHandler
        .getTondeuse("5 5 N", "AGAGA", inputHandler.getEnvironment("4 4"))
    )
  }

  test("Should fail reading tondeuse(Bad start state second case)") {
    assertThrows[DonneesIncorectesException](
      inputHandler
        .getTondeuse("5 5 N", "AGAGA", inputHandler.getEnvironment("5 4"))
    )
  }

  test("Should fail reading tondeuse(Bad start state third case") {
    assertThrows[DonneesIncorectesException](
      inputHandler
        .getTondeuse("5 5 N", "AGAGA", inputHandler.getEnvironment("4 5"))
    )
  }

}
