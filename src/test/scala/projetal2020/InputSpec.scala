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
      inputHandler.getTondeuse("55N", "AGAGAGA")
    )
  }

  test("Should fail reading tondeuse(Bad State point)") {
    assertThrows[DonneesIncorectesException](
      inputHandler.getTondeuse("5 B N", "AGAGAGA")
    )
  }

  test("Should fail reading tondeuse(Bad State direction)") {
    assertThrows[DonneesIncorectesException](
      inputHandler.getTondeuse("5 5 G", "AGAGAGA")
    )
  }

  test("Should fail reading tondeuse(Bad instruction format") {
    assertThrows[DonneesIncorectesException](
      inputHandler.getTondeuse("5 5 N", "A G A GA")
    )
  }

  test("Should fail reading tondeuse(Bad instruction code)") {
    assertThrows[DonneesIncorectesException](
      inputHandler.getTondeuse("5 5 N", "AGAP")
    )
  }

}
