package projetal2020

import better.files._
import play.api.libs.json._

case class ResultTemplate(limite: Point, tondeuses: List[TondeuseResult])

class OutputHandler(filePath: String) {

  implicit val pointWrites: Writes[Point] = new Writes[Point] {
    def writes(point: Point): JsValue =
      Json.obj(
        "x" -> point.x,
        "y" -> point.y
      )
  }

  implicit val stateWrites: Writes[State] = new Writes[State] {
    def writes(state: State): JsValue =
      Json.obj(
        "point" -> state.position,
        "direction" -> state.direction
      )
  }

  implicit val tondeuseWrites: Writes[TondeuseResult] =
    new Writes[TondeuseResult] {
      def writes(result: TondeuseResult): JsValue =
        Json.obj(
          "debut" -> result.debut,
          "instructions" -> result.instructions,
          "fin" -> result.fin
        )
    }

  implicit val resultTemplateWrites: Writes[ResultTemplate] =
    new Writes[ResultTemplate] {
      def writes(resultTemplate: ResultTemplate): JsValue =
        Json.obj(
          "limite" -> Json.obj(
            "x" -> resultTemplate.limite.x,
            "y" -> resultTemplate.limite.y
          ),
          "tondeuses" -> resultTemplate.tondeuses
        )
    }

  def write(result: ResultTemplate): Unit = {
    File(filePath)
      .createIfNotExists()
      .write(Json.prettyPrint(Json.toJson(result)))
    ()
  }
}
