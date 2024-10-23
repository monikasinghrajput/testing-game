package model.common.messages

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, JsValue, Json, Reads, Writes}


trait ChipCodec {
  implicit val chipReads: Reads[Chip] = (
    (JsPath \ "color").read[String] and
    (JsPath \ "value").read[Int] and
    (JsPath \ "img").read[String] and
    (JsPath \ "default").read[Boolean]
    ) (Chip.apply _)

  implicit val chipWrites: Writes[Chip] = new Writes[Chip] {
    def writes(chip: Chip): JsValue = Json.obj(
      "color" -> chip.color ,
      "value" -> chip.value ,
      "img" -> chip.img ,
      "default" -> chip.default ,
    )
  }
}