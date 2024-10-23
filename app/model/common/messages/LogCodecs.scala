package model.common.messages

import play.api.libs.functional.syntax._
import play.api.libs.json._

trait LogCodecs {

  implicit val logWrites: Writes[ServerLog] = new Writes[ServerLog] {
    def writes(log: ServerLog): JsValue = Json.obj(
      "logType" -> log.logType ,
      "runtimeClass" -> log.runtimeClass ,
      "content" -> log.content ,
      "timestamp" -> log.timestamp,
    )
  }
  implicit val logReads: Reads[ServerLog] = (
    (JsPath \ "logType").read[String] and
      (JsPath \ "runtimeClass").read[String] and
      (JsPath \ "content").read[String] and
      (JsPath \ "timestamp").read[String]
    )(ServerLog.apply _)



}
