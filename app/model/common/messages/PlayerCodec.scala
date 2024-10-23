package model.common.messages

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, JsValue, Json, Reads, Writes}

trait PlayerCodec {

  implicit val playerWrites: Writes[Player] = new Writes[Player] {
    def writes(player: Player): JsValue = Json.obj(
      "client_ip" -> player.clientIp,
      "client_id" -> player.clientId,
      "session_id" -> player.sessionId,
      "current_player_token" -> player.currentPlayerToken,
      "nickname" -> player.nickname,
      "operator_id" -> player.operatorId,
      "currency" -> player.currency,
      "uid" -> player.uid,
      "vip" -> player.vip,
      "status" -> player.status,
      "usage" -> player.usage,
      "balance" -> player.balance
    )
  }

  implicit val playerReads: Reads[Player] = (
    (JsPath \ "client_ip").read[String] and
      (JsPath \ "client_id").read[String] and
      (JsPath \ "session_id").read[Int] and
      (JsPath \ "current_player_token").read[String] and
      (JsPath \ "nickname").read[String] and
      (JsPath \ "operator_id").read[Int] and
      (JsPath \ "currency").read[String] and
      (JsPath \ "uid").read[String] and
      (JsPath \ "vip").read[Int] and
      (JsPath \ "status").read[String] and
      (JsPath \ "usage").read[String] and
      (JsPath \ "balance").read[Double]
    )(Player.apply _)

  implicit val playerUpdateWrites: Writes[PlayerUpdatedMsg] = new Writes[PlayerUpdatedMsg] {
    def writes(playerUpdatedMsg: PlayerUpdatedMsg): JsValue = Json.obj(
      "MessageType" -> playerUpdatedMsg.MessageType ,
      "player" -> playerUpdatedMsg.player,
      "timestamp" -> playerUpdatedMsg.timestamp,
    )
  }

  implicit val playerCreatedWrites: Writes[PlayerCreatedMsg] = new Writes[PlayerCreatedMsg] {
    def writes(playerCreatedMsg: PlayerCreatedMsg): JsValue = Json.obj(
      "MessageType" -> playerCreatedMsg.MessageType ,
      "player" -> playerCreatedMsg.player,
      "timestamp" -> playerCreatedMsg.timestamp
    )
  }

}
