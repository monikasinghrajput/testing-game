package model.common.messages

import play.api.libs.functional.syntax._
import play.api.libs.json._

trait RoundTransactionCodecMsg {

  implicit val roundTransactionMsgRead: Reads[RoundTransactionMsg] = (
    (JsPath \ "MessageType").read[String] and
      (JsPath \ "transType").read[String] and
      (JsPath \ "tableId").read[String] and
      (JsPath \ "gameName").read[String] and
      (JsPath \ "roundId").read[Long] and
      (JsPath \ "winningHand").read[Seq[String]] and
      (JsPath \ "gameResult").read[String] and
      (JsPath \ "playersTotalBet").read[List[(String, Double)]] and
      (JsPath \ "playerBetsList").read[String] and
      (JsPath \ "timestamp").read[String]
    ) (RoundTransactionMsg.apply _)

  implicit val roundTransactionMsgWrites: Writes[RoundTransactionMsg] = new Writes[RoundTransactionMsg] {
    def writes(roundTransactionMsg: RoundTransactionMsg): JsValue = Json.obj(
      "MessageType" -> roundTransactionMsg.MessageType,
      "transType" -> roundTransactionMsg.transType,
      "tableId" -> roundTransactionMsg.tableId,
      "gameName" -> roundTransactionMsg.gameName,
      "roundId" -> roundTransactionMsg.roundId,
      "winningHand" -> roundTransactionMsg.winningHand,
      "gameResult" -> roundTransactionMsg.gameResult,
      "playersTotalBet" -> roundTransactionMsg.playersTotalBet,
      "playerBetsList" -> roundTransactionMsg.playerBetsList,
      "timestamp" -> roundTransactionMsg.timestamp,
    )
  }
}
