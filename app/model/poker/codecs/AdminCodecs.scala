package model.poker.codecs

import model.common.messages._
import model.poker.data.TableData
import model.poker.data.admin.AdminInitialDataMsg
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}

trait AdminCodecs extends PokerCodecs
  with LogCodecs
  with PlayerCodec
  with OperationTransactionCodecMsg
  with RoundTransactionCodecMsg
  with MoneyTransactionCodecMsg {


  implicit val initialAdminDataMsgWrites: Writes[AdminInitialDataMsg] = (initialAdminDataMsg: AdminInitialDataMsg) =>
    Json.obj(
      "MessageType" -> initialAdminDataMsg.MessageType,
      "TableId" -> initialAdminDataMsg.tableId,
      "destination" -> initialAdminDataMsg.destination,
      "clientId" -> initialAdminDataMsg.clientId,
      "roundId" -> initialAdminDataMsg.roundId,
      "timestamp" -> initialAdminDataMsg.timestamp,
      "data" -> initialAdminDataMsg.data,
      "logs" -> initialAdminDataMsg.logs,
      "players" -> initialAdminDataMsg.players,
      "transactions" -> initialAdminDataMsg.transactions,
      "operations" -> initialAdminDataMsg.operations,
      "history" -> initialAdminDataMsg.history,
    )

//  implicit val initialAdminDataMsgReads: Reads[AdminInitialDataMsg] = (
//    (JsPath \ "MessageType").read[String] and
//      (JsPath \ "TableId").read[String] and
//      (JsPath \ "destination").read[String] and
//      (JsPath \ "clientId").read[String] and
//      (JsPath \ "roundId").read[Long] and
//      (JsPath \ "timestamp").read[String] and
//      (JsPath \ "data").read[TableData] and
//      (JsPath \ "logs").read[Seq[ServerLog]] and
//      (JsPath \ "players").read[Seq[Player]] and
//      (JsPath \ "transactions").read[Seq[MoneyTransactionMsg]] and
//      (JsPath \ "operations").read[Seq[OperationTransactionMsg]] and
//      (JsPath \ "history").read[Seq[RoundTransactionMsg]]
//    ) (AdminInitialDataMsg.apply _)
//

}
