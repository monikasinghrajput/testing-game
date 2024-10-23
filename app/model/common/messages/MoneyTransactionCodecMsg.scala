package model.common.messages

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, JsValue, Json, Reads, Writes}

//case class MoneyTransaction(message: String, name: String, amount: Double = 0, oldBalance: Double = 0, newBalance: Double = 0)
trait MoneyTransactionCodecMsg {

  implicit val moneyTransactionMsgReads: Reads[MoneyTransactionMsg] = (
    (JsPath \ "transType").read[String] and
    (JsPath \ "MessageType").read[String] and
      (JsPath \ "playerIp").read[String] and
      (JsPath \ "rake").read[Double] and
      (JsPath \ "roundId").read[Long] and
      (JsPath \ "amount").read[Double] and
      (JsPath \ "oldBalance").read[Double] and
      (JsPath \ "newBalance").read[Double] and
      (JsPath \ "timestamp").read[String]
    ) (MoneyTransactionMsg.apply _)

  implicit val moneyTransactionMsgWrites: Writes[MoneyTransactionMsg] = new Writes[MoneyTransactionMsg] {
    def writes(moneyTransaction: MoneyTransactionMsg): JsValue = Json.obj(
      "transType" -> moneyTransaction.transType ,
      "MessageType" -> moneyTransaction.MessageType ,
      "playerIp" -> moneyTransaction.playerIp ,
      "rake" -> moneyTransaction.rake ,
      "roundId" -> moneyTransaction.roundId ,
      "amount" -> moneyTransaction.amount ,
      "oldBalance" -> moneyTransaction.oldBalance ,
      "newBalance" -> moneyTransaction.newBalance ,
      "timestamp" -> moneyTransaction.timestamp ,
    )
  }
}
