package model.common.messages

import play.api.libs.functional.syntax._
import play.api.libs.json._
//
//case class OperationTransactionMsg(transType: String = "Admin",
//                                   MessageType: String,
//                                   uid: String,
//                                   client_ip: String,
//                                   nickname: Double = 0,
//                                   status: Double = 0,
//                                   usage: Double = 0,
//                                   timestamp: String)
trait OperationTransactionCodecMsg {

  implicit val operationTransactionMsgReads: Reads[OperationTransactionMsg] = (
    (JsPath \ "transType").read[String] and
    (JsPath \ "MessageType").read[String] and
      (JsPath \ "uid").read[String] and
      (JsPath \ "client_ip").read[String] and
      (JsPath \ "nickname").read[String] and
      (JsPath \ "status").read[String] and
      (JsPath \ "usage").read[String] and
      (JsPath \ "timestamp").read[String]
    ) (OperationTransactionMsg.apply _)

  implicit val operationTransactionMsgWrites: Writes[OperationTransactionMsg] = new Writes[OperationTransactionMsg] {
    def writes(operationTransaction: OperationTransactionMsg): JsValue = Json.obj(
      "transType" -> operationTransaction.transType ,
      "MessageType" -> operationTransaction.MessageType ,
      "uid" -> operationTransaction.uid ,
      "client_ip" -> operationTransaction.client_ip ,
      "nickname" -> operationTransaction.nickname ,
      "status" -> operationTransaction.status ,
      "usage" -> operationTransaction.usage ,
      "timestamp" -> operationTransaction.timestamp ,
    )
  }
}
