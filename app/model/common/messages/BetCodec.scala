package model.common.messages

import play.api.libs.functional.syntax._
import play.api.libs.json._

trait BetCodec {
  implicit val betWrites: Writes[Bet] = new Writes[Bet] {
    def writes(betData: Bet): JsValue = Json.obj(
      "index" -> betData.index ,
      "betValue" -> betData.betValue ,
      "group" -> betData.group ,
      "betType" -> betData.betType
    )
  }
  implicit val betReads: Reads[Bet] = (
    (JsPath \ "index").read[Int] and
      (JsPath \ "betValue").read[Double] and
      (JsPath \ "group").read[String] and
      (JsPath \ "betType").read[String]
    )(Bet.apply _)


  implicit val winBetWrites: Writes[WinBet] = new Writes[WinBet] {
    def writes(winBetData: WinBet): JsValue = Json.obj(
      "WinningIndex" -> winBetData.winningIndex ,
      "WinAmount" -> winBetData.winAmount
    )
  }
  implicit val winBetReads: Reads[WinBet] = (
    (JsPath \ "WinningIndex").read[Int] and
      (JsPath \ "WinAmount").read[Double]
    )(WinBet.apply _)

}
