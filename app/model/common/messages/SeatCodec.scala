package model.common.messages

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, JsValue, Json, Reads, Writes}

trait SeatCodec extends BetCodec {

  implicit val seatHistoryReads: Reads[SeatHistory] = (
    (JsPath \ "roundId").read[Int] and
      (JsPath \ "lastBalance").read[Double] and
      (JsPath \ "betList").read[Seq[Bet]] and
      (JsPath \ "winningBets").read[Seq[WinBet]] and
      (JsPath \ "totalBet").read[Double] and
      (JsPath \ "winAmount").read[Double] and
      (JsPath \ "currentBalance").read[Double]
    ) (SeatHistory.apply _)


  implicit val seatHistoryWrites: Writes[SeatHistory] = new Writes[SeatHistory] {
    def writes(seatHistory: SeatHistory): JsValue = Json.obj(
      "roundId" -> seatHistory.roundId,
      "lastBalance" -> seatHistory.lastBalance,
      "betList" -> seatHistory.betList,
      "winningBets" -> seatHistory.winningBets,
      "totalBet" -> seatHistory.totalBet,
      "winAmount" -> seatHistory.winAmount,
      "currentBalance" -> seatHistory.currentBalance,
    )
  }

  implicit val seatDataReads: Reads[Seat] = (
    (JsPath \ "uid").read[String] and
      (JsPath \ "connected").read[Boolean] and
      (JsPath \ "totalBet").read[Double] and
      (JsPath \ "winAmount").read[Double] and
      (JsPath \ "lastWin").read[Double] and
      (JsPath \ "history").read[Seq[SeatHistory]] and
      (JsPath \ "betList").read[Seq[Bet]] and
      (JsPath \ "winningBets").read[Seq[WinBet]] and
      (JsPath \ "isTurn").read[Boolean] and
      (JsPath \ "gameStatus").read[String] and
      (JsPath \ "isDealer").read[Boolean] and
      (JsPath \ "isSmallBet").read[Boolean] and
      (JsPath \ "isBigBet").read[Boolean] and
      (JsPath \ "cards").read[Seq[String]] and
      (JsPath \ "bets").read[Seq[Double]] and
      (JsPath \ "actions").read[Seq[String]]
    ) (Seat.apply _)


  implicit val seatDataWrites: Writes[Seat] = new Writes[Seat] {
    def writes(seatData: Seat): JsValue = Json.obj(
      "uid" -> seatData.uid,
      "connected" -> seatData.connected,
      "totalBet" -> seatData.totalBet,
      "winAmount" -> seatData.winAmount,
      "lastWin" -> seatData.lastWin,
      "history" -> seatData.history,
      "betList" -> seatData.betList,
      "winningBets" -> seatData.winningBets,
      "isTurn" -> seatData.isTurn,
      "gameStatus" -> seatData.gameStatus,
      "isDealer" -> seatData.isDealer,
      "isSmallBet" -> seatData.isSmallBet,
      "isBigBet" -> seatData.isBigBet,
      "cards" -> seatData.cards,
      "bets" -> seatData.bets,
      "actions" -> seatData.actions,
    )
  }
}
