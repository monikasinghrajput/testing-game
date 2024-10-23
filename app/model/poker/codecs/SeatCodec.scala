package model.poker.codecs

import play.api.libs.functional.syntax._
import model.poker.data.{Seat, SeatHistory, Bet, WinBet}
import play.api.libs.json._

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
    (JsPath \ "id").read[Int] and
    (JsPath \ "name").read[String] and
    (JsPath \ "ip").read[String] and
    (JsPath \ "hint").read[String] and
    (JsPath \ "balance").read[Double] and
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
      (JsPath \ "isPlaying").read[Boolean] and
      (JsPath \ "cards").read[Seq[String]] and
      (JsPath \ "bets").read[Seq[Double]] and
      (JsPath \ "actions").read[Seq[Boolean]]
    ) (Seat.apply _)


  implicit val seatDataWrites: Writes[Seat] = new Writes[Seat] {
    def writes(seatData: Seat): JsValue = Json.obj(
      "id" -> seatData.id,
      "name" -> seatData.name,
      "ip" -> seatData.ip,
      "hint" -> seatData.hint,
      "balance" -> seatData.balance,
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
      "isPlaying" -> seatData.isPlaying,
      "cards" -> seatData.cards,
      "bets" -> seatData.bets,
      "actions" -> seatData.actions,
    )
  }
}
