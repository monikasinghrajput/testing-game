package model.poker.data

import play.api.Logger

case class Seat(id: Int,
                name: String = "",
                ip: String = "",
                hint: String = "",
                balance: Double = 0,
                uid: String = "-1",
                connected: Boolean = false,
                totalBet: Double = 0,
                winAmount: Double = 0,
                lastWin: Double = 0,
                history: Seq[SeatHistory] = Seq.empty[SeatHistory],
                betList: Seq[Bet] = Seq.empty[Bet],
                winningBets: Seq[WinBet] = Seq.empty[WinBet],
                isTurn: Boolean = false,
                gameStatus: String = "CASH OUT",
                isDealer: Boolean = false,
                isSmallBet: Boolean = false,
                isBigBet: Boolean = false,
                isPlaying: Boolean = true,
                cards: Seq[String] = Seq.empty[String],
                bets: Seq[Double] = Seq.empty[Double],
                actions: Seq[Boolean] = Seq.empty[Boolean],
                ) {
  val log = Logger(this.getClass)


  /*
  * A function to clear the current bet bucket and move it to matching bucket
  *
  * */
  def seatSettleRoundBets(round: String ): Seat = {
    round match {
      case "PreFlop" =>
          val currentRoundBet: Double = this.bets.head
          val updatedBets = this.bets.updated(1, currentRoundBet)
          this.copy(bets = updatedBets.updated(0, 0))
      case "Flop" =>
          val currentRoundBet: Double = this.bets.head
          val updatedBets = this.bets.updated(2, currentRoundBet)
          this.copy(bets = updatedBets.updated(0, 0))
      case "Turn" =>
          val currentRoundBet: Double = this.bets.head
          val updatedBets = this.bets.updated(3, currentRoundBet)
          this.copy(bets = updatedBets.updated(0, 0))
      case "River" =>
          val currentRoundBet: Double = this.bets.head
          val updatedBets = this.bets.updated(4, currentRoundBet)
          this.copy(bets = updatedBets.updated(0, 0))
    }

  }

  def clearAllRoundsBets():Seat = this.copy(bets = Seq(0,0,0,0,0), betList = Seq.empty[Bet])

}

case class SeatHistory(roundId: Int = 0,
                       lastBalance: Double = 0,
                       betList: Seq[Bet] = Seq.empty[Bet],
                       winningBets: Seq[WinBet] = Seq.empty[WinBet],
                       totalBet: Double = 0,
                       winAmount: Double = 0,
                       currentBalance: Double = 0.0)