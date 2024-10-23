package model.common.messages

case class Seat(uid: String = "-1",
                connected: Boolean = false,
                totalBet: Double = 0,
                winAmount: Double = 0,
                lastWin: Double = 0,
                history: Seq[SeatHistory] = Seq.empty[SeatHistory],
                betList: Seq[Bet] = Seq.empty[Bet],
                winningBets: Seq[WinBet] = Seq.empty[WinBet],
                isTurn : Boolean = false,
                gameStatus: String = "Ready",
                isDealer: Boolean = false,
                isSmallBet: Boolean = false,
                isBigBet: Boolean = false,
                cards: Seq[String] = Seq.empty[String],
                bets: Seq[Double] = Seq.empty[Double],
                actions: Seq[String] = Seq.empty[String])

case class SeatHistory(roundId: Int = 0,
                       lastBalance: Double = 0,
                       betList: Seq[Bet] = Seq.empty[Bet],
                       winningBets: Seq[WinBet] = Seq.empty[WinBet],
                       totalBet: Double = 0,
                       winAmount: Double = 0,
                       currentBalance: Double = 0.0)