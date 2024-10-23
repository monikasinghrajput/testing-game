package model.common.messages

case class GameTransaction(roundId: Long,
                           rake: Double = 0.0,
                           game: String = "Roulette",
                           transType: String = "Undefined",
                           player: String,
                           totalBet: Double = 0,
                           betList : Seq[Bet] = Seq.empty[Bet],
                           totalWin: Double = 0,
                           wonList: Seq[WinBet] = Seq.empty[WinBet],
                           oldBalance: Double = 0,
                           balance: Double = 0)
