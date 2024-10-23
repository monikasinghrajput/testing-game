package model.poker.data

case class Winner(id: Int,
                  winningPot: Int = 0,
                  winAmount: Int = 0,
                  rake: Double = 0,
                  totalBet: Double = 0,
                  hand: String = "",
                  cards: Seq[String] = Seq.empty[String])
