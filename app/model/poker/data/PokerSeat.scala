package model.poker.data

case class PokerSeat(id: Int,
                     uid: String,
                     cards: Seq[String],
                     hand: Seq[String],
                     score: String,
                     betList: Seq[Bet],
                     winAmount: Double,
                     isDealer: Boolean,
                     isPlaying: Boolean,
                     gameStatus : String)
