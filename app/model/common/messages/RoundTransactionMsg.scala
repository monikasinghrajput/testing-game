package model.common.messages

case class RoundTransactionMsg(MessageType: String,
                               transType: String,
                               tableId: String,
                               gameName: String,
                               roundId: Long,
                               winningHand: Seq[String],
                               gameResult: String,
                               playersTotalBet: List[(String, Double)],
                               playerBetsList: String,
                               timestamp: String)
