package model.common.messages

import akka.actor.ActorRef

import java.time.Instant


case class InitialDataMessage(MessageType: String = "InitialData",
                      tableId: String = "",
                      destination: String = "player",
                      clientId: String = "",
                      roundId: Long = 0,
                      gameType: String = "AutomatedRoulette",
                      roundTripStartTime: Long = Instant.now.getEpochSecond,
                      timestamp: String = "",
                      data: PlayerGameData = null,
                      rouletteType: String = "automatedRoulette",
                      physicalTableId: String = "000000") {
}
case class CurrentBalanceMessage(MessageType: String = "CURRENT_BALANCE",
                      tableId: String = "",
                      destination: String = "player",
                      clientId: String = "",
                      roundId: Long = 0,
                      gameType: String = "AutomatedRoulette",
                      roundTripStartTime: Long = Instant.now.getEpochSecond,
                      timestamp: String = "",
                      balance: Double = 0,
                      sessionCurrency: String = "INR") {
}
case class PlaceYourBetsMessage(MessageType: String = "PLACE_YOUR_BETS",
                      tableId: String = "",
                      destination: String = "player",
                      clientId: String = "",
                      roundId: Long = 0,
                      gameType: String = "AutomatedRoulette",
                      roundTripStartTime: Long = Instant.now.getEpochSecond,
                      timestamp: String = "",
                      timerTimeLeft: Int = 0,
                      timerTime: Int = 30) {
}

case class NoMoreBetsMessage(MessageType: String = "NO_MORE_BETS",
                      tableId: String = "",
                      destination: String = "player",
                      clientId: String = "",
                      roundId: Long = 0,
                      gameType: String = "AutomatedRoulette",
                      roundTripStartTime: Long = Instant.now.getEpochSecond,
                      timestamp: String = "") {
}

case class WonBetsMessage(MessageType: String = "WonBets",
                      destination: String = "player",
                      clientId: String = "",
                      roundTripStartTime: Long = Instant.now.getEpochSecond,
                      winningBets: Seq[WinBet] = Seq.empty[WinBet]) {
}

case class GameResultMessage(MessageType: String = "GAME_RESULT",
                      tableId: String = "",
                      destination: String = "player",
                      clientId: String = "",
                      roundId: Long = 0,
                      gameType: String = "AutomatedRoulette",
                      roundTripStartTime: Long = Instant.now.getEpochSecond,
                      timestamp: String = "",
                     group: Group,
                     coldNumbers: Seq[Int],
                     hotNumbers: Seq[Int],
                     statistics: Seq[Stat],
                     lastWinners: Seq[Winner],
                     gameResults: Seq[Win],
                     winAmount: Double) {
}

case class InitialAdminDataMessage(MessageType: String = "InitialData",
                                   tableId: String = "",
                                   destination: String = "player",
                                   clientId: String = "",
                                   roundId: Long = 0,
                                   gameType: String = "AutomatedRoulette",
                                   roundTripStartTime: Long = Instant.now.getEpochSecond,
                                   timestamp: String = "",
                                   data: Roulette8SeaterData = null,
                                   players: Seq[Player] = Seq.empty[Player],
                                   transactions: Seq[MoneyTransactionMsg] = Seq.empty[MoneyTransactionMsg],
                                   operations: Seq[OperationTransactionMsg] = Seq.empty[OperationTransactionMsg],
                                   rouletteType: String = "automatedRoulette",
                                   physicalTableId: String = "000000") {
}



