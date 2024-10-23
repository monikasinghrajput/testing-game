package model.common.messages

import akka.actor.ActorRef

case class TerminalHistory(roundId: Int = 0,
                           lastBalance: Double = 0,
                           betList: Seq[Bet] = Seq.empty[Bet],
                           winningBets: Seq[WinBet] = Seq.empty[WinBet],
                           totalBet: Double = 0,
                           winAmount: Double = 0,
                           currentBalance: Double = 0.0)

case class TerminalData(connected: Boolean = false,
                        totalBet: Double = 0,
                        winAmount: Double = 0,
                        lastWin: Double = 0,
                        history: Seq[TerminalHistory] = Seq.empty[TerminalHistory],
                        betList: Seq[Bet] = Seq.empty[Bet],
                        winningBets: Seq[WinBet] = Seq.empty[WinBet],
                        balance: Double = 0.0)

case class WheelData(connected: Boolean = false, status: String = "Disconnected", lastResult: Int = 0)

case class Roulette8SeaterData(wheel: WheelData,
                               seats: Seq[Seat],
                               game: RouletteGameData,
                               logs: Seq[ServerLog] = Seq.empty[ServerLog])

case class InitialAdminData(tableId: String = "AutoRoulette", roundId: Int, data: Roulette8SeaterData = null)

case class AdminData(name: String = "0.0.0.0", connected: Boolean = false, balance: Double = 0.0)

case class AdminClientData(actor:ActorRef = null ,
                           client:ActorRef = null,
                           name: String = "0.0.0.0",
                           balance: Double = 0.0)

case class ServerLog(logType: String,
                     runtimeClass: String,
                     content: String,
                     timestamp: String)
