package model.poker.data.admin

import model.common.messages.{MoneyTransactionMsg, OperationTransactionMsg, Player, RoundTransactionMsg, ServerLog}
import model.poker.data.TableData

case class AdminInitialDataMsg(MessageType: String = "InitialData",
                               tableId: String = "",
                               destination: String = "admin",
                               clientId: String = "",
                               roundId: Long = 0,
                               timestamp: String = "",
                               data: TableData = null,
                               logs: Seq[ServerLog] = Seq.empty[ServerLog],
                               players: Seq[Player] = Seq.empty[Player],
                               transactions: Seq[MoneyTransactionMsg] = Seq.empty[MoneyTransactionMsg],
                               operations: Seq[OperationTransactionMsg] = Seq.empty[OperationTransactionMsg],
                               history: Seq[RoundTransactionMsg] = Seq.empty[RoundTransactionMsg])
