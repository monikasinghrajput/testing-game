package model.poker.data.admin

import model.poker.data.Seat

case class AdminTableData(seats: Seq[Seat] = Seq.empty[Seat],
                          potAmount: Double = 0.0,
                          betAmount: Double = 10.0,
                          stage: String = "1",
                          winner: Int = -1,
                          winningHand : String = "",
                          winningCards: Seq[String] = Seq.empty[String],
                          gameCards: Seq[String] = Seq.empty[String]
                         )
