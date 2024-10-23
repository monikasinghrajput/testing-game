package model.poker.data

case class TableData(roundId: Long = 0,
                     configData: ConfigData = ConfigData(),
                     action: String = "",
                     potAmount: Double = 0.0,
                     potLimit: Double = 0.0,
                     betAmount: Double = 0.0,
                     raiseAmount: Double = 0.0,
                     stage: String = "1",
                     winners: Seq[Winner] = Seq.empty[Winner],
                     gameCards: Seq[String] = Seq.empty[String],
                     sidePots: Seq[SidePot] = Seq.empty[SidePot],
                     seats: Seq[Seat] = Seq.empty[Seat],
                     hands: Seq[(Int, List[String], List[String], String)] = Seq.empty[(Int, List[String], List[String], String)],
                    )

case class TableDataUpdatedMsg(MessageType: String = "tableDataUpdated",
                               data: TableData,
                               timestamp: String)