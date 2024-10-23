package model.common.messages

import akka.actor.ActorRef


case class Group(GroupFirstLine: Double = 0.0,
                 GroupSecondLine: Double = 0.0,
                 GroupThirdLine: Double = 0.0,
                 Group1to12: Double = 0.0,
                 Group13to24: Double = 0.0,
                 Group25to36: Double = 0.0,
                 GroupBlack: Double = 0.0,
                 GroupRed: Double = 0.0,
                 GroupOdd: Double = 0.0,
                 GroupEven: Double = 0.0,
                 Group1to18: Double = 0.0,
                 Group19to36: Double = 0.0);

case class Winner(winAmount: Double, nickName: String);

case class Win(winningNUmber: Int, roundId: Long)

case class Stat(number: Int, percent: Double)

case class WinBet(winningIndex: Int, winAmount: Double)

case class Bet(index: Int, betValue: Double, group: String, betType: String)

case class VideoStreamData(videoStreamHtmlIP: String, videoStreamFlashURL: String, videoStreamHtmlURL: String, videoStreamFlashIP: String)

case class RouletteGameData(group: Group = Group(),
                            coldNumbers: Seq[Int] = Seq.empty[Int],
                            lastWinners: Seq[Winner] = Seq.empty[Winner],
                            hotNumbers: Seq[Int] = Seq.empty[Int],
                            history: Seq[Win] = Seq.empty[Win],
                            statistics: Seq[Stat] = Seq.empty[Stat])

case class PlayerGameData(group: Group,
                          playerBetsOfThisRound: Seq[Bet],
                          coldNumbers: Seq[Int],
                          balance: Double,
                          lastWinners: Seq[Winner],
                          hotNumbers: Seq[Int],
                          history: Seq[Win],
                          statistics: Seq[Stat])


case class InitialData(tableId: String = "",
                       roundId: Long = 0,
                       gameType: String = "",
                       data: RouletteGameData = RouletteGameData(),
                       rouletteType: String = "",
                       physicalTableId: String = "") {
}

case class ClientData(actor:ActorRef = null ,
                      client:ActorRef = null,
                      uid : String = "-1",
                      clientType: String = "web",
                      playerIp: String = "192.168.0.1",
                      betList: Seq[Bet] = Seq.empty[Bet],
                      winningBets: Seq[WinBet] = Seq.empty[WinBet],
                      balance: Double = 0.0)

case class Payout(StraightUpBet: Int = 35,
                  SplitBet: Int = 17,
                  StreetBet: Int = 11,
                  CornerBet: Int = 8,
                  BasketBet: Int = 6,
                  LineBet: Int = 5,
                  ColumnBet: Int = 2,
                  DozenBet: Int = 2,
                  OutsideBet: Int = 1)