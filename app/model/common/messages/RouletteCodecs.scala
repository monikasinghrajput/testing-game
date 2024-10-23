package model.common.messages

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, JsValue, Json, Reads, Writes}

trait RouletteCodecs extends PlayerCodec
  with SeatCodec
  with MoneyTransactionCodecMsg
  with OperationTransactionCodecMsg
  with LogCodecs {

  implicit val videoStreamDataWrites: Writes[VideoStreamData] = new Writes[VideoStreamData] {
    def writes(videoStreamData: VideoStreamData): JsValue = Json.obj(
      "videoStreamHtmlIP" -> videoStreamData.videoStreamHtmlIP ,
      "videoStreamFlashURL" -> videoStreamData.videoStreamFlashURL ,
      "videoStreamHtmlURL" -> videoStreamData.videoStreamHtmlURL ,
      "videoStreamFlashIP" -> videoStreamData.videoStreamFlashIP ,
    )
  }
  implicit val videoStreamDataReads: Reads[VideoStreamData] = (
    (JsPath \ "videoStreamHtmlIP").read[String] and
      (JsPath \ "videoStreamFlashURL").read[String] and
      (JsPath \ "videoStreamHtmlURL").read[String] and
      (JsPath \ "videoStreamFlashIP").read[String]
    )(VideoStreamData.apply _)



  implicit val statWrites: Writes[Stat] = new Writes[Stat] {
    def writes(statData: Stat): JsValue = Json.obj(
      "Number" -> statData.number ,
      "Percent" -> statData.percent ,
    )
  }
  implicit val statReads: Reads[Stat] = (
    (JsPath \ "Number").read[Int] and
      (JsPath \ "Percent").read[Double]
    )(Stat.apply _)


  implicit val winWrites: Writes[Win] = new Writes[Win] {
    def writes(winData: Win): JsValue = Json.obj(
      "WinningNumber" -> winData.winningNUmber ,
      "roundId" -> winData.roundId ,
    )
  }
  implicit val winReads: Reads[Win] = (
    (JsPath \ "WinningNumber").read[Int] and
      (JsPath \ "roundId").read[Long]
    )(Win.apply _)


  implicit val winnerWrites: Writes[Winner] = new Writes[Winner] {
    def writes(winnerData: Winner): JsValue = Json.obj(
      "WinAmount" -> winnerData.winAmount ,
      "Nickname" -> winnerData.nickName ,
    )
  }

  implicit val winnerReads: Reads[Winner] = (
    (JsPath \ "WinAmount").read[Double] and
      (JsPath \ "Nickname").read[String]
    )(Winner.apply _)


  implicit val groupWrites: Writes[Group] = new Writes[Group] {
    def writes(groupData: Group): JsValue = Json.obj(
      "GroupFirstLine" -> groupData.GroupFirstLine ,
      "GroupSecondLine" -> groupData.GroupSecondLine ,
      "GroupThirdLine" -> groupData.GroupThirdLine ,
      "Group1to12" -> groupData.Group1to12 ,
      "Group13to24" -> groupData.Group13to24 ,
      "Group25to36" -> groupData.Group25to36 ,
      "GroupBlack" -> groupData.GroupBlack ,
      "GroupRed" -> groupData.GroupRed ,
      "GroupOdd" -> groupData.GroupOdd ,
      "GroupEven" -> groupData.GroupEven ,
      "Group1to18" -> groupData.Group1to18 ,
      "Group19to36" -> groupData.Group19to36
    )
  }

  implicit val groupReads: Reads[Group] = (
    (JsPath \ "GroupFirstLine").read[Double] and
      (JsPath \ "GroupSecondLine").read[Double] and
      (JsPath \ "GroupThirdLine").read[Double] and
      (JsPath \ "Group1to12").read[Double] and
      (JsPath \ "Group13to24").read[Double] and
      (JsPath \ "Group25to36").read[Double] and
      (JsPath \ "GroupBlack").read[Double] and
      (JsPath \ "GroupRed").read[Double] and
      (JsPath \ "GroupOdd").read[Double] and
      (JsPath \ "GroupEven").read[Double] and
      (JsPath \ "Group1to18").read[Double] and
      (JsPath \ "Group19to36").read[Double]
    )(Group.apply _)

  implicit val gameDataWrites: Writes[RouletteGameData] = new Writes[RouletteGameData] {
    def writes(playerGameData: RouletteGameData): JsValue = Json.obj(
      "Group" -> playerGameData.group ,
      "ColdNumbers" -> playerGameData.coldNumbers ,
      "LastWinners" -> playerGameData.lastWinners ,
      "HotNumbers" -> playerGameData.hotNumbers ,
      "History" -> playerGameData.history ,
      "Statistics" -> playerGameData.statistics,
    )
  }

  implicit val gameDataReads: Reads[RouletteGameData] = (
    (JsPath \ "Group").read[Group] and
      (JsPath \ "ColdNumbers").read[Seq[Int]] and
      (JsPath \ "LastWinners").read[Seq[Winner]] and
      (JsPath \ "HotNumbers").read[Seq[Int]] and
      (JsPath \ "History").read[Seq[Win]] and
      (JsPath \ "Statistics").read[Seq[Stat]]
    )(RouletteGameData.apply _)

  implicit val playerGameDataWrites: Writes[PlayerGameData] = new Writes[PlayerGameData] {
    def writes(playerGameData: PlayerGameData): JsValue = Json.obj(
      "Group" -> playerGameData.group ,
      "playerBetOfThisRound" -> playerGameData.playerBetsOfThisRound ,
      "ColdNumbers" -> playerGameData.coldNumbers ,
      "balance" -> playerGameData.balance ,
      "LastWinners" -> playerGameData.lastWinners ,
      "HotNumbers" -> playerGameData.hotNumbers ,
      "History" -> playerGameData.history ,
      "Statistics" -> playerGameData.statistics,
    )
  }

  implicit val playerGameDataReads: Reads[PlayerGameData] = (
    (JsPath \ "Group").read[Group] and
      (JsPath \ "playerBetOfThisRound").read[Seq[Bet]] and
      (JsPath \ "ColdNumbers").read[Seq[Int]] and
      (JsPath \ "balance").read[Double] and
      (JsPath \ "LastWinners").read[Seq[Winner]] and
      (JsPath \ "HotNumbers").read[Seq[Int]] and
      (JsPath \ "History").read[Seq[Win]] and
      (JsPath \ "Statistics").read[Seq[Stat]]
    )(PlayerGameData.apply _)


  implicit val initialDataWrites: Writes[InitialData] = new Writes[InitialData] {
    def writes(initialData: InitialData): JsValue = Json.obj(
      "TableId" -> initialData.tableId,
      "roundId" -> initialData.roundId,
      "gameType" -> initialData.gameType,
      "data" -> initialData.data,
      "RouletteType" -> initialData.rouletteType,
      "physicalTableId" -> initialData.physicalTableId
    )
  }

  implicit val initialDataReads: Reads[InitialData] = (
    (JsPath \ "TableId").read[String] and
      (JsPath \ "roundId").read[Long] and
      (JsPath \ "gameType").read[String] and
      (JsPath \ "data").read[RouletteGameData] and
      (JsPath \ "RouletteType").read[String] and
      (JsPath \ "physicalTableId").read[String]
    )(InitialData.apply _)

// Messages

//  implicit val initializePlayerRead: Reads[InitializePlayer] = Json.reads[InitializePlayer];

  implicit val initialDataMsgWrites: Writes[InitialDataMessage] = new Writes[InitialDataMessage] {
    def writes(initialDataMsg: InitialDataMessage): JsValue = Json.obj(
      "MessageType" -> initialDataMsg.MessageType,
      "TableId" -> initialDataMsg.tableId,
      "destination" -> initialDataMsg.destination,
      "ClientId" -> initialDataMsg.clientId,
      "roundId" -> initialDataMsg.roundId,
      "gameType" -> initialDataMsg.gameType,
      "RoundTripStartTime" -> initialDataMsg.roundTripStartTime,
      "timestamp" -> initialDataMsg.timestamp,
      "data" -> initialDataMsg.data,
      "RouletteType" -> initialDataMsg.rouletteType,
      "physicalTableId" -> initialDataMsg.physicalTableId
    )
  }

  implicit val initialDataMsgReads: Reads[InitialDataMessage] = (
    (JsPath \ "MessageType").read[String] and
      (JsPath \ "TableId").read[String] and
      (JsPath \ "destination").read[String] and
      (JsPath \ "ClientId").read[String] and
      (JsPath \ "roundId").read[Long] and
      (JsPath \ "gameType").read[String] and
      (JsPath \ "RoundTripStartTime").read[Long] and
      (JsPath \ "timestamp").read[String] and
      (JsPath \ "data").read[PlayerGameData] and
      (JsPath \ "RouletteType").read[String] and
      (JsPath \ "physicalTableId").read[String]
    )(InitialDataMessage.apply _)

  implicit val currentBalanceMsgWrites: Writes[CurrentBalanceMessage] = new Writes[CurrentBalanceMessage] {
    def writes(currentBalanceMsg: CurrentBalanceMessage): JsValue = Json.obj(
      "MessageType" -> currentBalanceMsg.MessageType,
      "TableId" -> currentBalanceMsg.tableId,
      "destination" -> currentBalanceMsg.destination,
      "ClientId" -> currentBalanceMsg.clientId,
      "roundId" -> currentBalanceMsg.roundId,
      "gameType" -> currentBalanceMsg.gameType,
      "RoundTripStartTime" -> currentBalanceMsg.roundTripStartTime,
      "timestamp" -> currentBalanceMsg.timestamp,
      "balance" -> currentBalanceMsg.balance,
      "SessionCurrency" -> currentBalanceMsg.sessionCurrency
    )
  }

  implicit val currentBalanceMsgReads: Reads[CurrentBalanceMessage] = (
    (JsPath \ "MessageType").read[String] and
      (JsPath \ "TableId").read[String] and
      (JsPath \ "destination").read[String] and
      (JsPath \ "ClientId").read[String] and
      (JsPath \ "roundId").read[Long] and
      (JsPath \ "gameType").read[String] and
      (JsPath \ "RoundTripStartTime").read[Long] and
      (JsPath \ "timestamp").read[String] and
      (JsPath \ "balance").read[Double] and
      (JsPath \ "SessionCurrency").read[String]
    )(CurrentBalanceMessage.apply _)

  implicit val placeYourBetsMsgWrites: Writes[PlaceYourBetsMessage] = new Writes[PlaceYourBetsMessage] {
    def writes(placeYourBetsMsg: PlaceYourBetsMessage): JsValue = Json.obj(
      "MessageType" -> placeYourBetsMsg.MessageType,
      "TableId" -> placeYourBetsMsg.tableId,
      "destination" -> placeYourBetsMsg.destination,
      "ClientId" -> placeYourBetsMsg.clientId,
      "roundId" -> placeYourBetsMsg.roundId,
      "gameType" -> placeYourBetsMsg.gameType,
      "RoundTripStartTime" -> placeYourBetsMsg.roundTripStartTime,
      "timestamp" -> placeYourBetsMsg.timestamp,
      "TimerTimeLeft" -> placeYourBetsMsg.timerTimeLeft,
      "TimerTime" -> placeYourBetsMsg.timerTime
    )
  }

  implicit val placeYourBetsMsgReads: Reads[PlaceYourBetsMessage] = (
    (JsPath \ "MessageType").read[String] and
      (JsPath \ "TableId").read[String] and
      (JsPath \ "destination").read[String] and
      (JsPath \ "ClientId").read[String] and
      (JsPath \ "roundId").read[Long] and
      (JsPath \ "gameType").read[String] and
      (JsPath \ "RoundTripStartTime").read[Long] and
      (JsPath \ "timestamp").read[String] and
      (JsPath \ "TimerTimeLeft").read[Int] and
      (JsPath \ "TimerTime").read[Int]
    )(PlaceYourBetsMessage.apply _)


  implicit val noMoreBetsMsgWrites: Writes[NoMoreBetsMessage] = new Writes[NoMoreBetsMessage] {
    def writes(noMoreBetsMsg: NoMoreBetsMessage): JsValue = Json.obj(
      "MessageType" -> noMoreBetsMsg.MessageType,
      "TableId" -> noMoreBetsMsg.tableId,
      "destination" -> noMoreBetsMsg.destination,
      "ClientId" -> noMoreBetsMsg.clientId,
      "roundId" -> noMoreBetsMsg.roundId,
      "gameType" -> noMoreBetsMsg.gameType,
      "RoundTripStartTime" -> noMoreBetsMsg.roundTripStartTime,
      "timestamp" -> noMoreBetsMsg.timestamp
    )
  }

  implicit val noMoreBetsMsgReads: Reads[NoMoreBetsMessage] = (
    (JsPath \ "MessageType").read[String] and
      (JsPath \ "TableId").read[String] and
      (JsPath \ "destination").read[String] and
      (JsPath \ "ClientId").read[String] and
      (JsPath \ "roundId").read[Long] and
      (JsPath \ "gameType").read[String] and
      (JsPath \ "RoundTripStartTime").read[Long] and
      (JsPath \ "timestamp").read[String]
    )(NoMoreBetsMessage.apply _)

  implicit val wonBetsMsgWrites: Writes[WonBetsMessage] = new Writes[WonBetsMessage] {
    def writes(wonBetsMsg: WonBetsMessage): JsValue = Json.obj(
      "MessageType" -> wonBetsMsg.MessageType,
      "destination" -> wonBetsMsg.destination,
      "ClientId" -> wonBetsMsg.clientId,
      "RoundTripStartTime" -> wonBetsMsg.roundTripStartTime,
      "WinningBets" -> wonBetsMsg.winningBets
    )
  }

  implicit val wonBetsMsgReads: Reads[WonBetsMessage] = (
    (JsPath \ "MessageType").read[String] and
      (JsPath \ "destination").read[String] and
      (JsPath \ "ClientId").read[String] and
      (JsPath \ "RoundTripStartTime").read[Long] and
      (JsPath \ "WinningBets").read[Seq[WinBet]]
    )(WonBetsMessage.apply _)

  implicit val gameResultMsgWrites: Writes[GameResultMessage] = new Writes[GameResultMessage] {
    def writes(gameResultMsg: GameResultMessage): JsValue = Json.obj(
      "MessageType" -> gameResultMsg.MessageType,
      "TableId" -> gameResultMsg.tableId,
      "destination" -> gameResultMsg.destination,
      "ClientId" -> gameResultMsg.clientId,
      "roundId" -> gameResultMsg.roundId,
      "gameType" -> gameResultMsg.gameType,
      "RoundTripStartTime" -> gameResultMsg.roundTripStartTime,
      "timestamp" -> gameResultMsg.timestamp,
      "Group" -> gameResultMsg.group ,
      "ColdNumbers" -> gameResultMsg.coldNumbers ,
      "HotNumbers" -> gameResultMsg.hotNumbers ,
      "Statistics" -> gameResultMsg.statistics,
      "LastWinners" -> gameResultMsg.lastWinners ,
      "GameResults" -> gameResultMsg.gameResults ,
      "WinAmount" -> gameResultMsg.winAmount
    )
  }

  implicit val gameResultMsgReads: Reads[GameResultMessage] = (
    (JsPath \ "MessageType").read[String] and
      (JsPath \ "TableId").read[String] and
      (JsPath \ "destination").read[String] and
      (JsPath \ "ClientId").read[String] and
      (JsPath \ "roundId").read[Long] and
      (JsPath \ "gameType").read[String] and
      (JsPath \ "RoundTripStartTime").read[Long] and
      (JsPath \ "timestamp").read[String] and
      (JsPath \ "Group").read[Group] and
      (JsPath \ "ColdNumbers").read[Seq[Int]] and
      (JsPath \ "HotNumbers").read[Seq[Int]] and
      (JsPath \ "Statistics").read[Seq[Stat]] and
      (JsPath \ "LastWinners").read[Seq[Winner]] and
      (JsPath \ "GameResults").read[Seq[Win]] and
      (JsPath \ "WinAmount").read[Double]
    )(GameResultMessage.apply _)


  //Admin related codecs

//  case class TerminalHistory(roundId: Int = 0,
//                             lastBalance: Double = 0,
//                             betList: Seq[Bet] = Seq.empty[Bet],
//                             winningBets: Seq[WinBet] = Seq.empty[WinBet],
//                             totalBet: Double = 0,
//                             winAmount: Double = 0,
//                             currentBalance: Double = 0.0)
//


  implicit val terminalHistoryReads: Reads[TerminalHistory] = (
    (JsPath \ "roundId").read[Int] and
      (JsPath \ "lastBalance").read[Double] and
      (JsPath \ "betList").read[Seq[Bet]] and
      (JsPath \ "winningBets").read[Seq[WinBet]] and
      (JsPath \ "totalBet").read[Double] and
      (JsPath \ "winAmount").read[Double] and
      (JsPath \ "currentBalance").read[Double]
    )(TerminalHistory.apply _)


  implicit val terminalHistoryWrites: Writes[TerminalHistory] = new Writes[TerminalHistory] {
    def writes(terminalHistory: TerminalHistory): JsValue = Json.obj(
      "roundId" -> terminalHistory.roundId ,
      "lastBalance" -> terminalHistory.lastBalance ,
      "betList" -> terminalHistory.betList ,
      "winningBets" -> terminalHistory.winningBets ,
      "totalBet" -> terminalHistory.totalBet ,
      "winAmount" -> terminalHistory.winAmount ,
      "currentBalance" -> terminalHistory.currentBalance ,
    )
  }


  //  case class TerminalData(connected: Boolean = false,
//                          totalBet: Double = 0,
//                          winAmount: Double = 0,
//                          lastWin: Double = 0,
//                          history: Seq[TerminalHistory],
//                          betList: Seq[Bet] = Seq.empty[Bet],
//                          winningBets: Seq[WinBet] = Seq.empty[WinBet],
//                          balance: Double = 0.0)
//

  implicit val terminalDataReads: Reads[TerminalData] = (
    (JsPath \ "connected").read[Boolean] and
    (JsPath \ "totalBet").read[Double] and
    (JsPath \ "winAmount").read[Double] and
      (JsPath \ "lastWin").read[Double] and
      (JsPath \ "history").read[Seq[TerminalHistory]] and
      (JsPath \ "betList").read[Seq[Bet]] and
      (JsPath \ "winningBets").read[Seq[WinBet]] and
      (JsPath \ "balance").read[Double]
    )(TerminalData.apply _)


  implicit val terminalDataWrites: Writes[TerminalData] = new Writes[TerminalData] {
    def writes(terminalData: TerminalData): JsValue = Json.obj(
      "connected" -> terminalData.connected ,
      "totalBet" -> terminalData.totalBet ,
      "winAmount" -> terminalData.winAmount ,
      "lastWin" -> terminalData.lastWin ,
      "history" -> terminalData.history ,
      "betList" -> terminalData.betList ,
      "winningBets" -> terminalData.winningBets ,
      "balance" -> terminalData.balance ,
    )
  }

  //  case class WheelData(connected: Boolean = false, status: String = "Disconnected", lastResult: Int = 0)
//
  implicit val wheelDataReads: Reads[WheelData] = (
    (JsPath \ "connected").read[Boolean] and
    (JsPath \ "status").read[String] and
    (JsPath \ "lastResult").read[Int]
  )(WheelData.apply _)


  implicit val wheelDataWrites: Writes[WheelData] = new Writes[WheelData] {
    def writes(wheelData: WheelData): JsValue = Json.obj(
      "connected" -> wheelData.connected ,
      "status" -> wheelData.status ,
      "lastResult" -> wheelData.lastResult ,
    )
  }

  //  case class Roulette8SeaterData(wheel: WheelData, terminals: Seq[TerminalData], game: GameData)
  implicit val roulette8SeaterDataReads: Reads[Roulette8SeaterData] = (
    (JsPath \ "wheel").read[WheelData] and
    (JsPath \ "seats").read[Seq[Seat]] and
    (JsPath \ "game").read[RouletteGameData] and
    (JsPath \ "logs").read[Seq[ServerLog]]
  )(Roulette8SeaterData.apply _)


  implicit val roulette8SeaterDataWrites: Writes[Roulette8SeaterData] = new Writes[Roulette8SeaterData] {
    def writes(roulette8SeaterData: Roulette8SeaterData): JsValue = Json.obj(
      "wheel" -> roulette8SeaterData.wheel ,
      "seats" -> roulette8SeaterData.seats ,
      "game" -> roulette8SeaterData.game ,
      "logs" -> roulette8SeaterData.logs ,
    )
  }

//  case class InitialAdminData(tableId: String = "AutoRoulette", roundId: Int, data: Roulette8SeaterData = null)
//

  implicit val initialAdminDataReads: Reads[InitialAdminData] = (
    (JsPath \ "tableId").read[String] and
      (JsPath \ "roundId").read[Int] and
      (JsPath \ "data").read[Roulette8SeaterData]
    )(InitialAdminData.apply _)


  implicit val initialAdminDataWrites: Writes[InitialAdminData] = new Writes[InitialAdminData] {
    def writes(initialAdminData: InitialAdminData): JsValue = Json.obj(
      "tableId" -> initialAdminData.tableId ,
      "roundId" -> initialAdminData.roundId ,
      "data" -> initialAdminData.data ,
    )
  }

  //  case class AdminData(name: String = "0.0.0.0", connected: Boolean = false, balance: Double = 0.0 )

  implicit val adminDataReads: Reads[AdminData] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "connected").read[Boolean] and
      (JsPath \ "balance").read[Double]
    )(AdminData.apply _)


  implicit val adminDataWrites: Writes[AdminData] = new Writes[AdminData] {
    def writes(adminData: AdminData): JsValue = Json.obj(
      "name" -> adminData.name ,
      "connected" -> adminData.connected ,
      "balance" -> adminData.balance ,
    )
  }

  implicit val initialAdminDataMsgWrites: Writes[InitialAdminDataMessage] = new Writes[InitialAdminDataMessage] {
    def writes(initialAdminDataMsg: InitialAdminDataMessage): JsValue = Json.obj(
      "MessageType" -> initialAdminDataMsg.MessageType,
      "TableId" -> initialAdminDataMsg.tableId,
      "destination" -> initialAdminDataMsg.destination,
      "ClientId" -> initialAdminDataMsg.clientId,
      "roundId" -> initialAdminDataMsg.roundId,
      "gameType" -> initialAdminDataMsg.gameType,
      "RoundTripStartTime" -> initialAdminDataMsg.roundTripStartTime,
      "timestamp" -> initialAdminDataMsg.timestamp,
      "data" -> initialAdminDataMsg.data,
      "players" -> initialAdminDataMsg.players,
      "transactions" -> initialAdminDataMsg.transactions,
      "operations" -> initialAdminDataMsg.operations,
      "RouletteType" -> initialAdminDataMsg.rouletteType,
      "physicalTableId" -> initialAdminDataMsg.physicalTableId
    )
  }

  implicit val initialAdminDataMsgReads: Reads[InitialAdminDataMessage] = (
    (JsPath \ "MessageType").read[String] and
      (JsPath \ "TableId").read[String] and
      (JsPath \ "destination").read[String] and
      (JsPath \ "ClientId").read[String] and
      (JsPath \ "roundId").read[Long] and
      (JsPath \ "gameType").read[String] and
      (JsPath \ "RoundTripStartTime").read[Long] and
      (JsPath \ "timestamp").read[String] and
      (JsPath \ "data").read[Roulette8SeaterData] and
      (JsPath \ "players").read[Seq[Player]] and
      (JsPath \ "transactions").read[Seq[MoneyTransactionMsg]] and
      (JsPath \ "operations").read[Seq[OperationTransactionMsg]] and
      (JsPath \ "RouletteType").read[String] and
      (JsPath \ "physicalTableId").read[String]
    )(InitialAdminDataMessage.apply _)

}
