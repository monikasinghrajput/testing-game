package model.poker.codecs

import model.poker.data.{Bet, CardsConfig, ConfigData, GameResult, PokerSeat, Seat, SidePot, TableDataUpdatedMsg, TableState, Winner}
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}

trait PokerCodecs extends SeatCodec {

  import model.poker.data.TableData

  implicit val sidePotWrites: Writes[SidePot] = (sidePotData: SidePot) => Json.obj(
    "ids" -> sidePotData.ids,
    "capAmount" -> sidePotData.capAmount,
    "fids" -> sidePotData.fids,
    "foldAmount" -> sidePotData.foldAmount,
  )

  implicit val sidePotReads: Reads[SidePot] = (
    (JsPath \ "ids").read[Seq[Int]] and
      (JsPath \ "capAmount").read[Double] and
      (JsPath \ "fids").read[Seq[Int]] and
      (JsPath \ "foldAmount").read[Double]
    )(SidePot.apply _)


  implicit val winnerWrites: Writes[Winner] = (winnerData: Winner) => Json.obj(
    "id" -> winnerData.id,
    "winningPot" -> winnerData.winningPot,
    "winAmount" -> winnerData.winAmount,
    "rake" -> winnerData.rake,
    "totalBet" -> winnerData.totalBet,
    "hand" -> winnerData.hand,
    "cards" -> winnerData.cards
  )

  implicit val winnerReads: Reads[Winner] = (
    (JsPath \ "id").read[Int] and
      (JsPath \ "winningPot").read[Int] and
      (JsPath \ "winAmount").read[Int] and
      (JsPath \ "rake").read[Double] and
      (JsPath \ "totalBet").read[Double] and
      (JsPath \ "hand").read[String] and
      (JsPath \ "cards").read[Seq[String]]
    )(Winner.apply _)


  implicit val cardsDataWrites: Writes[CardsConfig] = (cardsConfig: CardsConfig) =>
    Json.obj(
      "count" -> cardsConfig.count,
      "min" -> cardsConfig.min,
      "max" -> cardsConfig.max,
    )

  implicit val configDataWrites: Writes[ConfigData] = (configData: ConfigData) =>
    Json.obj(
      "pokerVariant" -> configData.pokerVariant,
      "betLimit" -> configData.betLimit,
      "liveDealer" -> configData.liveDealer,
      "tournamentMode" -> configData.tournamentMode,
      "playerCardsConfig" -> configData.playerCardsConfig,
      "communityCardsConfig" -> configData.communityCardsConfig,
      "rakePercent" -> configData.rakePercent,
      "blind" -> configData.blind,
    )

  implicit val cardsConfigReads: Reads[CardsConfig] = (
    (JsPath \ "count").read[Int] and
    (JsPath \ "min").read[Int] and
    (JsPath \ "max").read[Int]
    )(CardsConfig.apply _)

  implicit val configDataReads: Reads[ConfigData] = (
    (JsPath \ "pokerVariant").read[String] and
      (JsPath \ "betLimit").read[String] and
      (JsPath \ "liveDealer").read[Boolean] and
      (JsPath \ "tournamentMode").read[Boolean] and
      (JsPath \ "playerCardsConfig").read[CardsConfig] and
      (JsPath \ "communityCardsConfig").read[CardsConfig] and
      (JsPath \ "rakePercent").read[Int] and
      (JsPath \ "blind").read[Int]
    ) (ConfigData.apply _)



  implicit val tableDataWrites: Writes[TableData] = (tableData: TableData) => Json.obj(
    "roundId" -> tableData.roundId,
    "configData" -> tableData.configData,
    "action" -> tableData.action,
    "potAmount" -> tableData.potAmount,
    "potLimit" -> tableData.potLimit,
    "betAmount" -> tableData.betAmount,
    "raiseAmount" -> tableData.raiseAmount,
    "stage" -> tableData.stage,
    "winners" -> tableData.winners,
    "gameCards" -> tableData.gameCards,
    "sidePots" -> tableData.sidePots,
    "seats" -> tableData.seats,
    "hands" -> tableData.hands,
  )

//  implicit val tableDataReads: Reads[TableData] = (
//    (JsPath \ "roundId").read[Long] and
//      (JsPath \ "configData").read[ConfigData] and
//    (JsPath \ "gameType").read[String] and
//      (JsPath \ "rake").read[Int] and
//      (JsPath \ "blind").read[Int] and
//      (JsPath \ "action").read[String] and
//      (JsPath \ "potAmount").read[Double] and
//      (JsPath \ "potLimit").read[Double] and
//      (JsPath \ "betAmount").read[Double] and
//      (JsPath \ "betAmountMax").read[Double] and
//      (JsPath \ "raiseAmount").read[Double] and
//      (JsPath \ "raiseAmountMax").read[Double] and
//      (JsPath \ "stage").read[String] and
//      (JsPath \ "winners").read[Seq[Winner]] and
//      (JsPath \ "gameCards").read[Seq[String]] and
//      (JsPath \ "sidePots").read[Seq[SidePot]] and
//        (JsPath \ "seats").read[Seq[Seat]] and
//      (JsPath \ "hands").read[Seq[(Int, List[String], List[String], String)]]
//    )(TableData.apply _)




  implicit val tableStateWrites: Writes[TableState] = (tableState: TableState) =>
    Json.obj(
    "roundId" -> tableState.roundId,
    "configData" -> tableState.configData,
    "action" -> tableState.action,
    "dealerId" -> tableState.dealerId,
    "smallBetId" -> tableState.smallBetId,
    "bigBetId" -> tableState.bigBetId,
    "turnId" -> tableState.turnId,
    "numPlayers" -> tableState.numPlayers,
    "potAmount" -> tableState.potAmount,
    "betIndex" -> tableState.betIndex,
    "betAmount" -> tableState.betAmount,
    "raiseAmount" -> tableState.raiseAmount,
    "stage" -> tableState.stage,
    "winners" -> tableState.winners,
    "gameCards" -> tableState.gameCards,
    "cards" -> tableState.cards,
    "sidePots" -> tableState.sidePots,
    "seats" -> tableState.seats,
    "hands" -> tableState.hands,
  )

  implicit val tableStateReads: Reads[TableState] = (
    (JsPath \ "roundId").read[Long] and
    (JsPath \ "configData").read[ConfigData] and
    (JsPath \ "action").read[String] and
    (JsPath \ "dealerId").read[Int] and
    (JsPath \ "smallBetId").read[Int] and
    (JsPath \ "bigBetId").read[Int] and
    (JsPath \ "turnId").read[Int] and
    (JsPath \ "numPlayers").read[Seq[Int]] and
    (JsPath \ "potAmount").read[Double] and
    (JsPath \ "potLimit").read[Double] and
    (JsPath \ "betIndex").read[Int] and
    (JsPath \ "betAmount").read[Double] and
    (JsPath \ "raiseAmount").read[Double] and
    (JsPath \ "stage").read[String] and
    (JsPath \ "winners").read[Seq[Winner]] and
    (JsPath \ "gameCards").read[Seq[String]] and
    (JsPath \ "cards").read[List[String]] and
    (JsPath \ "sidePots").read[Seq[SidePot]] and
    (JsPath \ "seats").read[Seq[Seat]] and
    (JsPath \ "hands").read[Seq[(Int, List[String], List[String], String)]]
    )(TableState.apply _)




  implicit val tableDataUpdatedWrites: Writes[TableDataUpdatedMsg] = (tableData: TableDataUpdatedMsg) => Json.obj(
    "MessageType" -> tableData.MessageType,
    "data" -> tableData.data,
    "timestamp" -> tableData.timestamp,
  )

//  implicit val tableDataUpdatedReads: Reads[TableDataUpdatedMsg] = (
//    (JsPath \ "MessageType").read[String] and
//    (JsPath \ "data").read[TableData] and
//    (JsPath \ "timestamp").read[String]
//    )(TableDataUpdatedMsg.apply _)

  implicit val pokerSeatWrite: Writes[PokerSeat] = (pokerSeat: PokerSeat) => Json.obj(
    "id" -> pokerSeat.id,
    "uid" -> pokerSeat.uid,
    "cards" -> pokerSeat.cards,
    "hand" -> pokerSeat.hand,
    "score" -> pokerSeat.score,
    "betList" -> pokerSeat.betList,
    "winAmount" -> pokerSeat.winAmount,
    "isDealer" -> pokerSeat.isDealer,
    "isPlaying" -> pokerSeat.isPlaying,
    "gameStatus" -> pokerSeat.gameStatus,
  )

  implicit val pokerSeatRead: Reads[PokerSeat] = (
    (JsPath \ "id").read[Int] and
    (JsPath \ "uid").read[String] and
    (JsPath \ "cards").read[Seq[String]] and
    (JsPath \ "hand").read[Seq[String]] and
    (JsPath \ "score").read[String] and
    (JsPath \ "betList").read[Seq[Bet]] and
    (JsPath \ "winAmount").read[Double] and
    (JsPath \ "isDealer").read[Boolean] and
    (JsPath \ "isPlaying").read[Boolean] and
    (JsPath \ "gameStatus").read[String]
    )(PokerSeat.apply _)

  implicit val gameResultWrites: Writes[GameResult] = (gameResult: GameResult) => Json.obj(
    "roundId" -> gameResult.roundId,
    "configData" -> gameResult.configData,
    "gameCards" -> gameResult.gameCards,
    "winners" -> gameResult.winners,
    "seats" -> gameResult.seats,
  )

  implicit val gameResultReads: Reads[GameResult] = (
    (JsPath \ "roundId").read[Long] and
    (JsPath \ "configData").read[ConfigData] and
    (JsPath \ "gameCards").read[Seq[String]] and
    (JsPath \ "winners").read[Seq[Winner]] and
    (JsPath \ "seats").read[Seq[PokerSeat]]
    )(GameResult.apply _)


}
