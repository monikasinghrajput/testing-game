package model.common.data

import model._

import play.api.libs.json.{JsValue, Json, Reads, Writes}


case class GameStateInitData(round: String,
                            gameStatus: String,
                            dealer: String,
                             minBet: Int,
                             maxBet: Int,
                            winner: String,
                            winHand: String,
                            winCards: List[String],
                            gameCards: Array[List[String]],
                            myBets: Array[BetCreatedData],
                            myWins: Array[BetWonData],
                            lastWins: Array[WinMessageData],
                            playerBetsHistory: Array[BetCreatedData],
                            playerBetsWonHistory: Array[BetWonData]
                        ){
                            def json = Json.writes[GameStateInitData].writes(this)
}

object GameStateInitData {


  implicit val GameStateInitDataReads: Reads[GameStateInitData] = Json.reads[GameStateInitData]
}
