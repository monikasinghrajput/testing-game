package model.common.data

import model._
import play.api.libs.json.{JsValue, Json, Reads, Writes}

case class GameStateData(round: String,
                            gameStatus: String,
                            dealer: String,
                            minBet: Int,
                            maxBet: Int,
                            winner: String,
                            winHand: String,
                            winCards: List[String],
                            gameCards: Array[List[String]],
                            myBets: Array[BetCreatedData],
                            myWins: Array[BetWonData]
                        ){
                            def json = Json.writes[GameStateData].writes(this)
}

object GameStateData {

  implicit val GameStateDataReads: Reads[GameStateData] = Json.reads[GameStateData]
}


