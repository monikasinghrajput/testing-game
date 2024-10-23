package model.common.data

import play.api.libs.json.{JsValue, Json, Reads, Writes}


case class WinMessageData( round: String,
                           winner: String,
                           winHand: String,
                           winCards: List[String],
                           handScores: List[String],
                           gameCards: Array[List[String]],
                          ) {


  def json = Json.writes[WinMessageData].writes(this)
}

object WinMessageData {

  implicit val implicitWinMessageData = new Writes[WinMessageData] {
    def writes(winMessageData : WinMessageData): JsValue = {
      Json.obj(
        "round" -> winMessageData.round,
        "winner" -> winMessageData.winner,
        "winHand" -> winMessageData.winHand,
        "winCards" -> winMessageData.winCards,
        "handScores" -> winMessageData.handScores,
        "gameCards" -> winMessageData.gameCards
      )
    }
  }

  implicit val WinMessageDataReads: Reads[WinMessageData] = Json.reads[WinMessageData]
}




