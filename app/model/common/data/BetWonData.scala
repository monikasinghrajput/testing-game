package model.common.data

import play.api.libs.json.{Json, _}

case class BetWonData(game: String, user: String, Round:String, Player: String, Hand: String, Stake: Int, Amount: Double) {
  def json = Json.writes[BetWonData].writes(this)
}

object BetWonData {
  implicit val implicitBetWonDataWrites = new Writes[BetWonData] {
    def writes(betWonData : BetWonData): JsValue = {
      Json.obj(
        "game" -> betWonData.game,
        "user" -> betWonData.user,
        "Round" -> betWonData.Round,
        "Player" -> betWonData.Player,
        "Hand" -> betWonData.Hand,
        "Stake" -> betWonData.Stake,
        "Amount" -> betWonData.Amount
      )
    }
  }

  implicit val BetWonDataReads: Reads[BetWonData] = Json.reads[BetWonData]
}


