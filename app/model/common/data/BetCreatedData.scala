package model.common.data

import play.api.libs.json.{Json, _}

case class BetCreatedData(game: String, user: String, Round:String, Player: String, Hand: String, Odds: Double, Stake: Int) {
  def json = Json.writes[BetCreatedData].writes(this)
}

object BetCreatedData {
  implicit val implicitBetCreatedDataWrites = new Writes[BetCreatedData] {
    def writes(betCreatedData : BetCreatedData): JsValue = {
      Json.obj(
        "game" -> betCreatedData.game,
        "user" -> betCreatedData.user,
        "Round" -> betCreatedData.Round,
        "Player" -> betCreatedData.Player,
        "Hand" -> betCreatedData.Hand,
        "Odds" -> betCreatedData.Odds,
        "Stake" -> betCreatedData.Stake
      )
    }
  }

  implicit val BetCreatedDataReads: Reads[BetCreatedData] = Json.reads[BetCreatedData]
}


