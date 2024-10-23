package model.common.data

import play.api.libs.json.{Json, _}

case class BetData(Player: String, Hand: String, Odds: Double, Stake: Int) {
  def json = Json.writes[BetData].writes(this)
}

object BetData {
  implicit val implicitBetDataWrites = new Writes[BetData] {
    def writes(betData : BetData): JsValue = {
      Json.obj(
        "Player" -> betData.Player,
        "Hand" -> betData.Hand,
        "Odds" -> betData.Odds,
        "Stake" -> betData.Stake
      )
    }
  }

  implicit val BetDataReads: Reads[BetData] = Json.reads[BetData]
}


