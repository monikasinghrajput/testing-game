package model.common.data

import play.api.libs.json.{Json, _}

case class PlayerAccountData(balance: Double) {
  def json = Json.writes[PlayerAccountData].writes(this)
}

object PlayerAccountData {
  implicit val playerAccountDataWrites = new Writes[PlayerAccountData] {
    def writes(playerAccountData : PlayerAccountData): JsValue = {
      Json.obj(
        "balance" -> playerAccountData.balance
      )
    }
  }

  implicit val playerAccountDataReads: Reads[PlayerAccountData] = Json.reads[PlayerAccountData]
}


