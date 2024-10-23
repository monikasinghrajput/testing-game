package model.common.data

import play.api.libs.json.{Json, _}

case class BettingCounterData(placeBetTimer: Double) {
  def json = Json.writes[BettingCounterData].writes(this)
}

object BettingCounterData {
  implicit val bettingCounterDataWrites = new Writes[BettingCounterData] {
    def writes(bettingCounterData : BettingCounterData): JsValue = {
      Json.obj(
        "placeBetTimer" -> bettingCounterData.placeBetTimer
      )
    }
  }

  implicit val bettingCounterDataReads: Reads[BettingCounterData] = Json.reads[BettingCounterData]
}


