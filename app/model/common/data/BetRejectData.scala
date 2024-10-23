package model.common.data

import play.api.libs.json.{Json, _}

case class BetRejectData(stake: Int, error: String) {
  def json = Json.writes[BetRejectData].writes(this)
}

object BetRejectData {
  implicit val BetRejectDataReads: Reads[BetRejectData] = Json.reads[BetRejectData]
}


