package model.common.data

import play.api.libs.json.{Json, _}

case class PlayerActivationData(userCode:String,
                balance: Double,
                exposure: Int) {
  def json = Json.writes[PlayerActivationData].writes(this)
}

object PlayerActivationData {
  implicit val playerActivationDataWrites = new Writes[PlayerActivationData] {
    def writes(playerActivationData : PlayerActivationData): JsValue = {
      Json.obj(
        "userCode" -> playerActivationData.userCode,
        "balance" -> playerActivationData.balance,
        "exposure" -> playerActivationData.exposure,
      )
    }
  }

  implicit val playerActivationDataReads: Reads[PlayerActivationData] = Json.reads[PlayerActivationData]
}


