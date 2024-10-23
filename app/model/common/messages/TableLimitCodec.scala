package model.common.messages

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}

trait TableLimitCodec extends  ChipCodec {


  implicit val tableLimitReads: Reads[TableLimit] = (
    (JsPath \ "chips").read[Seq[Chip]] and
      (JsPath \ "Min_Bet").read[Int] and
      (JsPath \ "Max_Bet").read[Int] and
      (JsPath \ "Min_SideBet").read[Int] and
      (JsPath \ "Max_SideBet").read[Int] and
      (JsPath \ "Min_StraightUpBet").read[Int] and
      (JsPath \ "Max_StraightUpBet").read[Int] and
      (JsPath \ "Min_SplitBet").read[Int] and
      (JsPath \ "Max_SplitBet").read[Int] and
      (JsPath \ "Min_StreetBet").read[Int] and
      (JsPath \ "Max_StreetBet").read[Int] and
      (JsPath \ "Min_CornerBet").read[Int] and
      (JsPath \ "Max_CornerBet").read[Int] and
      (JsPath \ "Min_LineBet").read[Int] and
      (JsPath \ "Max_LineBet").read[Int] and
      (JsPath \ "Min_Dozen_ColumnBet").read[Int] and
      (JsPath \ "Max_Dozen_ColumnBet").read[Int] and
      (JsPath \ "Min_OutsideBet").read[Int] and
      (JsPath \ "Max_OutsideBet").read[Int]
    ) (TableLimit.apply _)

  implicit val tableLimitWrites: Writes[TableLimit] = (
    (JsPath \ "chips").write[Seq[Chip]] and
      (JsPath \ "Min_Bet").write[Int] and
      (JsPath \ "Max_Bet").write[Int] and
      (JsPath \ "Min_SideBet").write[Int] and
      (JsPath \ "Max_SideBet").write[Int] and
      (JsPath \ "Min_StraightUpBet").write[Int] and
      (JsPath \ "Max_StraightUpBet").write[Int] and
      (JsPath \ "Min_SplitBet").write[Int] and
      (JsPath \ "Max_SplitBet").write[Int] and
      (JsPath \ "Min_StreetBet").write[Int] and
      (JsPath \ "Max_StreetBet").write[Int] and
      (JsPath \ "Min_CornerBet").write[Int] and
      (JsPath \ "Max_CornerBet").write[Int] and
      (JsPath \ "Min_LineBet").write[Int] and
      (JsPath \ "Max_LineBet").write[Int] and
      (JsPath \ "Min_Dozen_ColumnBet").write[Int] and
      (JsPath \ "Max_Dozen_ColumnBet").write[Int] and
      (JsPath \ "Min_OutsideBet").write[Int] and
      (JsPath \ "Max_OutsideBet").write[Int]
    ) (unlift(TableLimit.unapply))

}
