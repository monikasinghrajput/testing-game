package model.common.messages

import play.api.libs.json.{JsObject, Json}



case class TableLimit(chips: Seq[Chip],
                      Min_Bet: Int,
                      Max_Bet: Int,
                      Min_SideBet: Int,
                      Max_SideBet: Int,
                      Min_StraightUpBet: Int,
                      Max_StraightUpBet: Int,
                      Min_SplitBet: Int,
                      Max_SplitBet: Int,
                      Min_StreetBet: Int,
                      Max_StreetBet: Int,
                      Min_CornerBet: Int,
                      Max_CornerBet: Int,
                      Min_LineBet: Int,
                      Max_LineBet: Int,
                      Min_Dozen_ColumnBet: Int,
                      Max_Dozen_ColumnBet: Int,
                      Min_OutsideBet: Int,
                      Max_OutsideBet: Int)
