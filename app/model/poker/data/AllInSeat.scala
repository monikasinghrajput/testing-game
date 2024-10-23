package model.poker.data

/*
* There are a lot of different scenarios that can materialize in poker games,
*   >>First question shall be,  cash games mode ?? | tournament mode ??
*
* Lets see "all-in" scenario
*   >>This feature is applicable only for no-limit poker
*
* The feature creates a chance for a player to only bet what is in front of them.
* A player go "all-in" in poker means, placing all his remaining chips into the middle of the pot
*
*
*  The first idea is
*     
*
* */
case class AllInSeat(id: Int, round: String, capAmount: Int = 0, betList: Seq[Bet])
