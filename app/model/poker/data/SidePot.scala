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
* */
case class SidePot(ids: Seq[Int] = Seq.empty[Int],
                   capAmount: Double = 0,
                   fids: Seq[Int] = Seq.empty[Int],
                   foldAmount: Double = 0)
