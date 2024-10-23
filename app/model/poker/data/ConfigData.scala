package model.poker.data

case class ConfigData(pokerVariant: String = "Texas",
                      betLimit: String = "No Limit",
                      liveDealer: Boolean = false,
                      tournamentMode: Boolean = true,
                      playerCardsConfig: CardsConfig = CardsConfig(),
                      communityCardsConfig: CardsConfig = CardsConfig(count = 5, min = 3, max = 5),
                      rakePercent: Int = 0,
                      blind: Int = 100)
