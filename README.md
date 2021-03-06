# Blackjack Simulation

Considering the following rules for Blackjack, I'll make a function to simulate `n` runs.

The dealer starts dealing from their left to right. They dealt one card at a time until everyone gets two cards. All player cards are dealt face up and the dealer's first card is also face up, but the second one is dealt face down.

The player on the left goes first and can choose between `stand` or `hit`. If they stand they don't get any other card whereas if they hit they can get another card. If a player goes over 21 they `bust`. In this case, the player automatically loses. When the first players finishes their actions, the dealers serve the next player on their left.

After all, players have been served, the dealer starts by turning the face-down card up. If its total is 17 or more, it must stand. Else the dealer will hit, the dealer will continue to hit until they have a total of 17 or more. If the dealer has an ace and counting it as 11 would bring the total to 17 or more but less than 21, they must count it as 11 and stand.

Now for the shuffling of the cards, we can consider a couple of scenarios:

- shuffle every round.

- shuffle when there is only less than 75 cards left.

We also will be considering a few different types of players:

- **newbie** will play at random

- **cautious** will ask to hit when the total is below 17 and has a small chance (0.05) to hit when the total is 17, 18, or 19.

- **strategist** will ask to hit depending on the dealer's total (the dealer first card).

To run a shiny app with an application of this simulation just run: `runGitHub("blackjack-simulation", "devmedeiros", ref = "main")`.
