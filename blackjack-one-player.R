#should the dealer hit?
dealer <- function(dealer_total) {
  if (dealer_total >= 17) {
    F
  } else T
}

#should the player hit?
player <- function(player_total) {
  if (player_total < 21) {
    sample(c(T, F), 1)
  } else F
}

#a simple function to sum the cards
card_sum <- function(hand) {
  
  s_h <- 0 #high ace sum
  s_l <- 0 #low ace sum
  
  aces <- sum(hand == "A") #how many aces there is
  a <- 0 #a flag to count if the first ace was counted
  
  for (i in 1:length(hand)) {
    if (hand[i] %in% c("J", "Q", "K")) {
      s_h <- s_h + 10
      s_l <- s_l + 10
    } else if (hand[i] == "A") {
      if (aces == 1) {
        s_h <- s_h + 11
        s_l <- s_l + 1
      } else if (a == 0) {
        s_h <- s_h + 11
        s_l <- s_l + 1
        a <- a + 1
      } else if (a > 0) {
        s_h <- s_h + 1
        s_l <- s_l + 1
      }
    } else {
      s_h <- s_h + as.numeric(hand[i])
      s_l <- s_l + as.numeric(hand[i])
    }
  }
  if (s_h > 21) {
    return(s_l)
  } else {
    return(max(s_h, s_l))
  }
}
#deck_n #of decks
#deck_c current cards in the deck
#players #of players
play_one_round <- function(deck_n, players, deck_c) {
  
  if (length(deck_c) < 75){
    #shuffle the deck
    #aka start with a new deck
    cards <- rep(c(2:10, "J", "Q", "K", "A"), 4) #cards in a deck
    total_cards <- rep(cards, deck_n) #total cards in the game
  } else {
    #start with the current deck
    total_cards <- deck_c
  }
  
  #creating the objects
  cards <- rep(c(2:10, "J", "Q", "K", "A"), 4) #cards in a deck
  total_cards <- rep(cards, deck) #total cards in the game
  total_cards_played <- 0
  #shuffle <- F #should the cards be shuffled?
  players_name <- paste(rep("player", players), 1:players, sep="")
  players_na <- rep(NA, players) #first value for players
  players_win_los <- rep(0, players) #1 = player lost
  
  #a list with round, player1, player2, ..., dealer
  setup_list <- as.list(setNames(c(1, players_na, NA), c("round", players_name, "dealer")))
  #a list with the sum from players and dealer cards
  sum_list <- as.list(setNames(c(1, players_na, NA), c("round", players_name, "dealer")))
  #a list with the information about the win and loss of the players
  win_los <- as.list(setNames(c(1, players_win_los),c("round", players_name)))
  
  sample_card <- NA
  n <- players + 2 #to iterate on players and dealer
  m <- players + 1 #to iterate on players
  
  #dealing the first hand
  for (i in 1:2) {
    for (j in 2:n) {
      sample_card <- sample(total_cards, 1)
      total_cards_played <- total_cards_played + 1
      setup_list[[j]][i] <- sample_card
      #removing the card dealt from the pool of possible cards
      total_cards <- total_cards[-which(total_cards == sample_card)[1]]
    }
  }
  
  #sum of the player's hand
  for (u in 2:m) {
    sum_list[[u]] <- card_sum(setup_list[[u]])
  }
  
  #sum of the dealer's hand (both cards)
  sum_list$dealer <- card_sum(setup_list$dealer)
  
  #how will the players act?
  for (v in 2:m) {
    k <- 2 #is 2 because the starting hand is 2 cards
    sample_card <- NA
    while (player(sum_list[[v]])) {
      k <- k + 1
      sample_card <- sample(total_cards, 1)
      total_cards_played <- total_cards_played + 1
      setup_list[[v]][k] <- sample_card #adding the new card in the setup_list
      total_cards <- total_cards[-which(total_cards == sample_card)[1]]
      sum_list[[v]] <- card_sum(setup_list[[v]]) #updating the sum
    }
    #checking if the v player has burst
    if (sum_list[[v]] > 21) {
      win_los[[v]] <- 1
    }
  }
  
  k <- 2
  sample_card <- NA
    
  #dealer acting
  while (dealer(sum_list$dealer)) {
    total_cards_played <- total_cards_played + 1
    k <- k + 1
    sample_card <- sample(total_cards, 1)
    setup_list$dealer[k] <- sample_card
    total_cards <- total_cards[-which(total_cards == sample_card)[1]]
    sum_list$dealer <- card_sum(setup_list$dealer)
  }
    
  if (sum_list$dealer <= 21) {
    for (l in 2:m) {
      if (sum_list$dealer >= sum_list[[l]]) {
        win_los[[l]] <- 1
      } 
    }
  }
  
  len <- 0
  
  for (t in 1:n) {
    len[t] <- length(setup_list[[t]])
  }
  
  setup_list$round[2:max(len)] <- 1
  
  setup <- as.data.frame(lapply(setup_list, "length<-", max(lengths(setup_list))))
  output <- list("total_cards" = total_cards, "setup" = setup, "lost" = as.data.frame(win_los))
  return(output)
}

play_multiple_rounds <- function(deck, players, rounds){
  
}
