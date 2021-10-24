#should the dealer hit?
dealer <- function(dealer_total) {
  if (dealer_total >= 17) {
    F
  } else T
}

################################################################################

#should the player hit? (newbie)
newbie <- function(player_total) {
  if (player_total < 21) {
    return(sample(c(T, F), 1))
  } else return(F)
} #nb

cautious <- function(player_total) {
  u <- runif(1)
  if (player_total < 17){
    return(T)
  } else if (player_total < 19 & u < 0.05) {
    return(T)
  } else return(F)
} #ct

strategist <- function(player_total, dealer_total) {
  if (player_total >= 17) {
    return(F)
  } else if (player_total >= 13 & dealer_total <= 6) {
    return(F)
  } else if (player_total == 12 & dealer_total %in% 4:6) {
    return(F)
  } else if (player_total <= 11) {
    return(T)
  } else return(F)
} #st

################################################################################

#a function to sum the cards
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

################################################################################

#deck_n #of decks
#players #of players

play_one_round <- function(deck_n, players, deck_c = 0, archetype = rep("nb", players)) {
  
  if (length(deck_c) < 75){
    #shuffle the deck
    cards <- rep(c(2:10, "J", "Q", "K", "A"), 4) #cards in a deck
    total_cards <- rep(cards, deck_n) #total cards in the game
    total_cards_played <- 0
  } else {
    #start with the current deck
    total_cards <- deck_c
    total_cards_played <- (52*deck_n)-length(deck_c)
  }
  
  #uncomment this to run for as many players as you want
  #players_name <- paste(rep("player", players), 1:players, sep="")
  
  #comment this to run for as many players as you want
  players_name <- paste(rep("player", players), archetype, sep=" ")
  
  players_na <- rep(NA, players) #first value for players
  players_win_los <- rep(0, players) #1 = player lost
  
  #a list with round, player1, player2, ..., dealer
  setup_list <- as.list(setNames(c(players_na, NA), c(players_name, "dealer")))
  #a list with the sum from players and dealer cards
  sum_list <- as.list(setNames(c(players_na, NA), c(players_name, "dealer")))
  #a list with the information about the win and loss of the players
  win_los <- as.list(setNames(c(players_win_los),c(players_name)))
  
  sample_card <- NA
  n <- players + 1 #to iterate on players and dealer
  
  #dealing the first hand
  for (i in 1:2) {
    for (j in 1:n) {
      sample_card <- sample(total_cards, 1)
      total_cards_played <- total_cards_played + 1
      setup_list[[j]][i] <- sample_card
      #removing the card dealt from the pool of possible cards
      total_cards <- total_cards[-which(total_cards == sample_card)[1]]
    }
  }
  
  #sum of the player's hand
  for (u in 1:players) {
    sum_list[[u]] <- card_sum(setup_list[[u]])
  }
  
  #sum of the dealer's hand (both cards)
  sum_list$dealer <- card_sum(setup_list$dealer)
  
  #how will the players act?
  for (v in 1:players) {
    k <- 2 #is 2 because the starting hand is 2 cards
    sample_card <- NA
    if (archetype[v] == "nb") {
      while (newbie(sum_list[[v]])) {
        k <- k + 1
        sample_card <- sample(total_cards, 1)
        total_cards_played <- total_cards_played + 1
        setup_list[[v]][k] <- sample_card #adding the new card in the setup_list
        total_cards <- total_cards[-which(total_cards == sample_card)[1]]
        sum_list[[v]] <- card_sum(setup_list[[v]]) #updating the sum
      }
    } else if (archetype[v] == "ct") {
      while (cautious(sum_list[[v]])) {
        k <- k + 1
        sample_card <- sample(total_cards, 1)
        total_cards_played <- total_cards_played + 1
        setup_list[[v]][k] <- sample_card #adding the new card in the setup_list
        total_cards <- total_cards[-which(total_cards == sample_card)[1]]
        sum_list[[v]] <- card_sum(setup_list[[v]]) #updating the sum
      } 
    } else if (archetype[v] == "st") {
      while (strategist(sum_list[[v]], setup_list$dealer[1])) {
        k <- k + 1
        sample_card <- sample(total_cards, 1)
        total_cards_played <- total_cards_played + 1
        setup_list[[v]][k] <- sample_card #adding the new card in the setup_list
        total_cards <- total_cards[-which(total_cards == sample_card)[1]]
        sum_list[[v]] <- card_sum(setup_list[[v]]) #updating the sum
      }
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
    for (l in 1:players) {
      if (sum_list$dealer >= sum_list[[l]]) {
        win_los[[l]] <- 1
      } 
    }
  }
  
  setup <- as.data.frame(lapply(setup_list, "length<-", max(lengths(setup_list))))
  output <- list("total_cards" = total_cards,
                 "setup" = setup,
                 "lost" = as.data.frame(win_los),
                 "archetype" = archetype)
  return(output)
}

################################################################################

play_multiple_rounds <- function(deck_n, players, rounds, archetype){
  
  temp <- list()
  setup <- data.frame()
  lost <- c()
  
  for (r in 1:rounds) {
    if (r == 1) {
      temp <- play_one_round(deck_n, players, archetype = archetype)
      setup <- cbind(round = 1, temp$setup)
      lost <- cbind(round = 1, temp$lost)
    } else {
      temp <- play_one_round(deck_n, players, temp$total_cards, archetype = archetype)
      setup <- rbind(setup, cbind(round = r, temp$setup))
      lost <- rbind(lost, cbind(round = r, temp$lost))
    }
  }
  
  output <- list('last_deck' = temp$total_cards,
                 'setup' = setup,
                 'lost' = lost,
                 'archetype' = archetype)
  
  return(output) 
}
