#should the dealer hit?
dealer <- function(dealer_total){
  if(dealer_total >= 17){
    F
  } else T
}

#should the player hit?
player <- function(player_total){
  if(player_total < 21){
    sample(c(T,F),1)
  } else F
}

#a comlpete function to sum the cards
#there is no case where we have two high aces, but we can have up to 21 low aces
card_sum <- function(hand){
  
  s_h <- 0 #high ace sum
  s_l <- 0 #low ace sum
  
  aces <- sum(hand == "A") #how many aces there is
  a <- 0 #a flag to count if the first ace was counted
  
  for(i in 1:length(hand)){
    if(hand[i] %in% c("J", "Q", "K")){
      s_h <- s_h + 10
      s_l <- s_l + 10
    } else if(hand[i] == "A"){
      if(aces == 1){
        s_h <- s_h + 11
        s_l <- s_l + 1
      } else if(a == 0){
        s_h <- s_h + 11
        s_l <- s_l + 1
        a <- a + 1
      } else if(a > 0){
        s_h <- s_h + 1
        s_l <- s_l + 1
      }
    } else {
      s_h <- s_h + as.numeric(hand[i])
      s_l <- s_l + as.numeric(hand[i])
    }
  }
  if(s_h > 21){
    return(s_l)
  } else {
    return(max(s_h,s_l))
  }
}

#play one round with as many decks as you wish
play_one_round <- function(deck){
  
  cards <- rep(c(2:10, "J", "Q", "K", "A"), 4)
  total_cards <- rep(cards,deck) #total cards being played
  
  #each row in the setup is the 'row' round of carts dealt
  setup <- data.frame('player1' = NA, 'dealer' = NA)
  
  #if the player has lost yet
  lost <- F
  
  sample_card <- NA
  
  #sampling the cards and taking them off the total_cards
  for(i in 1:2){
    for(j in 1:2){
      sample_card <- sample(total_cards, 1)
      setup[i,j] <- sample_card
      total_cards <- total_cards[-which(total_cards == sample_card)]
    }
  }
  
  player_total <- card_sum(setup$player1)
  dealer_total <- card_sum(setup$dealer)
  
  #is 2 because each player starts with 2 cards
  k <- 2
  
  sample_card <- NA
  
  cat(paste0("The player total is: ", player_total), sep="\n")
  cat(paste0("The dealer total is: ", card_sum(setup[1,2])), sep="\n")
  
  #player acts
  while(player(player_total)){
    cat(paste0("Player Hits."), sep="\n")
    k <- k + 1
    sample_card <- sample(total_cards, 1)
    setup[k,1] <- sample_card
    total_cards <- total_cards[-which(total_cards == sample_card)]
    player_total <- card_sum(setup$player1)
    cat(paste0(sample_card,"\n" ,"Player new total is: ", player_total), sep="\n")
  }
  
  #test if the player has burst
  if(player_total > 21){
    cat(paste0("Player Burst and the dealer wins."))
    lost <- T
    #if the player is still in the game continue
  } else {
    cat(paste0("Player Stands. With a total of: ", player_total), sep="\n")
    cat(paste0("Dealer turns card, dealer total is: ", dealer_total), sep="\n")
  
    k <- 2
  
    sample_card <- NA
  #dealer acts
    while(dealer(dealer_total)){
      cat(paste0("Dealer Hits."), sep="\n")
      k <- k + 1
      sample_card <- sample(total_cards, 1)
      setup[k,2] <- sample_card
      total_cards <- total_cards[-which(total_cards == sample_card)]
      dealer_total <- card_sum(setup$dealer)
      cat(paste0(sample_card, "\n","Dealer new total is: ", dealer_total), sep="\n")
    }
    
    cat(paste0("Dealer Stands. With a total of: ", dealer_total), sep="\n")
    
    #checking who won
    if(dealer_total > 21){
      cat(paste0("The dealer Burst. The player wins."))
    } else if(dealer_total >= player_total){
      cat(paste0("Dealer wins."))
      lost = T #now this is redundant
    }
  }
}
