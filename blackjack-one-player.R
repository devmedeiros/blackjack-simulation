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

#play one round with as many decks as you wish and as many players
play_one_round <- function(deck, players){
  
  cards <- rep(c(2:10, "J", "Q", "K", "A"), 4)
  total_cards <- rep(cards,deck) #total cards being played
  total_cards_played <- 0
  
  #each row in the setup is the 'row' round of carts dealt
  
  a <- paste(rep("player",players),1:players,sep="")
  b <- rep(NA,players)
  c <- rep(F,players)
  setup_list <- as.list(setNames(c(1,b,NA),c("round",a,"dealer")))
  sum_list <- as.list(setNames(c(1,b,NA),c("round",a,"dealer")))
  win_los <- as.list(setNames(c(1,c),c("round",a)))
  #if the player has lost yet
  
  sample_card <- NA
  n <- players+2
  m <- players+1
  
  #sampling the cards and taking them off the total_cards
  for(i in 1:2){
    for(j in 2:n){
      sample_card <- sample(total_cards, 1)
      total_cards_played <- total_cards_played + 1
      setup_list[[j]][i] <- sample_card
      total_cards <- total_cards[-which(total_cards == sample_card)[1]]
    }
  }
  
  for(u in 2:m){
    sum_list[[u]] <- card_sum(setup_list[[u]])
  }
  
  sum_list$dealer <- card_sum(setup_list$dealer)
 
  #cat(paste0("The player total is: ", player_total), sep="\n")
  #cat(paste0("The dealer total is: ", card_sum(setup_list[[n]][1])), sep="\n")
  
  #player acts
  for(v in 2:m){
    k <- 2
    sample_card <- NA
    while(player(sum_list[[v]])){
      cat(paste0("Player Hits."), sep="\n")
      k <- k + 1
      sample_card <- sample(total_cards, 1)
      total_cards_played <- total_cards_played + 1
      setup_list[[v]][k] <- sample_card
      total_cards <- total_cards[-which(total_cards == sample_card)[1]]
      sum_list[[v]] <- card_sum(setup_list[[v]])
      cat(paste0(sample_card,"\n" ,"Player new total is: ", sum_list[[v]]), sep="\n")
    }
    if(sum_list[[v]] > 21){
      cat(paste0("Player ", a[v-1]," Burst and the dealer wins."))
      win_los[[v]] <- T
    } else {
      cat(paste("Player ", a[v-1]," Stands."))
    }
  }
  
    k <- 2
    sample_card <- NA
    
  #dealer acts
    while(dealer(sum_list$dealer)){
      total_cards_played <- total_cards_played + 1
      cat(paste0("Dealer Hits."), sep="\n")
      k <- k + 1
      sample_card <- sample(total_cards, 1)
      setup_list$dealer[k] <- sample_card
      total_cards <- total_cards[-which(total_cards == sample_card)[1]]
      sum_list$dealer <- card_sum(setup_list$dealer)
      cat(paste0(sample_card, "\n","Dealer new total is: ", sum_list$dealer), sep="\n")
    }
    
    cat(paste0("Dealer Stands. With a total of: ", sum_list$dealer), sep="\n")
    
    #checking who won
    if(sum_list$dealer > 21){
      cat(paste0("The dealer Burst. The remaining players wins."), sep="\n")
    } else {
      for(l in 2:m){
        if(sum_list$dealer >= sum_list[[l]]){
          cat(paste0("Dealer wins. Against player ", a[l-1]), sep="\n")
          win_los[[l]] <- T #now this is redundant
      } else if(sum_list[[l]] > sum_list$dealer){
          cat(paste0("Player ", a[l-1]," wins."), sep="\n")
        }
      }
    }
    return(length(total_cards))
  }
