# an attempt to simulate the Mines game on Stake.com
# note however that Stake appears to dynamically change their reward ratios,
# so this is not accurate for every round, merely the one that was picked

mines_sim <- function(n_rounds,start_cash,bet_size,n_picks){
  # start_cash you start with
  # n_rounds you play for
  # bet_size is minimum bet needed
  # n_picks is how many you try to pick per round
  # simulations run until rd_n = n_rounds or cash == 0
  cash<-start_cash
  rd_n<-1
  output <- c()
  increments_1 <- c(1.0313,1.0761,1.1250,1.1786,1.2375,
                    1.3026,1.3750,1.4559,1.5469,1.6500) # cash increment values correct for 1 mines
  while(rd_n<n_rounds){
    # assume 1 bomb per 25
    n_squares <- 25
    n_bombs <- 1
    picks <- 1
    while(picks < n_picks){ # start of round
      if(pick_square(n_squares,n_bombs)==FALSE){ # failure
        cash <- cash - bet_size # reduce cash
        break # break for loop
      }else{
        picks <- picks+1
      }
      if(picks==n_picks){ # didnt fail within N picks
        cash <- cash+(bet_size*increments_1[picks])
      }
    } # end of round
    
    if(cash<bet_size){ # have you run out of money?
      output <- c(rd_n,cash); return(output); break}
    rd_n <- rd_n+1
  }
  output <- c(rd_n,cash); return(output)
}


pick_square <- function(n_squares,n_bombs){
  # pick a square, is it a success?
  per_success <- 1-(n_bombs/n_squares)
  if(runif(1)<per_success){return(TRUE)}else{return(FALSE)}
}

mines_sim(n_rounds=20,start_cash=500,bet_size=2,n_picks=5)


# repeat simulation
simulate_mines <- function(params,n_reps){
  for(this_rep in 1:n_reps){
    this_sim <- mines_sim(params[1],params[2],params[3],params[4])
    this_row <- data.frame(n_rounds=params[1],
                           start_cash=params[2],
                           bet_size=params[3],
                           n_picks=params[4],
                           rounds_reached=this_sim[1],
                           final_cash=this_sim[2])
    mines_df <- rbind(mines_df,this_row)
    if(this_rep %% 100 == 0){print(this_rep)}
  }
  return(mines_df)
}

mines_df <- data.frame(n_rounds=as.numeric(),
                       start_cash=as.numeric(),
                       bet_size=as.numeric(),
                       n_picks=as.numeric(),
                       rounds_reached=as.numeric(),
                       final_cash=as.numeric())
# n_rounds,start_cash,bet_size,n_picks
params <- c(100,500,50,6)
out_df <- simulate_mines(params,1000)
hist(out_df$final_cash)
print(paste(length(out_df$final_cash>params[2])/length(out_df$final_cash)*100,"% of simulations make a profit",sep=""))
