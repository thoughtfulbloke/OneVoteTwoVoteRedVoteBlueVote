#oneVoteTwoVoteRedVoteBlueVote.R
#a very, very rough MMP election simulator
#prior to being added to Github with and MIT licence, this had contributions from 
# David Hood
# Jonathan Marshall

#will need rdirichlet so making sure the gtools library is in place
if (!(require(gtools))) {
    install.packages("gtools")
    require(gtools)
}

# useful things to do:
# 1. Simulate electorates given some assumptions
 
party <- c("Nat", "Lab", "Green", "NZF", "Cons", "Maori", "ACT", "United", "Mana", "Other")
side <- c("n", "l", "l", "w", "n", "n", "n", "n", "l", "o")
 
# likely electorates
electorates <- c(1, 1, 0, 0, 0, 1, 1, 1, 0, 0);
 
simulate_election <- function() {
  # we want to estimate the vector P of probabilities within a bayesian framework
  # based on one (or more) polls, and then generate the posterior distribution for
  # P and use this to forward-simulate for potential outcomes
 
  # Given P, the poll is multinomial(size, P)
  # Thus, if we put a dirichlet prior on P, the posterior is also dirichlet
  # with parameter P + poll_results
 
  poll <- c(.47, .31, .11, .07, .023, 0.007, 0.003, 0.001, 0.000, 0.004)
  num_people <- round(poll * 767) # the best we can do as we don't know the weighting
  prior <- rep(1, length(num_people))
 
  # number of votes in the simulated election
  total_votes <- 2000000
 
  prop <- rdirichlet(1, num_people + prior)
  return(round(total_votes * prop))
}
 
allocate_seats <- function(votes, electorates) {
 
  # uses Sainte Laguë to allocate seats in an election, assuming
  # that each party gets at least electorates
  total_seats <- 120
 
  # now determine seats, given 125 available...
 
  # exclude parties that don't make the threshold
  prop <- votes / sum(votes);
  exclude <- prop < 0.05 & !electorates
  prop[exclude] <- 0;
  # renormalize to 1
  prop <- prop / sum(prop)
 
  # figure out total number of votes via Sainte Laguë
  divisors <- seq(1, by=2, length.out=total_seats)
 
  r <- rep(1:length(votes), length(divisors))
  d <- expand.grid(votes, divisors)
  o <- order(-round(d[,1] / d[,2]))
 
  seats <- rep(0, length(votes))
  t <- tabulate(r[o[1:total_seats]])
  seats[1:length(t)] <- t
  return(pmax(seats, electorates))
}
 
decide_winner <- function(seats) {
  # now decide party allegiance, assuming NZF is king-maker
  nseats <- sum(seats[side == "n"])
  lseats <- sum(seats[side == "l"])
  wseats <- sum(seats[side == "w"])
 
  if (nseats > (lseats + wseats))  {
    victory <- "national_led"
  } else if (lseats > (nseats + wseats))  {
    victory <- "labour_led"
  }  else if ((lseats == nseats) & (wseats == 0)){
    victory <- "hung"
  } else { victory <- "nzf_decides"}
  return(victory)
}
 
elect <- function(support) {
  votes <- simulate_election()
  seats <- allocate_seats(votes, electorates)
  return(decide_winner(seats))
}
 
many_elections <- 1000
outcomes <- rep("", many_elections)
for (i in 1:many_elections)
  outcomes[i] <- elect()
print("Results for many elections")
print(prop.table(table(outcomes)))
