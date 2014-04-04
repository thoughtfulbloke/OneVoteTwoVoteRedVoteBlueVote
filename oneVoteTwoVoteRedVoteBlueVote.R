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

# a poll result - should use the same number of parties as above
colmar_brunton_party_vote <- c(.47, .31, .11, .07, .023, 0.007, 0.003, 0.001, 0.000, 0.004)
colmar_brunton_num_polled <- 767

#
# election_from_poll(poll, num_polled)
#
# simulates an election given a single poll.
#
# poll        the proportions for each party.
# num_polled  the number of respondents on the polls (excluding "don't know")
# turnout     the total number of votes to simulate in the election, defaults to 2000000.
#
election_from_poll <- function(poll, num_polled, turnout = 2000000) {

  # if P is the true proportions, then the poll is multinomial(num_polled, P)
  # if we assume a Dirichlet(alpha) prior, the posterior is also dirichlet
  # due to conjugacy, with paramater alpha + votes_in_poll.

  votes_in_poll <- round(poll * num_polled) # the best we can do as we don't know the weighting
  prior <- rep(1, length(votes_in_poll))    # equivalent to adding a vote for each party

  prop <- rdirichlet(1, votes_in_poll + prior)
  return(round(turnout * prop))
}

#
# allocate_seats(votes, electorates)
#
# Allocates seats in an MMP parliament using the Sainte Laguë
# system as employed in New Zealand.
#
# votes        the number of votes in the election per-party.
# electorates  the number of electorates won per-party.
#
allocate_seats <- function(votes, electorates) {
  total_seats <- 120

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

#
# decide_winner(seats)
#
# Decides the winner of an election, given the seat allocation.
#
# seats  the allocation of seats in parliament
#
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

many_elections <- 1000
outcomes <- rep("", many_elections)
for (i in 1:many_elections)
{
  votes <- election_from_poll(colmar_brunton_party_vote, colmar_brunton_num_polled)
  seats <- allocate_seats(votes, electorates)
  outcomes[i] <- decide_winner(seats)
}
print("Results for many elections")
print(prop.table(table(outcomes)))
