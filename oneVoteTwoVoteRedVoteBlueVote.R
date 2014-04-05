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
 
#
# election_from_poll(poll, num_polled, turnout = 2000000)
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
# election_from_poll_of_polls(proportions, confidence, turnout = 2000000)
#
# simulates an election given poll of polls results.
#
# proprtions  the proportions for each party.
# confidence  the number of respondents on the polls (excluding "don't know")
# turnout     the total number of votes to simulate in the election, defaults to 2000000.
#
election_from_poll_of_polls <- function(proportions, confidence, turnout = 2000000) {

  # We assume the confidence intervals given are normal and symmetric.
  # The proportions no doubt add to 1, so are not independent. We get around
  # this by sampling from the smaller parties first, and then setting the
  # large party to one minus the total. This gets rid of the problem with
  # proportions adding to 1, and also gets dependence between the largest
  # party and the rest. It doesn't take into account dependence between
  # smaller parties.

  # normalize the proportion results
  s <- sum(proportions)

  proportions <- proportions / s

  sd <- confidence / 1.96 / s # assumes 95% confidence intervals

  o  <- order(proportions)
  o_small <- o[-length(o)]

  p <- rep(0, length(proportions))
  p[o_small] <- rnorm(length(o_small), proportions[o_small], sd[o_small])

  # TODO: ensure that p is non-negative...

  # largest party is 1-sum of others
  p[o[length(o)]] <- 1 - sum(p)

  return(round(turnout * p))
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
  exclude <- votes / sum(votes) < 0.05 & !electorates
  votes[exclude] <- 0;

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
# sides  which side each party is likely to go with (n, l, w)
#
decide_winner <- function(seats, sides) {
  # now decide party allegiance, assuming NZF is king-maker
  nseats <- sum(seats[sides == "n"])
  lseats <- sum(seats[sides == "l"])
  wseats <- sum(seats[sides == "w"])

  if (nseats > (lseats + wseats))  {
    victory <- "national_led"
  } else if (lseats > (nseats + wseats))  {
    victory <- "labour_led"
  }  else if ((lseats == nseats) & (wseats == 0)){
    victory <- "hung"
  } else { victory <- "nzf_decides"}
  return(victory)
}

#
# plot_seats(seats, party)
#
# Plots the distribution of seats for each party
#
# seats  the simulated distribution of seats
# party  the information on each party
#
plot_seats <- function(seats, party) {
  png("seats.png", width=600, height=800)
  par(mfrow = c(nrow(party), 1), mai=rep(0.125,4), omi=rep(0.25,4))
  max_seats <- max(seats)
  o         <- order(-colMeans(seats))
  for (i in o) {
    h <- hist(seats[,i], col=rep(party$Contrast[i], length(t)), main="", border=NA, xlim=c(0,max_seats), breaks=-1:max_seats+0.5, axes=F)
    lab <- which.max(h$counts)-1
    off <- as.numeric(lab < max_seats/2)
    lab <- lab + (off-0.5)*2
    text(lab, max(h$counts)*0.9, rownames(party)[i], adj=1-off, col=party$Colour[i], cex=1.2)
    axis(1, at=c(-10, max_seats+10), labels=rep("" ,2))
    for (j in seq(0,max_seats,by=5))
      mtext(j, at=j, side=1, line=0, cex=0.7)
  }
  dev.off()
}

#
# plot_scenarios(seats, sides)
#
# Plots the scenarios of governing, given the distribution of seats
# and the sides of each party. Plots left, right, left+middle, right+middle.
#
# seats  the distribution of seats
# party  the information on each party
#
plot_scenarios <- function(seats, party)
{
  png("scenarios.png", width=600, height=400)
  par(mfrow = c(4, 1), mai=rep(0.125,4), omi=rep(0.25,4))

  r  <- rowSums(seats[,party$Side=="n"]) - rowSums(seats[,party$Side!="n"])
  l  <- rowSums(seats[,party$Side=="l"]) - rowSums(seats[,party$Side!="l"])
  rw <- rowSums(seats[,party$Side=="n" | party$Side=="w"]) - rowSums(seats[,party$Side!="n" & party$Side!="w"])
  lw <- rowSums(seats[,party$Side=="l" | party$Side=="w"]) - rowSums(seats[,party$Side!="l" & party$Side!="w"])

  scen   <- cbind(l, r, lw, rw)
  col    <- rep(c("#FF0000", "#00529F"), 2)
  con    <- rep(c("#FFBAA8", "#CCDDFF"), 2)
  labels <- c("Left alone", "Right alone", "Left + NZFirst", "Right + NZFirst")

  xlim <- range(scen)
  xlab <- seq(-60,60,by=5)
  xlab <- xlab[xlab >= xlim[1] & xlab <= xlim[2]]
  for (i in 1:ncol(scen))
  {
    h <- hist(scen[,i], col=con[i], main="", border=NA, xlim=xlim, breaks=(xlim[1]-1):xlim[2]+0.5, axes=F, yaxs="i", xaxs="i")
    lab <- h$mids[which.max(h$counts)]
    off <- as.numeric(lab < 0)
    lab <- lab + (off-0.5)*6
    text(xlim[2], max(h$counts), paste(round(sum(scen[,i] > 0) / many_elections * 100),"%",sep=""), cex=3, adj=c(1,1), col=col[i])
    text(lab, max(h$counts)*0.9, labels[i], adj=1-off, col=col[i], cex=1.2)
    axis(1, at=c(-60,60), labels=rep("" ,2), yaxs="i", cex=0.7)
    for (j in xlab)
      mtext(j+60, at=j, side=1, line=0, cex=0.7)
    abline(v=0.5)
  }
  dev.off()
}

# Party setup
party_ctl <- read.table(
  header = TRUE,
  row.names = "Party",
  stringsAsFactors = FALSE,
  fill = TRUE,
  comment = "",
  text = "

  Party           Colour   Contrast Include  Side  Electorate
  ACT             #FFCB05  #FFFF80    Y        n        1
  Conservative    #00AEEF  #33CCFF    Y        n        0
  Destiny         #FF0000  #000000
  Green           #098137  #B3FFB3    Y        l        0
  Labour          #FF0000  #FFBAA8    Y        l       22
  Mana            #770808  #FF6E6E    Y        l        1
  Maori           #EF4A42  #FFCC80    Y        n        3
  National        #00529F  #CCDDFF    Y        n       42
  'NZ First'      #000000  #CCCCCC    Y        w        0
  Progressive     #9E9E9E  #DDCCDD
  'United Future' #501557  #DD99DD    Y        n        1
")
party_ctl[, "Include"] <- (party_ctl[, "Include"] == "Y")
rownames(party_ctl)[match("Maori", rownames(party_ctl))] <- "M\u0101ori"
party <- party_ctl[party_ctl[, "Include"],]


# perform the simulation for the Colmar Brunton poll

poll_source_description   <- "source and date of poll should go here"
colmar_brunton_party_vote <- c(.003, .023, .11, .31, 0, .007, .47, .07, .001)
colmar_brunton_num_polled <- 767

many_elections <- 1000
outcomes <- rep("", many_elections)
for (i in 1:many_elections)
{
  votes <- election_from_poll(colmar_brunton_party_vote, colmar_brunton_num_polled)
  seats <- allocate_seats(votes, party$Electorate)
  outcomes[i] <- decide_winner(seats, party$Side)
}
print("Results for many elections")
print(prop.table(table(outcomes)))

#This is a fairly round about way of getting outcomes to produce a standard graph

showAGraph <- function(chances, details) {
  merge_id <- c("hung", "labour_led", "national_led", "nzf_decides")
  axis_text <- c("Hung Parliament", "Labour led\nw/o NZF", "national led\nw/o NZF", "Up to NZF")
  colourscheme <- c("#FF00FFFF","#FF0000FF","#0000FFFF","#000000FF")
  all_outcomes <- data.frame(merge_id,axis_text, colourscheme)
  poll_outcomes <- data.frame(chances)
  graphdata <- merge(all_outcomes, poll_outcomes, by.x="merge_id", by.y="outcomes", all.x=TRUE)
  print(str(graphdata))
  graphdata$Freq <- graphdata$Freq * 100
  graphdata$Freq[is.na(graphdata$Freq)] <- 0
  barplot(graphdata$Freq, names.arg=graphdata$axis_text, main="If the poll is accurate,\nthe election would be", cex.names=0.6, col= as.character(graphdata$colourscheme), ylab="Results Percentage", sub=details)
}

makePAsizedGraph <- function(chances, details) {
  png(filename = paste("election_estimate_", Sys.time(),".png",sep=""), width = 420, height = 315)
  showAGraph(chances, details)
  dev.off()
}

showAGraph(prop.table(table(outcomes)),poll_source_description)
#makePAsizedGraph(prop.table(table(outcomes)),poll_source_description)
#uncomment the above line to have it save convenient PA sized graphs in the current working directory.

# simulation from poll of polls. From the 'final' variable in Peter Green's script

party_votes <- c(0.6, 3.0, 10.7, 29.9, 0.9, 1.2, 44.1, 7.9, 0.6)
party_cis   <- c(0.3, 0.4,  1.3,  1.8, 0.2, 0.5,  2.0, 0.9, 0.1)

many_elections <- 1000
outcomes <- rep("", many_elections)
seats <- matrix(0, many_elections, nrow(party))
for (i in 1:many_elections)
{
  votes <- election_from_poll_of_polls(party_votes, party_cis)
  seats[i,] <- allocate_seats(votes, party$Electorate)
  outcomes[i] <- decide_winner(seats[i,], party$Side)
}
print("Results for many elections")
print(prop.table(table(outcomes)))

plot_seats(seats, party)
plot_scenarios(seats, party)
