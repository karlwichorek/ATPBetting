library(dplyr)

files = list.files(path = "G:/R/99 PersonalRProjects/ATPBetting-master/Data/", 
                   pattern="*.csv")
match_data <- data.frame()

# Merge the data
for (i in 1:length(files)) {
  tmp <- read.csv(files[i], sep=",", stringsAsFactors = FALSE)
  
  # Vector of columns you want in this data.frame
  nms <- c("ATP","Location","Tournament","Date","Series","Court","Surface",
           "Round","Best of","Winner","Loser","WRank","LRank","WPts","LPts",
           "W1","L1","W2","L2","W3","L3","W4","L4","W5","L5",
           "Wsets","Lsets","Comment","B365W","B365L","PSW","PSL")
  
  Missing <- setdiff(nms, names(tmp))  # Find names of missing columns
  tmp[Missing] <- NA                   # Add them, filled with NAs
  tmp[Missing] <- sapply(tmp[Missing], as.numeric) # Convert new columns to numeric
  tmp <- tmp[nms]                      # Put columns in desired order
  tmp$LRank <- as.numeric(tmp$LRank)
  
  match_data <- rbind(match_data, tmp)
}

# Cleaning data
summary(match_data)

# Get rid of time zone characters in date
match_data$Date <- gsub(pattern = "T00:00:00Z", replacement = "", 
                        x = match_data$Date, fixed = T)
match_data$Date <- as.Date(match_data$Date, format = "%Y-%m-%d")

# Filter out Retired, Walkover, and Disqualified matches
match_data <- match_data[!match_data$Comment %in% c("Retired", "Walkover", "Disqualified"), ]

# Messed up the Best of variable

# Check for close values of player names

### Elo rankings data
# Given the list on matches in chronological order, for each match, computes 
# the elo ranking of the 2 players after the match
# Finally working!
players <- match_data[, c(10, 11)]
elo <- matrix(rep(1500, nrow(players)*2), nrow = nrow(players), ncol = 2)

for(i in 1:nrow(players)){
    
    # Find the last match for the winner and loser of the current match
    playA <- players[i, 1]
    playB <- players[i, 2]
    matchesA <- which(players[1:(i-1), ] == playA, arr.ind = TRUE)
    matchesB <- which(players[1:(i-1), ] == playB, arr.ind = TRUE)
    #matchcountA <- nrow(matchesA)
    #matchcountB <- nrow(matchesB)
    A.row <- matchesA[which.max(matchesA[, 1]), 1]
    A.col <- matchesA[which.max(matchesA[, 1]), 2]
    #kA <- 250/((matchcountA + 5) ^ 0.4)
    B.row <- matchesB[which.max(matchesB[, 1]), 1]
    B.col <- matchesB[which.max(matchesB[, 1]), 2]
    #kB <- 250/((matchcountB + 5) ^ 0.4)
    
    # Return the elo value from that instance
    eloA <- ifelse(length(A.row) == 0, 1500, elo[A.row, A.col])
    eloB <- ifelse(length(B.row) == 0, 1500, elo[B.row, B.col])
    
    # Calculate the prob of the winner winning, given the elo dif 
    # before the match
    pA <- 1 / (1 + 10 ^ ((eloB - eloA) / 400))
    pB <- 1 / (1 + 10 ^ ((eloA - eloB) / 400))
    
    # Return a new elo for each, given the match result
    new_eloA <- eloA + 40 * (1 - pA)
    new_eloB <- eloB + 40 * (0 - pB)
    
    # Update the elo values
    elo[i, 1] <- new_eloA
    elo[i, 2] <- new_eloB
}

# Pull in pre-match elo rankings and calculate odds of winning
odds.pre <- matrix(rep(NA, nrow(players)*4), nrow = nrow(players), ncol = 4)
for(i in 1:nrow(players)){
  
  # Find the last match for the winner and loser of the current match
  playA <- players[i, 1]
  playB <- players[i, 2]
  matchesA <- which(players[1:(i-1), ] == playA, arr.ind = TRUE)
  matchesB <- which(players[1:(i-1), ] == playB, arr.ind = TRUE)
  
  A.row <- matchesA[which.max(matchesA[, 1]), 1]
  A.col <- matchesA[which.max(matchesA[, 1]), 2]
  
  B.row <- matchesB[which.max(matchesB[, 1]), 1]
  B.col <- matchesB[which.max(matchesB[, 1]), 2]
  
  # Return the elo value from that instance
  eloA <- ifelse(length(A.row) == 0, 1500, elo[A.row, A.col])
  eloB <- ifelse(length(B.row) == 0, 1500, elo[B.row, B.col])
  odds.pre[i, 1] <- eloA
  odds.pre[i, 2] <- eloB
  
  # Calculate the prob of the winner winning, given the elo dif 
  # before the match
  pA <- 1 / (1 + 10 ^ ((eloB - eloA) / 400))
  pB <- 1 / (1 + 10 ^ ((eloA - eloB) / 400))
  
  # Enter the pre-match odds
  odds.pre[i, 3] <- pA
  odds.pre[i, 4] <- pB
}

odds.betsites <- matrix(rep(NA, nrow(players)*4), nrow = nrow(players), ncol = 4)
for(i in 1:nrow(players)){
  
  # Convert odds to probability of winning for current match
  playA.Bodds <- match_data[i, 29]
  playB.Bodds <- match_data[i, 30]
  
  playA.Btot <- 1/playA.Bodds + 1/playB.Bodds
  
  playA.Bprob <- 1/playA.Bodds/playA.Btot
  playB.Bprob <- 1 - playA.Bprob
  
  
  playA.Podds <- match_data[i, 31]
  playB.Podds <- match_data[i, 32]
  
  playA.Ptot <- 1/playA.Podds + 1/playB.Podds
  
  playA.Pprob <- 1/playA.Podds/playA.Ptot
  playB.Pprob <- 1 - playA.Pprob
  
  odds.betsites[i, 1] <- playA.Bprob
  odds.betsites[i, 2] <- playB.Bprob
  odds.betsites[i, 3] <- playA.Pprob
  odds.betsites[i, 4] <- playB.Pprob
}

# Update match data to include pre-match Elo and market odds
match_data$EloW <- odds.pre[, 1]
match_data$EloL <- odds.pre[, 2]

match_data$EloWProb <- odds.pre[, 3]
match_data$EloLProb <- odds.pre[, 4]

match_data$B365WProb <- odds.betsites[, 1]
match_data$B365LProb <- odds.betsites[, 2]

match_data$PSWProb <- odds.betsites[, 3]
match_data$PSLProb <- odds.betsites[, 4]



# Save work so far
saveRDS(match_data, "match_data.rds")
#match_data <- readRDS("match_data.rds")

# Filter out data that does not have odds from Pinnacle Sports
# B365 data is weird/incorrect at first because it lists odds as 1:1 for multiple matches  
bet_data <- filter(match_data, !is.na(PSW))

# Count number of times that the odds are higher than 90%
nrow(bet_data[bet_data$PSWProb >= 0.9 | bet_data$PSLProb >= 0.9, ])
nrow(bet_data[bet_data$PSWProb >= 0.9, ]) # Frequency of correct predictions

# Average probability of winning when odds are higher than 90%
ex_ante_prob90 <- c(bet_data[bet_data$PSWProb >= 0.9, 39], 
                    bet_data[bet_data$PSLProb >= 0.9, 40])
mean(ex_ante_prob90)

# Count number of times that the odds are between 80-90%
nrow(bet_data[bet_data$PSWProb >= 0.8 & bet_data$PSWProb < 0.9 | 
                bet_data$PSLProb >= 0.8 & bet_data$PSLProb < 0.9, ])
# Frequency of correct predictions
nrow(bet_data[bet_data$PSWProb >= 0.8 & bet_data$PSWProb < 0.9, ])

# Average probability of winning when odds are between 80-90%
ex_ante_prob80 <- c(bet_data[bet_data$PSWProb >= 0.8 & bet_data$PSWProb < 0.9, 39], 
                    bet_data[bet_data$PSLProb >= 0.8 & bet_data$PSLProb < 0.9, 40])
mean(ex_ante_prob80)

# Count number of times that the odds are between 70-80%
nrow(bet_data[bet_data$PSWProb >= 0.7 & bet_data$PSWProb < 0.8 | 
                bet_data$PSLProb >= 0.7 & bet_data$PSLProb < 0.8, ])
# Frequency of correct predictions
nrow(bet_data[bet_data$PSWProb >= 0.7 & bet_data$PSWProb < 0.8, ])

# Average probability of winning when odds are between 70-80%
ex_ante_prob70 <- c(bet_data[bet_data$PSWProb >= 0.7 & bet_data$PSWProb < 0.8, 39], 
                    bet_data[bet_data$PSLProb >= 0.7 & bet_data$PSLProb < 0.8, 40])
mean(ex_ante_prob70)

# Count number of times that the odds are between 60-70%
nrow(bet_data[bet_data$PSWProb >= 0.6 & bet_data$PSWProb < 0.7 | 
                bet_data$PSLProb >= 0.6 & bet_data$PSLProb < 0.7, ])
# Frequency of correct predictions
nrow(bet_data[bet_data$PSWProb >= 0.6 & bet_data$PSWProb < 0.7, ])

# Average probability of winning when odds are between 60-70%
ex_ante_prob60 <- c(bet_data[bet_data$PSWProb >= 0.6 & bet_data$PSWProb < 0.7, 39], 
                    bet_data[bet_data$PSLProb >= 0.6 & bet_data$PSLProb < 0.7, 40])
mean(ex_ante_prob60)

# Count number of times that the odds are between 50-60%
nrow(bet_data[bet_data$PSWProb >= 0.5 & bet_data$PSWProb < 0.6 | 
                bet_data$PSLProb >= 0.5 & bet_data$PSLProb < 0.6, ])
# Frequency of correct predictions
nrow(bet_data[bet_data$PSWProb >= 0.5 & bet_data$PSWProb < 0.6, ])

# Average probability of winning when odds are between 50-60%
ex_ante_prob50 <- c(bet_data[bet_data$PSWProb >= 0.5 & bet_data$PSWProb < 0.6, 39], 
                    bet_data[bet_data$PSLProb >= 0.5 & bet_data$PSLProb < 0.6, 40])
mean(ex_ante_prob50)

###############################################################################
# Check if the difference is significant for grand slam matches
gslams <- c("Australian Open", "Wimbledon", "US Open", "French Open")

# Segment data by grand slam and non-gs events
bet_data_gs <- subset(bet_data, Tournament %in% gslams)
bet_data_egs <- subset(bet_data, !(Tournament %in% gslams))

# Count number of times that the odds are higher than 90%
nrow(bet_data_gs[bet_data_gs$PSWProb >= 0.9 | bet_data_gs$PSLProb >= 0.9, ])
nrow(bet_data_gs[bet_data_gs$PSWProb >= 0.9, ]) # Frequency of correct predictions

# Average probability of winning when odds are higher than 90%
ex_ante_prob90 <- c(bet_data_gs[bet_data_gs$PSWProb >= 0.9, 39], 
                    bet_data_gs[bet_data_gs$PSLProb >= 0.9, 40])
mean(ex_ante_prob90)

# Count number of times that the odds are between 80-90%
nrow(bet_data_gs[bet_data_gs$PSWProb >= 0.8 & bet_data_gs$PSWProb < 0.9 | 
                bet_data_gs$PSLProb >= 0.8 & bet_data_gs$PSLProb < 0.9, ])
# Frequency of correct predictions
nrow(bet_data_gs[bet_data_gs$PSWProb >= 0.8 & bet_data_gs$PSWProb < 0.9, ])

# Average probability of winning when odds are between 80-90%
ex_ante_prob80 <- c(bet_data_gs[bet_data_gs$PSWProb >= 0.8 & bet_data_gs$PSWProb < 0.9, 39], 
                    bet_data_gs[bet_data_gs$PSLProb >= 0.8 & bet_data_gs$PSLProb < 0.9, 40])
mean(ex_ante_prob80)

# Count number of times that the odds are between 70-80%
nrow(bet_data_gs[bet_data_gs$PSWProb >= 0.7 & bet_data_gs$PSWProb < 0.8 | 
                bet_data_gs$PSLProb >= 0.7 & bet_data_gs$PSLProb < 0.8, ])
# Frequency of correct predictions
nrow(bet_data_gs[bet_data_gs$PSWProb >= 0.7 & bet_data_gs$PSWProb < 0.8, ])

# Average probability of winning when odds are between 70-80%
ex_ante_prob70 <- c(bet_data_gs[bet_data_gs$PSWProb >= 0.7 & bet_data_gs$PSWProb < 0.8, 39], 
                    bet_data_gs[bet_data_gs$PSLProb >= 0.7 & bet_data_gs$PSLProb < 0.8, 40])
mean(ex_ante_prob70)

# Count number of times that the odds are between 60-70%
nrow(bet_data_gs[bet_data_gs$PSWProb >= 0.6 & bet_data_gs$PSWProb < 0.7 | 
                bet_data_gs$PSLProb >= 0.6 & bet_data_gs$PSLProb < 0.7, ])
# Frequency of correct predictions
nrow(bet_data_gs[bet_data_gs$PSWProb >= 0.6 & bet_data_gs$PSWProb < 0.7, ])

# Average probability of winning when odds are between 60-70%
ex_ante_prob60 <- c(bet_data_gs[bet_data_gs$PSWProb >= 0.6 & bet_data_gs$PSWProb < 0.7, 39], 
                    bet_data_gs[bet_data_gs$PSLProb >= 0.6 & bet_data_gs$PSLProb < 0.7, 40])
mean(ex_ante_prob60)

# Count number of times that the odds are between 50-60%
nrow(bet_data_gs[bet_data_gs$PSWProb >= 0.5 & bet_data_gs$PSWProb < 0.6 | 
                bet_data_gs$PSLProb >= 0.5 & bet_data_gs$PSLProb < 0.6, ])
# Frequency of correct predictions
nrow(bet_data_gs[bet_data_gs$PSWProb >= 0.5 & bet_data_gs$PSWProb < 0.6, ])

# Average probability of winning when odds are between 50-60%
ex_ante_prob50 <- c(bet_data_gs[bet_data_gs$PSWProb >= 0.5 & bet_data_gs$PSWProb < 0.6, 39], 
                    bet_data_gs[bet_data_gs$PSLProb >= 0.5 & bet_data_gs$PSLProb < 0.6, 40])
mean(ex_ante_prob50)
