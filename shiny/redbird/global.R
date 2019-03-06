library(shiny)
library(shinysky)
library(DT)
library(dplyr)
library(ggvis)
library(stringr)

# Number of teams in the league
tms <- 18

# Import batters and pitchers
batters <- read.csv("./data/batters.csv")
pitchers <- read.csv("./data/pitchers.csv")

# Select variables using a standard naming and switch first and last names
b <- batters %>% 
  select(PlayerName, Team, POS, PA.IP = PA, ADP, Dollars) %>% 
  mutate(PlayerName = paste0(word(PlayerName, -1), 
                             ", ", word(PlayerName, 1, -2)))

p <- pitchers %>% 
  select(PlayerName, Team, POS, PA.IP = IP, ADP, Dollars) %>% 
  mutate(PlayerName = paste0(word(PlayerName, -1),
                             ", ", word(PlayerName, 1, -2)))
# Format Dollars 
all.players <- rbind(b, p) %>%  # Combine batters and pitchers data
  mutate(Dollars = as.numeric(gsub(" ", "", chartr('$)(', '  -', Dollars)))) %>%
  arrange(desc(Dollars)) %>%  # Sort by dollars descending
  dplyr::mutate(Ovr.Rank = 1:n())  # Create ranking id

player.tier <- function(pos = "C", spts = 1, flr = 200) {
  # Assigns tiers to each player at each position 1-5 (best - worst)
  #
  # Args:
  #   pos: Position.
  #   spts: Starting spots on the fantasy roster
  #   flr: The minimum number of PA or IP required to be above tier 5
  #
  # Returns:
  #   The dataframe for that position with a new column corresponding to tier.
  fil <- all.players %>%  # Copy data
   filter(grepl(pos, POS) == TRUE) %>%  # Filter for the position
   mutate(rank = rank(-Dollars),  # Create position rank
          Tier = ifelse(PA.IP < flr, 5,  # Create initial tiers
                        ifelse( rank > (tms * spts), 4, 1)))
  
  # Create separate data frames for top tiers and bottom tiers
  fil.top <- fil[fil$Tier == 1, ]
  fil.bottom <- fil[fil$Tier != 1, ]
  
  # Cluster for tiers 1 - 3 
  clusters <- kmeans(fil.top$Dollar, 3, iter.max = 1000, nstart = 10)
  
  # Create a table of assignments to clusters
  cluster.assignments <- data.frame(clusters$cluster, 
                                   1:length(clusters$cluster))
  names(cluster.assignments) <- c("c", "order")
  
  # Have clusters named in numerical order
  cluster.names <- data.frame(1:3, rank(-clusters$centers))
  names(cluster.names) <- c("c", "cluster")
  
  # Merge assignments and custom names and order
  tiers.top <- inner_join(cluster.assignments, cluster.names, by = "c") %>%
   select(cluster)
  
  # Overwrite tier with cluster
  fil.top$Tier <- tiers.top$cluster
  
  #Recombine top and bottom tier groups
  fil.final <- rbind(fil.top, fil.bottom) %>%
   select(-rank)  # Drop rank column
  
  # Rename tier based on position
  colnames(fil.final)[8] <- paste("Tier", pos, sep = ".")
  
  # Return tiers for the selected position
  fil.final
}

# Join tier assignments and order by dollars
tier.data <- full_join(player.tier("C", 1, 200),
                       player.tier("1B", 1, 200),
                       by = c("PlayerName", "Team", "POS", 
                              "PA.IP", "Dollars", "Ovr.Rank", "ADP")) %>%
  full_join(., player.tier("2B", 1, 200), by = c("PlayerName", "Team", "POS", 
                                                 "PA.IP", "Dollars", 
                                                 "Ovr.Rank", "ADP")) %>%
  full_join(., player.tier("3B", 1, 200), by = c("PlayerName", "Team", "POS", 
                                                 "PA.IP", "Dollars", 
                                                 "Ovr.Rank", "ADP")) %>%
  full_join(., player.tier("SS", 1, 200), by = c("PlayerName", "Team", "POS", 
                                                 "PA.IP", "Dollars", 
                                                 "Ovr.Rank", "ADP")) %>%
  full_join(., player.tier("OF", 3, 200), by = c("PlayerName", "Team", "POS", 
                                                 "PA.IP", "Dollars", 
                                                 "Ovr.Rank", "ADP")) %>%
  full_join(., player.tier("SP", 4, 50), by = c("PlayerName", "Team", "POS", 
                                                 "PA.IP", "Dollars", 
                                                 "Ovr.Rank", "ADP")) %>%
  full_join(., player.tier("RP", 1, 50), by = c("PlayerName", "Team", "POS", 
                                                 "PA.IP", "Dollars", 
                                                 "Ovr.Rank", "ADP")) %>%
  full_join(., player.tier("P", 2, 50), by = c("PlayerName", "Team", "POS", 
                                                 "PA.IP", "Dollars", 
                                                 "Ovr.Rank", "ADP")) %>%
  arrange(desc(Dollars))

# Calculate max tier
tier.data$Tier <- do.call(pmin, c(tier.data[8:16], na.rm=T))

# Create lists for selectInputs
# Positions including "All" (for draft sheet filter)
positions.all <- c("All", "C", "1B", "2B", "3B", "SS", "OF", "SP", "RP", "P")
# Positions exlcuding "All" (for plot filter)
positions <- positions.all[-1]
# X-axis options (for plot)
xaxis <- c("Pos.Rank", "Ovr.Rank")
names(xaxis) <- c("Position Rank", "Overall Rank")

# Generate tier table before the draft
tier.start <- cbind(1:5, as.data.frame(lapply(tier.data[, 8:16], table))[, seq(2, ncol(tier.data[, 8:16]) * 2, by = 2)])
names(tier.start) <- c("Tier", "Pos.C", "Pos.1B", "Pos.2B", "Pos.3B", "Pos.SS",
                       "Pos.OF", "Pos.SP", "Pos.RP", "Pos.P")

# Create autocomplete list
autocomplete.list <- all.players %>% select(PlayerName)

# Merge on max tier
all.players <- tier.data %>% 
  select(PlayerName, Tier) %>%
  left_join(., all.players, by = "PlayerName") %>%
  select(ADP, PlayerName, Team, POS, Tier, PA.IP, Dollars, Ovr.Rank)

# List for team drop down
teams.all <- c("All", as.character(sort(unique(all.players$Team))))

# Clean up environment
rm(b, p, batters, pitchers)