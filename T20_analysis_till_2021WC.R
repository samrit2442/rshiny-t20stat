library(dplyr)
library(tidyverse)
library(tidyr)
library(cricketr)
library(plyr)
library(ggplot2)
library(stringr)
library(DT)
library(repr)
library(plotrix)
library(scales)
library(tictoc)

# print("T20I Match data updated till 2021-11-14 23:59:59 IST (T20 WC 2021)")

# mydir = "E:/Cricket_Project_Personal/T20I till T20WC2021"
# t20_data = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
# t20_data
# 
# class(t20_data[1])
# length(t20_data)
# 
# t = vector(mode = "list", length = length(t20_data)/2)
# 
# for(i in 1:(length(t20_data)/2))
# {
#  t[[i]] = read.csv(file = t20_data[2*i-1], header = T)
# }
# t20 = rbind.fill(t)
# 
# write.csv(t20, file = "")

t20 <- read.csv(file.choose(), header = T) ### Read the data from external source

tot_mat = t20 %>% select(match_id) %>% unique() %>% nrow()

t20_bkp <- t20  ## Backup of the original raw data

colnames(t20) ## Showing the column names

t20[t20 == ""] <- NA  ## Filling the blank entries as NA
t20[is.na(t20)] <- 0  ## Filling the NAs as 0

glimpse(t20) ## To see the variable structure

t20$over = ceiling(t20$ball) ## Creating a column for nth over number (n = 1 to 20)
t20$over_type = ifelse(t20$over >= 1 & t20$over <= 6, "Powerplay",
                       ifelse(t20$over >= 7 & t20$over <= 16, "Middle Over", "Death Over"))
t20$isDot = ifelse(t20$runs_off_bat == 0, 1, 0) ## No of dots
t20$isOne = ifelse(t20$runs_off_bat == 1, 1, 0) ## No of 1's
t20$isTwo = ifelse(t20$runs_off_bat == 2, 1, 0) ## No of 2's
t20$isThree = ifelse(t20$runs_off_bat == 3, 1, 0) ## No of 3's
t20$isFour = ifelse(t20$runs_off_bat == 4, 1, 0) ## No of 4's
t20$isSix = ifelse(t20$runs_off_bat == 6, 1, 0) ## No of 6's
t20$isOut = ifelse(t20$wicket_type != "0", 1, 0) ## No of out

t20$Runs = t20$runs_off_bat + t20$extras

### For Batting Analysis

t20 <- t20[t20$innings == 1 | t20$innings == 2, ] # Removing Super-Over Balls

## Computing Total Number of Dismissal

diss <- t20 %>% dplyr::filter(player_dismissed == "AJ Finch") %>%
  dplyr::select(match_id, wicket_type, over_type, bowler)

# Removing Wide Balls

plyr_data <- t20 %>% dplyr::filter(striker == "AJ Finch" & wides == 0)

## Computing Total Innings played

innings <- t20 %>% dplyr::filter(striker == "AJ Finch" | non_striker == "AJ Finch") %>% 
  dplyr::summarise(Innings = n_distinct(match_id)) %>% as.numeric()

## Deducing Innings wise Statistics

stat1 <- plyr_data %>% dplyr::filter(striker == "AJ Finch") %>% 
  dplyr::group_by(start_date, match_id) %>% 
  dplyr::summarise(Runs = sum(runs_off_bat), Balls = length(runs_off_bat),
                  SR = round(Runs/Balls*100,2), 
                  Fours = sum(isFour), Sixes = sum(isSix), Dots = sum(isDot))

stat2 <- left_join(stat1, diss) ## Innings wise Player's Data

stat2$wicket_type <- ifelse(is.na(stat2$wicket_type), "not out", stat2$wicket_type)
stat2$isThirty <- ifelse(stat2$Runs >= 30 & stat2$Runs < 50, 1, 0) # No of 30's
stat2$isFifty <- ifelse(stat2$Runs >= 50 & stat2$Runs < 100, 1, 0) # No of 50's
stat2$isHundred <- ifelse(stat2$Runs >= 100, 1, 0) # No of 100's
stat2$isNO <- ifelse(stat2$wicket_type == "not out", 1, 0) ## Not Out Innings flag

stat3 <- plyr_data %>% dplyr::filter(striker == "AJ Finch") %>%
  dplyr::group_by(match_id) %>% dplyr::select(match_id, innings, bowling_team, venue) %>% unique()

stat4 <- left_join(stat2, stat3)  

stat5 <- t20 %>% dplyr::group_by(match_id, innings) %>% 
  dplyr::summarise(team_runs = sum(Runs))


stat6 <- left_join(stat4, stat5, by = c("match_id", "innings"))  # Complete innings wise player data

stat6$contribution = round(stat6$Runs/stat6$team_runs*100, 2)
stat6$Year = substr(stat6$start_date, 1, 4)

stat6$bowler <- ifelse(stat6$wicket_type == "run out", NA, stat6$bowler)


## Over-wise Strike Rate and Average (Phase of Play)

stat7 <- plyr_data %>% dplyr::group_by(over_type) %>% 
  dplyr::summarise(Runs = sum(runs_off_bat), Six = sum(isSix), Four = sum(isFour),
                   SR = round(sum(runs_off_bat)/length(runs_off_bat)*100,2))
stat8 <- table(stat6$over_type) %>% t() %>% as.data.frame()
stat8 <- stat8[,-1]
colnames(stat8) = c("over_type", "Dismissed")

table1 <- left_join(stat7, stat8) %>% dplyr::arrange(desc(over_type))
colnames(table1) <- c("Over Type","Runs","Sixes","Fours","SR","Dismissed")

## Rolling Out Average  ********************************

roll_out = function(k){
  n = nrow(stat8)
  stat8 = arrange(stat6, start_date)
  ra = c()

  for (i in as.numeric(1:floor(n/k))) {
    a = as.numeric((i-1)*k+1)
    b = as.numeric(i*k)
    ra = c(ra,sum(stat8$Runs[a:b])/k)
  }

  m = n - (floor(n/k)*k)
  p = as.numeric((floor(n/k)*k)+1)

  ra = c(ra,(sum(stat8$Runs[p:n])/m)) %>% round(2)
  return(ra)
}

roll_out(14)

plot1 = ggplot(roll_out(12))

stat9 = stat6 %>% dplyr::filter(wicket_type != "not out")


## Checking the Batter's position innings-wise

m_id = unique(t20$match_id) # Vector containg all match ids
b = vector(mode = "list", length = length(m_id)) 
# created a blank list of dataframes with length = total t20 matches

for(i in 1:length(m_id))
{
  b[[i]] <- subset(t20, match_id == m_id[i], select = c(match_id, innings, striker, non_striker))
  # every df in this list indicates the bbb data with match_id, innings, striker and non-striker columns only
}


batter_pos = function(player_name){
  player_name = as.character(player_name)
  posi = c()
  mat_id = c()
  for(i in 1:length(b))
  {
    if(any(b[[i]]==player_name)) # searching the desired player if exists in the list of dfs
    {
      inn <- b[[i]] %>% dplyr::filter(striker == player_name | non_striker == player_name) %>%
        slice_head() %>% dplyr::select(innings) %>% as.integer()
      
      dat <- subset(b[[i]], innings == inn, select = c(striker, non_striker)) %>% t() %>% c() %>% unique()
      posi = c(posi, which(dat == player_name))
      mat_id = c(mat_id, b[[i]][1,1])
    }
    else
    {
      posi = posi
      mat_id = mat_id
    }
  }
  
  dff = data.frame(mat_id, posi) 
  # Data Frame containg the info of positions and match_id of desired player
  return(dff)
}




## Bowler wise Analysis

bw <- plyr_data %>% dplyr::filter(striker == "AJ Finch") %>% 
  dplyr::group_by(bowler) %>% 
  dplyr::summarise(Runs = sum(runs_off_bat), Balls = length(runs_off_bat),
                   SR = round(Runs/Balls*100,2), 
                   Fours = sum(isFour), Sixes = sum(isSix))







### For Bowling Analysis

wk_var = c("stumped", "caught", "hit wicket", "bowled", "caught and bowled", "lbw")

t20$isBowler_wicket = ifelse(t20$wicket_type %in% wk_var,1,0)

## To find out Runs and Wickets at each innings

bw_stat1 <- t20 %>% dplyr::filter(bowler == "SL Malinga" & legbyes == 0 & byes == 0 & penalty == 0) %>% 
  dplyr::group_by(start_date, match_id) %>%
  dplyr::summarise(Runs = sum(Runs), Wickets = sum(isBowler_wicket))

## To find out Dot Balls at each innings

bw_stat2 <- t20 %>% dplyr::filter(bowler == "SL Malinga") %>% 
  dplyr::mutate(isBowlDot = ifelse(runs_off_bat + wides + noballs == 0,1,0)) %>% 
  dplyr::group_by(match_id) %>% 
  dplyr::summarise(Dots = sum(isBowlDot))

bw_stat <- left_join(bw_stat1, bw_stat2, by = "match_id")

## To find out total balls bowled in each innings

bw_stat3 <- t20 %>% dplyr::filter(bowler == "SL Malinga" & wides == 0 & noballs == 0) %>%
  dplyr::group_by(match_id) %>% 
  dplyr::summarise(Balls = length(runs_off_bat))

bw_stat <- left_join(bw_stat, bw_stat3, by = "match_id")

# https://youtu.be/UmDItbiDV6o (23:08 - 23:45) ### Highligths of Ban vs SL 18 Sep 2007

bw_stat4 <- t20 %>% dplyr::filter(bowler == "SL Malinga") %>%
  dplyr::mutate(BowlRuns = runs_off_bat + wides + noballs) %>%
  dplyr::group_by(match_id, over) %>%
  dplyr::summarise(isMaiden = ifelse(sum(BowlRuns) == 0,1,0)) %>% 
  dplyr::group_by(match_id) %>% dplyr::summarise(Maiden = sum(isMaiden))

bw_stat <- left_join(bw_stat, bw_stat4, by = "match_id")

bw_stat5 <- t20 %>% dplyr::filter(bowler == "SL Malinga") %>%
  dplyr::group_by(match_id) %>% dplyr::select(match_id, innings, venue, batting_team) %>% unique()

bw_stat <- left_join(bw_stat, bw_stat5, by = "match_id")

bw_stat = bw_stat %>% dplyr::mutate(Econ = round(Runs/Balls*6,2),
                                    is4wkt = ifelse(Wickets == 4, 1, 0),
                                    is5wkt = ifelse(Wickets >= 5, 1, 0))

## Over wise Wickets taken








