# this is a function plus several supporting functions that has been tested and verified to work with Match Reports in PDF such as the one found here:
# https://www.fifa.com/worldcup/matches/round=249721/match=300061509/

library(pdftools)
library(tidyverse)
#library(jsonlite)


# helper functions
num_extract <- function(thestring){  
  str_extract(thestring, "\\-*\\d+\\,*\\d*")
} 

str_clean <- function(thestring){  
  thestring <- gsub("^ *|(?<= ) | *$", "", thestring, perl = TRUE)
  return(thestring)
} 

rem_parantheses <- function(thestring){
  
  thestring <- gsub("[()]", "", thestring)
  return(thestring)
}

player_parse <- function(thestring){
  # remove []s
  thestring <- gsub("\\[|\\]", "", thestring) # use regex to remove ()s
  # how to deal with (GK) and times in ()s: remove everything in ()?
  thestring <- gsub("\\s*\\([^\\)]+\\)","",as.character(thestring))
  
  return (thestring)
}


# use: pdf_text
setwd("<WORKING DIRECTORY>")

# this assumes the script is being run in the same directory as the pdf files
files <- list.files(pattern = "pdf$")

files

#allpdf <- lapply(files, pdftools::pdf_text)

soccer_pdf <-function(filename){ 
basepdf<-pdftools::pdf_text(filename)
# preliminary analysis: export as text file
write.table(basepdf, file="textminingsample1.txt", row.names=FALSE, col.names=FALSE)


txt1 <- character()

txt1 <- c(txt1,
          readLines(
            str_c("textminingsample1.txt")
          ))
txt1

## number of lines in document: 118 over two pages
length(txt1)

#for(i in length(txt1)){
#  txt1[[i]]  
#}

# For example, this contains: Game Name, Team Names, Final Score, Midgame Score
#txt1[[4]]  



game.data <- NULL
game.data
#- game (something like “Brazil - Germany”) 
#using txt1[[4]] 
#currentline<-txt1[[4]]

#txt1[[1]]
#txt1[[2]]
#txt1[[3]]

currentline <- str_clean(txt1[[4]])
currentline


#sentences <- txt1[[4]] 
#sentences
#sentences <-
#  gsub("^ *|(?<= ) | *$", "", sentences, perl = TRUE)
# replaced by function

team1 <- word(currentline, 1)
team1
team2 <- word(currentline, 3)
team2

# the full name of the match
gamename<-str_c(team1, team2, sep = " - ")
gamename

# - teams (e.g., team1 = “Brazil”, team2 = “Germany”) 
#team1
#team2

# - final score 
#txt1[[4]]  

finalscore <- word(currentline, 4)
finalscore


#- score at break  -- not all games have this, make if statement to see if "a.e.t." is present in this line
#txt1[[4]]  
if (grepl("a.e.t.", currentline)){
  scorehalftime<-"0:0"
} else{
  #scorehalftime<-gsub("[()]", "", word(currentline, 5)) # use regex to remove ()s
  scorehalftime<-rem_parantheses(word(currentline, 5))
}
scorehalftime



# DATE
#using txt1[[5]]  
currentline5 <- str_clean(txt1[5])
currentline5

#sentencesdate <- gsub("^ *|(?<= ) | *$", "", sentencesdate, perl = TRUE)

day <- word(currentline5, 3)
month <- word(currentline5, 4)
year <- word(currentline5, 5)


#date <- cat(word(sentences,3), '/', word(sentences,4), '/', word(sentences,5))
date<-str_c(day, '-', month, '-', year)
#using lubridate:
#date

#date<-lubridate::dmy(date)
#date

#date <- lubridate::dmy(str_c(day, '-', month, '-', year))
date


#TIME
#txt1[[5]]  
time<-word(currentline5,6)
time

#time<-lubridate::hm(time)

# CITY

#txt1[[5]]  
# sentencesdate
#word(sentences, 9) # first word (of possibly 2 or more)

# split string on /s and then see if there are 1 or two words after the date (word 5)

newlineparse <- unlist(strsplit(currentline5, "/", fixed = TRUE))
newlineparse

newlineparse2 <- unlist(strsplit(newlineparse[1], " ", fixed = TRUE))
newlineparse2

cityname <- newlineparse2[7]

if (length(newlineparse2) > 7){
  
  for (i in 8:length(newlineparse2)){
    cityname <- paste(cityname, word(currentline5,i), sep=' ')
  }
  
  
}
cityname # this one was tricky as i wanted to make sure it works in case FIFA wants to have a match in the Canada city of Corporation of the United Townships of Dysart, Dudley, Harcourt, Guilford, Harburn, Bruton, Havelock, Eyre and Clyde


#stadium 
#stadium <- newlineparse[2] # need to remove lead/trailing spaces
stadium<-trimws(newlineparse[2], "both")
stadium


#attendance 
#txt1[[5]]  
#newlineparse[3]
#attendance<-num_extract(newlineparse[3])
attendance <- as.numeric(gsub(",","",num_extract(newlineparse[3])))
attendance



#game id (this will help link different datasets, this should be unique. You could try to concatenate game, final score and year)

gameid<-str_c(gamename,finalscore,year, sep = "x")

gameid
game.data <- c("Game", "Team 1", "Team 2", "Final score", "Score at break", "Date", "Time", "City", "Stadium", "Attendance", "game id")

game.data <- rbind(game.data, c(gamename, team1, team2, finalscore, scorehalftime, date, time, cityname, stadium, attendance, gameid)) # lubridate stopped working when I did this 
game.data


# DATABASE TWO:
#b. referee data (10%). Should include:
#• name
#• role
#• country code
#• game id
#

# from lines 6-9:
# Referee:                    Ravshan IRMATOV (UZB)
# Assistant Referee 1:        Rafael ILYASOV (UZB)                               # Assistant Referee 2:             Bakhadyr KOCHKAROV (KGZ)
# 4th Official:               Yuichi NISHIMURA (JPN)                             # Reserve Assistant Referee: Toru SAGARA (JPN)
# Match Commissioner:         Hafez ALMEDLEJ (KSA)                               # General Coordinator:             Hugo SALCEDO (USA)

#txt1
#txt1[[6]]

refline1 <- str_clean(txt1[[6]])
refline23 <- str_clean(txt1[[7]])
refline45 <- str_clean(txt1[[8]])
refline67 <- str_clean(txt1[[9]])



#extract role (one-three words), name (two words), and the country code
#one-two entries per line (main referee is single, other roles are doubled up)

#do not have to make first name and last name two separate features

#try split on : 
#ref1<-str_split(":", refline1)
#ref1

word(refline1,4)

#x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" #or whatever
#str_replace_all(x, "[[:punct:]]", " ")

ref1.position<-gsub("[[:punct:]]", "", 
                    word(refline1,1))
ref1.position
#ref1.position <- trimws(ref1.position, "both")
#ref1.position

ref1.name <- str_c(word(refline1, 2), word(refline1, 3), sep = " ")
ref1.name


#ref1.country <- gsub("[()]", "", word(refline1, 4))
ref1.country <- rem_parantheses(word(refline1, 4))

ref1.position
ref1.name
ref1.country

ref.data <- c("Name", "Role", "Country Code", "game id")
ref.data


ref.data <- rbind(ref.data, c(ref1.name, ref1.position, ref1.country, gameid) )
ref.data

# ref2 and ref3 info
refline23 <- str_clean(txt1[[7]])
refline23u <- unlist(strsplit(refline23, ":", fixed = TRUE))
refline23u[2]<-trimws(refline23u[2], "l")

ref2.position <- refline23u[1]

ref2.name<-str_c(word(refline23u[2],1), word(refline23u[2],2), sep=" ")

#ref2.country <- gsub("[()]", "", word(refline2u[2], 3))
ref2.country <- rem_parantheses(word(refline23u[2], 3))

ref2.position
ref2.name
ref2.country

ref.data <- rbind(ref.data, c(ref2.name, ref2.position, ref2.country, gameid) )
ref.data

# need to find size for the rest of this line:
refline23y <- unlist(strsplit(refline23, ")", fixed = TRUE))
refline23y <- unlist(strsplit(refline23y, ":", fixed = TRUE))
refline23y

ref3.position <- trimws(refline23y[3], "left")
ref3.position

ref3.name <- str_c(word(trimws(refline23y[4],"left"),1), word(refline23y[4],3), sep=" ")
ref3.name

ref3.country <- rem_parantheses(word(refline23y[4],4))

ref.data <- rbind(ref.data, c(ref3.name, ref3.position, ref3.country, gameid) )
ref.data

## now do the same thing for refs4 & 5, 6 & 7
refline45 <- str_clean(txt1[[8]])
refline45

refline45u <- unlist(strsplit(refline45, ":", fixed = TRUE))
refline45u[2]<-trimws(refline45u[2], "l")

ref4.position <- refline45u[1]

ref4.name<-str_c(word(refline45u[2],1), word(refline45u[2],2), sep=" ")

ref4.country <- rem_parantheses(word(refline45u[2], 3))

ref4.position
ref4.name
ref4.country

ref.data <- rbind(ref.data, c(ref4.name, ref4.position, ref4.country, gameid) )
ref.data


# ref 5
refline45y <- unlist(strsplit(refline45, ")", fixed = TRUE))
refline45y <- unlist(strsplit(refline45y, ":", fixed = TRUE))
refline45y

ref5.position <- trimws(refline45y[3], "left")
ref5.position

ref5.name <- str_c(word(trimws(refline45y[4],"left"),1), word(refline45y[4],3), sep=" ")
ref5.name

ref5.country <- rem_parantheses(word(refline45y[4],4))

ref.data <- rbind(ref.data, c(ref5.name, ref5.position, ref5.country, gameid) )
ref.data



## now do the same thing for 6 & 7
refline67 <- str_clean(txt1[[9]])
refline67

refline67u <- unlist(strsplit(refline67, ":", fixed = TRUE))
refline67u[2]<-trimws(refline67u[2], "l")

ref6.position <- refline67u[1]

ref6.name<-str_c(word(refline67u[2],1), word(refline67u[2],2), sep=" ")


ref6.country <- rem_parantheses(word(refline67u[2], 3))

ref6.position
ref6.name
ref6.country

ref.data <- rbind(ref.data, c(ref6.name, ref6.position, ref6.country, gameid) )
ref.data


# ref 7

refline67y <- unlist(strsplit(refline67, ")", fixed = TRUE))
refline67y <- unlist(strsplit(refline67y, ":", fixed = TRUE))
refline67y

ref7.position <- trimws(refline67y[3], "left")
ref7.position

ref7.name <- str_c(word(trimws(refline67y[4],"left"),1), word(refline67y[4],3), sep=" ")
ref7.name

ref7.country <- rem_parantheses(word(refline67y[4],4))

ref.data <- rbind(ref.data, c(ref7.name, ref7.position, ref7.country, gameid) )
ref.data




#c. goals data (20%). Should include:
#• scorer
#• country code
#• minute
#• game id
#



#Goals Scored:
#Giovanni VAN BRONCKHORST (NED) 18' , Diego FORLAN (URU) 41' , Wesley SNEIJDER (NED) 70' , Arjen ROBBEN
#(NED) 73' , Maximiliano PEREIRA (URU) 90'+2

# this can be ONE or TWO lines, use team1 as stop word?
# split on ','


goalstartline <- 10

txt1[[goalstartline]]
txt1[[goalstartline + 1]] # this is the first (and possibly only) line of goals scored
txt1[[goalstartline + 2]] # this is possibly the second line of scores
txt1[[goalstartline + 3]] # this is either the first line of players, or the roster heading

#twolinesofscores <- str_detect(txt1[[goalstartline + 3]], team1)
#twolinesofscores
# rolled into ifelse

if(str_detect(txt1[[goalstartline + 3]], team1)){
  #this is in case there are two lines of scores
  singlelineofscores <- str_c(txt1[[goalstartline + 1]], txt1[[goalstartline + 2]], sep = " ")
  firstlineofroster <- goalstartline + 3
} else{
  singlelineofscores <- txt1[[goalstartline + 1]]
  firstlineofroster <- goalstartline + 2
}
# we shall iterate over singlelineofscores


# this is important because we must extract the team abbreviations:
# newroster <- strsplit(str_clean(gsub("[()]", "", txt1[[firstlineofroster]]))," ")
# txt1[[firstlineofroster]]
#newroster <- txt1[[firstlineofroster]] %>% gsub("[()]", "", .) %>% str_clean %>% strsplit(" ") %>% unlist
newroster <- txt1[[firstlineofroster]] %>% rem_parantheses() %>% str_clean %>% strsplit(" ") %>% unlist

team1.abb <- newroster[2]
team2.abb <- newroster[4]

#txt1[[firstlineofroster]]

#  str_clean(
#    gsub(
#      "[()]", "", 
#      )
#    )
#  , 

#  txt1[[goalstartline + 1]], txt1[[goalstartline + 2]]


# RECORD TIME OF GOALS AS A NUMBER IN ORDER TO ITERATE PROPERLY
numberofgoals <- as.numeric(substr(finalscore, 1, 1)) + as.numeric(substr(finalscore, 3, 3))

# instead of extracting the abbreviation, just check to see what team is in the score

goals.data <- c("Goal Number", "Scorer Name", "Team", "Time", "game id")


individualscores <- unlist(str_split(singlelineofscores, ","))



individualscores<-individualscores %>% trimws("both") %>% rem_parantheses 


word(individualscores[1], 5)


#scorelist <- unlist(str_split(singlelineofscores, ","))
#scorelist


for (i in c(1:numberofgoals)) {
  
  
  if (word(individualscores[i],2) == team1.abb || word(individualscores[i],2) == team2.abb){
    goal.name <- word(individualscores[i],1)
    goal.time <- word(individualscores[i],3)
    
  } else if (word(individualscores[i],3) == team1.abb || word(individualscores[i],3) == team2.abb){
    goal.name <- str_c(word(individualscores[i],1), word(individualscores[i],2), sep = " ")
    goal.time <- word(individualscores[i],4)
    
  } else if (word(individualscores[i],4) == team1.abb || word(individualscores[i],4) == team2.abb){
    goal.name <- str_c(word(individualscores[i],1), word(individualscores[i],2), word(individualscores[i],3), sep = " ")
    goal.time <- word(individualscores[i],5)
    
  }
  
  
  goal.number <- as.numeric(i)
  
  if(str_detect(individualscores[i], team1.abb)){ 
    goal.team <- team1
  } else {
    goal.team <- team2
  }
  
  
  
  goals.data <- rbind(goals.data, c(goal.number, goal.name, goal.team, goal.time, gameid ))
}

goals.data

#d. line up (20%). Should include:
#• name
#• shirt number
#• team
#• game id

# the line-up is two columns with the shirt number in brackets before the name 

# occurs on lines 13:37
#basically each line consists of:
#[   1]          Fernando MUSLERA (GK)                                            [   1]       Maarten STEKELENBURG (GK)

# first 11 are starting players; the rest are substitutes 
# column for status?


#player_parse(txt1[firstlineofroster+1]) # now this is just a number, a name, a number, a name

#playerparsedline
# playerparsedline <- unlist(strsplit(str_clean(player_parse(txt1[firstlineofroster+1])), " ", fixed = TRUE))
#playerparsedline <- txt1[firstlineofroster+1] %>% player_parse %>% str_clean %>% strsplit(" ") %>% unlist
#playerparsedline

player.data <- c("Team", "Roster Number", "Player Name","game id")
# c("Team", "Roster Number", "Player Name","game id")


#playerparsedline <- txt1[firstlineofroster+1] %>% player_parse %>% str_clean %>% strsplit(" ") %>% unlist
#playerparsedline

rosterlines <- c(1,2,3,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23,24) # this has to exclude the line that says substitute

for (i in rosterlines){
  print(txt1[firstlineofroster+i])
  
  playerparsedline <- txt1[firstlineofroster+i] %>% player_parse %>% str_clean %>% strsplit(" ") %>% unlist
  #  playerparsedline
  
  
  # i used artificial intelligence instead of regexes
  
  if(length(playerparsedline)==5){  # this is a really weird case where one player just has one name (Pele, Pedro, Socrates etc)
    
    if(nchar(playerparsedline[4]) < 3){
      player1.num <- playerparsedline[1]
      player1.team <- team1
      player1.name <- str_c(playerparsedline[2], playerparsedline[3], sep = " ")
      player.data <- rbind(player.data, c(team1, as.numeric(player1.num), player1.name, gameid))
      
      player2.num <- playerparsedline[4]
      player2.team <- team2
      player2.name <- playerparsedline[5]
    } else {
      
      player1.num <- playerparsedline[1]
      player1.team <- team1
      player1.name <- playerparsedline[2]
      player.data <- rbind(player.data, c(team1, as.numeric(player1.num), player1.name, gameid))
      
      player2.num <- playerparsedline[3]
      player2.team <- team2
      player2.name <- str_c(playerparsedline[4], playerparsedline[5], sep = " ")
      player.data <- rbind(player.data, c(team2, as.numeric(player2.num), player2.name, gameid))
    }
    
    
  } else if(length(playerparsedline)==6){
    # this is for when the roster line is ordinary but there is a weird case when first person has 3 word name and second has 1 word name
    
    if(nchar(playerparsedline[4]) < 3){
      player1.num <- playerparsedline[1]
      player1.team <- team1
      player1.name <- str_c(playerparsedline[2], playerparsedline[3], sep = " ")
      player.data <- rbind(player.data, c(team1, as.numeric(player1.num), player1.name, gameid))
      
      player2.num <- playerparsedline[4]
      player2.team <- team2
      player2.name <- str_c(playerparsedline[5], playerparsedline[6], sep = " ")
      player.data <- rbind(player.data, c(team2, as.numeric(player2.num), player2.name, gameid))
      
    } else if(nchar(playerparsedline[5]) < 3){
      player1.num <- playerparsedline[1]
      player1.team <- team1
      player1.name <- str_c(playerparsedline[2], playerparsedline[3],playerparsedline[4], sep = " ")
      player.data <- rbind(player.data, c(team1, as.numeric(player1.num), player1.name, gameid))
      
      player2.num <- playerparsedline[5]
      player2.team <- team2
      player2.name <- playerparsedline[6]
      player.data <- rbind(player.data, c(team2, as.numeric(player2.num), player2.name, gameid))
      
    }
    else{
      player1.num <- playerparsedline[1]
      player1.team <- team1
      player1.name <- str_c(playerparsedline[2], playerparsedline[3], playerparsedline[4], sep = " ")
      player.data <- rbind(player.data, c(team1, as.numeric(player1.num), player1.name, gameid))
      
      player2.num <- playerparsedline[6]
      player2.team <- team2
      player2.name <- playerparsedline[7]
      player.data <- rbind(player.data, c(team2, as.numeric(player2.num), player2.name, gameid))
    }
    
    
  } else {
    # hard case:
    if(nchar(playerparsedline[4]) < 3){ #this means no type conversion/checking has to happen
      # print("player 1 has a short name ")
      player1.num <- playerparsedline[1] # this is invariant
      player1.team <- team1
      player1.name <- str_c(playerparsedline[2], playerparsedline[3], sep = " ")
      #team1.playernumber <- playerparsedline[1]
      player.data <- rbind(player.data, c(team1, as.numeric(player1.num), player1.name, gameid))
      
      player2.num <- playerparsedline[4]
      player2.team <- team2
      player2.name <- str_c(playerparsedline[5], playerparsedline[6], playerparsedline[7], sep = " ")
      player.data <- rbind(player.data, c(team2, as.numeric(player2.num), player2.name, gameid))
      
    } else if (nchar(playerparsedline[5]) < 3){
      # this means playerparsedline[5] was part of first player's name; their name is 4 parts
      player1.num <- playerparsedline[1] # this is invariant
      player1.team <- team1
      player1.name <- str_c(playerparsedline[2], playerparsedline[3], playerparsedline[4], sep = " ")
      player.data <- rbind(player.data, c(team1, as.numeric(player1.num), player1.name, gameid))
      
      player2.num <- playerparsedline[5]
      player2.team <- team2
      player2.name <- str_c(playerparsedline[6], playerparsedline[7], sep = " ")
      player.data <- rbind(player.data, c(team2, as.numeric(player2.num), player2.name, gameid))
      
    }else{
      player1.num <- playerparsedline[1] # this is invariant
      player1.team <- team1
      player1.name <- str_c(playerparsedline[2], playerparsedline[3], playerparsedline[4],playerparsedline[5], sep = " ")
      player.data <- rbind(player.data, c(team1, as.numeric(player1.num), player1.name, gameid))
      
      player2.num <- playerparsedline[6]
      player2.team <- team2
      player2.name <- str_c(playerparsedline[7], playerparsedline[8], sep = " ")
      player.data <- rbind(player.data, c(team2, as.numeric(player2.num), player2.name, gameid))
    }
    
    
  }
  
  
  
}


# check the list to see it contains all four databases:
#gamedatalist <- list(game.data, ref.data, goals.data, lineup.data)

#gamedatalist


gamedatalist <- list(game.data = game.data, ref.data = ref.data, goals.data = goals.data, player.data = player.data)

gamedatalist[[1]]

gamedatalist[[2]]

gamedatalist[[3]]

gamedatalist[[4]]

return (gamedatalist)

} 

# create colnames() to save variable names

finaloutput <- soccer_pdf(files[2]) # change this to the pdf name you wish to use
finaloutput

# verify column names are correct
colnames(finaloutput[[1]]) <- c("Game", "Team 1", "Team 2", "Final score", "Score at break", "Date", "Time", "City", "Stadium", "Attendance", "game id")
colnames(finaloutput[[1]]) 

finaloutput[[1]]

# do the rest of the column names, and then remove first row


write.csv(finaloutput[1], file = "my_data1.csv")
write.csv(finaloutput[2], file = "my_data2.csv")
write.csv(finaloutput[3], file = "my_data3.csv")
write.csv(finaloutput[4], file = "my_data4.csv")
