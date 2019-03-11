## title : 
## description:
## inputs: 
## outputs: 


## Read the files
getwd()
setwd("C:/Berkeley/Stat-133/Workout/workout01/code")
iguodala <- read.csv('../data/andre-iguodala.csv', stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)


##Add the name column to each data file
iguodala$name <- rep("Andre Iguodala", length(iguodala$team_name))
green$name <- rep("Draymond Green", length(green$team_name))
durant$name <- rep("Kevin Durant", length(durant$team_name))
thompson$name <- rep("Klay Thompson", length(thompson$team_name))
curry$name <- rep("Stephen Curry", length(curry$team_name))


## Make shot_made_flag more informative
iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] <- "shot_no"
iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] <- "shot_yes"
green$shot_made_flag[green$shot_made_flag == "n"] <- "shot_no"
green$shot_made_flag[green$shot_made_flag == "y"] <- "shot_yes"
durant$shot_made_flag[durant$shot_made_flag == "n"] <- "shot_no"
durant$shot_made_flag[durant$shot_made_flag == "y"] <- "shot_yes"
thompson$shot_made_flag[thompson$shot_made_flag == "n"] <- "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag == "y"] <- "shot_yes"
curry$shot_made_flag[curry$shot_made_flag == "n"] <- "shot_no"
curry$shot_made_flag[curry$shot_made_flag == "y"] <- "shot_yes"


## Add the minute columns to each data set 
iguodala$minute <- iguodala$period*12 - iguodala$minutes_remaining
green$minute <- green$period*12 - green$minutes_remaining
durant$minute <- durant$period*12 - durant$minutes_remaining
thompson$minute <- thompson$period*12 - thompson$minutes_remaining
curry$minute <- curry$period*12 - curry$minutes_remaining

## Use sink to send files 
sink(file = '../output/andre-iguodala-summary.txt')
summary(iguodala)
# closing sinking operation
sink()

sink(file = '../output/draymond-green-summary.txt')
summary(green)
# closing sinking operation
sink()

sink(file = '../output/kevin-durant-summary.txt')
summary(durant)
# closing sinking operation
sink()

sink(file = '../output/klay-thompson-summary.txt')
summary(thompson)
# closing sinking operation
sink()

sink(file = '../output/stephen-curry-summary.txt')
summary(curry)
# closing sinking operation
sink()

## Write shots-data.csv and export
write.csv(
  total_shots <- rbind(iguodala, green, curry, durant, thompson), # R object to be exported
  file = '../data/shots-data.csv'  # file path
)


sink(file = '../output/shots-data-summary.txt')
summary(rbind(iguodala, green, durant, thompson, curry))
# closing sinking operation
sink()