#################################################################
# HSG course: Big Data Statistics for R and Python
# Test-Assignment Handling
# 
# NOTE:Before using this script, use the GitHub Classroom assistant to
# download all test-assignment repositories. (BigData/assignments/test-assignment)
# 
# What this script does:
# 1. read in and parse all the myteam.txt-files
# 2. create a dt of all the teams
# 3. store the dt as csv to the admin folder
#
# U.Matter, Zurich
# First version: September March, 2019
#################################################################


# SET UP -------------

# load packages 
library(data.table)


# fix vars
INPUT_PATH <- "assignments/test-assignment"
OUTPUT_PATH <- "admin/test-assignment.csv"
OUTPUT_PATH_TEAMS <- "admin/groupexam_teams.csv"

# function definitions

# a function to parse myteam.txt files
# x a path to a myteam.txt file
parse_myteam <- 
     function(x) {
          lines <- readLines(x)
          
          # extract user that handed in assignment
          assignment_user <- strsplit(x, "/")[[1]][4]
          
          # extract name of the team
          teamname <- gsub("team_name: ", "", lines[grepl("team_name", lines)])
          teamname <- gsub('\\"', "", teamname)
          
          # extract team members
          w_members <- which(grepl("team_members:", lines)) + 1
          members <- lines[w_members:length(lines)]
          members <- members[trimws(members)!=""]
          members <- members[!grepl("member", members)]
          members <- members[!grepl("team_name:", members)]
          n_members <- ifelse(length(members)==6|length(members)==9, 3, 2)
          members_roster <- t(matrix(members, ncol = n_members))
          members_roster <- gsub('\\"', "", members_roster)
          members_roster <- data.table(trimws(gsub("\\-", "", members_roster)))
     
          # format and clean roster
          if (ncol(members_roster)==3) {
               names(members_roster) <- c("member", "username", "matr_nr")
               
          } else {
               names(members_roster) <- c("username", "matr_nr")
               
          }
          members_roster$team <- gsub(" ", "", teamname)
          members_roster$handedin_by <- assignment_user
          # remove cases not yet filled in
          members_roster <- members_roster[matr_nr!="Matr.Nr."]
          members_roster <- members_roster[teamname!="TEAMNAME"]
          
          # clean data
          members_roster$matr_nr <- gsub("Matr.Nr. ", "", members_roster$matr_nr)
          members_roster$username <- gsub("username ", "", members_roster$username)
          

          return(members_roster)
     }


# READ/PARSE TEAM-FILES ----------- 

# get list of all file-paths
paths <- list.files(INPUT_PATH, pattern = "\\.txt",
                    all.files = TRUE,
                    full.names = TRUE,
                    recursive = TRUE)

# parse all files
for (i in paths) {parse_myteam(i)}
teams <- lapply(paths, parse_myteam)


# STACK ALL IN ONE FILE, WRITE TO DISK ----

# stack teams
all_teams <- unique(rbindlist(teams,
                              use.names = TRUE,
                              fill = TRUE
                              ))
teams_exam <- unique(all_teams[,4:2])
teams_exam <- teams_exam[order(team, decreasing = FALSE)]

# write to disk
fwrite(all_teams, OUTPUT_PATH)
fwrite(teams_exam, OUTPUT_PATH_TEAMS)
