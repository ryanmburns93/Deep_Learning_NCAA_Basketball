library(bigballR)
library(dplyr)
library(stringr)

# teamids located at https://github.com/jflancer/bigballR/blob/master/data/teamids.RData

get_conference_team_list <- function(conference, teamids){
  temp_subset = subset(teamids, Conference==conference)
  team_list = unique(temp_subset$Team)
  return(team_list)
}

acc_team_list = list('Virginia',
                     'Duke',
                     'North Carolina',
                     'Virginia Tech',
                     'Florida St.',
                     'Louisville',
                     'Clemson',
                     'Syracuse',
                     'NC State',
                     'Miami (FL)',
                     'Notre Dame',
                     'Pittsburgh',
                     'Boston College',
                     'Georgia Tech',
                     'Wake Forest')

sec_team_list = list("Auburn",
                     "Vanderbilt",
                     "Kentucky",
                     "Tennessee",
                     "LSU",
                     "Arkansas",
                     "Ole Miss",
                     "Mississippi St.",
                     "Georgia",
                     "Alabama",
                     "Texas A&M",
                     "South Carolina",
                     "Florida",
                     "Missouri")

big_ten_list = list("Michigan St.",
                    "Iowa",
                    "Purdue",
                    "Maryland",
                    "Indiana",
                    "Illinois",
                    "Nebraska",
                    "Ohio St.",
                    "Minnesota",
                    "Wisconsin",
                    "Northwestern",
                    "Michigan",
                    "Penn St.",
                    "Rutgers")

big_twelve_list = list("Kansas",
                       "West Virginia",
                       "Iowa St.",
                       "Texas Tech",
                       "Texas",
                       "Baylor",
                       "Oklahoma St.",
                       "TCU",
                       "Oklahoma",
                       "Kansas St.")

pac_twelve_list = list("Arizona St.",
                       "Colorado",
                       "Washington St.",
                       "UCLA",
                       "Southern California",
                       "Oregon",
                       "Utah",
                       "Arizona",
                       "Washington",
                       "Oregon St.",
                       "California",
                       "Stanford")

big_east_list = list("Creighton",
                     "St. John's (NY)",
                     "Xavier",
                     "DePaul",
                     "Georgetown",
                     "Butler",
                     "Marquette",
                     "Villanova",
                     "Seton Hall",
                     "Providence")

all_teams_list = unlist(c(big_east_list,
                       big_ten_list,
                       big_twelve_list,
                       pac_twelve_list,
                       sec_team_list))

#i = 1
#for (team in all_teams_list){
#  print(i)
#  print(team)
#  i = i + 1
#}

seasons_list = list('2011-12',
                    '2012-13',
                    '2013-14',
                    '2014-15',
                    '2015-16',
                    '2016-17',
                    '2017-18',
                    '2018-19',
                    '2019-20',
                    '2020-21')


collect_schedules <- function(seasons_list, teams_list, schedules_df=NULL) {
  # first if checks for restarting call or new call
  if (is.null(schedules_df)) {
    schedules_df <- data.frame()
  }
  for (year in seasons_list)
  {
    single_year_schedules_df <- data.frame()
    for (team_name in teams_list)
    {
      temp_df <- get_team_schedule(season = year, team.name = team_name)
      single_year_schedules_df <- rbind(single_year_schedules_df, temp_df)
    }
    schedules_df <- filter_for_listofteams_matchups(single_year_schedules_df, teams_list)
    schedules_df <- schedules_df[!duplicated(schedules_df), ]
    filename <- c("C:/Users/Ryan/Documents/P6_Game_Scraping/",
                  paste(str_replace(year, " ", "_"), 'P6_conferences_schedules_df'),
                         ".csv")
    write.csv(schedules_df,
              paste(filename, collapse=""),
              row.names = FALSE)
    print(paste(year, 'is completed.'))
  }
  return()
}


filter_for_listofteams_matchups <- function(schedules_df, team_list){
  schedules_filtered_df <- data.frame()
  for (row in 1:nrow(schedules_df))
  {
    temp_home <- schedules_df[row, "Home"]
    temp_away <- schedules_df[row, "Away"]
    if ((temp_home %in% team_list) | (temp_away %in% team_list)){
      schedules_filtered_df <- rbind(schedules_filtered_df, schedules_df[row,])
    }
  }
  return(schedules_filtered_df)
}


collect_pbp_data <- function(schedules_df, team_name = NULL){
  if (is.null(team_name)){
    play_by_play <- get_play_by_play(game_ids=select(schedules_df, Game_ID))
  }
  else{
    schedules_filtered_df <- filter(schedules_df, Home == team_name | Away == team_name)
    play_by_play <- get_play_by_play(game_ids=select(schedules_filtered_df, Game_ID))
  }
  return(play_by_play)
}


gather_play_by_play_data <- function(all_teams_list){
  file_list = list.files('C:/Users/Ryan/Documents/P6_Game_Scraping/')
  for (file in file_list){
    single_year_schedules_df <- read.csv(paste('C:/Users/Ryan/Documents/P6_Game_Scraping/', file))
    single_year_pbp_df <- data.frame()
    for (team in all_teams_list){
      oneyearoneteam_pbp_df <- collect_pbp_data(single_year_schedules_df, team)
      single_year_pbp_df <- rbind(single_year_pbp_df, oneyearoneteam_pbp_df)
    }
    filename <- c("C:/Users/Ryan/Documents/P6_Game_Scraping/",
                  paste(substr(file,1,nchar(file)-16), "pbp_df.csv"))
    write.csv(single_year_pbp_df,
              paste(filename, collapse=""),
              row.names = FALSE)
    print(paste(file, 'has been completely called for play-by-play data.'))
  }
  return(TRUE)
}


collect_schedules(seasons_list, all_teams_list)
print('COMPLETED WITH COLLECTING SCHEDULES!')
success = gather_play_by_play_data(all_teams_list)
print(paste0("!!!IT'S ",
            success,
            "! THE PLAY-BY-PLAY DATA COLLECTION IS COMPLETE!!!!"))

#acc_pbp_data_17_19_bbr_df <- collect_pbp_data(acc_schedules_df)