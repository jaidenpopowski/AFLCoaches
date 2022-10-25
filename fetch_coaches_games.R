library(dplyr)
library(fitzRoy)
library(stringr)
library(janitor)
library(xml2)
library(rvest)

# Function to get all the coach URLs and return their games
fetch_coaches_games <- function() {
  # Coaches Main index from AFL Tables link
  coaches.main <- read_html(x = "https://afltables.com/afl/stats/coaches/coaches_idx.html")
  
  names <- coaches.main %>%
    html_nodes("a") %>%
    html_text()
  
  urls <- coaches.main %>%
    html_nodes("a") %>%
    html_attr("href")
  
  coach.names <- data.frame(cbind(names, urls)) %>% # combine them into dataframe
    filter(grepl(",",names)) %>% # only keep links for coach names
    distinct() %>% # remove 'Most Games as Player/Coach' table links
    mutate(urls = str_replace(urls, " ", "%20")) # fix Allan La Fontaine link
  
  message(paste("Getting matches for", length(coach.names$urls), "coaches")) # print start message
  
  pb <- progress_estimated(length(coach.names$urls))  # Create progress bar

  tables <- coach.names$urls %>%
    purrr::map_df(~ {
      pb$tick()$print() # progress bar updates
      get_individual(.x) # scrape individual coach URLs
    })
  message("\nDone!") # Hooray!
  
  data <- tables %>%
    mutate(Coach_ID = cumsum(number == "1")) %>%
    left_join(mutate(coach.names, Coach_ID = row_number()),by="Coach_ID") %>%
    mutate(Coach = str_replace_all(urls, c("_"=" ","%20"=" ", "0" = "", "1"="", "2"="", ".html"=""))) %>% # remove other characters to give coach names
    mutate(Season = as.numeric(substr(game,nchar(game)-3,nchar(game)))) %>%
    mutate(Round = str_replace(sub(",.*","",game), "R", "")) %>% 
    mutate(Crowd = as.integer(crowd), Game_Number = as.integer(number), Margin = as.integer(m)) %>% 
    select(Coach_ID, Coach, Game_Number, Season, Round, Team = team, Team_Score = tot, Oppo = team_2, Oppo_Score = tot_2,Venue = venue, Result = r, Margin)
  
  return(data)
}

# Function to retrieve the table for an individual coach
get_individual <- function(x) {
  individual_page <- read_html(x = paste0("http://afltables.com/afl/stats/coaches/", x)) # read coach page
  
  # Find table
  table.name <- "" # table names on page, initialised as nothing
  i <- 0 # counter
  
  while (table.name != "Games Coached") { # check through all tables on coach page 
    i <- i + 1
    games_list <- individual_page %>% # scrape a table
      html_nodes(xpath = paste("/html/body/center/table[", i, "]", sep = "")) %>%
      html_table(fill = TRUE)
    games_list <- games_list[[1]] # save the data
    table.name <- names(games_list[1]) # update table name
  }
  games_list <- suppressWarnings(janitor::row_to_names(games_list, row_number = 1)) %>% # set column names as first row (duplicates cause warnings)
    janitor::clean_names() # clean column names to remove duplicates
  
  return(games_list) # return coach games list
}

coaches_raw <- fetch_coaches_games()

coaches_tidy <- coaches_raw %>% 
  mutate(Team = case_when(
    Team == "Kangaroos" ~ "North Melbourne",
    Team == "Footscray" ~ "Western Bulldogs",
    Team == "South Melbourne" ~ "Sydney",
    TRUE ~ Team
  ))

# example - which player has played under the most coaches?
afl_tables <- fetch_player_stats_afltables(season=1897:2022)

afl_tables %>% 
  mutate(Player=paste(First.name,Surname)) %>% 
  select(Season,Round,Date,ID,Player,Team = Playing.for) %>% 
  arrange(Date) %>% 
  full_join(coaches_tidy, by=c("Season","Round","Team")) %>%
  filter(Season>=1965) %>% 
  group_by(ID) %>% 
  summarise(
    Player = last(Player),
    Clubs = n_distinct(Team),
    Seasons = n_distinct(Season),
    Games = n(),
    Career = ifelse(first(Season)!=last(Season),paste(first(Season),last(Season),sep="-"),as.character(first(Season))),
    Coaches = n_distinct(Coach_ID),
    Coach_Names = paste(unique(Coach), collapse = ", "),
    .groups = 'drop'
  ) %>% 
  arrange(-Coaches) %>% View()
