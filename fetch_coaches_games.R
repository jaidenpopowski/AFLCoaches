library(dplyr)
library(fitzRoy)
library(stringr)
library(rvest)

# Function to get all the coach URLs and return their games
fetch_coaches_games <- function() {
  # Coaches Main index from AFL Tables link
  coaches.index <- read_html(x = "https://afltables.com/afl/stats/coaches/coaches_idx.html")
  
  coaches <- data.frame(urls = coaches.index %>%
    html_nodes("table") %>% 
    .[[1]] %>% 
    html_nodes("a") %>% 
    html_attr("href")) %>% 
    mutate(urls = str_replace(urls," ","%20")) # fix Allan La Fontaine link
  
  message(paste("Getting matches for", length(coaches$urls), "coaches")) # print start message
  
  pb <- suppressWarnings(progress_estimated(length(coaches$urls)))  # Create progress bar

  games_list <- coaches$urls %>%
    purrr::map_df(~ {
      pb$tick()$print() # progress bar updates
      read_html(x = paste0("http://afltables.com/afl/stats/coaches/", .x)) %>%  # read coach page
        html_nodes("tbody") %>% # find table elements
        .[[length(.)]] %>% 
        html_table() # scrape individual coach URLs
    }) %>% 
    mutate(Coach_ID = cumsum(X1 == "1")) %>% 
    mutate(Coach = str_replace_all(coaches$urls[Coach_ID], c("_"=" ","%20"=" ", "0" = "", "1"="", "2"="", ".html"=""))) %>% # coach names
    mutate(Crowd = as.integer(X12), Game_Number = as.integer(X1), Margin = as.integer(X10)) %>% 
    mutate(Season = as.numeric(substr(X2,nchar(X2)-3,nchar(X2)))) %>%
    mutate(Round = str_replace(sub(",.*","",X2), "R", "")) %>%
    select(Coach_ID, Coach, Game_Number, Season, Round, Venue = X11,Team = X4, Team_Score = X6, Oppo = X7, Oppo_Score = X9, Result = X3, Margin, Crowd)
  
  return(games_list)
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
