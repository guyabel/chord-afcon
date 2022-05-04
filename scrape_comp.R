##
## scrape_players: players in squads
## scrape_flag: national team flags
## scrape_colours: national team kits
## scrape_comp: competition teams and logos
##

library(tidyvese)
library(rvest)
library(countrycode)
library(janitor)
library(magick)

d <- tibble(
  year = c(1957, 1959, 1962, 1963, 1965, seq(1968, 2012, 2), seq(2013, 2021, 2)),
  teams = c(3, 3, 4, 6, 6, rep(8, 12), 12, 12, 15, rep(16, 6), 15, rep(16, 4), 24, 24)
)
d <- d %>%
  mutate(url = paste0("https://en.wikipedia.org/wiki/", year, "_", 
                      ifelse(year >= 2006, "Africa", "African"), 
                      "_Cup_of_Nations"))

get_logo <- function(u){
  # u = d$url[1]
  h <- read_html(u)
  h %>%
    html_nodes(".infobox-image img") %>%
    html_attr("src")
}

get_teams <- function(u){
  # u = d$url[i]
  h <- read_html(u)
  
  k <- 1
  # if(str_detect(string = u, pattern = "2020"))
  #   k <- 2
  d <- h %>%
    html_nodes("table.sortable") %>%
    .[[k]] %>%
    html_table(fill = TRUE)
  
  d$url_team <- h %>%
    html_nodes("table.sortable") %>%
    .[[k]] %>%
    html_nodes("span a:nth-child(2)") %>%
    html_attr("href")
  
  return(d)
}

d <- d %>%
  filter(year == 2021) %>%
  mutate(url_comp_logo = map(.x = url, .f = ~get_logo(u = .x)),
         team = map(.x = url, .f = ~get_teams(u = .x)))

d0 <- d %>%
  unnest(url_comp_logo) %>%
  unnest(team) %>%
  clean_names() %>%
  mutate(team = ifelse(!is.na(team), team, team_a),
         team = str_remove(string = team, pattern = " \\s*\\([^\\)]+\\)"),
         team = str_remove_all(string = team, pattern = "\\[.*?\\]"))
  #        comp_prev = ifelse(!is.na(previous_appearances_in_tournament_a),
  #                           previous_appearances_in_tournament_a, 
  #                           previous_appearances_in_tournament_b)) %>%
  # select(-team_a, -contains("previous_appearances_in_tournament"))

# cm <- c("CIS" = "CIS", 
#         "CSSR" = "CZK",
#         "Czechoslovakia" = "CSK",
#         "England" = "GB-ENG", 
#         "Northern Ireland" = "GB-NIR",
#         "Scotland" = "GB-SCT",
#         "Wales" = "GB-WLS", 
#         "Yugoslavia" = "YUG", 
#         "FR Yugoslavia" = "SCG",
#         "Soviet Union" = "SUN",
#         "USSR" = "SUN")

d0 <- d0 %>%
  mutate(team_alpha3 = countrycode(sourcevar = team, origin = "country.name", 
                                   destination = "iso3c")) #, custom_match = cm))
# d0 <- read_csv("./data/wiki_comp.csv")
write_excel_csv(d0,  "./data/wiki_comp.csv")

##
## download logos
##
d0 <- read_csv("data/wiki_comp.csv")
d1 <- d0 %>%
  distinct(year, url_comp_logo)

for(i in 1:nrow(d1))
  d1$url_comp_logo[i] %>%
  paste0("https:", .) %>%
  image_read(density = 300) %>% 
  image_write(paste0("./logo/", d1$year[i], ".png"))



