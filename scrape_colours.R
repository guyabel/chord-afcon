##
## scrape_players: players in squads
## scrape_flag: national team flags
## scrape_colours: national team kits
## scrape_comp: competition teams and logos
##

library(tidyverse)
library(rvest)
library(countrycode)

d <- read_csv("./data/wiki_comp.csv") %>%
  select(team_alpha3, url_team) %>%
  distinct()

get_kit_colours <- function(u){
  # u <- d$url_team[10]
  h <- paste0("https://en.wikipedia.org/", u) %>%
    read_html()
  
  #kit colour
  tibble(
    shirt = h %>%
      html_nodes(".toccolours td:nth-child(1) div:nth-child(3)") %>%
      html_attr("style") %>%
      str_split(";") %>%
      .[[1]] %>%
      str_subset("background-color") %>%
      str_remove("background-color:") %>%
      str_trim(),
    away = h %>%
      html_nodes(".toccolours td:nth-child(2) div:nth-child(3)") %>%
      html_attr("style") %>%
      str_split(";") %>%
      .[[1]] %>%
      str_subset("background-color") %>%
      str_remove("background-color:") %>%
      str_trim(),
    shorts = h %>%
      html_nodes(".toccolours td:nth-child(1) div:nth-child(7)") %>%
      html_attr("style") %>%
      str_split(";") %>%
      .[[1]] %>%
      str_subset("background-color") %>%
      str_remove("background-color:") %>%
      str_trim(),
    socks = h %>%
      html_nodes(".toccolours td:nth-child(1) div:nth-child(9)") %>%
      html_attr("style") %>%
      str_split(";") %>%
      .[[1]] %>%
      str_subset("background-color") %>%
      str_remove("background-color:") %>%
      str_trim()
  )
}

d0 <- d %>%
  mutate(kit = map(.x = url_team, .f = ~get_kit_colours(u = .x)))

d1 <- d0 %>%
  unnest(kit) %>%
  mutate(shirt = ifelse(str_length(shirt) == 7, shirt, paste0(shirt, "0"))) %>%
  rename(kit_shirt = shirt,
         kit_shorts = shorts,
         kit_socks = socks,
         kit_away = away)

col_mis = c(GMB = "#FF0000", MWI = "#FF0000", GNB = "#FF0000", SLE = "#0000ea")
d1 <- d1 %>%
  mutate(kit_shirt = ifelse(team_alpha3 %in% names(col_mis), 
                            col_mis, kit_shirt))

write_excel_csv(x = d1, file = "./data/wiki_colours.csv")

# # manually edit in colours_team.xlxs the team colours