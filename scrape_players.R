##
## scrape_players: players in squads
## scrape_flag: national team flags
## scrape_colours: national team kits
## scrape_comp: competition teams and logos
##

library(tidyverse)
library(rvest)
library(countrycode)
library(janitor)

##
## wikipedia
##

d <- tibble(
  year = c(1957, 1959, 1962, 1963, 1965, seq(1968, 2012, 2), seq(2013, 2021, 2)),
  teams = c(3, 3, 4, 6, 6, rep(8, 12), 12, 12, 15, rep(16, 6), 15, rep(16, 4), 24, 24)
  )
d <- d %>%
  mutate(url = paste0("https://en.wikipedia.org/wiki/", year, "_", 
                      ifelse(year >= 2006, "Africa", "African"), 
                      "_Cup_of_Nations_squads"))


get_players <- function(u, n){
  # u = d$url[i];u
  # n = d$teams[i]
  h <- read_html(u)
  
  hh <- h %>%
    html_nodes("table.sortable")
  
  p <- tibble(
    squads = hh %>%
      html_table()
  ) %>%
    mutate(nn = map(.x = squads, .f = ~nrow(x = .x)), 
           nn = unlist(nn),
           squad_list = nn >= 11, 
           table_no = cumsum(squad_list), 
           squad_list = ifelse(table_no > n, FALSE, squad_list)) %>%
    mutate(squads = map(.x = squads, .f = function(dd = .x){
      r1 <- paste(slice(dd, 1), collapse = " ")
      if(str_detect(r1, "Player"))
        dd <- row_to_names(dd, 1)
      if(is.na(names(dd)[length(dd)]))
        dd <- dd[,-length(dd)]
      return(dd)
    }))
  
  
  s <- h %>%
    html_nodes(".mw-headline") %>%
    html_text() %>%
    str_subset(pattern = "Group", negate = TRUE) %>%
    .[1:n]
  
  not_line <- hh %>%
    .[p$squad_list] %>%
    html_nodes("td:nth-last-child(1)") %>%
    html_attr("colspan") %>%
    is.na()
  
  tibble(
    squad = s,
    player = p$squads[p$squad_list]
  ) %>%
    mutate(
      player = map(
        .x = player, 
        .f = ~mutate(.x, across(everything(), as.character))
      )) %>%
    unnest(cols = player) %>%
    filter(Player != "") %>%
    mutate(player_url = hh %>%
             .[p$squad_list] %>%
             html_nodes(".nat-fs-player th") %>%
             html_node("a") %>%
             html_attr("href"),
           club_fa_url = hh %>%
             .[p$squad_list] %>%
             html_nodes("td:nth-last-child(1)") %>%
             .[not_line] %>%
             html_node("a") %>%
             html_attr("href"),
           club_fa = hh %>%
             .[p$squad_list] %>%
             html_nodes("td:nth-last-child(1)") %>%
             .[not_line] %>%
             html_node("a") %>%
             html_attr("title"),
           club = hh %>%
             .[p$squad_list] %>%
             html_nodes("td:nth-last-child(1)") %>%
             .[not_line] %>%
             html_text(trim = TRUE),
           club_country = hh %>%
             .[p$squad_list] %>%
             html_nodes("td:nth-last-child(1)") %>%
             .[not_line] %>%
             html_node(".flagicon") %>%
             html_node(".thumbborder") %>%
             html_attr("alt"),
           club_country_flag = hh %>%
             .[p$squad_list] %>%
             html_nodes("td:nth-last-child(1)") %>%
             .[not_line] %>%
             html_node(".flagicon") %>%
             html_node("img") %>%
             html_attr("src")
    )
}

d0 <- d %>%
  # filter(year >= 1996) %>%
  slice(33) %>%
  mutate(players = map2(.x = url, .y = teams, 
                        .f = ~get_players(u = .x, n = .y)))

# i = 33
# get_players(u = d$url[i], n = d$teams[i]) %>%
#   group_by(squad) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n)) %>%
#   print(n = 30)
# for(i in 1:nrow(d))
#   get_players(u = d$url[i], n = d$teams[i])

d1 <- d0 %>%
  unnest(cols = c("players")) %>%
  clean_names() %>%
  select(-url, -teams) %>%
  mutate(pos = str_sub(string = pos, start = 2),
         captain = str_detect(string = player, pattern = "captain|Captain|\\(c\\)"),
         player_original = player,
         player = str_remove(string = player, pattern = " \\s*\\([^\\)]+\\)"))

cm <- c("CIS" = "CIS", 
        "CSSR" = "CSK",
        "Czechoslovakia" = "CSK",
        "England" = "GB-ENG", 
        "Northern Ireland" = "GB-NIR",
        "Scotland" = "GB-SCT",
        "Wales" = "GB-WLS", 
        "FR Yugoslavia" = "SCG", 
        "Yugoslavia" = "YUG", 
        "Soviet Union" = "SUN",
        "USSR" = "SUN")

d2 <- d1 %>%
  mutate(
    # nat_team = case_when(
    #   squad == "Soviet Union" ~ "USSR",
    #   TRUE ~ squad
    # ),
    # club_country_harm = case_when(
    #   club_country == "Wales" ~ "England",
    #   club_country == "Soviet Union" ~ "USSR",
    #   club_country == "Socialist Federal Republic of Yugoslavia" ~ "Yugoslavia",
    #   club_country == "Federal Republic of Yugoslavia"  ~ "FR Yugoslavia",
    #   TRUE ~ club_country),
    nat_team = squad,
    nat_team_alpha3 = countrycode(
      sourcevar = nat_team, origin = "country.name", 
      destination = "iso3c", custom_match = cm),
    club_alpha3 = countrycode(
      sourcevar = club_country_harm, origin = "country.name", 
      destination = "iso3c", custom_match = cm),
    club_country_flag = str_remove(string = club_country_flag, pattern = "thumb"),
    club_country_flag = str_remove(string = club_country_flag, pattern = "/[^/]+$")
  )

# checks on nat_team and club_country_harm match

d2 %>%
#   filter(is.na(nat_team))
  # filter(is.na(nat_team_alpha3))
  # filter(is.na(club_country_harm))
  filter(is.na(club_alpha3))

x = d2 %>%
  select(nat_team, club_country_harm) %>%
  pivot_longer(cols = 1:2, names_to = "type", values_to = "label") %>%
  select(-type) %>%
  distinct() %>%
  arrange(label)
# group_by(label) %>%
# mutate(n = n()) %>%
# arrange(desc(n))

write_excel_csv(d2 , "./data/wiki_players.csv")

