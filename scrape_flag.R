##
## scrape_players: players in squads
## scrape_flag: national team flags
## scrape_colours: national team kits
## scrape_comp: competition teams and logos
##

library(tidyverse)
library(magick)

w <- read_csv("./data/wiki_players.csv", guess_max = 1e5)

n0 <- w %>%
  select(nat_team, nat_team_alpha3) %>%
  distinct() %>%
  rename(label = nat_team, 
         alpha3 = nat_team_alpha3)

d0 <- w %>%
  select(club_country_harm, club_alpha3, club_country_flag, year) %>%
  filter(club_alpha3 %in% n0$alpha3) %>%
  group_by_at(1:3) %>%
  mutate(year_min = min(year), 
         year_max = max(year),
         years = paste0(unique(sort(year)), collapse = ",")) %>%
  ungroup() %>%
  select(-year) %>%
  distinct() %>%
  rename(label = club_country_harm, 
         alpha3 = club_alpha3,
         flag_url = club_country_flag) %>%
  drop_na() %>%
  arrange(label) %>%
  mutate(label = ifelse(str_detect(string = flag_url, pattern = "Wales"), 
                        "Wales", label),
         alpha3 = ifelse(str_detect(string = flag_url, pattern = "Wales"), 
                         "GB-WLS", alpha3),
         flag_url = paste0("https:", flag_url)) 

# any countries that never had any national team players
# in their league?
n0 %>%
  filter(!alpha3 %in% unique(d0$alpha3)) 

d1 <- n0 %>% 
  filter(!alpha3 %in% unique(d0$alpha3)) %>%
  mutate(
    flag_url = c(
      "https://upload.wikimedia.org/wikipedia/commons/4/4f/Flag_of_Cameroon.svg",
      "https://upload.wikimedia.org/wikipedia/commons/f/fd/Flag_of_Senegal.svg",
      "https://upload.wikimedia.org/wikipedia/commons/9/94/Flag_of_the_Comoros.svg",
      "https://upload.wikimedia.org/wikipedia/commons/0/01/Flag_of_Guinea-Bissau.svg",
      "https://upload.wikimedia.org/wikipedia/commons/0/04/Flag_of_Gabon.svg",
      "https://upload.wikimedia.org/wikipedia/commons/9/92/Flag_of_Mali.svg"
    ),
    years = "2021",
    year_min = 2021,
    year_max = 2021
  )

# serbia update of the yugoslavia flag
d2 <- NULL

# use most upto date flag
d3 <- d0 %>%
  bind_rows(d1) %>%
  bind_rows(d2) %>%
  arrange(alpha3, year_min) %>%
  group_by(alpha3) %>%
  slice(n())

b <- image_blank(width = 100, height = 75, color = "grey40")
for(i in 1:nrow(d3)){
  message(d3$alpha3[i])
  f <- image_read_svg(path = d3$flag_url[i])
  f %>%
    image_resize("100x75") %>%
    image_composite(image = b, composite_image = ., gravity = "center") %>%
    image_write(path = paste0("./flag/", d3$alpha3[i], ".png"))
}