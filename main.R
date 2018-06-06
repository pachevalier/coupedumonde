library("rvest")
library("purrr")
library("dplyr")
library("ggplot2")
library("lubridate")
library("ggthemes")
library("wesanderson")
patrick <- "([[:digit:]])\\-([[:digit:]]).*"

list_tables <- read_html("https://fr.wikipedia.org/w/index.php?title=Liste_des_matchs_de_l%27%C3%A9quipe_de_France_de_football_par_adversaire&oldid=149219728") %>% 
  html_table(fill = TRUE) 

df_matches <- list_tables %>%
  keep(.p = function(x) { names(x)[1] == "X1"}) %>%
  keep(
    .p = function(x) {x %>% slice(1) %>% .$X2 == "Date"}
    ) %>% 
  bind_rows() %>% 
  filter(X2 != "Date") %>%
  select(-c(X1,X3)) %>%
  mutate(
    adversaire = ifelse(
      test = grepl(pattern = "^France", x = X4), 
      yes = sub(pattern = ".*\\-[[:blank:]]([[:alpha:]]+)", x = X4, replacement = "\\1"), 
      no = sub(pattern = "([[:alpha:]]+)[[:blank:]]\\-.*", x = X4, replacement = "\\1")
    ),
    score_france = ifelse(
      test = grepl(pattern = "^France", x = X4), 
      yes = as.numeric(
        sub(pattern = patrick, x = X5, replacement = "\\1")
        ), 
      no = as.numeric(
        sub(pattern = patrick, x = X5, replacement = "\\2")
      )
    ),
    score_adversaire = ifelse(
      test = grepl(pattern = "^France", x = X4), 
      yes = as.numeric(
        sub(pattern = patrick, x = X5, replacement = "\\2")
        ), 
      no = as.numeric(
        sub(pattern = patrick, x = X5, replacement = "\\1")
      )
    ), 
    penalty_france = ifelse(
      test = grepl(pattern = "^France", x = X4), 
      yes = as.numeric(
        sub(
          pattern = ".*\\(([[:digit:]])\\-([[:digit:]])\\).*", 
          replacement = "\\1", 
          x = X5)
        ),
      no = as.numeric(
        sub(
          pattern = ".*\\(([[:digit:]])\\-([[:digit:]])\\).*", 
          replacement = "\\2", 
          x = X5)
        )
    ),
    penalty_adversaire = ifelse(
      test = grepl(pattern = "^France", x = X4), 
      yes = as.numeric(
        sub(
          pattern = ".*\\(([[:digit:]])\\-([[:digit:]])\\).*", 
          replacement = "\\2", 
          x = X5)
        ), 
      no = as.numeric(
        sub(
          pattern = ".*\\(([[:digit:]])\\-([[:digit:]])\\).*", 
          replacement = "\\1", 
          x = X5)
        )
    ), 
    date = dmy(
      sub(
        pattern = "([[:digit:]]{1,2}[[:blank:]][[:alpha:]]+[[:blank:]][[:digit:]]{4}).*", 
        replacement = "\\1", 
        x = X2)
      ), 
    year = year(date), 
    outcome = case_when(
      score_france > score_adversaire ~ "win", 
      score_france == score_adversaire ~ "draw", 
      TRUE ~ "loss"
    )
  ) %>%
  group_by(year) %>%
  arrange(date) %>% 
  mutate(no = row_number()) 
save(df_matches, file = "df_matches.Rda")

df_matches %>% 
  glimpse()

df_matches %>%
  mutate(

    ) %>%
  glimpse()

    
df_matches %>%
  group_by(outcome) %>%
  count() %>% 
  ggplot()


df_matches %>% View()
# 
# df_matches %>% 
#   ggplot() +
#   geom_tile(
#     mapping = aes(y = year, x = no, fill = outcome), 
#     color = "white"
#       ) + 
#   scale_fill_manual(
#     values = c("#879CA2", "#CB361E", "#006B97"), 
#     labels = c("Nul", "Défaite", "Victoire"),
#     name = "Code couleur"
#     ) + 
#   scale_y_reverse(
#     name = "Année"
#   ) + 
#   theme_fivethirtyeight() + 
#   theme(
#     rect = element_blank(), 
#     legend.position = "bottom"
#     ) + 
#   coord_equal()
# 
# library(tweenr)
