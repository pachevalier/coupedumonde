library("rvest")
library("purrr")
library("dplyr")
library("ggplot2")
library("lubridate")
library("ggthemes")

list_tables <- read_html("https://fr.wikipedia.org/wiki/Liste_des_matchs_de_l%27%C3%A9quipe_de_France_de_football_par_adversaire") %>% 
  html_table(fill = TRUE) 

df_matches <- list_tables %>%
  keep(.p = function(x) { names(x)[1] == "X1"}) %>%
  keep(
    .p = function(x) {x %>% slice(1) %>% .$X2 == "Date"}
    ) %>% 
  bind_rows() %>% 
  filter(X2 != "Date") 

df_matches %>% 
  glimpse()

patrick <- "([[:digit:]])\\-([[:digit:]])"

df_final <- df_matches %>%
  select(-c(X1,X3)) %>%
  filter(
    grepl(pattern = "^Coupe du monde", x = X6), 
    X5 != "-"    
    ) %>%
  mutate(
    score_france = ifelse(
      test = grepl(pattern = "^France", x = X4), 
      yes = sub(pattern = patrick, x = X5, replacement = "\\1"), 
      no = sub(pattern = patrick, x = X5, replacement = "\\2")
      ),
    score_adversaire = ifelse(
      test = grepl(pattern = "^France", x = X4), 
      yes = sub(pattern = patrick, x = X5, replacement = "\\2"), 
      no = sub(pattern = patrick, x = X5, replacement = "\\1")
    ) 
  ) %>%
  mutate(
    score_france = as.numeric(score_france), 
    score_adversaire = as.numeric(score_adversaire)
    ) %>% 
  filter(!is.na(score_france)) %>%
  mutate(
    win = (score_france > score_adversaire)
    ) %>% 
  mutate(
    year = as.numeric(sub(pattern = ".*([[:digit:]]{4}).*", replacement = "\\1", x = X6)), 
    date = dmy(X2)
    )  %>%
  group_by(year) %>%
  arrange(date) %>% 
  mutate(no = row_number()) %>%
  group_by(year) %>%
  arrange(date) %>% 
  mutate(no = row_number()) 

df_final %>% 
  ggplot() +
  geom_tile(
    mapping = aes(y = year, x = no, fill = win), 
    color = "white"
      ) + 
  scale_y_discrete() + 
  theme_fivethirtyeight()
