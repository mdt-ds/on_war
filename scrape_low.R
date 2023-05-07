## scrape list of wars
library(rvest)
library(janitor)
library(dplyr)
library(stringr)
library(purrr)

# reading html page
page <- read_html("https://www.britannica.com/topic/list-of-wars-2031197")

# reading periods groupings
periods <- page %>% 
  html_node("#-topic-list-of-wars-2031197") %>% 
  html_nodes("h2") %>% 
  html_text()

# parsing function for nested ul li
parse_wars <- function(node){
  war <- html_children(node) %>% 
    html_text()
  return(war)
}

# list of war
wars_list <- page %>% 
  html_node("#-topic-list-of-wars-2031197") %>% 
  html_nodes("ul") %>%
  map(parse_wars)

wars_tbl <- list(sequence = 1:19, period = periods, war = wars_list) %>% 
  as_tibble() %>% 
  unnest(war) 
saveRDS(wars_tbl, "data/list_of_wars.RDS")

## scrape list of wars by death toll
# reading html page
page <- read_html("https://en.wikipedia.org/wiki/List_of_wars_by_death_toll")

# extracting tables
wiki_list_tbl <- page %>% 
  html_nodes("table") %>% 
  html_table()

wiki_wars_tbl0 <- 
  wiki_list_tbl[[5]] %>% 
  mutate(historical_period = "ancient wars") %>% 
  bind_rows(
    wiki_list_tbl[[8]] %>% 
      mutate(historical_period = "medieval wars")
  ) %>% 
  bind_rows(
    wiki_list_tbl[[10]] %>% 
      mutate(historical_period = "modern wars")
  )

to_toll <- function(death_range){
  if (str_detect(death_range, "–")) {
    dr <- str_split(death_range, "–")
    death_toll <-  round(mean(as.numeric(dr[[1]])))
  } else  {
    death_toll <-  as.numeric(death_range)
  }
  return(death_toll)
}

to_duration <- function(period){
  if (str_detect(period, "–")) {
  dr <- str_split(period, "–")
  duration <-  abs(as.numeric(dr[[1]][1]) - as.numeric(dr[[1]][2]))
  } else {
    duration <- 0.75
  }
  return(duration)
}

to_century <- function(period){
  c <- str_extract(period, "^\\d+") 
  if (str_length(c) == 4) {
    century <- as.numeric(str_sub(c, 1,2)) * 100
  } else if (str_length(c) == 3) {
    century <- as.numeric(str_sub(c, 1,1)) * 100
  } else if (str_length(c) < 3) {
    century <- 0
  }
  return(century)
}

wiki_wars_tbl <- wiki_wars_tbl0 %>%
  clean_names() %>% 
  mutate(deathrange = str_remove(deathrange, "\\+")) %>% 
  mutate(deathrange = str_remove_all(deathrange, ",")) %>% 
  mutate(deathrange = str_remove_all(deathrange, "[:alpha:]")) %>% 
  mutate(deathrange = str_remove_all(deathrange, "\\[\\d*\\]")) %>% 
  mutate(deathrange = str_remove(deathrange, "\\[ \\]")) %>% 
  mutate(deathrange = str_remove(deathrange, "\\(.*\\)")) %>% 
  mutate(deathrange = str_remove_all(deathrange, " ")) %>% 
  mutate(deathrange = str_replace(deathrange, "-", "–")) %>% 
  mutate(deathrange = str_trim(deathrange, "both")) %>% 
  mutate(death_toll = map_dbl(deathrange, to_toll)) %>% 
  mutate(period = str_remove_all(date, "BC")) %>% 
  mutate(period = str_remove_all(period, " ")) %>%
  mutate(period = str_remove(period, "c\\.")) %>%
  mutate(period = str_remove_all(period, "s")) %>%
  mutate(period = str_replace(period, "preent", "2023")) %>%
  mutate(period = str_replace(period, "-", "–")) %>%
  mutate(period = if_else(period == "1655–1660or1648–1667", 
                          "1655–1660", period)) %>% 
  mutate(duration = map_dbl(period, to_duration)) %>% 
  mutate(era = if_else(str_detect(date, "BC"), "BC", "AC")) %>%
  mutate(century = map_dbl(period, to_century))
           


saveRDS(wiki_wars_tbl, "data/wiki_wars.RDS")
