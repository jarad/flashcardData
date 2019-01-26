library("dplyr")
library("tidyr")

my_read_csv = function(f, into) {
  readr::read_csv(f) %>%
    mutate(file=f) %>%
    separate(file, into)
}

read_dir = function(path, pattern, into) {
  files = list.files(path = path,
                     pattern = pattern,
                     recursive = TRUE,
                     full.names = TRUE)
  plyr::ldply(files, my_read_csv, into = into)
}



flashcards <- read_dir(path = "flashcards",
              pattern = "*.csv",
              into = c("dir","ymd","extension")) %>%
  select(-dir, -ymd, -extension) %>%
#  na.omit() %>%

  mutate(date = lubridate::ymd(date),

         # Time was either 5:00 for 5 minutes or # of seconds
         seconds = time,

         # Count is either count or n_cards
         count = ifelse(is.na(count), n_cards, count),

         # Student is either student or child
         student = ifelse(is.na(student), child, student),

         # card is operation
         card = ifelse(is.na(card), operation, card),
         card = tolower(card)) %>%

  rename(synonym = card) %>%
  left_join(readr::read_csv("card_synonyms.csv"), by="synonym") %>%
  mutate(card = factor(card,
                       levels = c("addition","subtraction",
                                  "multiplication","division"))) %>%

  select(date, student, card, seconds, count)


devtools::use_data(flashcards, overwrite = TRUE)
