library(dplyr)
library(tidyr)

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
  na.omit() %>%

  mutate(date = as.Date(date),

         card = tolower(card),
         card = plyr::revalue(card, c(
           "a" = "addition",
           "add" = "addition",

           "s" = "subtraction",
           "sub" = "subtraction",

           "m" = "multiplication",
           "mult" = "multiplication",
           "multi" = "multiplication",

           "d" = "division")),

         minutes = as.numeric(substr(time, 1, 1)) +
           ifelse(nchar(time)<4, 0, as.numeric(substr(time, 3, 4)) / 60)) %>%

  select(-time)


devtools::use_data(flashcards, overwrite = TRUE)
