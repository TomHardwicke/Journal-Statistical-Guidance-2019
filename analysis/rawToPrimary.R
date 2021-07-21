# convert raw data to primary data

library(tidyverse)
library(here)

# Load raw (Google drive) data

d_coding <- read_csv(here("data","raw","Statistical guidance for authors coding form 2.0 (Responses) - Form Responses.csv")) # load raw data
d_coding <- d_coding %>% select(-Timestamp, -`If relevant, use this space to note anything unusual about this journal that the team needs to discuss.`) # remove timestamps, remove notes
write_csv(d_coding,here("data","primary","data_coding.csv")) # save file to primary data folder

d_journals <- read_csv(here("data","raw","data_journals.csv")) # load raw data
# no changes necessary
write_csv(d_journals,here("data","primary","data_journals.csv")) # save file to primary data folder





