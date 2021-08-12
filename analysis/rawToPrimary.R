# convert raw data to primary data

###!!! NB - the data file created by the code below in the primary folder results in a parsing warning that we've been unable to resolve. 
# The problem is fixed by opening the csv file in the primary folder in Excel and saving it. Hopefully we can find a programmatic solution, but currently this manual step needs to be performed whenever the raw data are converted to primary.

library(tidyverse)
library(here)

# Load raw (Google drive) data

d_coding <- read_csv(here("data","raw","Statistical guidance for authors coding form 2.0 (Responses) - Form Responses.csv")) # load raw data
d_coding <- d_coding %>% 
  select(-Timestamp, -`If relevant, use this space to note anything unusual about this journal that the team needs to discuss.`) # remove timestamps, remove notes
write_csv(d_coding,here("data","primary","data_coding.csv")) # save file to primary data folder

d_journals <- read_csv(here("data","raw","statGuide_journalList - PPPR_journalList.csv")) %>% # load raw data
  select(-`TH check`) # remove column
write_csv(d_journals,here("data","primary","data_journals.csv")) # save file to primary data folder





