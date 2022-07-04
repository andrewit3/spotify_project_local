library("tidyverse")
print("Hello world")
df <- read_csv("dataset-of-10s.csv")
df  %>%
  filter(target == 1)
