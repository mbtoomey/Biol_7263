require(tidyverse)

#relig_income is a messy data set built into the tidyverse

relig_income

relig_income %>% 
  pivot_longer(!religion, names_to = "income", values_to = "count")

relig_income %>% 
  pivot_longer(cols = c("<$10k", "$10-20k", "$20-30k", "$30-40k", "$40-50k", "$50-75k", "$75-100k", "$100-150k", ">150k", "Don't know/refused"), names_to = "income", values_to = "count")

dput(colnames(relig_income)[2:11])


billboard %>% 
  pivot_longer(
    cols = starts_with("wk"), #pivot all columns with names beginning with "wk"
    names_to = "week", #convert column names to a variable called week
    names_prefix = "wk", #this command strips off the "wk" prefie from the column name
    names_transform = list(week = as.integer), #this option transforms the variable type from a string to integer
    values_to = "rank", #reorganize observations in each column into a variable called rank
    values_drop_na = TRUE, #drop NAs from the pivoted data set
  ) %>% arrange(week)

relig_income %>% 
  pivot_longer(cols = "<$10k":"Don't know/refused", names_to = "income", values_to = "count")

family <- tribble(
  ~family,  ~dob_child1,  ~dob_child2, ~gender_child1, ~gender_child2,
  1L, "1998-11-26", "2000-01-29",             1L,             2L,
  2L, "1996-06-22",           NA,             2L,             NA,
  3L, "2002-07-11", "2004-04-05",             2L,             2L,
  4L, "2004-10-10", "2009-08-27",             1L,             1L,
  5L, "2000-12-05", "2005-02-28",             2L,             1L,
)
family <- family %>% mutate_at(vars(starts_with("dob")), parse_date)

family

family %>% 
  pivot_longer(
    col = !family, #pivot columns that are not family
    names_to = c(".value", "child"), #transform column names to two variables. The special name ".value" tells pivot_longer() that that part of the column name specifies the “value” being measured (which will become a variable in the output) 
    names_sep = "_", #This tells pivot_longer() to split the column names at the "_". 
    values_drop_na = TRUE
  )

table2 %>%
  pivot_wider(names_from = type, values_from = count)


field_notes<-tibble(
  ID = c(023, 456, 167, 897), 
  sex = c("F","F","M","F"),
  mass = c("15.6 first sample of the day","16.0","17.2 caught with female 456","14.9 last sample of the day"))

field_notes %>% separate(mass, into = c("mass","notes"), sep = "(^\\d+\\.\\d$).*)")

field_notes %>% separate(mass, into = c("mass","notes"), sep = " ", extra = "merge", convert = TRUE)

field_notes %>% 
  separate(mass, into = c("mass","notes"), sep = " ")
