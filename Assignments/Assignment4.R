require("tidyverse")

MBT_ebird<-read_csv("https://github.com/mbtoomey/Biol_7263/blob/main/Data/MBT_ebird.csv?raw=true")

glimpse(MBT_ebird)

#Q1

Best_year<-MBT_ebird %>% 
  group_by(year) %>% 
  summarise(tot_birds = sum(count)) %>% 
  filter(tot_birds == max(tot_birds))

#Q2

species_count<-MBT_ebird %>%
  filter(year == 2014) %>%
  summarise(total_sp = n_distinct(common_name))

#Q3

RWBB<-MBT_ebird %>%
  filter(common_name == "Red-winged Blackbird") %>%
  group_by(location) %>% 
  summarise(obs = n()) %>%
  filter(obs == max(obs))

#Q4

encounter_rate<-MBT_ebird %>%
  filter(duration < 200 & duration > 5) %>% 
  group_by(list_ID) %>%
  summarise(total_sp = n_distinct(common_name), duration = first(duration), year = first(year)) %>%
  mutate(encounter_rate = total_sp/duration) %>%
  group_by(year) %>%
  summarise(mean_encounter_rate = mean(encounter_rate), total_checklists = n())

#Q5

Top_10_species<-MBT_ebird %>%
  group_by(common_name) %>%
  summarise(num_lists = n())%>%
  arrange(desc(num_lists)) %>%
  top_n(10)
  

Top_10_only<-MBT_ebird %>%
  filter(common_name %in% Top_10_species$common_name)

