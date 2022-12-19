require(tidyverse)

part1<-read_csv("https://github.com/mbtoomey/Biol_7263/raw/main/Data/assignment6part1.csv")

part2<-read_csv("https://github.com/mbtoomey/Biol_7263/raw/main/Data/assignment6part2.csv")

#Q1

part1_piv<-part1 %>%
  pivot_longer(cols = !ID, names_to = c("sample","sex","treatment"), names_sep = "_")

part1_pw<-part1_piv %>%
  pivot_wider(names_from = ID, values_from = value)

part2_piv<- part2 %>%
  pivot_longer(cols = !ID, names_to = c("sample_treatment"))

part2_pw <- part2_piv %>%
  separate(sample_treatment, into = c("sample","treatment"))

part2_pw <- part2_pw %>%
  pivot_wider(names_from = ID, values_from = value)

full_set <- full_join(part1_pw, part2_pw, by = c("sample","treatment"))

glimpse(full_set)

#Q2

resid_mass_table <- full_set %>%
  mutate(resid_mass = mass/body_length) %>%
  na.omit() %>%
  group_by(sex, treatment) %>%
  summarise(mean_resid_mass = mean(resid_mass), SD_resid_mass = sd(resid_mass))
  
           

