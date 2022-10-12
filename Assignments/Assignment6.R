require("tidyverse")

MBT_ebird<-read_csv("https://github.com/mbtoomey/Biol_7263/blob/main/Data/MBT_ebird.csv?raw=true")

glimpse(MBT_ebird)

#birds per month faceted by location 

bird_month<-MBT_ebird %>%
  group_by(month, year, location) %>%
  summarize(total_sp = n_distinct(common_name)) %>%
  mutate(year = factor(year), month = factor(month))

glimpse(bird_month)

p<-ggplot(bird_month, aes(month, total_sp)) +
            geom_point(aes(color = year)) +
            facet_wrap(~location)
p

#using the full dataset from exercise #5 plot a comparison of mass by treatment including the individual observations, the mean, and standard error of the mean. Use point color or shape to indicate the sex. 

p1<-ggplot(full_set, aes(treatment, mass)) +
  geom_jitter(aes(color = sex),width = 0.3, 
              alpha = 0.4, size = 4)+ # adding transparency with the alpha option makes overlaps easier to discern
  stat_summary(fun = mean, geom = "crossbar",
               width = 0.5, color = "red") +
  stat_summary(geom = "errorbar", 
               width = 0.3,
               color = "red")
p1

#using the full dataset from exercise #5 plot a comparison of mass vs. age, indicate sex with point shape or color, and fit separate regression lines (without CI) to each treatment.  

p2<-ggplot(full_set, aes(age, mass)) +
  geom_point(aes(color = treatment),width = 0.3, 
              alpha = 0.4, size = 4)+
  geom_smooth(aes(color = treatment), method = "lm", se = FALSE)+
  scale_color_manual(values = c("orange","green"))
p2

#combine the plots from XXXX into a single plot with a title and labeled panels 

require(patchwork)

p1 / p2

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

