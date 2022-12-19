require("tidyverse")

MBT_ebird<-read_csv("https://github.com/mbtoomey/Biol_7263/blob/main/Data/MBT_ebird.csv?raw=true")

glimpse(MBT_ebird)

# 1. birds per month faceted by location 

bird_month<-MBT_ebird %>%
  group_by(month, year, location) %>%
  summarize(total_sp = n_distinct(common_name)) %>%
  mutate(year = factor(year), month = factor(month))

glimpse(bird_month)

p<-ggplot(bird_month, aes(month, total_sp)) +
            geom_point(aes(color = year)) +
            facet_wrap(~location)
p

# 2. using the full dataset from exercise #5 plot a comparison of mass by treatment including the individual observations, the mean, and standard error of the mean. Use point color or shape to indicate the sex. 

###Prep data from assignment 5 ######################################

part1<-read_csv("https://github.com/mbtoomey/Biol_7263/raw/main/Data/assignment6part1.csv")

part2<-read_csv("https://github.com/mbtoomey/Biol_7263/raw/main/Data/assignment6part2.csv")

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

###############################################################

p1<-ggplot(full_set, aes(treatment, mass)) +
  geom_jitter(aes(color = sex),width = 0.3, 
              alpha = 0.4, size = 4)+ # adding transparency with the alpha option makes overlaps easier to discern
  stat_summary(fun = mean, geom = "crossbar",
               width = 0.5, color = "red") +
  stat_summary(geom = "errorbar", 
               width = 0.3,
               color = "red")
p1

# 3. using the full dataset from exercise #5 plot a comparison of mass vs. age, indicate sex with point shape or color, and fit separate regression lines (without CI) to each treatment.  

p2<-ggplot(full_set, aes(age, mass)) +
  geom_point(aes(color = treatment),width = 0.3, 
              alpha = 0.4, size = 4)+
  geom_smooth(aes(color = treatment), method = "lm", se = FALSE)+
  scale_color_manual(values = c("orange","green"))
p2

#4. combine the plots from 2 and 3 into a single plot with a title and labeled panels 

require(patchwork)

p1 / p2 + plot_annotation("Predictors of Mass", tag_levels = 'A')



