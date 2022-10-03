library(tidyverse)
library(ggplot2) 
library(ggthemes) #extended themes for ggplot
library(patchwork) #tools for composing multiple panels

mpg

qplot(x=mpg$cty)

# simple scatter plot
qplot(x=mpg$cty,y=mpg$hwy,geom=c("point","smooth"), method="lm")

qplot(x=mpg$cty,y=mpg$hwy, col = mpg$class, geom=c("point","smooth"), method="lm")

qplot(x=mpg$cty,y=mpg$hwy, col = mpg$class, geom=c("point"))

qplot(x=mpg$class, y=mpg$hwy,geom="bar")

# basic barplot (long format)
qplot(x=mpg$hwy,geom="bar")

mpg_summary<-mpg %>%
  group_by(class) %>%
  summarise(mean_hwy = mean(hwy)) %>%
  arrange(mean_hwy) %>%

qplot(x=mpg_summary$class, y=mpg_summary$mean_hwy, geom="col")

unique(mpg_summary$class) 

p1 <- ggplot(data=mpg, mapping=aes(x=hwy,y=cty)) +
  geom_point()
print(p1)

p1 + theme_excel()

p1 + theme_classic(base_size=25,base_family="sans")

p1 + labs(x="Highway Gas Milage (mpg)") + # replace the default label on the y-axis
  theme(axis.title.x = element_text(size = 15, angle = 45, vjust = 0.5)) # specifically change font size and angle of y-axis label

p1 + labs(x="Highway Gas Milage (mpg)") + # replace the default label on the y-axis
  theme_classic() +
theme(axis.title.x = element_text(size = 15, angle = 45, vjust = 0.5)) # specifically change font size and angle of y-axis label

mpg_summary<-mpg %>%
  group_by(class) %>%
  summarise(mean_hwy = mean(hwy))

p<-qplot(x=mpg_summary$class, y=mpg_summary$mean_hwy, geom="col")

p

p + coord_flip()

p + coord_flip()

colors()

p1 <- ggplot(data=mpg) +
  aes(x=hwy,y=cty) + 
  geom_point(size=7,
             shape=25,
             color="black",
             fill="hotpink") +
  labs(title="My graph title here",
       subtitle="An extended subtitle that will print below the main title",
       x="My x axis label",
       y="My y axis label") +
  xlim(0,50) +
  ylim(0,50)
      
p1