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


#####################

require(patchwork)

# Setup a basic mean +/- plot: 

#calculate mean, SD, and order the levels of class
mpg_summary<-mpg %>%
  mutate(class = factor(class, levels = c("2seater","subcompact","compact","midsize","minivan","pickup","suv")))%>%
  group_by(class) %>%
  summarise(mean_hwy = mean(hwy), SD = sd(hwy))

p<-ggplot(mpg_summary, aes(class, mean_hwy)) +
  geom_col(position=position_dodge(0.5), width = 0.5)+
  geom_errorbar(aes(ymin = mean_hwy - SD, ymax = mean_hwy + SD), width = 0.25)
p

# order factor levels in the full dataset
mpg_fact<-mpg %>% 
  mutate(class = factor(class, levels = c("2seater","subcompact","compact","midsize","minivan","pickup","suv")))


# another way to make an bar plot
p1<-ggplot(mpg_fact, aes(class, hwy)) +
  stat_summary(fun = mean, geom = "col", width = 0.5, position=position_dodge(0.3), color = "red", fill = "white") +
  stat_summary(geom = "errorbar", width = 0.3, position=position_dodge(0.3)) +
  ylim(0,45) + 
  coord_flip()
p1

p2<-ggplot(mpg_fact, aes(class, hwy)) +
  geom_boxplot(width = 0.4, color = "red") + 
  ylim(0,45) + 
  coord_flip()
p2

#  violin and mean plot
p3<-ggplot(mpg_fact, aes(class, hwy)) +
  geom_violin(width = 0.4, draw_quantiles = c(0.25, 0.5, 0.75))+
  stat_summary(fun = mean, geom = "crossbar", width = 0.4, color = "red") +
  ylim(0,45) +
  coord_flip()
p3

# Dot and mean plot
p4<-ggplot(mpg_fact, aes(class, hwy)) +
  geom_jitter(width = 0.3, alpha = 0.4, size = 1.6)+ # adding transparency with the alpha option makes overlaps easier to discern
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "red") +
  ylim(0,45) + 
  coord_flip()
p4

(p1 + p2)/(p3+p4) +  plot_layout(ncol = 1)


# violin with dot

p4<-ggplot(mpg_fact, aes(class, hwy)) +
  geom_dotplot(binaxis='y', stackdir='center', stackratio = 0.5, binwidth = 0.3, position=position_dodge(1), dotsize = 2)+
  geom_density(position = position_nudge(x = .2, y = 0),adjust =2) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, position=position_dodge(0.3), color = "red") +
  ylim(0,45) + 
  coord_flip()
p4

p4<-ggplot(mpg_fact, aes(class, hwy)) +
  geom_density(position = position_nudge(x = .2, y = 0),adjust =2) +
  ylim(0,45) + 
  coord_flip()
p4

install.packages("ggdist")
require(ggdist)
install.packages("gghalves")
require(gghalves)
install.packages("beeswarm")

p4<-ggplot(mpg_fact, aes(class, hwy)) +
  stat_dist_halfeye(aes(fill = class), width = 0.5, adjust = .5, .width = 0, justification = -.2, point_colour = NA) +
  geom_boxplot(
    width = .25, 
    outlier.shape = NA) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1)) + 
  ylim(0,45) + 
  coord_flip()
p4

p4<-ggplot(mpg_fact, aes(class, hwy)) +
  stat_dist_halfeye(aes(fill = class), width = 0.5, adjust = .5, .width = 0, justification = -.2, point_colour = NA) +
  stat_dots(aes(color = class, fill = class),
    side = "left",
    justification = 1.05,
    binwidth = 0.4,
    stackratio = 0.5
  ) + 
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, position=position_dodge(0.3), color = "black") +
  ylim(0,45) + 
  coord_flip()
p4
  


# Dot and mean plot
p4<-ggplot(mpg_fact, aes(class, hwy)) +
  geom_jitter(width = 0.3, 
              alpha = 0.4, size = 1.6)+ # adding transparency with the alpha option makes overlaps easier to discern
  stat_summary(fun = mean, geom = "crossbar",
               width = 0.5, color = "red") +
  stat_summary(geom = "errorbar", 
               width = 0.3,
               color = "red") + 
  ylim(0,45) + 
  coord_flip()
p4

########################################

install.packages("gghalves")
require(gghalves)

p5<-ggplot(mpg_fact, aes(class, hwy)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  geom_jitter(width = 0.3, alpha = 0.4, size = 1.6)+
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "red") +
  ylim(0,45) + 
  coord_flip()
p5

p5<-ggplot(mpg_fact, aes(class, hwy)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  geom_jitter(width = 0.3, alpha = 0.4, size = 1.6)+
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "red") +
  ylim(0,45) + 
  coord_flip()
p5


p5<-ggplot(mpg_fact, aes(class, hwy)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), position = position_nudge (x = 0.5, y = 0), width = 0.4)+
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.6)+
  stat_summary(fun = mean, geom = "crossbar", width = 0.4, color = "red") +
  ylim(0,45) + 
  coord_flip()
p5


######################################################

ID<-c("A","B","C","D","E","F","G","A","B","C","D","E","F","G")
Obs<-c("before","before","before","before","before","before","before","after","after","after","after","after","after","after")
Measure<-c(runif(1:7),runif(1:7)*4)

RepEx<-tibble(ID,Obs,Measure)

RepEx<-RepEx %>% 
  mutate(Obs = factor(Obs, levels = c("before","after")))

p6<-ggplot(RepEx, aes(Obs, Measure))+
  geom_point(alpha = 0.4, size = 4, aes(color = ID))+
  geom_line(size = 2, alpha = 0.4, aes(color = ID,  group = ID))
p6
