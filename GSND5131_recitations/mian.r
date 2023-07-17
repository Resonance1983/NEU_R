# I use this github repository for my whole R learning
# so setwd() is only used to simulate
# setwd("~/GSND5131_recitations")

# load packages
# install.packages(c("package1",”package2”),repos=”http://mirrors.aliyun.com/CRAN/”,type="binary")
library(tidyverse)
library(psych)
library(rstatix)
library(ggpubr)
library(ggcorrplot)

# import gamedata
gamedata <- read_csv("GSND5131_recitations/represent.csv")

# rename columns name
colnames(gamedata)[4] <- "Recommend"
colnames(gamedata)[5] <- "Learning"
colnames(gamedata)[6] <- "Enjoyment"
colnames(gamedata)[7] <- "Helpful"
colnames(gamedata)[9] <- "visited_court"
# check it
head(gamedata)
sapply(gamedata, class)

# recode the data
# mutate_at to recode target, recode() to mapping the text to number
gamedata <- gamedata %>%
  mutate_at(c("Recommend", "Learning", "Enjoyment", "Helpful"),
  list(~recode(.,`Strongly Disagree`=1,`Somewhat Disagree`=2,`Disagree`=3,`Neutral`=4,
     `Agree`=5,`Somewhat Agree`=6,`Strongly Agree`=7, .default = NaN)))

# sapply() and lapply() to make the function apply to object
# lapply means list apply; sapply's object include list,vector and dataframe
numdata<-gamedata[sapply(gamedata, is.numeric)]
sapply(numdata, mean)
sapply(numdata, median)
sapply(numdata, SD)
sapply(numdata, summary)

# par to make the graph into (2,2) structure
# use hist() to structure anonymous function
par(mfrow = c(2,2))
lapply(c("Recommend", "Learning", "Enjoyment","Helpful"), FUN=function(s)
  hist(gamedata[[s]], xlab=s,main=paste("Histogram of",s),col = "blue"))


# statistic visited_court
table(gamedata$visited_court)

# stat.data select this data(key) as dataframe by gather()
# use na.omit() to delete the N/A data
stat.data <- gamedata %>%
  select(Recommend,Learning,Enjoyment,Helpful,visited_court) %>%
  gather(key, value,-visited_court) %>%
  na.omit()

# group key and visited_court as summarise objects
# summarise their count number,mean and Standard Deviation
stat.data %>%
  group_by(key, visited_court) %>%
  summarise(
    n = n(),
    mean = mean(value),
    sd = sd(value)
  ) %>%
  ungroup()

# take t_test
stat.test <- stat.data %>%
  group_by(key) %>%
  t_test(value ~ visited_court) %>%
  add_significance()

# Remove unnecessary columns and display the outputs
stat.test %>% select(-.y., -statistic, -df)

# take wilcox_test
stat.test2 <- stat.data %>%
  group_by(key) %>%
  wilcox_test(value ~ visited_court) %>%
  add_significance()

# Remove unnecessary columns and display the outputs
stat.test2 %>% select(-.y., -statistic)

# combine correlations data
cor.data <- gamedata %>%
  select(Recommend,Learning,Enjoyment,Helpful) %>%
  na.omit()

# calculate the linear correlation coefficient
cor.coef <- cor(cor.data)
cor.coef
# round number, keep two decimal places
corr <- round(cor.coef,2)
corr

# calculate p_value
p.mat <- cor_pmat(cor.data)
p.mat

# plot heatmap based on p_value
corr.plot <- ggcorrplot(
  corr, hc.order = TRUE, type = "lower", outline.col = "white",
  p.mat = p.mat
)
corr.plot