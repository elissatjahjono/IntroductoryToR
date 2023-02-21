# Statistical Analysis for Bios314

## Install R and RStudio
### https://cran.r-project.org/bin/windows/base/
### https://posit.co/download/rstudio-desktop/


## Load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(DescTools)
library(car)
library(PMCMRplus)


## Load excel file
experiment1 <- read_excel("RawData.xlsx", sheet = "Exp1")


## Categorize "Biological Replicate", "Replicate", and "Treatment" columns
experiment1$Bio_Rep <- factor(experiment1$Bio_Rep, levels = unique(experiment1$Bio_Rep))
experiment1$Rep <- factor(experiment1$Rep, levels = unique(experiment1$Rep))
experiment1$Treatment <- factor(experiment1$Treatment, levels = unique(experiment1$Treatment))


## Calculate percentage survival for each treatment condition
experiment1$Percent_worms <- c(0)

for (i in 1:9 ) {
  grouped_data <- experiment1[experiment1$Rep == i, ]
  grouped_data$Percent_worms <- grouped_data$Number_worms/sum(grouped_data$Number_worms)*100
  experiment1[experiment1$Rep == i, 6] <- grouped_data$Percent_worms
}


## Function to produce summary statistics
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-(sd(x)/sqrt(length(x)))
  ymax <- m+(sd(x)/sqrt(length(x)))
  return(c(y=m,ymin=ymin,ymax=ymax))
}


## Create basic dot plot
p <- ggplot(experiment1, aes(x=Treatment, y= Percent_worms)) + 
  geom_dotplot(binaxis='y', binwidth = 2, stackdir='center',
               stackratio=1, dotsize=0.8)

p +
  scale_fill_brewer(palette="Dark2") +
  stat_summary(fun.data=data_summary, color="red") +
  theme_minimal() +
  theme(axis.text.x = element_text(size=12, color = "black"),
             axis.text.y = element_text(size=12, color = "black"),
             axis.line = element_line(color = "black", size = 0.5),
             axis.title.x = element_text(size = 15, face = "bold", color = "black"),
             axis.title.y = element_text(size = 15, face = "bold", color = "black"),
             panel.background = element_blank(),
             legend.position = "right",
             legend.box.margin = margin(5, 5, 5, 5),
             legend.title = element_text(face = "bold"),
             legend.text=element_text(size=rel(1.2)))


p <- ggplot(experiment1, aes(x=Treatment, y= Percent_worms)) + 
  geom_bar(stat="identity")

p +
  theme_minimal()




## ANOVA one-way test
my_anova <- aov(Percent_worms ~ Treatment, data = experiment1)
summary(my_anova)


## Post-hoc test
tukeyTest(experiment1$Percent_worms, experiment1$Treatment)
TukeyHSD(my_anova) ## same result as the above line


## T-test
a_on <- c(7, 30, 33, 27, 13, 2, 42, 30, 28)
a_off <- c(42, 18, 10, 65, 37, 41, 20, 16, 18)

b_on <- c(24, 29, 25, 12, 5, 13, 28, 20, 33)
b_off <- c(23, 5, 7, 57, 25, 43, 31, 22, 19)

a_percent <- a_on/(a_on+a_off)*100
b_percent <- b_on/(b_on+b_off)*100

mean(a_percent)
mean(b_percent)

t.test(a_percent, b_percent)
