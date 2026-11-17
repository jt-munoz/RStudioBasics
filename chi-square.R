#STATISTICS IN R
#--------------------------------------------------------------------------------------------------
#CHI-SQUARE CONTINGENCY ANALYSIS

#Clear the decks
rm(list = ls())

#libraries I always use
library(tidyverse)

#read in ladybug data
lady <- read.csv("ladybirds_morph_colour.csv")

#check it out
glimpse(lady)

#group data by habitat and color
totals <- lady %>%
  group_by(Habitat, morph_colour) %>%
  summarise(total.number = sum(number))

#make a bar graph of grouped data
ggplot(totals, aes(x=Habitat, y=total.number,
                   fill = morph_colour)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = c(black = "black", red = "red"))

#create count matrix for chi-square test
lady.mat <- xtabs(number ~ Habitat + morph_colour, data = lady)

#run chi-squared test on the table we created
chisq.test(lady.mat)

#create object for chi-square results
lady.chi <- chisq.test(lady.mat)

#list names of objects within the test results
names(lady.chi)

#list expected frequencies calculated by chi-square test
lady.chi$expected

#-----------------------------------------------------------------------------------------------
#TWO-SAMPLE T-TEST

