#Emily Black
#Example data cleaning - floppy disk
#Created: 1 June 2023

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 0. Script setup
#clear R's brain
rm(list=ls())

#load relevant libraries for script
pkgs <- c("tidyverse", "Hmisc")
#install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 1: Opening the data

strut_89 <- read.csv("prelim_clean/1989_strut_frequency_data.csv", 
                     header=FALSE)
head(strut_89)
#looks like column names got a bit messed up
#Correct for proper column names

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 2: Cleaning the data


#Assign proper column names 
merged_names <- paste(strut_89[1, ], strut_89[2, ], sep = " ") #collapse split column names 
colnames(strut_89) <- ifelse(str_trim(strut_89[1, ]) 
                             == "", strut_89[2, ], merged_names) #case whether to merge or not
colnames(strut_89) <- tolower(colnames(strut_89)) #make lower case
colnames(strut_89) <- gsub(" ", "_", colnames(strut_89)) #sub underscore for spaces
colnames(strut_89) <- gsub("[^A-Za-z0-9_.]", "", colnames(strut_89)) #remove special characters
strut_89 <- strut_89[-c(1, 2), ]  # remove rows 1 and 2

#We now need to fill blank spaces with NAs
#And remove redundant columns
#Looks like the lice column is blank, so I will just remove it
strut_89 <- strut_89 %>%
  select(-lice)

#Now, we can remove blank rows
strut_89<- strut_89 %>% mutate_all(na_if,"")
strut_89<- strut_89 %>% mutate_all(na_if," ") #Looks like some rows had a space instead of a blank
strut_89 <- strut_89[rowSums(is.na(strut_89)) != ncol(strut_89), ]

#looks like the years need a 19 added to them
#let's fix that
strut_89$year <- paste0("19", strut_89$year)
#check that worked: 
unique(strut_89$year) #it worked! 

#The names of the leks should be standardized for analyses
strut_89$lek <- tolower(strut_89$lek) #make lower case
 strut_89$lek <-  gsub(" ", "_", strut_89$lek) 
 
 #That data is looking good to me - let's save!
 write.csv(strut_89, 
           'prelim_clean/1989_strut_frequency_data_prelim.csv', 
           row.names=FALSE)
 
 
 #Do some preliminary analyses for fun
 
 #Contingency table - breed vs malaria
 strut_89_no3 <- strut_89 %>%
   filter(!breed==3, !malaria==3)
 breed_malaria <- table(strut_89_no3$breed, strut_89_no3$malaria)
 breed_malaria
 
 
 #Quick plot of number of struts vs breeding
 strut_89_no3 %>%
   ggplot(aes(x=breed, y=as.numeric(struts_5_min))) + 
   geom_violin()+
   stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
   stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, color = "red") +
   stat_summary(geom = "crossbar", fun.data = mean_sdl, width = 0.4, color = "red") +
   labs(x="Breeding success", y="Struts in 5 minutes") + 
   theme_classic()



