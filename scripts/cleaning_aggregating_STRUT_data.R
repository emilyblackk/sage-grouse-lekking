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


#Assign proper column names with function
fix_column_names <- function(data) {
  merged_names <- paste(data[1, ], data[2, ], sep = " ")
  colnames(data) <- ifelse(str_trim(data[1, ]) == "", data[2, ], merged_names)
  colnames(data) <- tolower(colnames(data))
  colnames(data) <- gsub(" ", "_", colnames(data))
  colnames(data) <- gsub("[^A-Za-z0-9_.]", "", colnames(data))
  data <- data[-c(1, 2), ]
  return(data)
}

strut_89 <- fix_column_names(strut_89)

#We now need to fill blank spaces with NAs
#And remove redundant columns, or replace them
strut_89<- strut_89 %>% mutate_all(na_if,"")
strut_89<- strut_89 %>% mutate_all(na_if," ") #Looks like some rows had a space instead of a blank
strut_89 <- strut_89[rowSums(is.na(strut_89)) != ncol(strut_89), ]

#looks like the years need a 19 added to them
#let's fix that
strut_89$year <- paste0("19", strut_89$year)
#check that worked: 
unique(strut_89$year) #it worked! 
#We also need to standardize those dates


#The names of the leks should be standardized for analyses
strut_89$lek <- tolower(strut_89$lek) #make lower case
 strut_89$lek <-  gsub(" ", "_", strut_89$lek) 
 
 
 
 #with information found elsewhere
 #Looks like the lice column is cross-referenced in the averages dataset. Let's read that in
 strut_89_averages <- read.csv("prelim_clean/1989_strut_average_data.csv")
 colnames(strut_89_averages) <- colnames(strut_89)
 strut_89_averages_lice <- strut_89_averages %>%
   dplyr::select(l_tag, r_tag, lice)
 
 #We can remove the lice column now from strut_89
 strut_89 <- strut_89 %>%
   select(-lice)
 
 #I think we can cross-reference the lice information to the original by tag
strut_89 <- merge(strut_89, 
                    strut_89_averages_lice, 
                    by = c("l_tag", "r_tag"), 
                    all.x = TRUE)
 
 
 
 
 
 
 #That data is looking good to me - let's save!
 write.csv(strut_89, 
           'prelim_clean/1989_strut_frequency_data_prelim.csv', 
           row.names=FALSE)
 
 
 #Cool - now can we add in the 1990 data with the same workflow, and then merge?
 
 
 strut_90 <- read.csv("prelim_clean/1990_strut_frequency_data.csv", 
                      header=FALSE)
 head(strut_90)
 #looks like column names got a bit messed up
 #Correct for proper column names
 
 
 #Assign proper column names 
 merged_names <- paste(strut_90[1, ], strut_90[2, ], sep = " ") #collapse split column names 
 colnames(strut_90) <- ifelse(str_trim(strut_90[1, ]) 
                              == "", strut_90[2, ], merged_names) #case whether to merge or not
 colnames(strut_90) <- tolower(colnames(strut_90)) #make lower case
 colnames(strut_90) <- gsub(" ", "_", colnames(strut_90)) #sub underscore for spaces
 colnames(strut_90) <- gsub("[^A-Za-z0-9_.]", "", colnames(strut_90)) #remove special characters
 strut_90 <- strut_90[-c(1, 2), ]  # remove rows 1 and 2
 
 
 
 
 #Now, we can remove blank rows
 strut_90<- strut_90 %>% mutate_all(na_if,"")
 strut_90<- strut_90 %>% mutate_all(na_if," ") #Looks like some rows had a space instead of a blank
 strut_90 <- strut_90[rowSums(is.na(strut_90)) != ncol(strut_90), ]
 
 
 
 
 
 
 #I see an interesting inconsistency with the breeding data - there are no zeroes. 
 #I will assume that no data means no breeding, but I should double check this
 # Replace NA with 0
 strut_90$breed[is.na(strut_90$breed)] <- 0
 

 #And remove redundant columns, or replace them
 # #We can remove the lice and malaria column, even though we will go back to it later
 # strut_90 <- strut_90 %>%
 #   select(-lice, -malaria)
 
 #looks like the years need a 19 added to them
 #let's fix that
 strut_90$year <- paste0("19", strut_90$year)
 #check that worked: 
 unique(strut_90$year) #it worked! 
 
 #The names of the leks should be standardized for analyses
 strut_90$lek <- tolower(strut_90$lek) #make lower case
 strut_90$lek <-  gsub(" ", "_", strut_90$lek) 
 
 
 
 #with information found elsewhere
 #Looks like the lice column is cross-referenced in the averages dataset. Let's read that in
 strut_90_averages <- read.csv("prelim_clean/1990_strut_average_data.csv")
 colnames(strut_90_averages) <- colnames(strut_90)
 strut_90_averages_select <- strut_90_averages %>%
   dplyr::select(l_tag, r_tag, malaria, lice)
 
 #Remove the existing lice and malaria columns in strut_90 to make the merge easier
 strut_90 <- strut_90 %>%
  select(-lice, -malaria)
 
 #I think we can cross-reference the lice information to the original by tag
 strut_90 <- merge(strut_90, 
                   strut_90_averages_select, 
                   by = c("l_tag", "r_tag"), 
                   all.x = TRUE)
 
 #Looks like it worked! Now I can make a preliminary combined dataset
 strut_merged <- rbind(strut_89, strut_90)
 write.csv(strut_merged, "prelim_clean/strut_merged_df_prelim.csv")
 
 
 
 
 
 #Do some preliminary analyses for fun
 
 #Contingency table - breed vs malaria
 strut_89_no3 <- strut_merged %>%
   filter(!breed==3, !malaria==3, !lice==3)
 breed_malaria <- table(strut_89_no3$breed, strut_89_no3$malaria)
 breed_malaria
 
 breed_lice <- table(strut_89_no3$breed, strut_89_no3$lice)
 breed_lice
 
 
 #Quick plot of number of struts vs lice
 strut_89_no3 %>%
   ggplot(aes(x = as.character(lice), y = as.numeric(struts_5_min), colour = as.character(year))) +
   geom_violin(position = "dodge") +
   stat_summary(aes(group = as.character(year)), fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(width = 0.8)) +
   stat_summary(aes(group = as.character(year)), geom = "errorbar", fun.data = mean_se, width = 0.2, color = "black", position = position_dodge(width = 0.8)) +
   stat_summary(aes(group = as.character(year)), geom = "crossbar", fun.data = mean_sdl, width = 0.4, color = "black", position = position_dodge(width = 0.8)) +
   labs(x = "Lice", y = "Number of struts in 5 minutes", colour = "Year") +
   scale_colour_manual(values = c("1990" = "blue", "1989" = "green")) +
   theme_classic()
 
 #Run an anova to see how lice affects...
 strut_89_no3$lice <- as.character(strut_89_no3$lice)
 aov(struts_5_min ~ lice + year, 
     data=strut_89_no3)
 
#significant difference! 
 


