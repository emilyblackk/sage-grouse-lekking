#Emily Black
#Cleaning STRUT data from floppy disk
#Created: 1 June 2023
#Last modified: 9 June 2023

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 0. Script setup
#clear R's brain
rm(list=ls())

#load relevant libraries for script
pkgs <- c("tidyverse", "Hmisc", 'lubridate')
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

#The names of the leks should be standardized for analyses
strut_89$lek <- tolower(strut_89$lek) #make lower case
 strut_89$lek <-  gsub(" ", "_", strut_89$lek) 
 
 
 
 #with information found elsewhere
 #Looks like the lice column is cross-referenced in the averages dataset. Let's read that in
 strut_89_averages <- read.csv("prelim_clean/1989_strut_average_data.csv", 
                               header=FALSE)
strut_89_averages <- fix_column_names(strut_89_averages)
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

 
#Finally, we also need to standardize those dates using a function
convert_date_columns <- function(data) {
  # Split day and month using gsub
  data$day <- gsub("-.*", "", data$date)
  data$month <- gsub(".*-", "", data$date)
  
  # Convert month to numerical month
  data$month <- match(data$month, month.abb)
  
  # Remove the original date column
  data$date <- NULL
  
  #Move month and year to immediately after year
  data <- data %>%
    relocate(month, .after=year) %>%
    relocate(day, .after=month)
  
  return(data)
}
strut_89 <- convert_date_columns(strut_89)

 
 
 
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
 
strut_90 <- fix_column_names(strut_90)
 
 
 #Now, we can remove blank rows
 strut_90<- strut_90 %>% mutate_all(na_if,"")
 strut_90<- strut_90 %>% mutate_all(na_if," ") #Looks like some rows had a space instead of a blank
 strut_90 <- strut_90[rowSums(is.na(strut_90)) != ncol(strut_90), ]
 
 
 
 
 
 
 #I see an interesting inconsistency with the breeding data - there are no zeroes. 
 #However, there are zeros in the average data
 # So, for this reason I believe we can assume that NAs are no breeds 
 strut_90$breed[is.na(strut_90$breed)] <- 0

 #looks like the years need a 19 added to them
 #let's fix that
 strut_90$year <- paste0("19", strut_90$year)
 #check that worked: 
 unique(strut_90$year) #it worked! 
 
 #The names of the leks should be standardized for analyses
 strut_90$lek <- tolower(strut_90$lek) #make lower case
 strut_90$lek <-  gsub(" ", "_", strut_90$lek) 
 
 
 
 #Looks like the lice column is cross-referenced in the averages dataset. Let's read that in
 strut_90_averages <- read.csv("prelim_clean/1990_strut_average_data.csv", 
                               header=FALSE)
strut_90_averages <- fix_column_names(strut_90_averages)
 strut_90_averages_select <- strut_90_averages %>%
   dplyr::select(l_tag, r_tag, malaria, lice)
 
 #Remove the existing lice, breed, and malaria columns in strut_90 to make the merge easier
 strut_90 <- strut_90 %>%
  select(-lice, -malaria)
 
 #I think we can cross-reference the lice information to the original by tag
 strut_90 <- merge(strut_90, 
                   strut_90_averages_select, 
                   by = c("l_tag", "r_tag"), 
                   all.x = TRUE)
 
 #Fix the dates into new columns 
 strut_90 <- convert_date_columns(strut_90)
 
 
 
 
 #Cool - now can we add in the 1988 data with the same workflow, and then merge
 
 strut_88 <- read.csv("prelim_clean/1988_strut_frequency_data.csv", 
                      header=FALSE)
 head(strut_88)
 #looks like column names got a bit messed up
 #Correct for proper column names
 
 strut_88 <- fix_column_names(strut_88)
 #Looks like two columns are empty, and need be removed
 strut_88 <- strut_88 %>%
   dplyr::select(-na_na)
 #We will also need to add a lice column, since it's missing from this one
 strut_88$lice <- ''
 
 
 #Now, we can remove blank rows
 strut_88<- strut_88 %>% mutate_all(na_if,"")
 strut_88<- strut_88 %>% mutate_all(na_if," ") #Looks like some rows had a space instead of a blank
 strut_88 <- strut_88[rowSums(is.na(strut_88)) != ncol(strut_88), ]
 
 
 
 #I see an interesting inconsistency with the breeding data - there are no zeroes. 
 #However, there are zeros in the average data
 # So, for this reason I believe we can assume that NAs are no breeds 
 strut_88$breed[is.na(strut_88$breed)] <- 0
 
 #looks like the years need a 19 added to them
 #let's fix that
 strut_88$year <- paste0("19", strut_88$year)
 #check that worked: 
 unique(strut_88$year) #it worked! 
 
 #The names of the leks should be standardized for analyses
 strut_88$lek <- tolower(strut_88$lek) #make lower case
 strut_88$lek <-  gsub(" ", "_", strut_88$lek) 
 
 
 #Looks like the lice column is cross-referenced in the averages dataset. Let's read that in
 strut_88_averages <- read.csv("prelim_clean/1988_strut_average_data.csv", header=FALSE)
 strut_88_averages <- fix_column_names(strut_88_averages)
 #replace the last column name with lice 
 strut_88_averages$lice <- strut_88_averages$na_na
 strut_88_averages <- strut_88_averages%>%
   select(-na_na)
 
 
 strut_88_averages_select <- strut_88_averages %>%
   dplyr::select(l_tag, r_tag, malaria, lice)
 
 #Remove the existing lice and malaria columns in strut_88 to make the merge easier
 strut_88 <- strut_88 %>%
   select(-lice, -malaria)
 
 #I think we can cross-reference the lice information to the original by tag
 strut_88 <- merge(strut_88, 
                   strut_88_averages_select, 
                   by = c("l_tag", "r_tag"), 
                   all.x = TRUE)
 
 #Fix the dates into new columns 
 strut_88 <- convert_date_columns(strut_88)
 
 
 
 
 
 
 #Cool - now can we add in the 1987 data with the same workflow, and then merge
 
 strut_87 <- read.csv("prelim_clean/1987_strut_frequency_data.csv", 
                      header=FALSE)
 head(strut_87)
 #looks like column names got a bit messed up
 #Correct for proper column names
 
 strut_87 <- fix_column_names(strut_87)
 #Looks like two columns are empty, and need be removed
 strut_87 <- strut_87 %>%
   dplyr::select(-na_na)
 
 
 #Now, we can remove blank rows
 strut_87<- strut_87 %>% mutate_all(na_if,"")
 strut_87<- strut_87 %>% mutate_all(na_if," ") #Looks like some rows had a space instead of a blank
 strut_87 <- strut_87[rowSums(is.na(strut_87)) != ncol(strut_87), ]
 
 
 #looks like the years need a 19 added to them
 #let's fix that
 strut_87$year <- paste0("19", strut_87$year)
 #check that worked: 
 unique(strut_87$year) #it worked! 
 
 #The names of the leks should be standardized for analyses
 strut_87$lek <- tolower(strut_87$lek) #make lower case
 strut_87$lek <-  gsub(" ", "_", strut_87$lek) 
 
 
 
 #Looks like the lice column is cross-referenced in the averages dataset. Let's read that in
 strut_87_averages <- read.csv("prelim_clean/1987_strut_average_data.csv", 
                               header=FALSE)
 strut_87_averages <- fix_column_names(strut_87_averages)
 strut_87_averages_select <- strut_87_averages %>%
   dplyr::select(l_tag, r_tag, malaria, lice)
 
 #Remove the existing lice and malaria columns in strut_87 to make the merge easier
 strut_87 <- strut_87 %>%
   select(-lice, -malaria)
 
 #I think we can cross-reference the lice information to the original by tag
 strut_87 <- merge(strut_87, 
                   strut_87_averages_select, 
                   by = c("l_tag", "r_tag"), 
                   all.x = TRUE)
 
 
 #Fix the dates into new columns 
 strut_87 <- convert_date_columns(strut_87)
 
 
 
 #Looks like it worked! Now I can make a preliminary combined dataset
 strut_merged <- rbind(strut_90, strut_89, strut_88, strut_87)
 
 
 #One last thing to fix - the names of the leks aren't standard across dataframes
 #s. needs to be replaced with south, fetter needs to be replaced with fetterman, 
 #wind. needs to be replaced with windmill, and spgs needs ot be replaced with springs
 # Define the replacement patterns and their corresponding replacements
 patterns <- c("s\\.","south_fetter", "wind\\.", "spgs", "sybille")
 replacements <- c("south", "south_fetterman", "windmill", "springs", "sybille_springs")
 
 # Iterate over each pattern and replacement
 for (i in seq_along(patterns)) {
   strut_merged$lek <- gsub(patterns[i], replacements[i], strut_merged$lek, ignore.case = TRUE)
 }
 strut_merged$lek <- str_replace(strut_merged$lek, "south_fettermanman", "south_fetterman")
 strut_merged$lek <- str_replace(strut_merged$lek, "sybille_springs_springs", "sybille_springs")
 
 unique(strut_merged$lek)
 
 #looks like we do actually need new tag columns: colour and number
 #This allows us to compare between datasets
 #split the tags into colour and number for easier comparison between data
 strut_merged$right_tag_colour <- str_extract(strut_merged$r_tag, "[A-Za-z]+")
 strut_merged$right_tag_number <- str_extract(strut_merged$r_tag, "\\d+")
 strut_merged <- strut_merged %>%
   relocate(contains('right_tag'), .after = r_tag)
 
 strut_merged$left_tag_colour <- str_extract(strut_merged$l_tag, "[A-Za-z]+")
 strut_merged$left_tag_number <- str_extract(strut_merged$l_tag, "\\d+")
 strut_merged <- strut_merged %>%
   relocate(contains('left_tag'), .after = l_tag)
 
 
 #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
 #_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
 
 #Part 3: Get data to match tagsort standards 
 
 #Tagsort has colours as actual colours - need to replace 
 # Define a lookup table for color replacements
 color_lookup <- c("G" = "green", "R" = "red", "Y" = "yellow", "W" = "white", "O" = "orange", "B" = "blue")
 
 # Replace single letters with corresponding colors
 strut_merged$left_tag_colour <- color_lookup[strut_merged$left_tag_colour]
 strut_merged$right_tag_colour <- color_lookup[strut_merged$right_tag_colour]
 
 #now rename the code columns to match tagsort 
 strut_merged <- rename(strut_merged, right_tag_code = r_tag)
 strut_merged <- rename(strut_merged, left_tag_code = l_tag)
 
 strut_merged <- rename(strut_merged, lek_name = lek)
 
 #Replace "juven" with "juv" to match strut
 strut_merged$age <- gsub("juven", "juv", strut_merged$age)
 
 
 
 #Rather than 0 and 1, have yes and no
 strut_merged <- strut_merged %>%
   mutate(breed = case_when(breed == "0" ~ "no",
                            breed=="1" ~ 'yes',
                            breed=="3" ~ 'NA',
                                      TRUE ~ breed),
          malaria = case_when(malaria == "0" ~ "no",
                            malaria=="1" ~ 'yes',
                            malaria=="3" ~ 'NA',
                            TRUE ~ malaria),
          lice = case_when(lice == "0" ~ "no",
                            lice=="1" ~ 'yes',
                           lice=="3" ~ 'NA',
                            TRUE ~ lice))
 
 write.csv(strut_merged, "prelim_clean/strut_merged_df_prelim.csv", 
           row.names=FALSE)
 
 
 
 
 
 #Do some preliminary analyses for fun
 
 #Contingency table - breed vs malaria
 strut_89_no3 <- strut_merged %>%
   filter(!breed==3, !malaria==3, !lice==3)
 breed_malaria <- table(strut_89_no3$breed, strut_89_no3$malaria)
 breed_malaria
 chisq.test(breed_malaria)
 
 breed_lice <- table(strut_89_no3$breed, strut_89_no3$lice)
 breed_lice
 chisq.test(breed_lice)
 
 
 #Quick plot of number of struts vs lice
 strut_89_no3 %>%
   ggplot(aes(x = as.character(lice), y = as.numeric(struts_5_min), colour = as.character(year))) +
   geom_violin(position = "dodge") +
   stat_summary(aes(group = as.character(year)), fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(width = 0.8)) +
   stat_summary(aes(group = as.character(year)), geom = "errorbar", fun.data = mean_se, width = 0.2, color = "black", position = position_dodge(width = 0.8)) +
   stat_summary(aes(group = as.character(year)), geom = "crossbar", fun.data = mean_sdl, width = 0.4, color = "black", position = position_dodge(width = 0.8)) +
   labs(x = "lice", y = "Number of struts in 5 minutes", colour = "Year") +
   scale_colour_manual(values = c("1990" = "blue", "1989" = "green", 
                                  '1988' = 'red', '1987'='orange')) +
   theme_classic()
 
 strut_merged %>%
   filter(!breed==3) %>%
   ggplot(aes(y=as.numeric(struts_5_min), x=as.character(breed), colour = as.character(year)))+
   geom_violin()+
   labs(x="Breed", y="Time showing before sunrise")+
   stat_summary(aes(group = as.character(year)), fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(width = 0.8)) +
   stat_summary(aes(group = as.character(year)), geom = "errorbar", fun.data = mean_se, width = 0.2, color = "black", position = position_dodge(width = 0.8)) +
   stat_summary(aes(group = as.character(year)), geom = "crossbar", fun.data = mean_sdl, width = 0.4, color = "black", position = position_dodge(width = 0.8)) +
   labs(x = "Breed", y = "Number of struts in 5 minutes", colour = "Year") +
   scale_colour_manual(values = c("1990" = "blue", "1989" = "green", 
                                  '1988' = 'red', '1987'='orange')) +
   theme_classic()
 
 #Run an anova to see how lice affects...
 strut_89_no3$lice <- as.factor(strut_89_no3$lice)
 summary(stats::aov(struts_5_min ~ lice*year, 
     data=strut_merged))
 
#significant difference! 
 
 strut_89_avg_test <- strut_merged %>%
    filter(year==1989) %>%
   filter(!dist_to_hens==0, !dist_to_hens==4) %>%
   group_by(l_tag, r_tag) %>%
   dplyr::summarize(mean_struts_5 = mean(as.numeric(struts_5_min)), 
             mean_time_strut = mean(as.numeric(time_strut)), 
             mean_sunrise = mean(as.numeric(sunrise)), 
             mean_time_to_sunrise = mean(as.numeric(time_to_sunrise)), 
             mean_dist_to_hens = mean(as.numeric(dist_to_hens)), 
             mean_breed = mean(as.numeric(breed)), 
             mean_malaria = mean(as.numeric(malaria)), 
             mean_lice = mean(as.numeric(lice)))

