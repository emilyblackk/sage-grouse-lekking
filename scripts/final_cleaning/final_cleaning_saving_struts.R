#Emily Black
#Cleaning up, checking all strut binders
#Created: 12 July 2023
#Last Modified: 12 July 2023


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


#Part 1. Read in all the finalized strutting datasets


struts_1990 <- read.csv("prelim_clean/1990/merged_struts_binders_floppy_1990_MS_N_SF_RS_SS.csv")
struts_1989 <- read.csv("prelim_clean/1989/merged_struts_binders_floppy_1989_C_MC_NM_SF_LL_N_VP_MS_SS.csv")
struts_1988 <- read.csv("prelim_clean/1988/merged_struts_binders_floppy_1988_C_BW_RP_LL_WD_MS_N_SS_V_SF_MCJ.csv")
struts_1987 <- read.csv("prelim_clean/1987/merged_struts_binders_floppy_1987_C_BW_MS_N_SF_WD_SS.csv")

#Merge them all into one big strut dataset
all_struts <- rbind(struts_1990, struts_1989, struts_1988, struts_1987)

#Check that all years and leks read in
table(all_struts$year)
#add missing dates back into page 45 north lek from struts_1987
all_struts$year <- ifelse(all_struts$binder_pdf_page_number == '45' & all_struts$lek_name == 'north', '1987', all_struts$year)


#Check leks
table(all_struts$lek_name)
#looks great!

#Now, go column-by-column and correct inconsistencies
table(all_struts$source)
#31 floppies that didn't find a match, with 1229 that did
#That's
31/(1229+31)
#percent, which is pretty good

#binder page
unique(all_struts$binder_pdf_page_number)
#all looking good, not seeing anything really weird

#year
table(all_struts$year)
#looks good

#month
table(all_struts$month)
#looks good

#day
table(all_struts$day)
#also looks good

#arrival time
unique(all_struts$arrival_time)
#remove AM
all_struts$arrival_time<- gsub(":\\d{2}(?=\\s)", "", all_struts$arrival_time, perl = TRUE)
all_struts$arrival_time <- gsub("\\s*AM", "", all_struts$arrival_time)
#looks pretty good
#a few are not explicitly times, 
#but contain important information
#so i will leave as is

#observer
unique(all_struts$observer)
#looks good

#lek name
unique(all_struts$lek_name)
#looks great, I don't see any repeats

#trapping activity
unique(all_struts$trapping_activity)
#this should all probably be made lower case just to make it look more standardized
all_struts$trapping_activity <- tolower(all_struts$trapping_activity)
#easier to read

#wind_mph
unique(all_struts$wind_mph)
#looks like there are a few values that need to be tidied
#Due to a quirk in excel, some winds are set as dates
#will fix that
all_struts <- all_struts %>%
  mutate(wind_mph = case_when(
    wind_mph=="10-May" ~ "5 to 10", 
    TRUE~wind_mph)
  )
#also, put all to lower
all_struts$wind_mph <- tolower(all_struts$wind_mph)
#replace every dash and typo with "to" to prevent further excel mixups
all_struts$wind_mph <- gsub("-", "to", all_struts$wind_mph)
all_struts$wind_mph <- gsub("tl", "to", all_struts$wind_mph)
#make sure there is only one space on either side of the "to"
all_struts$wind_mph <- gsub("(\\b\\d+)\\s*to\\s*(\\d+\\b)", "\\1 to \\2", all_struts$wind_mph, perl = TRUE)
#there's too much written information for it all to be cleaned up perfectly, 
#but that's much better visually!


#cloud_cover_percent
unique(all_struts$cloud_cover_percent)
#looks like there are a few values that need to be tidied
#Due to a quirk in excel, some clouds are set as dates
#will fix that
all_struts <- all_struts %>%
  mutate(cloud_cover_percent = case_when(
    cloud_cover_percent=="10-May" ~ "5 to 10", 
    cloud_cover_percent=="20-Oct" ~ "10 to 20", 
    TRUE~cloud_cover_percent)
  )
all_struts$cloud_cover_percent <- gsub("-", "to", all_struts$cloud_cover_percent)
all_struts$cloud_cover_percent <- gsub("(\\b\\d+)\\s*to\\s*(\\d+\\b)", "\\1 to \\2", all_struts$cloud_cover_percent, perl = TRUE)
all_struts$cloud_cover_percent <- tolower(all_struts$cloud_cover_percent)

#I believe the column name is misleading - there are a lot of comments
#that are not %. 
#I'll rename it to 'cloud_cover_percent_comments' 
all_struts <- all_struts %>%
  rename(cloud_cover_percent_comments = cloud_cover_percent)


#snowing status
table(all_struts$snowing_status)
unique(all_struts$snowing_status)
#put to lower case
all_struts$snowing_status <- tolower(all_struts$snowing_status)
#replace n and y with no and yes
all_struts$snowing_status[all_struts$snowing_status=="y"] <- "yes"
all_struts$snowing_status[all_struts$snowing_status=="n"] <- "no"
#two different "lightly"s, standardize
all_struts$snowing_status[all_struts$snowing_status=="lightly " ] <- "lightly"  
#much cleaner - again, still "notes" style but better


#raining status
table(all_struts$raining_status)
unique(all_struts$raining_status)
#put to lower case
all_struts$raining_status <- tolower(all_struts$raining_status)
#replace n and y with no and yes
all_struts$raining_status[all_struts$raining_status=="y"] <- "yes"
all_struts$raining_status[all_struts$raining_status=="n"] <- "no"
#occasional has an extra space, fix
all_struts$raining_status[all_struts$raining_status=="occasional " ] <- "occasional"


#temp
table(all_struts$temp_f)
unique(all_struts$temp_f)
#Fix the excel dates
all_struts <- all_struts %>%
  mutate(temp_f = case_when(
    temp_f=="10-May" ~ "5 to 10", 
    temp_f=="20-Oct" ~ "10 to 20", 
    temp_f=="15-Oct" ~ "10 to 15", 
    TRUE~temp_f))
all_struts$temp_f <- gsub("-", "to", all_struts$temp_f)
all_struts$temp_f <- gsub("(\\b\\d+)\\s*to\\s*(\\d+\\b)", "\\1 to \\2", all_struts$temp_f, perl = TRUE)
all_struts$temp_f <- tolower(all_struts$temp_f)
#make sure the tilde is flush with the number
all_struts$temp_f <- gsub("~\\s+(\\d+)", "~\\1", all_struts$temp_f)
#Again, this is more of an observation than a strict following of numerical temp
#leave as is so as to keep info

#other weather conditions
unique(all_struts$other_weather_conditions)
#fix a quick typo
all_struts$other_weather_conditions[all_struts$other_weather_conditions=="Ver cold, 17 degrees record low for data"] <- "Very cold, 17 degrees record low for data"  
#looks good!

#lek ground condition
unique(all_struts$lek_ground_condition)
#put to lower and then good
all_struts$lek_ground_condition <- tolower(all_struts$lek_ground_condition)

#predator disturbance type
unique(all_struts$predator_disturbance_type_)
#put to lower case
all_struts$predator_disturbance_type_ <- tolower(all_struts$predator_disturbance_type_)
#replace double question mark with single
all_struts$predator_disturbance_type_[all_struts$predator_disturbance_type_=="??"] <- "?"
#fix extra space question mark
all_struts$predator_disturbance_type_[all_struts$predator_disturbance_type_=="? "  ] <- "?"
#fix extra space golden eagle and raptor
all_struts$predator_disturbance_type_[all_struts$predator_disturbance_type_=="golden eagle "] <- "golden eagle"
all_struts$predator_disturbance_type_[all_struts$predator_disturbance_type_=="raptor " ] <- "raptor"
#replace "none" with NA to avoid confusion
all_struts$predator_disturbance_type_[all_struts$predator_disturbance_type_=="none" ] <- NA
#rename column to avoid trailing underscore
all_struts <- all_struts %>%
  rename(predator_disturbance_type = predator_disturbance_type_)

#predator time
unique(all_struts$predator_disturbance_time)
#replace the AM with just the time
all_struts$predator_disturbance_time<- gsub(":\\d{2}(?=\\s)", "", all_struts$predator_disturbance_time, perl = TRUE)
all_struts$predator_disturbance_time <- gsub("\\s*AM", "", all_struts$predator_disturbance_time)
#much better!

#type
unique(all_struts$type)
table(all_struts$type)
#great! Looks like we've got 1468 struts


#left_tag_color
#This might get tricky
unique(all_struts$left_tag_color)
table(all_struts$left_tag_color)
#Things separated by commas indicate a disgreement between
#struts and floppies during the match
#there are a few weird ones that should get fixed first
all_struts$left_tag_color[all_struts$left_tag_color=="UM red"] <- "UM"
all_struts$left_tag_color[all_struts$left_tag_color=="UM red"] <- "UM"
#remove UM blue row and white NA - not found in dataset
all_struts <- all_struts[!(all_struts$left_tag_color == "UM blue" & !is.na(all_struts$left_tag_color)), ]
all_struts <- all_struts[!(all_struts$left_tag_color == "white NA" & !is.na(all_struts$left_tag_color)), ]
#rename all the different ways to say unmarked
all_struts$left_tag_color[all_struts$left_tag_color=="UM"] <- "unmarked"
#rename NA repeats
all_struts$left_tag_color <- gsub("NA NA", "NA", all_struts$left_tag_color)
all_struts$left_tag_color <- gsub("NA NA", "NA", all_struts$left_tag_color)
#So, the commas show a bit of disagreement between floppies and binders
count_comma_cells <- function(column) {
  sum(grepl(",", column, fixed = TRUE))
}
count_comma_cells(all_struts$left_tag_color)
#I trust the floppies, as these were reviewed by Pat Diebert (nee White)
#who had more experience with these birds
#however, if there is a NA value, take whichever one is not NA. 
#extract value function
extract_value <- function(x) {
  parts <- strsplit(x, ", ")[[1]]  # Split the string by comma
  if (length(parts) == 1) {
    return(parts[1])  # Return the single value
  } else {
    if (is.na(parts[1])) {
      return(parts[2])  # Return the second value if first value is NA
    } else if (is.na(parts[2])) {
      return(parts[1])  # Return the first value if second value is NA
    } else {
      return(parts[2])  # Return the second value if both are non-NA
    }
  }
}

all_struts$left_tag_color_2 <- sapply(all_struts$left_tag_color, extract_value)
#relocate this one next to the original to compare
all_struts <- all_struts %>%
  relocate(left_tag_color_2, .after = left_tag_color)
unique(all_struts$left_tag_color_2)
#that worked fantastic! now replace left_tag_color with left_tag_color_2
all_struts <- all_struts %>%
  select(!left_tag_color) %>%
  rename(left_tag_color = left_tag_color_2)

#now do the same for number and code
all_struts$left_tag_number_2 <- sapply(all_struts$left_tag_number, extract_value)
#relocate this one next to the original to compare
all_struts <- all_struts %>%
  relocate(left_tag_number_2, .after = left_tag_number)
unique(all_struts$left_tag_number_2)
#that worked fantastic! now replace left_tag_number with left_tag_number_2
all_struts <- all_struts %>%
  select(!left_tag_number) %>%
  rename(left_tag_number = left_tag_number_2)


all_struts$left_tag_code_2 <- sapply(all_struts$left_tag_code, extract_value)
#relocate this one next to the original to compare
all_struts <- all_struts %>%
  relocate(left_tag_code_2, .after = left_tag_code)
unique(all_struts$left_tag_code_2)
#that worked fantastic! now replace left_tag_code with left_tag_code_2
all_struts <- all_struts %>%
  select(!left_tag_code) %>%
  rename(left_tag_code = left_tag_code_2)


#now do the same cleanup for right 
#right_tag_color
#This might get tricky
unique(all_struts$right_tag_color)
table(all_struts$right_tag_color)
#Things separated by commas indicate a disgreement between
#struts and floppies during the match
#there are a few weird ones that should get fixed first
#somehow these are much nicer than left! 
#move right into replacement

all_struts$right_tag_color_2 <- sapply(all_struts$right_tag_color, extract_value)
#relocate this one next to the original to compare
all_struts <- all_struts %>%
  relocate(right_tag_color_2, .after = right_tag_color)
unique(all_struts$right_tag_color_2)
#that worked fantastic! now replace right_tag_color with right_tag_color_2
all_struts <- all_struts %>%
  select(!right_tag_color) %>%
  rename(right_tag_color = right_tag_color_2)

#now do the same for number and code
all_struts$right_tag_number_2 <- sapply(all_struts$right_tag_number, extract_value)
#relocate this one next to the original to compare
all_struts <- all_struts %>%
  relocate(right_tag_number_2, .after = right_tag_number)
unique(all_struts$right_tag_number_2)
#that worked fantastic! now replace right_tag_number with right_tag_number_2
all_struts <- all_struts %>%
  select(!right_tag_number) %>%
  rename(right_tag_number = right_tag_number_2)


all_struts$right_tag_code_2 <- sapply(all_struts$right_tag_code, extract_value)
#relocate this one next to the original to compare
all_struts <- all_struts %>%
  relocate(right_tag_code_2, .after = right_tag_code)
unique(all_struts$right_tag_code_2)
#that worked fantastic! now replace right_tag_code with right_tag_code_2
all_struts <- all_struts %>%
  select(!right_tag_code) %>%
  rename(right_tag_code = right_tag_code_2)



#age
unique(all_struts$age)
#easy peasy!

#time
unique(all_struts$time)
#this was observation time - the column should be renamed as such
all_struts <- all_struts %>%
  rename(observation_time = time)
all_struts$observation_time <- gsub("^\\s+|\\s+$", "", all_struts$observation_time)


#sunrise time
#I do tend to trust the binder observations more for this one
unique(all_struts$sunrise_time)
#remove tildes and question marks
all_struts$sunrise_time <- gsub("~", "", all_struts$sunrise_time)
all_struts$sunrise_time <- gsub("\\?", "", all_struts$sunrise_time)
#create new function to extract from before comma
extract_value_before_comma <- function(x) {
  parts <- strsplit(x, ", ")[[1]]  # Split the string by comma
  if (length(parts) == 1) {
    return(parts[1])  # Return the single value
  } else {
    if (is.na(parts[1])) {
      return(parts[2])  # Return the second value if first value is NA
    } else if (is.na(parts[2])) {
      return(parts[1])  # Return the first value if second value is NA
    } else {
      return(parts[1])  # Return the first value if both are non-NA
    }
  }
}
all_struts$sunrise_time <- sapply(all_struts$sunrise_time, extract_value_before_comma)
#remove extra question marks
all_struts$sunrise_time <- gsub(" ?", "", all_struts$sunrise_time)
all_struts$sunrise_time <- gsub("?", "", all_struts$sunrise_time)
#remove the 1 and the 54
all_struts <- all_struts %>%
  filter(!sunrise_time=="54") %>%
  filter(!sunrise_time=="1")
all_struts$sunrise_time <- gsub("toocloudytotell", NA, all_struts$sunrise_time)
all_struts$sunrise_time[all_struts$sunrise_time=="6:00(06:10onlek)"] <- "6:00"
all_struts$sunrise_time[all_struts$sunrise_time=="6:38-6:44"] <- "6:38"
#much cleaner!



#num males observed
unique(all_struts$num_males_observed)
#there are some values with plusses
#this can indicate the researchers keeping track of their count, 
#or multiple mating centers. 
#multiple mating centers are indicated in the comments, typically
#so for simplicities sake I will just remove ones with pluses, 
#and remove extra text
#These are important to be standardized to just numbers for analyses later
all_struts$num_males_observed <- gsub("\\s*\\(.*?\\)", "", all_struts$num_males_observed)
all_struts$num_males_observed_2 <- sapply(all_struts$num_males_observed, function(x) {
  if (grepl("\\+", x)) {
    sum(as.numeric(strsplit(x, "\\+")[[1]]))
  } else {
    x
  }
})
unique(all_struts$num_males_observed_2)
#remove the tilde, and the erroneous character value
all_struts$num_males_observed_2 <- gsub("~", "", all_struts$num_males_observed_2)
all_struts$num_males_observed_2[all_struts$num_males_observed_2=="Eagle flew over"] <- NA
all_struts <- all_struts %>%
  relocate(num_males_observed_2, .after = num_males_observed) %>%
  select(-num_males_observed) %>%
  rename(num_males_observed =num_males_observed_2)
all_struts$num_males_observed <- as.numeric(all_struts$num_males_observed)


#num females observed
unique(all_struts$num_females_observed)
#remove the tilde, and the erroneous character value
all_struts$num_females_observed <- gsub("~", "", all_struts$num_females_observed)
all_struts$num_females_observed[all_struts$num_females_observed=="1 at 5:36"] <- "1"
all_struts$num_females_observed[all_struts$num_females_observed=="?" ] <- NA
all_struts$num_females_observed <- gsub("?", "", all_struts$num_females_observed)
all_struts$num_females_observed <- gsub("\\s*\\(.*?\\)", "", all_struts$num_females_observed)
all_struts$num_females_observed <- as.numeric(all_struts$num_females_observed)

#that's clean!

#comments_observations_or_marks
unique(all_struts$comments_observations_or_marks)
#mostly just comments, not much I can correct 

#time start
unique(all_struts$time_start)
#Firstly, correct everything with AM in it
all_struts$time_start<- gsub(":\\d{2}(?=\\s)", "", all_struts$time_start, perl = TRUE)
all_struts$time_start <- gsub("\\s*AM", "", all_struts$time_start)
#now, there are a lot separated by commas, indicating a mismatch
#note that many of these are likely typos in the floppies or 
table(all_struts$time_start)
count_comma_cells(all_struts$time_start)
#I do trust the binder data, and they are accurate to the calculations of struts_per_5 from binders, 
#which were often incorrect in the floppies 
#So keep the value before the comma
all_struts$time_start <- sapply(all_struts$time_start, extract_value_before_comma)
#replace a few erroneous values
all_struts$time_start[all_struts$time_start=="- "] <- NA
all_struts$time_start[all_struts$time_start=="0"] <- NA
all_struts$time_start[all_struts$time_start=="NA:NA"] <- NA
all_struts$time_start[all_struts$time_start=="709"] <- "7:09"

#time end
unique(all_struts$time_end)
#no commas with a mismatch!
table(all_struts$time_end)
count_comma_cells(all_struts$time_end)
#replace a few erroneous values
all_struts$time_end[all_struts$time_end=="Female"] <- NA
all_struts$time_end[all_struts$time_end=="5"] <- NA
all_struts$time_end[all_struts$time_end=="- "] <- NA

#num_of_struts 
unique(all_struts$num_of_struts)
#mostly good, just need to replace a few erroneous values
all_struts$num_of_struts[all_struts$num_of_struts=="9+ but lost sight before finished"] <- NA
all_struts$num_of_struts[all_struts$num_of_struts=="?"] <- NA
#make numeric
all_struts$num_of_struts <- as.numeric(all_struts$num_of_struts)
#plot to see distribution
all_struts %>%
  ggplot(aes(x=num_of_struts))+
  geom_histogram()+
  theme_classic()
#looks great!

#struts_5_min
unique(all_struts$struts_5_min)
#some disagreement here with the binderes and floppies
count_comma_cells(all_struts$struts_5_min)
#I trust the binders on this one - plus, they were calculated from the actual
#times on the binders
#and the floppies sometimes make mistakes
all_struts$struts_5_min <- sapply(all_struts$struts_5_min, extract_value_before_comma)
unique(all_struts$struts_5_min)
all_struts$struts_5_min <- as.numeric(all_struts$struts_5_min)
#plot to see distribution
all_struts %>%
  ggplot(aes(x=struts_5_min))+
  geom_histogram()+
  theme_classic()
#looks good!


#distance to hens category
unique(all_struts$distance_to_hens_category)
#the floppies are probably more correct, and have less formatting errors
#keep floppy values
all_struts$distance_to_hens_category <- sapply(all_struts$distance_to_hens_category, extract_value)
table(all_struts$distance_to_hens_category)
#looks awesome!
#let's plot num of struts against distance to hens to see if there's a relationship 
all_struts$distance_to_hens_category <- as.character(all_struts$distance_to_hens_category)
all_struts %>%
  ggplot(aes(x=distance_to_hens_category, y=struts_5_min)) + 
  geom_violin() + 
  theme_classic()
#looks very reasonable to me

#distance to hens comments
unique(all_struts$distance_to_hens_notes)
#remove leading and trailing white space
all_struts$distance_to_hens_notes <- gsub("^\\s+|\\s+$", "", all_struts$distance_to_hens_notes)
all_struts$distance_to_hens_notes <- tolower(all_struts$distance_to_hens_notes)

#fights
unique(all_struts$fights)
all_struts$fights <- gsub("^\\s+|\\s+$", "", all_struts$fights)
#looks good - mostly comments so can't clean too much

#num_of_copulations
unique(all_struts$num_of_copulations)
#remove brackets 
#fix a few erroneous values
all_struts$num_of_copulations[all_struts$num_of_copulations== "1 - interrupted by BG (?) was successful"] <- "1"
all_struts$num_of_copulations[all_struts$num_of_copulations== "1(?)"] <- "1"
all_struts$num_of_copulations <- as.numeric(all_struts$num_of_copulations)
table(all_struts$num_of_copulations)

#breed
unique(all_struts$breed)
table(all_struts$breed)
#looks good!

#lice
unique(all_struts$lice)
table(all_struts$lice)

#malaria
unique(all_struts$malaria)
table(all_struts$malaria)

#notes
unique(all_struts$other_fights_and_comments)
#probably not worth cleaning up other than removing leading and trailing white space
all_struts$other_fights_and_comments <- gsub("^\\s+|\\s+$", "", all_struts$other_fights_and_comments)
#looks like excel also messed up some values - replace
all_struts$other_fights_and_comments <- gsub("02-Jan", "1 of 2", all_struts$other_fights_and_comments)
all_struts$other_fights_and_comments <- gsub("02-Feb", "2 of 2", all_struts$other_fights_and_comments)


#last thing to do - split struts and copulations from observations to clean up columns
struts_cops <- all_struts %>%
  filter(type %in% c("strut", "copulation"))
unique(struts_cops$type)
#remove columns with only NA
struts_cops <- struts_cops[, colSums(is.na(struts_cops)) != nrow(struts_cops)]

observations <- all_struts %>%
  filter(type %in% c("observation"))
unique(observations$type)
#remove columns with only NA
observations <- observations[, colSums(is.na(observations)) != nrow(observations)]


#save to file
write.csv(observations, "clean_data/lek_observations_1987_1990.csv", 
          row.names=FALSE)
write.csv(struts_cops, "clean_data/lek_struts_copulations_1987_1990.csv", 
          row.names=FALSE)







