#Emily Black
#Cleaning egg and chick
#Created: 19 June 2023
#Last modified: 

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
chick <- read.csv('raw_data/binders/chick_data_from_eggandchick.csv')
egg <- read.csv('raw_data/binders/egg_data_from_eggandchick.csv')
nest_search <- read_csv('raw_data/binders/nest_search_data_from_eggandchick.csv')
nest_followup <- read_csv('raw_data/binders/nest_follow_up_data_from_eggandchick.csv')


#For all dataframes, remove columns with only NA
# Identify columns with only NA values
na_columns <- colSums(is.na(chick)) == nrow(chick)

# Remove columns with only NA values
chick <- chick[, !na_columns]


# Identify columns with only NA values
na_columns <- colSums(is.na(egg)) == nrow(egg)

# Remove columns with only NA values
egg <-egg[, !na_columns]

# Identify columns with only NA values
na_columns <- colSums(is.na(nest_search)) == nrow(nest_search)

# Remove columns with only NA values
nest_search <-nest_search[, !na_columns]


# Identify columns with only NA values
na_columns <- colSums(is.na(nest_followup)) == nrow(nest_followup)

# Remove columns with only NA values
nest_followup <-nest_followup[, !na_columns]



#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 2: Cleaning chick



#Add in new column for each obs saying how many days after hatching it is 
chick$date_hatch <- as.Date(paste0(chick$month_hatch, "-", chick$day_hatch, "-", 1990), format = "%m-%d-%Y")
chick$date_obs_1 <- as.Date(paste0(chick$obs_1_month, "-", chick$obs_1_day, "-", 1990), format = "%m-%d-%Y")
chick$obs_1_days_after_hatch <- as.integer(difftime(chick$date_obs_1, chick$date_hatch, units = "days")) 
chick <- chick %>%
  select(-date_hatch, -date_obs_1) %>%
  relocate(obs_1_days_after_hatch, .after=obs_1_day)

chick$date_hatch <- as.Date(paste0(chick$month_hatch, "-", chick$day_hatch, "-", 1990), format = "%m-%d-%Y")
chick$date_obs_2 <- as.Date(paste0(chick$obs_2_month, "-", chick$obs_2_day, "-", 1990), format = "%m-%d-%Y")
chick$obs_2_days_after_hatch <- as.integer(difftime(chick$date_obs_2, chick$date_hatch, units = "days")) 
chick <- chick %>%
  select(-date_hatch, -date_obs_2) %>%
  relocate(obs_2_days_after_hatch, .after=obs_2_day)

chick$date_hatch <- as.Date(paste0(chick$month_hatch, "-", chick$day_hatch, "-", 1990), format = "%m-%d-%Y")
chick$date_obs_3 <- as.Date(paste0(chick$obs_3_month, "-", chick$obs_3_day, "-", 1990), format = "%m-%d-%Y")
chick$obs_3_days_after_hatch <- as.integer(difftime(chick$date_obs_3, chick$date_hatch, units = "days")) 
chick <- chick %>%
  select(-date_hatch, -date_obs_3) %>%
  relocate(obs_3_days_after_hatch, .after=obs_3_day)

chick$date_hatch <- as.Date(paste0(chick$month_hatch, "-", chick$day_hatch, "-", 1990), format = "%m-%d-%Y")
chick$date_obs_4 <- as.Date(paste0(chick$obs_4_month, "-", chick$obs_4_day, "-", 1990), format = "%m-%d-%Y")
chick$obs_4_days_after_hatch <- as.integer(difftime(chick$date_obs_4, chick$date_hatch, units = "days")) 
chick <- chick %>%
  select(-date_hatch, -date_obs_4) %>%
  relocate(obs_4_days_after_hatch, .after=obs_4_day)


df_condensed <- chick %>%
  pivot_longer(cols =starts_with("obs_"),
    names_to = 'observation', 
               names_prefix = 'obs_', 
               values_to = 'values'
               )

df_condensed <- df_condensed %>%
  tidyr::separate(observation, into = c("observation_num", "variable"),
                  sep = "(?<=\\d)(?=\\D)", remove = FALSE)
# Remove the underscore from the "text" column
df_condensed$variable <- sub("_", "", df_condensed$variable)
df_condensed <- df_condensed %>%
  select(-observation)

#repair column names for pivot_wider
df_condensed$variable <- paste0("followup_", df_condensed$variable)


df_wide <- df_condensed %>%
  pivot_wider(names_from = variable, values_from = values, 
              names_repair = 'unique')

df_longer_hatch_followup <- df_wide %>%
  pivot_longer(cols =contains(c("hatch", 'followup')),
               names_to = 'stage', 
               values_to = 'values', 
               values_transform = list(values = as.character))

# If chicks contains the word 'hatch', set the observation_num value to zero
df_longer_hatch_followup$observation_num <- ifelse(grepl("hatch", df_longer_hatch_followup$stage, 
                                              ignore.case = TRUE) & !grepl("days_after_hatch", 
                                                df_longer_hatch_followup$stage, ignore.case = TRUE), 
                                              0, df_longer_hatch_followup$observation_num)

# Remove the words "followup" and "hatch" from the column
df_longer_hatch_followup$stage <- gsub("_?(followup|hatch)_?", "", df_longer_hatch_followup$stage)
df_longer_hatch_followup$stage <- str_replace(df_longer_hatch_followup$stage, "days_after", "days_after_hatch")
df_longer_hatch_followup <- distinct(df_longer_hatch_followup)


#now pivot wider
df_final <- df_longer_hatch_followup %>%
  pivot_wider(names_from = stage, 
              values_from = values, 
              values_fill=NA)

df_final <- df_final[apply(df_final[(16+1):ncol(df_final)], 1, function(row) !all(is.na(row))), ]

#Do some reorganizing 
chick_final <- df_final %>%
  relocate(notes, .after=prim_num)%>%
  relocate(days_after_hatch, .after = time24h) %>%
  mutate(year = 1990)

#Fix days after hatch to contain zero values for chicks whose weight was recorded on day 0
chick_final$days_after_hatch <- ifelse(chick_final$observation_num == 0 & !is.na(chick_final$month), 0, chick_final$days_after_hatch)


#Quick plot to check everything looks ok
chick_final %>%
  ggplot(aes(x=as.numeric(days_after_hatch), y=as.numeric(weight_g), group = egg_number, 
             color=as.character(egg_number))) +
  geom_point()+
  geom_line()+
  labs(x='Days after hatch', y="Weight (g)", color = "Individual") + 
  theme_classic()


#looks great! Write to file

write.csv(chick_final, 'prelim_clean/chick_hatch_data_prelim_clean.csv', 
          row.names = FALSE)

#read in the cleaned data cause I added stuff in excel manually - 
#new column: approximate days since hatch from notes on data
cleaned_chick <- read.csv('prelim_clean/chick_hatch_data_prelim_clean.csv')

cleaned_chick <- cleaned_chick %>%
  filter(observation_num != 0) %>%
  group_by(notes) %>%
  mutate(days_diff = as.numeric(difftime(paste(year, month, day, sep = "-"), 
                                         paste(year[observation_num == 1], 
                                               month[observation_num == 1], 
                                               day[observation_num == 1], 
                                               sep = "-"), 
                                         units = "days")),
         approx_days_after_hatch = ifelse(observation_num == 1, approx_days_after_hatch, 
                                          approx_days_after_hatch[1] + days_diff))

#write back into prelim_clean
write.csv(cleaned_chick, 'prelim_clean/chick_hatch_data_final_clean.csv', 
          row.names = FALSE)

#merge columns into single hatch column
cleaned_chick$days_after_hatch <- coalesce(cleaned_chick$days_after_hatch, cleaned_chick$approx_days_after_hatch)

#Plot the new
#Quick plot to check everything looks ok
cleaned_chick %>%
  ggplot(aes(x=as.numeric(days_after_hatch), y=as.numeric(weight_g), group = second_band_number, 
             color=as.character(second_band_number))) +
  geom_point()+
  geom_line()+
  labs(x='Days after hatch', y="Weight (g)", color = "Individual") + 
  theme_classic()


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 3: Cleaning egg

head(egg)

#Add new column saying how many days after the initial observation each observation was taken 
#Add in new column for each obs saying how many days after hatching it is 
egg$date_collected <- as.Date(paste0(egg$month_collected, "-", egg$day_collected, "-", egg$year_collected), format = "%m-%d-%Y")
egg$date_obs_1 <- as.Date(paste0(egg$obs_1_month, "-", egg$obs_1_day, "-", 1990), format = "%m-%d-%Y")
egg$obs_1_days_after_collection <- as.integer(difftime(egg$date_obs_1, egg$date_collected, units = "days")) 
egg <- egg %>%
  select(-date_collected, -date_obs_1) %>%
  relocate(obs_1_days_after_collection, .after=obs_1_day)


egg$date_collected <- as.Date(paste0(egg$month_collected, "-", egg$day_collected, "-", egg$year_collected), format = "%m-%d-%Y")
egg$date_obs_2 <- as.Date(paste0(egg$obs_2_month, "-", egg$obs_2_day, "-", 1990), format = "%m-%d-%Y")
egg$obs_2_days_after_collection <- as.integer(difftime(egg$date_obs_2, egg$date_collected, units = "days")) 
egg <- egg %>%
  select(-date_collected, -date_obs_2) %>%
  relocate(obs_2_days_after_collection, .after=obs_2_day)

egg$date_collected <- as.Date(paste0(egg$month_collected, "-", egg$day_collected, "-", egg$year_collected), format = "%m-%d-%Y")
egg$date_obs_3 <- as.Date(paste0(egg$obs_3_month, "-", egg$obs_3_day, "-", 1990), format = "%m-%d-%Y")
egg$obs_3_days_after_collection <- as.integer(difftime(egg$date_obs_3, egg$date_collected, units = "days")) 
egg <- egg %>%
  select(-date_collected, -date_obs_3) %>%
  relocate(obs_3_days_after_collection, .after=obs_3_day)

egg$date_collected <- as.Date(paste0(egg$month_collected, "-", egg$day_collected, "-", egg$year_collected), format = "%m-%d-%Y")
egg$date_obs_4 <- as.Date(paste0(egg$obs_4_month, "-", egg$obs_4_day, "-", 1990), format = "%m-%d-%Y")
egg$obs_4_days_after_collection <- as.integer(difftime(egg$date_obs_4, egg$date_collected, units = "days")) 
egg <- egg %>%
  select(-date_collected, -date_obs_4) %>%
  relocate(obs_4_days_after_collection, .after=obs_4_day)


#now start moving things around to make the dataframe tidy 
df_condensed <- egg %>%
  pivot_longer(cols =starts_with("obs_"),
               names_to = 'observation', 
               names_prefix = 'obs_', 
               values_to = 'values', 
               values_transform = list(values = as.character)
  )

#rename columns to prevent errors in pivot_wider
df_condensed <- df_condensed %>%
  rename(length_mm_collected = length_mm, 
         width_mm_collected = width_mm, 
         weight_g_collected = initial_weight_g)


#separate observation number from observation
df_condensed <- df_condensed %>%
  tidyr::separate(observation, into = c("observation_num", "variable"),
                  sep = "(?<=\\d)(?=\\D)", remove = FALSE)
# Remove the underscore from the "text" column
df_condensed$variable <- sub("_", "", df_condensed$variable)
df_condensed <- df_condensed %>%
  select(-observation)

#repair column names for pivot_wider
df_condensed$variable <- paste0("followup_", df_condensed$variable)


df_wide <- df_condensed %>%
  pivot_wider(names_from = variable, values_from = values, 
              names_repair = 'unique')

df_longer_collection_followup <- df_wide %>%
  pivot_longer(cols =contains(c("collected", 'followup')),
               names_to = 'stage', 
               values_to = 'values', 
               values_transform = list(values = as.character))

# If chicks contains the word 'hatch', set the observation_num value to zero
df_longer_collection_followup$observation_num <- ifelse(grepl("collected", df_longer_collection_followup$stage, 
                                                         ignore.case = TRUE) & !grepl("days_after_collection", 
                                                                                      df_longer_collection_followup$stage, ignore.case = TRUE), 
                                                   0, df_longer_collection_followup$observation_num)

# Remove the words "followup" and "collection" from the column
df_longer_collection_followup$stage <- gsub("_?(followup|collection|collected)_?", "", df_longer_collection_followup$stage)
df_longer_collection_followup$stage <- str_replace(df_longer_collection_followup$stage, "days_after", "days_after_collection")
df_longer_collection_followup <- distinct(df_longer_collection_followup)

#now pivot wider
df_final <- df_longer_collection_followup %>%
  pivot_wider(names_from = stage, 
              values_from = values, 
              values_fill=NA)
#Remove rows with only NAs after observation_num
df_final <- df_final[apply(df_final[(11+1):ncol(df_final)], 1, function(row) !all(is.na(row))), ]

#Now relocate some columns
egg_final <- df_final %>%
  relocate(general_notes, .after = notes) %>%
  relocate(days_after_collection, .after = day)

#Fix days after hatch to contain zero values for chicks whose weight was recorded on day 0
egg_final$days_after_collection <- ifelse(egg_final$observation_num == 0 & !is.na(egg_final$month), 0, egg_final$days_after_collection)

#Make the year column into 1990 across the board
egg_final <- egg_final %>%
  mutate(year = 1990)

#plot data to check everything transfered 
egg_final %>%
  ggplot(aes(x=as.numeric(days_after_collection), y=as.numeric(weight_g),
             group = id_number, colour  = actual_hatch_day)) +
  geom_point() + 
geom_line() +
  labs(x="Days after collection", y = "Weight (g)", group = "Egg ID")+
  theme_classic()

write.csv(egg_final, 'prelim_clean/egg_collection_morphometrics_1990.csv')



