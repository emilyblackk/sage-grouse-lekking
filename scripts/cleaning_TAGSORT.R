#Emily Black
#Cleaning TAGSORT from floppy disk
#Created: 7 June 2023
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

tagsort <- read.csv("prelim_clean/TAGSORT.csv", 
                     header=FALSE)
head(tagsort)
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

tagsort <- fix_column_names(tagsort)



#We now need to fill blank spaces with NAs
#And remove redundant columns, or replace them
tagsort <- tagsort %>% mutate_all(~ifelse(grepl("^\\s*$", .x), NA, .x))
tagsort <- tagsort[rowSums(is.na(tagsort)) != ncol(tagsort), ]

#The dates need a lot of standardizing, some are in - format and some are in " " format
tagsort$date <- gsub("^\\s*(\\d)", "\\1", tagsort$date)
tagsort$date <- gsub(" ", "-", tagsort$date)  # Replace spaces with dashes
tagsort$date <- format(as.Date(tagsort$date, format = "%d-%b-%y"), "%d-%b-%y")  # Convert to standard format
#Create month day year columns 
tagsort$year <- format(as.Date(tagsort$date, format = "%d-%b-%y"), "%Y")
tagsort <- tagsort %>%
  relocate(year, .after = date)
tagsort$month <- format(as.POSIXlt(tagsort$date, format = "%d-%b-%y"), "%m")
tagsort <- tagsort %>%
  relocate(month, .after = year)
tagsort$day <- format(as.Date(tagsort$date, format = "%d-%b-%y"), "%d")
tagsort <- tagsort %>%
  relocate(day, .after = month)
#Remove original date column 
tagsort <- tagsort %>%
  select(-date)


#To match the strut data, change the time to proportion of time after midnight
time_to_minutes <- function(time) {
  if (is.na(time)) {
    return(NA)
  } else {
    time_parts <- strsplit(time, ":")[[1]]
    hours <- as.numeric(time_parts[1])
    minutes <- as.numeric(time_parts[2])
    total_minutes <- (hours * 60) + minutes
    return(total_minutes)
  }
}

tagsort$minutes_after_midnight <- sapply(tagsort$time, time_to_minutes) 
tagsort <- tagsort %>%
  mutate(prop_time=minutes_after_midnight/(24*60)) %>%
  relocate(prop_time, .after = time)
#Remove minutes after midnight 
tagsort <- tagsort %>%
  select(-minutes_after_midnight)


#The strut datasets have tag codes, versus this one has colours and numbers. 
#It would be great to standardize that so they can "talk" to each other
tagsort <- tagsort %>%
  mutate(left_tag_code = ifelse(!is.na(left_tag_color),
                paste0(str_to_upper(substr(left_tag_color, 1, 1)), left_tag_number),
                NA)) %>%
  relocate(left_tag_code, .after = left_tag_number)

tagsort <- tagsort %>%
  mutate(right_tag_code = ifelse(!is.na(right_tag_color),
                                paste0(str_to_upper(substr(right_tag_color, 1, 1)), right_tag_number),
                                NA)) %>%
  relocate(right_tag_code, .after = right_tag_number)

#The names of the leks should be standardized for analyses
tagsort$lek_name <- tolower(tagsort$lek_name) #make lower case
tagsort$lek_name <-  gsub(" ", "_", tagsort$lek_name) 
unique(tagsort$lek_name)


#Looks like there is one bird with a plasma value of "36"
#That can't be right, so replace with NA
tagsort$blood_plasm[tagsort$blood_plasm > 2] <- NA

#There is also a question mark in the age column that needs to be NA
tagsort$age <- ifelse(tagsort$age %in% c("?"), NA, tagsort$age)



#Move breed to after hematomas_on_combs
tagsort <- tagsort %>%
  relocate(breed, .after=hematomas_air_sacs)

#Rename the blood columns to match the parasitology being done
tagsort <- tagsort %>%
  rename(blood_leuco = blood_l)
tagsort <- tagsort %>%
  rename(blood_haema = blood_h)
tagsort <- tagsort %>%
  select(-blood_p)



#Looks like for the colour columns, rather than putting NA where no colour
#was observed they put XXXX or xxx. This needs to be standardized. 
# Define the columns where 'x' values need to be replaced with NAs
columns_to_replace <- c("comb_color", "air_sac_color")

# Iterate over the columns and replace 'x' values with NAs
for (column in columns_to_replace) {
  tagsort[[column]][grepl("^x+$", tagsort[[column]])] <- NA
}



#many columns have pluses and minuses
#Spoke with data owner, not certain what different pluses and minuses might mean
#Just replace with no and yes to make simple and not over-interpret
# Replace values using case_when
tagsort <- tagsort %>%
  mutate(hematomas_combs = case_when(hematomas_combs == "-" ~ "no",
                                     hematomas_combs %in% c("+", "++", "+++") ~ "yes",
                       TRUE ~ hematomas_combs),
         hematomas_air_sacs = case_when(hematomas_air_sacs == "-" ~ "no",
                                        hematomas_air_sacs %in% c("+", "++") ~ "yes",
                       TRUE ~ hematomas_air_sacs))

tagsort <- tagsort %>%
  mutate(blood_haema = case_when(blood_haema == "-" ~ "no",
                                 blood_haema %in% c("+", "++", "+++") ~ "yes",
                                 blood_haema %in% c("+?") ~ "maybe",
                                     TRUE ~ blood_haema),
         blood_leuco = case_when(blood_leuco == "-" ~ "no",
                                 blood_leuco %in% c("+", "++") ~ "yes",
                                 blood_leuco %in% c("+?") ~ "maybe",
                                        TRUE ~ blood_leuco))

tagsort <- tagsort %>%
  mutate(lice_back_of_head = case_when(lice_back_of_head %in% c("_", "-") ~ "no",
                                       lice_back_of_head %in% c("+", "++") ~ "yes",
                                     TRUE ~ lice_back_of_head))

tagsort <- tagsort %>%
  mutate(blood_tryps = case_when(blood_tryps == "-" ~ "no",
                                       blood_tryps %in% c("+", "++") ~ "yes",
                                       TRUE ~ blood_tryps))

tagsort <- tagsort %>%
  mutate(breed = case_when(breed == "0" ~ "no",
                                       breed %in% c("1") ~ "yes",
                                       TRUE ~ breed))
tagsort <- tagsort %>%
  mutate(blood_plasm = case_when(blood_plasm == "0" ~ "no",
                           blood_plasm %in% c("1") ~ "yes",
                           TRUE ~ blood_plasm))

tagsort <- tagsort %>%
  mutate(fecal_samples_mites = case_when(fecal_samples_mites == "-" ~ "no",
                                     fecal_samples_mites %in% c("+", "++") ~ "yes",
                                     TRUE ~ fecal_samples_mites),
         fecal_samples_spores = case_when(fecal_samples_spores == "-" ~ "no",
                                        fecal_samples_spores %in% c("+", "++") ~ "yes",
                                        TRUE ~ fecal_samples_spores), 
         fecal_samples_coccidia = case_when(fecal_samples_coccidia == "-" ~ "no",
                                         fecal_samples_coccidia %in% c("+", "++") ~ "yes",
                                         TRUE ~ fecal_samples_coccidia))

#Move the comments column to the very end
tagsort <- tagsort %>%
  relocate(comments, .after=proportion_of_days_on_lek)



#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 3. Sorting out the lice column inconsistencies 
#In the lice counts, some are counts, and some are plusses or minuses
#I think I need to create columns for presence alone and then adjust the columns with 
#counts to remove plusses and minuses 

tagsort$lice_present_on_combs <- NA
tagsort$lice_present_on_air_sacs <- NA

tagsort$lice_present_on_combs[!is.na(tagsort$lice_number__on_combs)] <- 
  ifelse(tagsort$lice_number__on_combs[!is.na(tagsort$lice_number__on_combs)] > 0 | 
           tagsort$lice_number__on_combs[!is.na(tagsort$lice_number__on_combs)] == "+", "yes", "no")
tagsort <- tagsort %>%
  relocate(lice_present_on_combs, .before=lice_number__on_combs)

tagsort$lice_present_on_air_sacs[!is.na(tagsort$lice_number_on_air_sacs)] <- 
  ifelse(tagsort$lice_number_on_air_sacs[!is.na(tagsort$lice_number_on_air_sacs)] > 0 | 
                    tagsort$lice_number_on_air_sacs[!is.na(tagsort$lice_number_on_air_sacs)] == "+"| 
           tagsort$lice_number_on_air_sacs[!is.na(tagsort$lice_number_on_air_sacs)] == "++", "yes", "no")
tagsort <- tagsort %>%
  relocate(lice_present_on_air_sacs, .before=lice_number_on_air_sacs)

#Great, now remove pluses and minuses from the counts 
tagsort$lice_number__on_combs <- ifelse(tagsort$lice_number__on_combs %in% c("+", "-", "many", "++"), NA, tagsort$lice_number__on_combs)
tagsort$lice_number_on_air_sacs <- ifelse(tagsort$lice_number_on_air_sacs %in% c("+", "-", "many", "++"), NA, tagsort$lice_number_on_air_sacs)

write.csv(tagsort, 'prelim_clean/morphology_parasites_1987.csv', 
          row.names=FALSE)


#Having fun with the data

tagsort %>%
  ggplot(aes(x=as.character(blood_leuco), y=as.numeric(blood_pcv_mm), 
             colour = as.character(hematomas_air_sacs))) + 
  geom_point() + 
  labs(x="Leuco", y="Packed cell volume", colour="Hematomas present") + 
  theme_classic()



#Checking that tagsort matches binder data
unique_tags_longlake <- tagsort %>%
  filter(lek_name=="long_lake") 
