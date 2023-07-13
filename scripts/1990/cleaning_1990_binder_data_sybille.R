#Emily Black
#Cleaning 1990 binder data - sybille springs
#Created: 22 June 2023
#Last modified: 26 June 2023

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


#Tagsort is from the floppy disks,which also contains morphologica data
#some code is commented out for tagsort below
# #Open tagsort as a template for the data we're reading in
# 
# tagsort <- read.csv('prelim_clean/morphology_parasites_1987.csv')
# 
# #Add a new column to tagsort to indicate source 
# tagsort$data_source <- c("floppy_disk")

#Read in the joined dataframe so far
original_joined_df <- read.csv("prelim_clean/binder_capture_data_prelim_N_MS.csv")

#read in the north lek data 
df_1 <- read.csv('raw_data/binders/sybille_springs_1990_capture_binder_data.csv', 
                 header=FALSE)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 2: Cleaning the binder data - field capture data
#This is going to be a lot, so we can just go column by column 


#Assign proper column names with function
#This function can be found across much of the code,and fixes the two-row column name structure
#That a lot of the data have 
fix_column_names <- function(data) {
  merged_names <- paste(data[1, ], data[2, ], sep = " ") #bring the first two rows together
  colnames(data) <- ifelse(str_trim(data[1, ]) == "", data[2, ], merged_names) #merge their names 
  colnames(data) <- tolower(colnames(data)) # put it all to lower case
  colnames(data) <- gsub(" ", "_", colnames(data)) #replace spaces with _
  colnames(data) <- gsub("[^A-Za-z0-9_.]", "", colnames(data)) #replace anything that is not a number or letter 
  data <- data[-c(1, 2), ] #delete the original two rows
  return(data) #return the data
}

#Run the fix columns function on the mud springs data
df_1<-  fix_column_names(df_1)

#Do more general cleanup: 
#Add in NAs so that blanks/empty cells, rows and columns are deleted or standardized 
df_1 <- df_1 %>% mutate_all(~ifelse(grepl("^\\s*$", .x), NA, .x))
df_1[df_1 == "N/A"] <- NA
df_1 <- df_1[rowSums(is.na(df_1)) != ncol(df_1), ]

#Split those dates up into day, month, year columns 
#The dates need a lot of standardizing, some are in - format and some are in " " format
df_1$date <- gsub("^\\s*(\\d)", "\\1", df_1$date) #remove leading white space from dates
df_1$date <- gsub(" ", "-", df_1$date)  # Replace spaces with dashes
df_1$date <- format(as.Date(df_1$date, format = "%d-%m-%Y"), "%d-%m-%Y") #help R read the dates

#Create month day year columns 
df_1$year <- format(as.Date(df_1$date, format = "%d-%m-%Y"), "%Y") #collect year from date
df_1 <- df_1 %>%
  relocate(year, .after = date) #relocate the year column after the date column
df_1$month <- format(as.POSIXlt(df_1$date, format = "%d-%m-%y"), "%m") #same as above for month
df_1 <- df_1 %>%
  relocate(month, .after = year)
df_1$day <- format(as.Date(df_1$date, format = "%d-%m-%Y"), "%d") #same as above for year
df_1 <- df_1 %>%
  relocate(day, .after = month)

#Remove original date column 
df_1 <- df_1 %>%
  subset(select=c(-date))

#Standardize the time column names
df_1 <- df_1 %>%
  rename(time_search_start_24h = time_start ) %>%
  rename(time_search_end_24h = time_end )

#We need standardized lek names so the different datasets can "talk" to each other
#rename North lek to match the standardized dataset 
unique(df_1$lek)
df_1 <- df_1 %>%
  mutate(lek_name = case_when(lek == "SYBILLE" ~ "sybille_springs", 
                              lek == "SS" ~ 'sybille_springs')) %>%
  relocate(lek_name, .after = lek) %>%
  select(-lek)

#standardize the age column 
#This is another column that needs to match the standards in others, 
#because it's going to get merged later 
df_1$age <- tolower(df_1$age) #make lower case
df_1 <- df_1 %>%
  mutate(age = case_when(age == 'adult ' ~ 'adult', 
                         age == 'adult' ~ 'adult', 
                         age=='juvenile' ~ "juven")) #change the "juvenile" code in the data
#Also standardize the sex column in the same way 
df_1$sex <- tolower(df_1$sex)
df_1$capture_technique <- tolower(df_1$capture_technique)


#The weight bird and bag column has question marks instead of NAs
#also, note that weights are actually in kg, but for consistency this will be
#changed at the end
df_1$weight_bird_and_bag_g[df_1$weight_bird_and_bag_g == "?"] <- NA
df_1$weight_bag_g[df_1$weight_bag_g == "?"] <- NA
df_1$bird_weight_g[df_1$bird_weight_g == "?"] <- NA

#There are lots of missing values in bird_weight, 
#Which could be calculated from subtracting bag weight from bird_and_bag
#This may have been because the original data collecters were trying to save time 
#So let's replace the original with a new calculated one
df_1 <- df_1 %>%
  mutate(weight_bird_g = as.numeric(weight_bird_and_bag_g) - as.numeric(weight_bag_g)) %>%
  relocate(weight_bird_g, .after = bird_weight_g) %>%
  select(-bird_weight_g)


#Comb colours are very inconsistent across the dataframe
#These need to be standardized
unique(df_1$comb_color) #unique colours 
#Looks like some substitutions need to be made
df_1 <- df_1 %>%
  mutate(comb_color = case_when(
    tolower(comb_color) == "olive yell" ~ "olive yellow",
    tolower(comb_color) == "yell - olive" ~ "yellow olive",
    tolower(comb_color) == "yell olive" ~ "yellow olive",
    tolower(comb_color) == "yell- olive" ~ "yellow olive",
    tolower(comb_color) == "yellolive" ~ "yellow olive",
    tolower(comb_color) == "yell-olive" ~ "yellow olive",
    tolower(comb_color) == "chrome yellow" ~ "chrome yellow",
    tolower(comb_color) == "greenish yellow" ~ "greenish yellow",
    tolower(comb_color) == "yellow-green" ~ "yellow green",
    tolower(comb_color) == "bistre yell" ~ "bistre yellow",
    tolower(comb_color) == "bistre" ~ "bistre yellow",
    tolower(comb_color) == "chrome yell" ~ "chrome yellow",
    comb_color == "n/a" ~ NA,
    comb_color == "-" ~ NA,
    TRUE ~ comb_color
  ),
  comb_color = tolower(comb_color))



#Do the same for air sac colors
unique(df_1$air_sac_color)
df_1 <- df_1 %>%
  mutate(air_sac_color = case_when(
    tolower(air_sac_color) == "bistre yell" ~ "bistre yellow",
    tolower(air_sac_color) == "bistre Yell" ~ "bistre yellow",
    tolower(air_sac_color) == "bistre yell" ~ "bistre yellow",
    tolower(air_sac_color) == "bistre" ~ "bistre yellow",
    tolower(air_sac_color) == "chrome yell" ~ "chrome yellow",
    air_sac_color == "chrome yellow" ~ "chrome yellow",
    air_sac_color == "n/a" ~ NA,
    air_sac_color == "na" ~ NA,
    air_sac_color == "-" ~ NA,
    TRUE ~ air_sac_color
  ),
  air_sac_color = tolower(air_sac_color))

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 1b. Fixing lice and hematoma columns 



#Lice on air sacs and combs has two unique values: a R value, and a L value
#For analyses, these should be in different columns 
#The code below splits them into two columns - air sacs 
# Split the values and assign them to the new columns
df_1$num_lice_on_air_sac_right <- NA
df_1$num_lice_on_air_sac_left <- NA
split_values <- strsplit(df_1$num_lice_on_air_sac, ",")
for (i in seq_along(split_values)) {
  if (length(split_values[[i]]) == 2) {
    value_1 <- trimws(split_values[[i]][1])
    value_2 <- trimws(split_values[[i]][2])
    
    if (grepl("\\d+R", value_1)) {
      df_1$num_lice_on_air_sac_right[i] <- gsub("\\D", "", value_1)
    }
    if (grepl("\\d+L", value_1)) {
      df_1$num_lice_on_air_sac_left[i] <- gsub("\\D", "", value_1)
    }
    
    if (grepl("\\d+R", value_2)) {
      df_1$num_lice_on_air_sac_right[i] <- gsub("\\D", "", value_2)
    }
    if (grepl("\\d+L", value_2)) {
      df_1$num_lice_on_air_sac_left[i] <- gsub("\\D", "", value_2)
    }
  } else if (length(split_values[[i]]) == 1) {
    value <- trimws(split_values[[i]])
    if (value == "0" && !is.na(value)) {
      df_1$num_lice_on_air_sac_left[i] <- "0"
      df_1$num_lice_on_air_sac_right[i] <- "0"
    } else if (is.na(value)) {
      df_1$num_lice_on_air_sac_left[i] <- NA
      df_1$num_lice_on_air_sac_right[i] <- NA
    }
  }
}

# Remove leading/trailing whitespaces from the columns
df_1$num_lice_on_air_sac_left <- trimws(df_1$num_lice_on_air_sac_left)
df_1$num_lice_on_air_sac_right <- trimws(df_1$num_lice_on_air_sac_right)
# Remove the letters 'R' and 'L' from the left and right columns
df_1$num_lice_on_air_sac_left <- gsub("[RL]", "", df_1$num_lice_on_air_sac_left)
df_1$num_lice_on_air_sac_right <- gsub("[RL]", "", df_1$num_lice_on_air_sac_right)
df_1$num_lice_on_air_sac_left <- as.numeric(df_1$num_lice_on_air_sac_left)
df_1$num_lice_on_air_sac_right <- as.numeric(df_1$num_lice_on_air_sac_right)


df_1 <- df_1 %>%
  relocate(num_lice_on_air_sac_left, .after = num_lice_on_air_sac) %>%
  relocate(num_lice_on_air_sac_right, .after = num_lice_on_air_sac_left) %>%
  mutate(total_num_lice_on_air_sacs = 
           rowSums(select(., num_lice_on_air_sac_left, num_lice_on_air_sac_right), na.rm = FALSE)) %>%
  relocate(total_num_lice_on_air_sacs, .after=num_lice_on_air_sac_right) %>%
  select(-num_lice_on_air_sac)


#Lice on back of head has plusses and minuses, which is hard to read and analyze
#Make these yes and no for ease of reading 
df_1 <- df_1 %>%
rename(lice_on_back_of_head = lice_on_back_of_head_) %>%
  mutate(lice_on_back_of_head = case_when(
    lice_on_back_of_head=="+" ~ "yes", 
    lice_on_back_of_head== "-" ~ "no")) 

#Looks like num_lice_on_comb needs some fixing too 
df_1 <- df_1 %>%
  mutate(num_lice_on_comb = case_when(
    tolower(num__lice_on_comb)=='none' ~ "0", 
    tolower(num__lice_on_comb)=="no" ~ "0", 
    num__lice_on_comb== "- " ~ NA, 
    num__lice_on_comb== "-" ~ NA, 
    num__lice_on_comb== 'n/a' ~ NA, 
    TRUE ~ num__lice_on_comb)) %>%
  relocate(num_lice_on_comb, .after = num__lice_on_comb) %>%
  select(-num__lice_on_comb)


#hematomas on air sacs and combs has two unique values: a R value, and a L value
#For analyses, these should be in different columns 
#The code below splits them into two columns - air sacs 
# Split the values and assign them to the new columns
df_1 <- df_1 %>%
  rename(hematomas_num_air_sacs = hematomas_num_air_sacs)
df_1$num_hematomas_on_air_sac_right <- NA
df_1$num_hematomas_on_air_sac_left <- NA
split_values <- strsplit(df_1$hematomas_num_air_sacs, ",")
for (i in seq_along(split_values)) {
  if (length(split_values[[i]]) == 2) {
    value_1 <- trimws(split_values[[i]][1])
    value_2 <- trimws(split_values[[i]][2])
    
    if (grepl("\\d+R", value_1)) {
      df_1$num_hematomas_on_air_sac_right[i] <- gsub("\\D", "", value_1)
    }
    if (grepl("\\d+L", value_1)) {
      df_1$num_hematomas_on_air_sac_left[i] <- gsub("\\D", "", value_1)
    }
    
    if (grepl("\\d+R", value_2)) {
      df_1$num_hematomas_on_air_sac_right[i] <- gsub("\\D", "", value_2)
    }
    if (grepl("\\d+L", value_2)) {
      df_1$num_hematomas_on_air_sac_left[i] <- gsub("\\D", "", value_2)
    }
  } else if (length(split_values[[i]]) == 1) {
    value <- trimws(split_values[[i]])
    if (value == "0" && !is.na(value)) {
      df_1$num_hematomas_on_air_sac_left[i] <- "0"
      df_1$num_hematomas_on_air_sac_right[i] <- "0"
    } else if (is.na(value)) {
      df_1$num_hematomas_on_air_sac_left[i] <- NA
      df_1$num_hematomas_on_air_sac_right[i] <- NA
    }
  }
}

# Remove leading/trailing whitespaces from the columns
df_1$num_hematomas_on_air_sac_left <- trimws(df_1$num_hematomas_on_air_sac_left)
df_1$num_hematomas_on_air_sac_right <- trimws(df_1$num_hematomas_on_air_sac_right)
# Remove the letters 'R' and 'L' from the left and right columns
df_1$num_hematomas_on_air_sac_left <- gsub("[RL]", "", df_1$num_hematomas_on_air_sac_left)
df_1$num_hematomas_on_air_sac_right <- gsub("[RL]", "", df_1$num_hematomas_on_air_sac_right)
df_1$num_hematomas_on_air_sac_left <- as.numeric(df_1$num_hematomas_on_air_sac_left)
df_1$num_hematomas_on_air_sac_right <- as.numeric(df_1$num_hematomas_on_air_sac_right)


df_1 <- df_1 %>%
  relocate(num_hematomas_on_air_sac_left, .after = hematomas_num_air_sacs) %>%
  relocate(num_hematomas_on_air_sac_right, .after = num_hematomas_on_air_sac_left) %>%
  mutate(total_num_hematomas_on_air_sacs = 
           rowSums(select(., num_hematomas_on_air_sac_left, num_hematomas_on_air_sac_right), na.rm = FALSE)) %>%
  relocate(total_num_hematomas_on_air_sacs, .after=num_hematomas_on_air_sac_right) %>%
  select(-hematomas_num_air_sacs)

#Create new columns for hematomas on combs 
#Rename combs column
df_1 <- df_1 %>%
  rename(hematomas_num_combs = num_combs)

#hematomas on air sacs and combs has two unique values: a R value, and a L value
#The code below splits them into two columns - combs
# Split the values and assign them to the new columns
df_1$num_hematomas_on_comb_right <- NA
df_1$num_hematomas_on_comb_left <- NA
split_values <- strsplit(df_1$hematomas_num_combs, ",")
for (i in seq_along(split_values)) {
  if (length(split_values[[i]]) == 2) {
    value_1 <- trimws(split_values[[i]][1])
    value_2 <- trimws(split_values[[i]][2])
    
    if (grepl("\\d+R", value_1)) {
      df_1$num_hematomas_on_comb_right[i] <- gsub("\\D", "", value_1)
    }
    if (grepl("\\d+L", value_1)) {
      df_1$num_hematomas_on_comb_left[i] <- gsub("\\D", "", value_1)
    }
    
    if (grepl("\\d+R", value_2)) {
      df_1$num_hematomas_on_comb_right[i] <- gsub("\\D", "", value_2)
    }
    if (grepl("\\d+L", value_2)) {
      df_1$num_hematomas_on_comb_left[i] <- gsub("\\D", "", value_2)
    }
  } else if (length(split_values[[i]]) == 1) {
    value <- trimws(split_values[[i]])
    if (value == "0" && !is.na(value)) {
      df_1$num_hematomas_on_comb_left[i] <- "0"
      df_1$num_hematomas_on_comb_right[i] <- "0"
    } else if (is.na(value)) {
      df_1$num_hematomas_on_comb_left[i] <- NA
      df_1$num_hematomas_on_comb_right[i] <- NA
    }
  }
}

# Remove leading/trailing whitespaces from the columns
df_1$num_hematomas_on_comb_left <- trimws(df_1$num_hematomas_on_comb_left)
df_1$num_hematomas_on_comb_right <- trimws(df_1$num_hematomas_on_comb_right)
# Remove the letters 'R' and 'L' from the left and right columns
df_1$num_hematomas_on_comb_left <- gsub("[RL]", "", df_1$num_hematomas_on_comb_left)
df_1$num_hematomas_on_comb_right <- gsub("[RL]", "", df_1$num_hematomas_on_comb_right)
df_1$num_hematomas_on_comb_left <- as.numeric(df_1$num_hematomas_on_comb_left)
df_1$num_hematomas_on_comb_right <- as.numeric(df_1$num_hematomas_on_comb_right)


df_1 <- df_1 %>%
  relocate(num_hematomas_on_comb_left, .after = hematomas_num_combs) %>%
  relocate(num_hematomas_on_comb_right, .after = num_hematomas_on_comb_left) %>%
  mutate(total_num_hematomas_on_combs = 
           rowSums(select(., num_hematomas_on_comb_left, num_hematomas_on_comb_right), na.rm = FALSE)) %>%
  relocate(total_num_hematomas_on_combs, .after=num_hematomas_on_comb_right) %>%
  select(-hematomas_num_combs)



#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Back to regular cleaning
#A little less intense than above! 

#Standardize the tag colours - in strutting data, we have columns with tag number, colour, 
#and overall code. We need to make that the case here as well: 
df_1$color_left_wing <- gsub("W([A-Z][a-z]+)", "White \\1", df_1$color_left_wing) #If there is a W, replace with white
df_1$color_left_wing <- gsub("Yell", "yellow", df_1$color_left_wing) #Fix shortened yellow
df_1$color_left_wing <- gsub("yellowow", "yellow", df_1$color_left_wing) #fix small inconsistency with replacement
df_1$color_left_wing <- tolower(df_1$color_left_wing)
df_1 <- df_1 %>%
  rename(left_tag_color = color_left_wing) %>%
  rename(left_tag_number = number_left_wing) %>%
  relocate(left_tag_number, .after = left_tag_color)
df_1$left_tag_color <- str_trim(df_1$left_tag_color)


df_1$color_right_wing <- gsub("W([A-Z][a-z]+)", "White \\1", df_1$color_right_wing)
df_1$color_right_wing <- gsub("Yell", "yellow", df_1$color_right_wing)
df_1$color_right_wing <- gsub("yellowow", "yellow", df_1$color_right_wing)
df_1$color_right_wing <- tolower(df_1$color_right_wing)
df_1 <- df_1 %>%
  rename(right_tag_color = color_right_wing) %>%
  rename(right_tag_number = number_right_wing)%>%
  relocate(right_tag_number, .after = right_tag_color)
df_1$right_tag_color <- str_trim(df_1$right_tag_color)

#Now create tag code column - left
df_1 <- df_1 %>%
  mutate(left_tag_code = ifelse(!is.na(left_tag_color),
                                paste0(
                                  str_to_upper(substr(gsub("[^a-zA-Z ]", "", left_tag_color), 1, 1)),
                                  ifelse(
                                    nchar(gsub("[^a-zA-Z ]", "", left_tag_color)) > 1,
                                    substr(word(gsub("[^a-zA-Z ]", "", left_tag_color), 2, 2), 1, 1),
                                    ""
                                  ),
                                  left_tag_number
                                ),
                                "")) %>%
  relocate(left_tag_code, .after = left_tag_number) %>%
  mutate(left_tag_code = gsub("NA", "", left_tag_code),
         left_tag_code = str_to_upper(left_tag_code))



#Now create tag code column - right
df_1 <- df_1 %>%
  mutate(right_tag_code = ifelse(!is.na(right_tag_color),
                                 paste0(
                                   str_to_upper(substr(gsub("[^a-zA-Z ]", "", right_tag_color), 1, 1)),
                                   ifelse(
                                     nchar(gsub("[^a-zA-Z ]", "", right_tag_color)) > 1,
                                     substr(word(gsub("[^a-zA-Z ]", "", right_tag_color), 2, 2), 1, 1),
                                     ""
                                   ),
                                   right_tag_number
                                 ),
                                 "")) %>%
  relocate(right_tag_code, .after = right_tag_number) %>%
  mutate(right_tag_code = gsub("NA", "", right_tag_code),
         right_tag_code = str_to_upper(right_tag_code))

# Find columns containing "left_tag" or "right_tag"
cols_to_relocate <- colnames(df_1)[grepl("left_tag|right_tag", colnames(df_1))]

# Relocate the identified columns after "capture_technique"
df_1 <- df_1 %>%
  relocate(all_of(cols_to_relocate), .after = capture_technique) %>%
  relocate(wygf_leg_band__, .after = right_tag_code) %>%
  rename(wygf_leg_band =wygf_leg_band__)




#In the cut feathers, collected, and labelled columns, replace TRUE FALSE with yes no
#TRUE FALSE is difficult to read and interpret - should be replaced with something more readable
df_1 <- df_1 %>%
  mutate(cut_feathers_ = case_when(
    cut_feathers_=="TRUE" ~ "yes", 
    cut_feathers_=="FALSE" ~ "no"
  )) %>%
  rename(cut_feathers = cut_feathers_)

df_1 <- df_1 %>%
  mutate(collect_ = case_when(
    collect_=="TRUE" ~ "yes", 
    collect_=="FALSE" ~ "no"
  )) %>%
  rename(collect = collect_)

df_1 <- df_1 %>%
  mutate(labelled_ = case_when(
    labelled_=="TRUE" ~ "yes", 
    labelled_=="FALSE" ~ "no"
  )) %>%
  rename(labelled = labelled_)

df_1 <- df_1 %>%
  mutate(hematomas_applied_ = case_when(
    hematomas_applied_=="TRUE" ~ "yes", 
    hematomas_applied_=="FALSE" ~ "no"
  )) %>%
  rename(hematomas_applied = hematomas_applied_) %>%
  rename(hematoma_group = hematoma_group__)  %>%
  mutate(hematoma_group = case_when(
    hematoma_group == "-" ~ NA, 
    TRUE ~ hematoma_group
  )) 


df_1 <- df_1 %>%
  mutate(samples_collected_blood_ = case_when(
    samples_collected_blood_=="TRUE" ~ "yes", 
    samples_collected_blood_=="FALSE" ~ "no"
  )) %>%
  rename(samples_collected_blood = samples_collected_blood_)

df_1 <- df_1 %>%
  mutate(samples_collected_ectoparasites = case_when(
    samples_collected_ectoparasites=="TRUE" ~ "yes", 
    samples_collected_ectoparasites=="FALSE" ~ "no"
  )) %>%
  rename(samples_collected_ectoparasites = samples_collected_ectoparasites)

df_1 <- df_1 %>%
  mutate(samples_collected_fecal = case_when(
    samples_collected_fecal=="TRUE" ~ "yes", 
    samples_collected_fecal=="FALSE" ~ "no"
  )) %>%
  rename(samples_collected_fecal = samples_collected_fecal)


df_1 <- df_1 %>%
  mutate(samples_collected_cecal = case_when(
    samples_collected_cecal=="TRUE" ~ "yes", 
    samples_collected_cecal=="FALSE" ~ "no"
  )) %>%
  rename(samples_collected_cecal = samples_collected_cecal)


#In the existing injuries column, there are dashes
#These aren't standardized, and should be NA
#existing injuries column - replace dashes with NA
df_1$existing_injuries[df_1$existing_injuries == "-"] <- NA
df_1$handling_injury[df_1$handling_injury == "-"] <- NA
df_1 <- df_1 %>%
  rename(stresses_while_handling_describe = "_stresses_while_handling_describe")
df_1$stresses_while_handling_describe[df_1$stresses_while_handling_describe == "-"] <- NA



#Remove trailing underscores from names 
remove_trailing_underscores <- function(data) {
  colnames(data) <- sub("_+$", "", colnames(data))
  return(data)
}
df_1 <- remove_trailing_underscores(df_1)

#Looks like there are still some dashes hanging around instead of NAs
#Make those dashes and blank cells NAs
df_1[df_1 == "-"] <- NA
df_1[df_1 == ""] <- NA




#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 3. Cleaning up the laboratory sample analyses data 


#read in the lab data
df_2 <- read.csv('raw_data/binders/sybille_springs_1990_capture_lab_binder_data.csv', 
                 header=FALSE)

#Fix column names using function from top
df_2 <- fix_column_names(df_2)

#replace blank spaces with NA, and remove blank columns and rows
df_2 <- df_2 %>% mutate_all(~ifelse(grepl("^\\s*$", .x), NA, .x))
df_2[df_2 == "N/A"] <- NA
df_2 <- df_2[rowSums(is.na(df_2)) != ncol(df_2), ]

#Split those dates
#The dates need a lot of standardizing, some are in - format and some are in " " format
#As above, make them into year month and day columns 
df_2$date_capture_ <- gsub("^\\s*(\\d)", "\\1", df_2$date_capture_)
df_2$date_capture_ <- gsub(" ", "-", df_2$date_capture_)  # Replace spaces with dashes
df_2$date_capture_ <- format(as.Date(df_2$date_capture_, format = "%d-%m-%Y"), "%d-%m-%Y") 
#Create month day year columns 
df_2$year <- format(as.Date(df_2$date_capture_, format = "%d-%m-%Y"), "%Y")
df_2 <- df_2 %>%
  relocate(year, .after = date_capture_)
df_2$month <- format(as.POSIXlt(df_2$date_capture_, format = "%d-%m-%y"), "%m")
df_2 <- df_2 %>%
  relocate(month, .after = year)
df_2$day <- format(as.Date(df_2$date_capture_, format = "%d-%m-%Y"), "%d")
df_2 <- df_2 %>%
  relocate(day, .after = month)
#Remove original date column 
df_2 <- df_2 %>%
  select(-date_capture_)

#Again, standardize the lek names 
unique(df_2$lek_)
df_2 <- df_2 %>%
  mutate(lek_name = case_when(lek_ == "SYBILLE" ~ "sybille_springs",
                              lek_ ==  "SYBILLE SPRINGS" ~ "sybille_springs", 
                              lek_ ==  "SYBILLE SPGS" ~ "sybille_springs")) %>%
  relocate(lek_name, .after = lek_) %>%
  select(-lek_)

#Since tag codes are more standardized here, can just rename some to begin
df_2 <- df_2 %>%
  rename(left_tag_code = tags_left) %>%
  rename(right_tag_code = tags_right) %>%
  rename(wygf_leg_band = leg_band_num_)



#Need to add left_tag_color and number, and same for right 
df_2$right_tag_color <- str_extract(df_2$right_tag_code, "[A-Za-z]+")
df_2$right_tag_number <- str_extract(df_2$right_tag_code, "\\d+")
df_2 <- df_2 %>%
  relocate(contains('right_tag'), .before= right_tag_code)

df_2$left_tag_color <- str_extract(df_2$left_tag_code, "[A-Za-z]+")
df_2$left_tag_number <- str_extract(df_2$left_tag_code, "\\d+")
df_2 <- df_2 %>%
  relocate(contains('left_tag'), .before = left_tag_code)

# Define a lookup table for color replacements
color_lookup <- c("G" = "green", "R" = "red", "Y" = "yellow", "W" = "white", "O" = "orange", "B" = "blue")

replace_colors <- function(x, color_lookup) {
  # Split the string into individual color codes
  color_codes <- strsplit(x, "")
  
  # Replace each color code with its corresponding value from the lookup table
  replaced_colors <- lapply(color_codes, function(colors) {
    replaced <- sapply(colors, function(c) color_lookup[c])
    paste(replaced, collapse = " ")
  })
  
  # Combine the replaced colors into a single string
  replaced_string <- sapply(replaced_colors, function(colors) ifelse(length(colors) > 0, colors, ""))
  
  # Return the replaced string
  return(replaced_string)
}

# Replace colors in the left_tag_color column
df_2$left_tag_color <- replace_colors(df_2$left_tag_color, color_lookup)

# Replace colors in the right_tag_color column
df_2$right_tag_color <- replace_colors(df_2$right_tag_color, color_lookup)



#Many of the lab sample columns have TRUE and FALSE instead of yes and no
#They are also split up across two columns - this is an artifact of the way the data were entered
#Correc these to become yes and no 
#Use only the "yes" columns, since this contains presence information. The "yes" and "no" columns are functionally
#the same, just opposites 
df_2 <- df_2 %>%
  mutate(fecal_samples_coccidia = case_when (
    fecal_sample_coccidea_yes_cysts_g == TRUE ~ "yes", 
    fecal_sample_coccidea_yes_cysts_g == FALSE ~ "no"
  )) %>%
  relocate(fecal_samples_coccidia, .after  = fecal_sample_coccidea_yes_cysts_g)
df_2 <- df_2 %>%
  select(-fecal_sample_coccidea_yes_cysts_g, -no)


df_2 <- df_2 %>%
  mutate(fecal_samples_mites = case_when (
    mites_yes == TRUE ~ "yes", 
    mites_yes == FALSE ~ "no"
  )) %>%
  relocate(fecal_samples_mites, .after  = mites_yes)
df_2 <- df_2 %>%
  select(-mites_yes,-mites_no)

df_2 <- df_2 %>%
  rename(fecal_samples_comments = comments_)

#Fix more duplicate columns 
#Fix a lot of these duplicate columns
df_2 <- df_2 %>%
  rename(blood_malaria_yes = "_blood_malaria_yes", 
         blood_malaria_no = "_blood_malaria_no")
df_2 <- df_2 %>%
  mutate(blood_samples_malaria = case_when (
    blood_malaria_yes == TRUE ~ "yes", 
    blood_malaria_yes == FALSE ~ "no"
  )) %>%
  relocate(blood_samples_malaria, .after  = blood_malaria_yes)
df_2 <- df_2 %>%
  select(-blood_malaria_yes,-blood_malaria_no)

df_2 <- df_2 %>%
  mutate(blood_samples_haema = case_when (
    blood_haemaproteus_yes == TRUE ~ "yes", 
    blood_haemaproteus_yes == FALSE ~ "no"
  )) %>%
  relocate(blood_samples_haema, .after  =    blood_haemaproteus_yes)
df_2 <- df_2 %>%
  select(-blood_haemaproteus_yes,-blood_haemaproteus_no)


df_2 <- df_2 %>%
  mutate(blood_samples_leuco = case_when (
    blood_leucocytozoan_yes == TRUE ~ "yes", 
    blood_leucocytozoan_yes == FALSE ~ "no"
  )) %>%
  relocate(blood_samples_leuco, .after  =    blood_leucocytozoan_yes)
df_2 <- df_2 %>%
  select(-blood_leucocytozoan_yes,-blood_leucocytozoan_no)


#remove column na_na
df_2 <- df_2 %>%
  select(-na_na)

#case blood other
df_2 <- df_2 %>%
  mutate(blood_samples_other  = case_when(
    blood_other_ ==TRUE ~ 'yes', 
    blood_other_ == FALSE ~ "no")) %>%
  select(-blood_other_) %>%
  relocate(blood_samples_other, .after = blood_samples_leuco)


#Fix the lice columns 
df_2 <- df_2 %>%
  mutate(lice_present = case_when (
    lice_yes_ == TRUE ~ "yes", 
    lice_yes_ == FALSE ~ "no"
  )) %>%
  relocate(lice_present, .after  =   lice_yes_)
df_2 <- df_2 %>%
  select(-lice_yes_,-lice_no) 

#rename lice ID column
df_2 <- df_2 %>%
  rename(lice_ID_ifpresent = identification_if_yes)



#rename the hematocrit columns to be more informative
df_2 <- df_2 %>%
  rename(blood_cells_volume_1 = hematocrit_cells_1)%>%
  rename(blood_cells_volume_2 = hematocrit_cells_2)  %>%
  rename(blood_plasma_volume_1 = '_hematocrit_plasma_1')%>%
  rename(blood_plasma_volume_2 = '_hematocrit_plasma_2')  %>%
  rename(blood_total_volume_1 = '_hematocrit_total__1')%>%
  rename(blood_total_volume_2 = '_hematocrit_total__2') 

#Create some new summary variables from the hematocrit data 
df_2$blood_cells_volume_1 <- as.numeric(df_2$blood_cells_volume_1)
df_2$blood_cells_volume_2 <- as.numeric(df_2$blood_cells_volume_2)
df_2$blood_plasma_volume_1 <- as.numeric(df_2$blood_plasma_volume_1)
df_2$blood_plasma_volume_2 <- as.numeric(df_2$blood_plasma_volume_2)
df_2$blood_total_volume_1 <- as.numeric(df_2$blood_total_volume_1)
df_2$blood_total_volume_2 <- as.numeric(df_2$blood_total_volume_2)

df_2 <- df_2 %>%
  mutate(mean_blood_pcv_mm = (blood_cells_volume_1 + blood_cells_volume_2)/2, 
         mean_blood_total_volume_mm = (blood_total_volume_1 + blood_total_volume_2)/2, 
         mean_blood_plasma_mm = (blood_plasma_volume_1 + blood_plasma_volume_2)/2, 
         mean_prop_blood_pcv_mm  = mean_blood_pcv_mm/mean_blood_total_volume_mm, 
         mean_prop_blood_plasma_mm = mean_blood_plasma_mm/mean_blood_total_volume_mm) %>%
  relocate(contains("mean"), .after = blood_total_volume_2)

#Rename the box and slide columns
df_2 <- df_2 %>%
  rename(box_num = box_num_, 
         slide_num = slide_num_)


#rename the pdf page column
df_2 <- df_2 %>%
  rename(binder_pdf_scan_page = pdf_page)
#Add a column called "source" that just says binder
df_2 <- df_2 %>%
  mutate(source = "binder") %>%
  relocate(source, .after = binder_pdf_scan_page)

#fix same name in df_1 to match the name in df_2 for later 
df_1 <- df_1 %>%
  rename(binder_pdf_scan_page = binder_pdf_scan_page) %>% 
  mutate(source = "binder") %>%
  relocate(source, .after = binder_pdf_scan_page)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 4. Joining df_1 and df_2 to get a single dataset


#Moment of truth: full join df_1 and df_2 
#for this one, we can't actually use left and right tag code because one is missing
#better to use the wygf tag code 
joined_df <- full_join(df_1, df_2, by = c("wygf_leg_band"))


#Columns that are the same but not used as an ID for the joined are assigned prefixes of .x and .y
#We want them all together, merging everything into a single row 

#Make everything a character for ease of merging 
joined_df <- joined_df %>%
  mutate_all(as.character)


#Handle comments separately first, as these can get complex 
# joined_df$comments <- ifelse(is.na(joined_df$comments.x), joined_df$comments.y,
#                              ifelse(is.na(joined_df$comments.y), joined_df$comments.x,
#                                     paste(joined_df$comments.x, joined_df$comments.y, sep = ", "))) #separate comments from df_1 and df_2 by a comma
# joined_df <- select(joined_df, -comments.x, -comments.y)

for (col in intersect(names(df_1), names(df_2))) {
  if (col %in% c("wygf_leg_band", 'comments')) {
    next  # Skip the ID columns
  }
  
  joined_df[[col]] <- ifelse(is.na(joined_df[[paste0(col, ".x")]]) & !is.na(joined_df[[paste0(col, ".y")]]),
                             joined_df[[paste0(col, ".y")]],
                             ifelse(is.na(joined_df[[paste0(col, ".y")]]) & !is.na(joined_df[[paste0(col, ".x")]]),
                                    joined_df[[paste0(col, ".x")]],
                                    ifelse(joined_df[[paste0(col, ".x")]] == joined_df[[paste0(col, ".y")]],
                                           joined_df[[paste0(col, ".x")]],
                                           paste(joined_df[[paste0(col, ".x")]], joined_df[[paste0(col, ".y")]], sep = ", "))
                             )
  )
  
  # Remove the unnecessary columns
  joined_df <- select(joined_df, -matches(paste0(col, ".x")), -matches(paste0(col, ".y")))
}


#move some things around in the joined_df
joined_df <- joined_df %>%
  relocate(binder_pdf_scan_page, .before = time_search_start_24h)
duplicated_rows <- duplicated(joined_df$left_tag_code)


#Bring back the regular NA values (these got lost along the way)
joined_df[joined_df == ""] <- NA

#Relocate some more columns
#Relocate some more columns
joined_df <- joined_df %>%
  relocate(lek_name, .after = binder_pdf_scan_page) %>%
  relocate(wygf_leg_band, .after = right_tag_code) %>%
  relocate(year, .after = capture_technique) %>%
  relocate(month, .after = year) %>%
  relocate(day, .after = month) %>%
  relocate(source, .after = binder_pdf_scan_page) %>%
  relocate(left_tag_color, .after = day)%>%
  relocate(left_tag_code, .after = left_tag_color)%>%
  relocate(left_tag_number, .after = left_tag_color)%>%
  relocate(right_tag_color, .after = left_tag_code)%>%
  relocate(right_tag_number, .after = right_tag_color) %>%
  relocate(right_tag_code, .after = right_tag_color)%>%
  relocate(right_tag_number, .after = right_tag_color)

#now join the original_join and the new joined_df 

#quick column fix: 
joined_df[joined_df == "n/a"]  <- NA


# Get non-matching column names
non_matching_cols <- setdiff(names(original_joined_df), names(joined_df))
# Print the non-matching column names
print(non_matching_cols)
joined_df <- joined_df %>%
  rename(lice_back_of_head = lice_on_back_of_head) %>%
  rename(hematomas_comb_comments = hematomas__comb_comments)
# %>%
#   rename(lice_on_back_of_head = lice_back_of_head)

full_join <- rbind(original_joined_df, joined_df)
write.csv(full_join, "prelim_clean/binder_capture_data_prelim_N_MS_SS.csv")

# 
# #Ok, that worked great! Now to join with tagsort - will need to rename some columns to make everything mesh
# # Get the column names of each data frame
# col_names_df1 <- names(joined_df)
# col_names_df2 <- names(tagsort)
# 
# # Find the matching column names
# matching_cols <- intersect(col_names_df1, col_names_df2)
# matching_cols
# 
# non_matching_cols_df1 <- setdiff(col_names_df1, matching_cols)
# non_matching_cols_df1
# non_matching_cols_df2 <- setdiff(col_names_df2, matching_cols)
# non_matching_cols_df2
# 
# #Start renaming columns
# tagsort <- tagsort %>%
#   rename(time_search_general  = time) %>%
#   rename(weight_bird_and_bag_g = weight_g_with_bag) %>%
#   rename(hematomas_combs_present = hematomas_combs) %>%
#   rename(hematomas_air_sacs_present = hematomas_air_sacs) %>%
#   rename(total_num_lice_on_combs = lice_number__on_combs) %>%
#   rename(total_num_lice_on_air_sacs =lice_number_on_air_sacs) %>%
#   rename(fecal_any_other = any_other ) %>%
#   rename(blood_samples_malaria = blood_plasm) %>%
#   rename(blood_samples_haema = blood_haema) %>%
#   rename(blood_samples_leuco = blood_leuco) %>%
#   rename(mean_blood_pcv_mm = blood_pcv_mm) %>%
#   rename(mean_blood_total_volume_mm = total_volume_mm) %>%
#   rename(source = data_source)
# 
# #Check different columns now
# 
# col_names_df1 <- names(joined_df)
# col_names_df2 <- names(tagsort)
# 
# # Find the matching column names
# matching_cols <- intersect(col_names_df1, col_names_df2)
# matching_cols
# 
# non_matching_cols_df1 <- setdiff(col_names_df1, matching_cols)
# non_matching_cols_df1
# non_matching_cols_df2 <- setdiff(col_names_df2, matching_cols)
# non_matching_cols_df2
# 
# #Ok, here we go - combine!
# joined_df$year <- as.numeric(joined_df$year)
# tagsort$year <- as.numeric(tagsort$year)
# joined_df2 <- full_join(joined_df, tagsort, by = c("left_tag_code", "right_tag_code", "year"))
# 
# joined_df2 <- joined_df2 %>%
#   mutate_all(as.character)
# 
# for (col in names(joined_df2)) {
#   # Check if the column name ends with .x or .y suffix
#   if (endsWith(col, ".x")) {
#     # Get the base column name without the suffix
#     base_col <- str_remove(col, "\\.x$")
#     # Merge the columns using coalesce
#     joined_df2 <- joined_df2 %>%
#       mutate(!!base_col := coalesce(!!sym(paste0(base_col, ".x")), !!sym(paste0(base_col, ".y")))) %>%
#       select(-matches(paste0(base_col, "\\.")))
#   }
# }
# 
# 
# #There are some inconsistencies in the weight columns - looks like some are in miligrams. 
# #Correct that 
# weight_columns <- grep("weight", names(joined_df2), value = TRUE)
# joined_df2 <- joined_df2 %>%
#   mutate(across(all_of(weight_columns), as.numeric)) %>% 
#   mutate(across(all_of(weight_columns), ~ ifelse(. > 100, ./1000, .))) %>%
#   rename(weight_bag_kg = weight_bag_g) %>%
#   rename(weight_bird_and_bag_kg = weight_bird_and_bag_g) %>%
#   rename(weight_bird_kg = weight_bird_g)
# hist(joined_df2$weight_bird_and_bag_kg)
# 
# #Fix some columns unique to tagsort, like lice presence/absence
# #lice present column
# lice_columns <- grep("lice", names(joined_df2), value = TRUE)
# 
# # Update columns based on conditions
# joined_df2 <- joined_df2%>%
#   mutate(across(all_of(lice_columns), ~ ifelse(. > 1, "yes", ifelse(is.na(lice_present), "yes", .))))
# 
# 




