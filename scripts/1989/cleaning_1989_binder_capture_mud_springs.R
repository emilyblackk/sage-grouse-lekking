#Emily Black
#Cleaning 1989 binder data - mud_springs
#Created: 11 July 2023
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

#read in the mud_springs lek data 
df_1 <- read.csv('raw_data/binders/mud_springs_1989_capture_binder_data.csv', 
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

unique(df_1$lek)
df_1 <- df_1 %>%
  mutate(lek_name = c("mud_springs")) %>%
  relocate(lek_name, .after = lek) %>%
  select(-lek)

#standardize the age column 
#This is another column that needs to match the standards in others, 
#because it's going to get merged later 
df_1$age <- tolower(df_1$age) #make lower case
unique(df_1$age)
df_1 <- df_1 %>%
  mutate(age = case_when(age == 'adult' ~ 'adult', 
                         age == "adult " ~ 'adult', 
                         age=='juvenile' ~ "juven")) #change the "juvenile" code in the data
#Also standardize the sex column in the same way 
df_1$sex <- tolower(df_1$sex)
df_1$capture_technique <- tolower(df_1$capture_technique)


#The weight bird and bag column has question marks instead of NAs
#also, note that weights are actually in kg, but for consistency this will be
#changed at the end
df_1 <- df_1 %>%
  rename(weight_bird_and_bag_g= weight_bird_and_bag_g) %>%
  rename(weight_bag_g = weight_bag_g) %>%
  rename(bird_weight_g = bird_weight_g)
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
unique(df_1$comb_color_) #unique colours 
#Looks like some substitutions need to be made
df_1 <- df_1 %>%
  rename(comb_color = comb_color) %>%
  mutate(comb_color = case_when(
    tolower(comb_color) == "olive yell" ~ "olive yellow",
    tolower(comb_color) == "yell - olive" ~ "yellow olive",
    tolower(comb_color) == "yell olive" ~ "yellow olive",
    tolower(comb_color) == "yellow/olive" ~ "yellow olive",
    tolower(comb_color) == "chrome yellow" ~ "chrome yellow",
    tolower(comb_color) == "greenish yellow" ~ "greenish yellow",
    tolower(comb_color) == "green/yellow" ~ "green yellow",
    tolower(comb_color) == "yellow-green" ~ "yellow green",
    tolower(comb_color) == "bistre yell" ~ "bistre yellow",
    tolower(comb_color) == "bistre" ~ "bistre yellow",
    tolower(comb_color) == "bistre yellow "  ~ "bistre yellow",
    tolower(comb_color) == "chrome yell" ~ "chrome yellow",
    tolower(comb_color) == "yellow/olive " ~ "yellow olive",
    tolower(comb_color) == "olive/yellow" ~ "olive yellow",
    tolower(comb_color) == "olive - yellow" ~ "olive yellow",
    comb_color == "n/a" ~ NA,
    TRUE ~ tolower(comb_color)
  ),
  comb_color = tolower(comb_color))
unique(df_1$comb_color)



#Do the same for air sac colors
unique(df_1$air_sac_color)
df_1 <- df_1 %>%
  rename(air_sac_color = air_sac_color) %>%
  mutate(air_sac_color = case_when(
    tolower(air_sac_color) == "bistre yell" ~ "bistre yellow",
    tolower(air_sac_color) == "bistre Yell" ~ "bistre yellow",
    tolower(air_sac_color) == "bistre yell" ~ "bistre yellow",
    tolower(air_sac_color) == "bistre yellow "  ~ "bistre yellow",
    tolower(air_sac_color) == "bistre" ~ "bistre yellow",
    tolower(air_sac_color) == "chrome yell" ~ "chrome yellow",
    tolower(air_sac_color) == "chrome yellow" ~ "chrome yellow",
    tolower(air_sac_color) == "olive/yellow" ~ "olive yellow",
    air_sac_color == "n/a" ~ NA,
    air_sac_color == "na" ~ NA,
    TRUE ~ tolower(air_sac_color)
  ),
  air_sac_color = tolower(air_sac_color))
unique(df_1$air_sac_color)

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 1b. Fixing lice and hematoma columns 

#Lice on air sacs actually has left and right indicated
#And add these columns into the final strut
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
  relocate(total_num_lice_on_air_sacs, .after=num_lice_on_air_sac_right)



df_1 <- df_1 %>% select(-num_lice_on_air_sac)



#hematomas on air sacs and combs has two unique values: a R value, and a L value
#The code below splits them into two columns - combs
# Split the values and assign them to the new columns
df_1 <- df_1 %>%
  rename(hematomas_num_combs = num_combs)
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


#Correct a few rows manually 
df_1$total_num_lice_on_air_sacs[1] <- 2
df_1$num_hematomas_on_comb_left[2] <- 0






#Lice on back of head has plusses and minuses, which is hard to read and analyze
#Make these yes and no for ease of reading 
df_1 <- df_1 %>%
  mutate(lice_back_of_head = case_when(
    lice_on_back_of_head_=="+" ~ "yes", 
    lice_on_back_of_head_== "-" ~ "no")) %>%
  relocate(lice_back_of_head, .after = lice_on_back_of_head_) %>%
  select(-lice_on_back_of_head_)

#In the existing injuries column, there are dashes
#These aren't standardized, and should be NA
#existing injuries column - replace dashes with NA
df_1$existing_injuries[df_1$existing_injuries == "-"] <- NA
df_1$handling_injury[df_1$handling_injury == "-"] <- NA
# df_1 <- df_1 %>%
#   rename(stresses_while_handling_describe = "_stresses_while_handling_describe")
df_1$stresses_while_handling_describe[df_1$stresses_while_handling_describe == "-"] <- NA


#Replace + with yes and - with no everywhere in the dataset
df_1[df_1=="+"] <- "yes"
df_1[df_1=="+ "] <- "yes"
df_1[df_1=="-"] <- "no"
df_1[df_1=="- "] <- "no"



#Do the same with true and false
df_1[df_1=="TRUE"] <- "yes"
df_1[df_1=="FALSE"] <- "no"



#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Back to regular cleaning
#A little less intense than above! 

#Standardize the tag colours - in strutting data, we have columns with tag number, colour, 
#and overall code. We need to make that the case here as well: 
df_1$color_left_wing <- gsub("W([A-Z][a-z]+)", "White \\1", df_1$color_left_wing) #If there is a W, replace with white
df_1$color_left_wing <- gsub("R([A-Z][a-z]+)", "Red \\1", df_1$color_left_wing) #If there is a R, replace with red

df_1$color_left_wing <- gsub("Yell", "yellow", df_1$color_left_wing) #Fix shortened yellow
df_1$color_left_wing <- gsub("yellowow", "yellow", df_1$color_left_wing) #fix small inconsistency with replacement
df_1$color_left_wing <- tolower(df_1$color_left_wing)
df_1 <- df_1 %>%
  rename(left_tag_color = color_left_wing) %>%
  rename(left_tag_number = number_left_wing) %>%
  relocate(left_tag_number, .after = left_tag_color)
df_1$left_tag_color <- str_trim(df_1$left_tag_color)


df_1$color_right_wing <- gsub("W([A-Z][a-z]+)", "White \\1", df_1$color_right_wing)
df_1$color_right_wing <- gsub("R([A-Z][a-z]+)", "Red \\1", df_1$color_right_wing)

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


# df_1 <- df_1 %>%
#   mutate(samples_collected_cecal = case_when(
#     samples_collected_cecal=="TRUE" ~ "yes", 
#     samples_collected_cecal=="FALSE" ~ "no"
#   )) %>%
#   rename(samples_collected_cecal = samples_collected_cecal)




#Remove trailing underscores from names 
remove_trailing_underscores <- function(data) {
  colnames(data) <- sub("_+$", "", colnames(data))
  return(data)
}
df_1 <- remove_trailing_underscores(df_1)


# #rename hematoma air sacs
# df_1 <- df_1 %>%
#   rename(hematomas_air_sacs  = "_hematomas_air_sacs")


#Open previous strut file
previous_strut <- read.csv("prelim_clean/binder_capture_data_1989_prelim_CL_LL_N_VP.csv")


final_strut <- rbind(previous_strut, df_1)
final_strut$blood_malaria <- tolower(final_strut$blood_malaria)
final_strut$blood_haema <- tolower(final_strut$blood_haema)



# final_strut <- final_strut %>%
#   relocate(num_lice_on_air_sac_left, .before = total_num_lice_on_air_sacs) %>%
#   relocate(num_lice_on_air_sac_right, .after = num_lice_on_air_sac_left)
# #correct inconsistencies in sex
unique(final_strut$sex)
final_strut[final_strut=="female "] <- "female"

#Write to save
write.csv(final_strut,  "prelim_clean/binder_capture_data_1989_prelim_CL_LL_N_VP_MS.csv", 
          row.names=FALSE)

# #small plot to check things worked
unique(final_strut$lek_name)

final_strut %>%
  ggplot(aes(x=as.numeric(weight_bird_g), color = sex)) +
  geom_histogram() +
  theme_classic()