#Emily Black
#Cleaning 1990 strut binder data - removal study
#Created: 27 June 2023
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

#read in the mud springs data 
df_1 <- read.csv('raw_data/binders/removal_study_1990_strut_binder_data.csv', 
                 header=FALSE)


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
#Make blank spaces NA
df_1[df_1 == ""] <- NA



#some of the columns need to be transferred across the rows, because they are true for all rows
#This is the case for the first 16 columns

# Iterate over each column 
for (col in c(2:16, 33)) {
  # Initialize the previous value
  prev_value <- NA
  
  # Iterate over each row
  for (i in 1:nrow(df_1)) {
    # Check if the reference column has a new value
    if (!is.na(df_1[i, 1])) {
      # Update the previous value
      prev_value <- df_1[i, col]
    } else {
      # Fill the empty row with the previous non-empty value
      df_1[i, col] <- prev_value
    }
  }
}

#Do the same for the first column
# Initialize the previous value for the first column
prev_value <- NA

#Iterate over each row
for (i in 1:nrow(df_1)) {
  if (!is.na(df_1[i, 1])) {
    prev_value <- df_1[i, 1]
  } else {
    df_1[i, 1] <- prev_value
  }
}


#Standardize the lek names

#Remove the na_na columns before doing anything dplyr
# df_1 <- df_1 %>%
#   select(-na_na)

df_1 <- df_1 %>%
  mutate(lek_name = case_when(lek == "North Lek" ~ "north")) %>%
  relocate(lek_name, .after = lek) %>%
  select(-lek)

#in the snowing column, replace dashes with "no" 
df_1$snowing_status[df_1$snowing_status=="- "] <- "no"
df_1$snowing_status[df_1$snowing_status=="-"] <- "no"
df_1$snowing_status <- tolower(df_1$snowing_status)
unique(df_1$snowing_status)


#Do the same for rain
df_1$raining_status[df_1$raining_status=="-"] <- "no"
df_1$raining_status[df_1$raining_status=="- "] <- "no"
df_1$raining_status <- tolower(df_1$raining_status)
unique(df_1$raining_status)

#In other weather conditions, replace dashes with NA
df_1$other_weather_conditions[df_1$other_weather_conditions=="-"] <- NA
unique(df_1$other_weather_conditions)


#for predator disturbance type, make GEagle into golden eagle
unique(df_1$predator_disturbance_type_)
#Predator values look ok, other than setting "None" as NA
df_1$predator_disturbance_type_[df_1$predator_disturbance_type_=="None"] <- NA

#Replace the rest of the dashes with NA
df_1[df_1=="-"] <- NA



#I think what makes the most sense here is a pivot_longer, with three different types of observations, 
#observation, strut, and copulation. 
#note that many of these will be found in the strut dataset, which will be read in 
#later to fix up everything

#Ideally, we would like as many of these columns to match as possible
#so take a look at the names first 
obs_columns <- grep("obs", names(df_1), value = TRUE)
obs_columns
#Clean up some column names 
df_1 <- df_1 %>%
  rename(observation_time = observation_time_) %>%
  rename(observation_comments_observations_or_marks = observations_or_marks) %>%
  rename(observation_num_males_observed = num_males_observed) %>%
  rename(observation_num_females_observed = num_females_observed)
strut_columns <- grep("strut", names(df_1), value = TRUE)
strut_columns
df_1 <- df_1 %>%
  rename(strut_time_start = '_strut_time_start')%>%
  rename(strut_time_end = 'strut_time__end') %>%
  rename(strut_num_of_struts = num_of_struts_) %>%
  rename(strut_tag_colors_left = tag_colours_left_) %>%
  rename(strut_tag_colors_right = tag_colours_right) %>%
  rename(strut_distance_to_hens_category = distance_to_hens_category_) %>%
  rename(strut_distance_to_hens_notes = distance_to_hens_comments_) %>% 
  rename(strut_fights = fights_)
cop_columns <- grep("copulation", names(df_1), value = TRUE)
cop_columns
#Rename some copulation columns 
df_1 <- df_1 %>%
  rename(copulation_tag_colors_left = copulation_tag_colours_left_) %>%
  rename(copulation_tag_colors_right = copulation_right) %>%
  rename(copulation_num_of_copulations = num_copulations_) %>%
  rename(copulation_time = copulation_time_)


#add a row numbers column for sorting later
df_1$row_number <- c(1:nrow(df_1))


df_pivoted <- df_1 %>%
  pivot_longer(cols = starts_with(c("observation", "strut", "copulation")),
               names_to = "column",
               values_to = "value")%>%
  mutate(type = str_extract(column, "^[^_]+"))

df_pivoted

df_wider <- df_pivoted %>%
  pivot_wider(names_from = column, 
              values_from = value)

df_wider<- unite(df_wider, merged_time, observation_time, copulation_time, sep = " ", na.rm=TRUE)
df_wider<- unite(df_wider, merged_left_tag_number, strut_tag_colors_left, copulation_tag_colors_left, sep = " ", na.rm=TRUE)
df_wider<- unite(df_wider, merged_right_tag_number, strut_tag_colors_right, copulation_tag_colors_right, sep = " ", na.rm=TRUE)


new_column_names <- str_replace(names(df_wider), "^(observation|strut|copulation|merged)_", "")
colnames(df_wider) <- new_column_names



#Need to fix up the left and right tag numbers
#One, they are a mix of numbers and letters
#Two, they aren't standardized
df_wider$left_tag_color <- str_extract(df_wider$left_tag_number, "[A-Za-z]+( [A-Za-z]+)?")
df_wider$left_tag_color <- tolower(df_wider$left_tag_color)


df_wider$left_tag_number <- ifelse(grepl("-", df_wider$left_tag_number), NA, df_wider$left_tag_number)
df_wider$left_tag_number <- str_extract(df_wider$left_tag_number,  "\\d+")
#now need to case some of the colours to all be the same 
unique(df_wider$left_tag_color)

#standardize colour values prior to colour lookup
replace_values <- function(column) {
  result <- ifelse(column == "UNMARKED", "UM", column)  # Replace "UNMARKED" with "UM"
  result <- ifelse(nchar(result) >= 3, sapply(strsplit(result, "\\s+"), function(x) paste0(substr(x, 1, 1), collapse = "")), result)  # Replace longer values with first letter of each word
  return(result)
}
#Move W to the front
move_w_to_front <- function(column) {
  for (i in 1:length(column)) {
    if (!is.na(column[i]) && grepl("w", column[i])) {
      column[i] <- gsub("w", "", column[i], fixed = TRUE)
      column[i] <- paste0("w", column[i])
    }
  }
  return(column)
}

df_wider$left_tag_color <- replace_values(df_wider$left_tag_color)
df_wider$left_tag_color <- move_w_to_front(df_wider$left_tag_color)
df_wider$left_tag_color <- toupper(df_wider$left_tag_color)


# Define a lookup table for color replacements
color_lookup <- c("G" = "green", "R" = "red", "Y" = "yellow", "W" = "white", "O" = "orange", "B" = "blue", "P" = "purple",  "U" = "UM")

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
df_wider$left_tag_color <- replace_colors(df_wider$left_tag_color, color_lookup)
unique(df_wider$left_tag_color)

#Great! Now move things around... 
df_wider <- df_wider %>%
  relocate(left_tag_color, .before = left_tag_number)



#Do the same for the right 
df_wider$right_tag_color <- str_extract(df_wider$right_tag_number, "[A-Za-z]+( [A-Za-z]+)?")
df_wider$right_tag_color <- tolower(df_wider$right_tag_color)


df_wider$right_tag_number <- ifelse(grepl("-", df_wider$right_tag_number), NA, df_wider$right_tag_number)
df_wider$right_tag_number <- str_extract(df_wider$right_tag_number,  "\\d+")
#now need to case some of the colours to all be the same 
unique(df_wider$right_tag_color)

#standardize colour values prior to colour lookup

df_wider$right_tag_color <- replace_values(df_wider$right_tag_color)
df_wider$right_tag_color <- move_w_to_front(df_wider$right_tag_color)
df_wider$right_tag_color <- toupper(df_wider$right_tag_color)


# Replace colors in the right_tag_color column
df_wider$right_tag_color <- replace_colors(df_wider$right_tag_color, color_lookup)
unique(df_wider$right_tag_color)

#Great! Now move things around... 
df_wider <- df_wider %>%
  relocate(right_tag_color, .before = right_tag_number)


#If there is a number and a color, make a tag code
df_wider <- df_wider %>%
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
df_wider$right_tag_code<- ifelse(grepl("\\d", df_wider$right_tag_code), df_wider$right_tag_code, NA)


#If there is a number and a color, make a tag code
df_wider <- df_wider %>%
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
df_wider$left_tag_code<- ifelse(grepl("\\d", df_wider$left_tag_code), df_wider$left_tag_code, NA)

#There are also some rows from the join that are all NA. We can eliminate those here: 
df_wider[df_wider==""] <- NA
df_wider[df_wider=="NA"] <- NA
df_wider[df_wider=="NA NA"] <- NA

df_wider <- df_wider[rowSums(!is.na(df_wider[, 20:ncol(df_wider)])) > 0, ]



#The dates need a lot of standardizing, some are in - format and some are in " " format
#make them into year month and day columns 
df_wider$date <- gsub("^\\s*(\\d)", "\\1", df_wider$date)
df_wider$date <- gsub(" ", "-", df_wider$date)  # Replace spaces with dashes
df_wider$date <- format(as.Date(df_wider$date, format = "%d-%m-%Y"), "%d-%m-%Y") 
#Create month day year columns 
df_wider$year <- format(as.Date(df_wider$date, format = "%d-%m-%Y"), "%Y")
df_wider <- df_wider %>%
  relocate(year, .after = date)
df_wider$month <- format(as.POSIXlt(df_wider$date, format = "%d-%m-%y"), "%m")
df_wider <- df_wider %>%
  relocate(month, .after = year)
#remove leading zeroes
df_wider$month <- str_remove(df_wider$month, "^0")
df_wider$day <- format(as.Date(df_wider$date, format = "%d-%m-%Y"), "%d")
df_wider <- df_wider %>%
  relocate(day, .after = month)
#remove leading zeroes
df_wider$day <- str_remove(df_wider$day, "^0")
#Remove original date column 
df_wider <- df_wider %>%
  select(-date)

#Add source column, and indicate from binder
df_wider$source <- c('binder')

#Add row with struts_5_min
df_wider <- df_wider %>%
  mutate(
    time_start_calc = as.POSIXct(time_start, format = "%H:%M"),
    time_end_calc = as.POSIXct(time_end, format = "%H:%M"),
    time_diff = as.numeric(difftime(time_end_calc, time_start_calc, units = "mins")),
    struts_5_min = ifelse(
      is.na(time_start_calc) | is.na(time_end_calc),
      as.numeric(num_of_struts),
      ifelse(time_diff < 5, as.numeric(num_of_struts), round(as.numeric(num_of_struts) / time_diff * 5))
    )
  ) %>%
  select(-time_start_calc, -time_end_calc, -time_diff)



#The struts dataset from the floppy disks probably contains a lot of repeats
#It also has more comprehensive tag names, sunset times, additional columns, etc. 
#So we should add those in here
#However, for removal study I doubt they would have these data 
strut_floppy <- read.csv('prelim_clean/strut_merged_df_prelim.csv')
#There are north lek struts; however, in these data we only have observations 

#So we can move on with the cleaning 
#Now relocate a bunch of things for ease of reading
filtered_df_full_join <- df_wider %>%
  relocate(lek_name, .after = observer) %>%
  select(-row_number) %>%
  relocate(left_tag_color, .after = type) %>%
  relocate(left_tag_number, .after = left_tag_color) %>%
  relocate(left_tag_code, .after = left_tag_number) %>%
  relocate(right_tag_color, .after = left_tag_code) %>%
  relocate(right_tag_number, .after = right_tag_color) %>%
  relocate(right_tag_code, .after = right_tag_number) %>%
  relocate(year, .before = month) %>%
  relocate(source, .before = binder_pdf_page_number) %>%
  #select(-time_to_sunrise) %>%
  #relocate(age, .after = right_tag_code) %>%
  relocate(num_of_struts, .after = time_end) %>%
  relocate(sunrise_time, .after = time) %>%
  relocate(distance_to_hens_category, .before = distance_to_hens_notes) %>%
  #relocate(breed, .after = num_of_copulations) %>%
  #relocate(malaria, .after = breed ) %>%
  #relocate(lice, .after = malaria) %>%
  #relocate(other_fights_and_comments_, .after= lice ) %>%
  rename(other_fights_and_comments = other_fights_and_comments_)
  #select(-row)

#Open the previous strut dataset
previous_strut <- read.csv('prelim_clean/1990/merged_struts_binders_floppy_MS_N_SF.csv')

#merge the two 
previous_strut <- mutate_all(previous_strut, as.character)
filtered_df_full_join <- mutate_all(filtered_df_full_join, as.character)


final_strut <- bind_rows(previous_strut, filtered_df_full_join)

#write to computer
write.csv(final_strut, 'prelim_clean/1990/merged_struts_binders_floppy_MS_N_SF_RS.csv', 
          row.names=FALSE)
