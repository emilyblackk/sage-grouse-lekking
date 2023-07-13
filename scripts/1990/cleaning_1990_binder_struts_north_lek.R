#Emily Black
#Cleaning 1990 strut binder data - north lek
#Created: 27 June 2023
#Last modified: 10 July 2023

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
df_1 <- read.csv('raw_data/binders/north_lek_1990_strut_binder_data.csv', 
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
df_1 <- df_1 %>%
  select(-na_na)

df_1 <- df_1 %>%
  mutate(lek_name = case_when(lek == "NORTH" ~ "north")) %>%
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
df_wider <- df_wider %>%
  mutate(left_tag_color = case_when(
    left_tag_color=="UM NA" ~ "unmarked", 
    TRUE~left_tag_color))
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
df_wider <- df_wider %>%
  mutate(right_tag_color = case_when(
    right_tag_color=="UM NA" ~ "unmarked", 
    TRUE~right_tag_color))
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

#Calculate number of struts per 5 minute
#This standardizes with the floppies
#Calculate the number of struts per five minutes
#If time strutting was less than 5 minutes, original value retained
#If more, adjusted for 5 minute period
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


#fix an error in df_wider
df_wider$day <- ifelse(df_wider$binder_pdf_page_number == '77', '25', df_wider$day)








#The struts dataset from the floppy disks probably contains a lot of repeats
#It also has more comprehensive tag names, sunset times, additional columns, etc. 
#So we should add those in here
strut_floppy <- read.csv('prelim_clean/strut_merged_df_prelim.csv')
#huh - looks like there are actually some issues with the colours! Let's fix that
strut_floppy$right_tag_colour <- str_extract(strut_floppy$right_tag_code, "[A-Za-z]+")
strut_floppy$right_tag_number <- str_extract(strut_floppy$right_tag_code, "\\d+")
strut_floppy <- strut_floppy %>%
  relocate(contains('right_tag'), .before= right_tag_code)
#add source column indicating that it's from the floppy disks
strut_floppy$source <- c("floppy")

strut_floppy$left_tag_colour <- str_extract(strut_floppy$left_tag_code, "[A-Za-z]+")
strut_floppy$left_tag_number <- str_extract(strut_floppy$left_tag_code, "\\d+")
strut_floppy <- strut_floppy %>%
  relocate(contains('left_tag'), .before= left_tag_code)

# Replace colours in the left_tag_colour column
strut_floppy$left_tag_colour <- replace_colors(strut_floppy$left_tag_colour, color_lookup)

# Replace colours in the right_tag_colour column
strut_floppy$right_tag_colour <- replace_colors(strut_floppy$right_tag_colour, color_lookup)



#Get sturts for this year and lek - 1990 and mud springs
strut_filtered <- strut_floppy %>%
  filter(year == 1990, lek_name =="north") %>%
  rename(left_tag_color= left_tag_colour) %>%
  rename(right_tag_color=right_tag_colour) 
#Get a time column so we can compare
# Function to convert decimal time to 24-hour format
convert_decimal_to_24hour <- function(decimal_time) {
  hours <- floor(decimal_time * 24)
  minutes <- round((decimal_time * 24 * 60) %% 60)
  paste0(hours, ":", sprintf("%02d", minutes))
}
strut_filtered$time_strut <- convert_decimal_to_24hour(strut_filtered$time_strut)
strut_filtered$sunrise <- convert_decimal_to_24hour(strut_filtered$sunrise)
strut_filtered <- strut_filtered %>%
  rename(time_start = time_strut) %>%
  rename(sunrise_time = sunrise)  %>%
  rename(distance_to_hens_category = dist_to_hens)

table(df_wider$type)
#there are 36 struts from the floppy, 
#and 42 in our dataframe
#So let's see how many matches we can find between the two 

#Let's do a full join and see what we get with just struts from the binders
strut_binders <- df_wider %>%
  filter(type=="strut")

strut_binders <- strut_binders %>% mutate_all(as.character)
strut_filtered <- strut_filtered %>% mutate_all(as.character) 
#Compare dates for strut binders and strut floppies
#To see how well they match up 
table_dates_binders <- table(strut_binders$month, strut_binders$day)
table_dates_floppy <- table(strut_filtered$month, strut_filtered$day)
#Some overlap, but not all 



#Join strut from binders and floppies
#note - there are some inconsistencies with the tags
#Some tags that are labelled "white" in the floppies and capture data
#are not white in the binder data
#Need to adjust the 
full_join <- full_join(strut_binders, strut_filtered, by = c('lek_name', 'month', 'day', 'time_start', 'left_tag_color', 'right_tag_color'))

#merge columns with commas to see where tags went wrong
for (col in intersect(names(strut_binders), names(strut_filtered))) {
  if (col %in% c('lek_name', 'month', 'day', 'time_start', 'left_tag_color', 'right_tag_color')) {
    next  # Skip the ID columns
  }
  
  full_join[[col]] <- ifelse(is.na(full_join[[paste0(col, ".x")]]) & !is.na(full_join[[paste0(col, ".y")]]),
                             full_join[[paste0(col, ".y")]],
                             ifelse(is.na(full_join[[paste0(col, ".y")]]) & !is.na(full_join[[paste0(col, ".x")]]),
                                    full_join[[paste0(col, ".x")]],
                                    ifelse(full_join[[paste0(col, ".x")]] == full_join[[paste0(col, ".y")]],
                                           full_join[[paste0(col, ".x")]],
                                           paste(full_join[[paste0(col, ".x")]], full_join[[paste0(col, ".y")]], sep = ", "))
                             )
  )
  # Remove the unnecessary columns
  full_join <- select(full_join, -matches(paste0(col, ".x")), -matches(paste0(col, ".y")))
}

#Do another full join but with unmatched columns
no_match_binder <- full_join %>%
  filter(!source=="binder, floppy") %>%
  filter(source=="binder")
no_match_floppy <- full_join %>%
  filter(!source=="binder, floppy") %>%
  filter(source=="floppy")
join_no_match <- full_join(no_match_binder, no_match_floppy, by = c('lek_name', "month", "day", 'time_start', "struts_5_min"))

for (col in intersect(names(no_match_binder), names(no_match_floppy))) {
  if (col %in% c('lek_name', "month", "day", "time_start", "struts_5_min")) {
    next  # Skip the ID columns
  }
  
  join_no_match[[col]] <- ifelse(is.na(join_no_match[[paste0(col, ".x")]]) & !is.na(join_no_match[[paste0(col, ".y")]]),
                                 join_no_match[[paste0(col, ".y")]],
                                 ifelse(is.na(join_no_match[[paste0(col, ".y")]]) & !is.na(join_no_match[[paste0(col, ".x")]]),
                                        join_no_match[[paste0(col, ".x")]],
                                        ifelse(join_no_match[[paste0(col, ".x")]] == join_no_match[[paste0(col, ".y")]],
                                               join_no_match[[paste0(col, ".x")]],
                                               paste(join_no_match[[paste0(col, ".x")]], join_no_match[[paste0(col, ".y")]], sep = ", "))
                                 )
  )
  # Remove the unnecessary columns
  suffixes <- c(".x", ".y")
  cols_to_remove <- paste0(col, suffixes)
  join_no_match <- join_no_match[, !(names(join_no_match) %in% cols_to_remove)]
}

#Great! So since that worked, we should be able to add it to the full dataset
#add a new column called "type" to make sure the data can talk to each other
strut_filtered$type <- c('strut')

df_wider <- df_wider %>% mutate_all(as.character)
df_full_join <- full_join(df_wider, strut_filtered, by = c('type', 'month', 'day', 'time_start', 'left_tag_color', 'right_tag_color'))
#merge columns with commas to see where tags went wrong
for (col in intersect(names(df_wider), names(strut_filtered))) {
  if (col %in% c('type', 'month', 'day', 'time_start', 'left_tag_color', 'right_tag_color')) {
    next  # Skip the ID columns
  }
  
  df_full_join[[col]] <- ifelse(is.na(df_full_join[[paste0(col, ".x")]]) & !is.na(df_full_join[[paste0(col, ".y")]]),
                                df_full_join[[paste0(col, ".y")]],
                                ifelse(is.na(df_full_join[[paste0(col, ".y")]]) & !is.na(df_full_join[[paste0(col, ".x")]]),
                                       df_full_join[[paste0(col, ".x")]],
                                       ifelse(df_full_join[[paste0(col, ".x")]] == df_full_join[[paste0(col, ".y")]],
                                              df_full_join[[paste0(col, ".x")]],
                                              paste(df_full_join[[paste0(col, ".x")]], df_full_join[[paste0(col, ".y")]], sep = ", "))
                                )
  )
  # Remove the unnecessary columns
  df_full_join <- select(df_full_join, -matches(paste0(col, ".x")), -matches(paste0(col, ".y")))
}

#Do another full join but with unmatched columns
#Add a unique identifier as well for removal later
df_full_join <- df_full_join %>%
  mutate(row = c(1:n()))
no_match_binder <- df_full_join %>%
  filter(!source=="binder, floppy") %>%
  filter(source=="binder") %>%
  filter(type=="strut")
no_match_floppy <- df_full_join %>%
  filter(!source=="binder, floppy") %>%
  filter(source=="floppy") %>%
  filter(type=="strut")
join_no_match <- full_join(no_match_binder, no_match_floppy, by = c("month", "day", 'time_start', "struts_5_min"))

for (col in intersect(names(no_match_binder), names(no_match_floppy))) {
  if (col %in% c("month", "day", "time_start", "struts_5_min")) {
    next  # Skip the ID columns
  }
  
  join_no_match[[col]] <- ifelse(is.na(join_no_match[[paste0(col, ".x")]]) & !is.na(join_no_match[[paste0(col, ".y")]]),
                                 join_no_match[[paste0(col, ".y")]],
                                 ifelse(is.na(join_no_match[[paste0(col, ".y")]]) & !is.na(join_no_match[[paste0(col, ".x")]]),
                                        join_no_match[[paste0(col, ".x")]],
                                        ifelse(join_no_match[[paste0(col, ".x")]] == join_no_match[[paste0(col, ".y")]],
                                               join_no_match[[paste0(col, ".x")]],
                                               paste(join_no_match[[paste0(col, ".x")]], join_no_match[[paste0(col, ".y")]], sep = ", "))
                                 )
  )
  # Remove the unnecessary columns
  suffixes <- c(".x", ".y")
  cols_to_remove <- paste0(col, suffixes)
  join_no_match <- join_no_match[, !(names(join_no_match) %in% cols_to_remove)]
}

#filter to rows that contain commas 
join_no_match <- join_no_match %>%
  filter(str_detect(source, ","))

#Get rid of the original merged rows in df_full_join
filtered_df_full_join <- df_full_join %>%
  filter(!(row %in% unlist(strsplit(join_no_match$row, ", "))))
#Join the rows back in 
filtered_df_full_join <- rbind(filtered_df_full_join, join_no_match)


#Do again, but with right tag code
#Do another full join but with unmatched columns
#Add a unique identifier as well for removal later
no_match_binder <- filtered_df_full_join %>%
  filter(!source=="binder, floppy") %>%
  filter(source=="binder") %>%
  filter(type=="strut")
no_match_floppy <- df_full_join %>%
  filter(!source=="binder, floppy") %>%
  filter(source=="floppy") %>%
  filter(type=="strut")
join_no_match <- full_join(no_match_binder, no_match_floppy, by = c("month", "day", 'right_tag_color', "struts_5_min"))

for (col in intersect(names(no_match_binder), names(no_match_floppy))) {
  if (col %in% c("month", "day", "right_tag_color", "struts_5_min")) {
    next  # Skip the ID columns
  }
  
  join_no_match[[col]] <- ifelse(is.na(join_no_match[[paste0(col, ".x")]]) & !is.na(join_no_match[[paste0(col, ".y")]]),
                                 join_no_match[[paste0(col, ".y")]],
                                 ifelse(is.na(join_no_match[[paste0(col, ".y")]]) & !is.na(join_no_match[[paste0(col, ".x")]]),
                                        join_no_match[[paste0(col, ".x")]],
                                        ifelse(join_no_match[[paste0(col, ".x")]] == join_no_match[[paste0(col, ".y")]],
                                               join_no_match[[paste0(col, ".x")]],
                                               paste(join_no_match[[paste0(col, ".x")]], join_no_match[[paste0(col, ".y")]], sep = ", "))
                                 )
  )
  # Remove the unnecessary columns
  suffixes <- c(".x", ".y")
  cols_to_remove <- paste0(col, suffixes)
  join_no_match <- join_no_match[, !(names(join_no_match) %in% cols_to_remove)]
}

#filter to rows that contain commas 
join_no_match <- join_no_match %>%
  filter(str_detect(source, ","))

#Get rid of the original merged rows in df_full_join
filtered_df_full_join <- filtered_df_full_join %>%
  filter(!(row %in% unlist(strsplit(join_no_match$row, ", "))))

#Join the rows back in 
filtered_df_full_join <- rbind(filtered_df_full_join, join_no_match)




#Update the breed column to reflect copulations from the binder data
# Assign "yes" to "breed" if "num_of_copulations" has a value
filtered_df_full_join$breed <- ifelse(!is.na(filtered_df_full_join$num_of_copulations), "yes", filtered_df_full_join$breed)

#Now relocate a bunch of things for ease of reading
filtered_df_full_join <- filtered_df_full_join %>%
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
  select(-time_to_sunrise) %>%
  relocate(age, .after = right_tag_code) %>%
  relocate(num_of_struts, .after = time_end) %>%
  relocate(sunrise_time, .after = time) %>%
  relocate(distance_to_hens_category, .before = distance_to_hens_notes) %>%
  relocate(breed, .after = num_of_copulations) %>%
  relocate(malaria, .after = breed ) %>%
  relocate(lice, .after = malaria) %>%
  relocate(other_fights_and_comments_, .after= lice ) %>%
  relocate(struts_5_min, .after= num_of_struts) %>%
  rename(other_fights_and_comments = other_fights_and_comments_)%>%
  select(-row)
#Open the previous strut dataset
previous_strut <- read.csv('prelim_clean/1990/merged_struts_binders_floppy_MS.csv')

#merge the two 
final_strut <- rbind(previous_strut, filtered_df_full_join)

#write to computer
write.csv(final_strut, 'prelim_clean/1990/merged_struts_binders_floppy_MS_N.csv', 
          row.names=FALSE)
