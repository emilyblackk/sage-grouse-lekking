#Emily Black
#Cleaning 1987 binder data - windmill dam
#Created: 10 July 2023
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

df_1 <- read.csv('raw_data/binders/sybille_springs_1987_capture_binder_data.csv', 
                 header=FALSE)



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
df_1$date <- format(as.Date(df_1$date, format = "%Y-%m-%d"), "%Y-%m-%d") #help R read the dates

#Create month day year columns 
df_1$year <- format(as.Date(df_1$date, format = "%Y-%m-%d"), "%Y") #collect year from date
df_1 <- df_1 %>%
  relocate(year, .after = date) #relocate the year column after the date column
df_1$month <- format(as.POSIXlt(df_1$date, format = "%Y-%m-%d"), "%m") #same as above for month
df_1 <- df_1 %>%
  relocate(month, .after = year)
df_1$day <- format(as.Date(df_1$date, format = "%Y-%m-%d"), "%d") #same as above for year
df_1 <- df_1 %>%
  relocate(day, .after = month)

#Remove original date column 
df_1 <- df_1 %>%
  subset(select=c(-date))

#We need standardized lek names so the different datasets can "talk" to each other
#rename cordingly
unique(df_1$lek)
df_1$lek_name <- c("sybille_springs")
df_1 <- df_1 %>%
  relocate(lek_name, .after=lek) %>%
  select(-lek)

# #Looks like weights are in grams - need to make kg
# df_1 <- df_1 %>%
#   mutate(bird_weight_g = as.numeric(bird_weight_g)/1000)

#standardize the age column 
#This is another column that needs to match the standards in others, 
#because it's going to get merged later 
unique(df_1$age)
df_1$age <- tolower(df_1$age) #make lower case
df_1 <- df_1 %>%
  mutate(age = case_when(age == "adult " ~ 'adult', 
                         age == "adult" ~ 'adult',
                         age=='juvenile' ~ "juven", 
                         age=='yearling' ~ "juven", 
  )) #change the "juvenile" code in the data
#Also standardize the sex column in the same way 
df_1$sex <- tolower(df_1$sex)

#Standardize comb color
df_1 <- df_1 %>%
  mutate(comb_color = case_when(
    tolower(comb_color) == "olive yell" ~ "olive yellow",
    tolower(comb_color) == "yell - olive" ~ "yellow olive",
    tolower(comb_color) == "yell olive" ~ "yellow olive",
    tolower(comb_color) == "chrome yellow" ~ "chrome yellow",
    tolower(comb_color) == "greenish yellow" ~ "greenish yellow",
    tolower(comb_color) == "yellow-green" ~ "yellow green",
    tolower(comb_color) == "bistre yell" ~ "bistre yellow",
    tolower(comb_color) == "bistre" ~ "bistre yellow",
    tolower(comb_color) == "chrome yell" ~ "chrome yellow",
    tolower(comb_color) == "yellow/green" ~ "yellow green",
    tolower(comb_color) == "green/yellow" ~ "green yellow",
    
    comb_color == "n/a" ~ NA,
    TRUE ~ comb_color
  ),
  comb_color = tolower(comb_color))
unique(df_1$comb_color)


#Do the same for air sac
df_1 <- df_1 %>%
  mutate(air_sac_color = case_when(
    tolower(air_sac_color) == "olive yell" ~ "olive yellow",
    tolower(air_sac_color) == "yell - olive" ~ "yellow olive",
    tolower(air_sac_color) == "yell olive" ~ "yellow olive",
    tolower(air_sac_color) == "chrome yellow" ~ "chrome yellow",
    tolower(air_sac_color) == "greenish yellow" ~ "greenish yellow",
    tolower(air_sac_color) == "yellow-green" ~ "yellow green",
    tolower(air_sac_color) == "bistre yell" ~ "bistre yellow",
    tolower(air_sac_color) == "bistre-yellow" ~ "bistre yellow",
    tolower(air_sac_color) == "bristre-yellow" ~ "bistre yellow",
    tolower(air_sac_color) == "bistre" ~ "bistre yellow",
    tolower(air_sac_color) == "chrome yell" ~ "chrome yellow",
    tolower(air_sac_color) == "yellow/green" ~ "yellow green",
    tolower(air_sac_color) == "grey/yellow" ~ "gray yellow",
    air_sac_color == "n/a" ~ NA,
    TRUE ~ air_sac_color
  ),
  air_sac_color = tolower(air_sac_color))
unique(df_1$air_sac_color)

#Fix lice back of head
unique(df_1$lice_on_back_of_head_)
df_1 <- df_1 %>%
  mutate(lice_back_of_head = case_when(
    lice_on_back_of_head_=="+" ~ "yes", 
    lice_on_back_of_head_== "-" ~ "no",
    lice_on_back_of_head_== "2+" ~ "yes")) %>%
  relocate(lice_back_of_head, .after = lice_on_back_of_head_) %>%
  select(-lice_on_back_of_head_)
unique(df_1$lice_back_of_head)


#Add a tag code to dataframe
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

# Relocate the identified columns after "lek_name"
df_1 <- df_1 %>%
  relocate(all_of(cols_to_relocate), .after = lek_name) 

#Fix collect, cut feathers, and labelled columns
df_1 <- df_1 %>%
  mutate(cut_feathers_ = case_when(
    cut_feathers_=="TRUE" ~ "yes", 
    cut_feathers_=="FALSE" ~ "no"
  )) %>%
  rename(cut_feathers = cut_feathers_)
unique(df_1$cut_feathers)

df_1 <- df_1 %>%
  mutate(collect_ = case_when(
    collect_=="TRUE" ~ "yes", 
    collect_=="FALSE" ~ "no"
  )) %>%
  rename(collect = collect_)
unique(df_1$collect)

df_1 <- df_1 %>%
  mutate(labelled_ = case_when(
    labelled_=="TRUE" ~ "yes", 
    labelled_=="FALSE" ~ "no"
  )) %>%
  rename(labelled = labelled_)
unique(df_1$labelled)

#relabel samples columns

df_1 <- df_1 %>%
  mutate(samples_collected_blood_ = case_when(
    samples_collected_blood_=="TRUE" ~ "yes", 
    samples_collected_blood_=="FALSE" ~ "no"
  )) %>%
  rename(samples_collected_blood = samples_collected_blood_)
unique(df_1$samples_collected_blood)

df_1 <- df_1 %>%
  mutate(samples_collected_ectoparasites = case_when(
    samples_collected_ectoparasites=="TRUE" ~ "yes", 
    samples_collected_ectoparasites=="FALSE" ~ "no"
  )) %>%
  rename(samples_collected_ectoparasites = samples_collected_ectoparasites)
unique(df_1$samples_collected_ectoparasites)


df_1 <- df_1 %>%
  mutate(samples_collected_fecal = case_when(
    samples_collected_fecal=="TRUE" ~ "yes", 
    samples_collected_fecal=="FALSE" ~ "no"
  )) %>%
  rename(samples_collected_fecal = samples_collected_fecal)
unique(df_1$samples_collected_fecal)

#In the rest of the dataframe, replace + and - with yes and no
#And TRUE and FALSE
df_1[df_1=="+"] <- "yes"
df_1[df_1=="-"] <- "no"
df_1[df_1=="TRUE"] <- "yes"
df_1[df_1=="FALSE"] <- "no"


#Rename the notes column
df_1 <- df_1 %>%
  rename(notes = notes) %>%
  rename(num_lice_on_comb = num__lice_on_comb) %>%
  rename(wygf_leg_band = wygf_leg_band__) %>%
  rename(hematoma_group = hematoma_group__) %>%
  rename(stresses_while_handling_describe = `_stresses_while_handling_describe`)

#Merge with previous 1987 capture date
previous_capture <- read.csv("prelim_clean/1987/binder_capture_data_1987_prelim_CL_BW_MS_MC_N_SF_WD.csv")
head(previous_capture)

previous_capture <- previous_capture %>%
  mutate_all(as.character)
df_1 <- df_1 %>%
  mutate_all(as.character)
full_capture <- bind_rows(previous_capture, df_1)

#Fix a small inconsistency in the sexes in full capture
unique(full_capture$sex)
#full_capture[full_capture=="female "] <- 'female'

#Write to github
write.csv(full_capture, "prelim_clean/1987/binder_capture_data_1987_prelim_CL_BW_MS_MC_N_SF_WD_SS.csv", 
          row.names=FALSE)


#small plot to check things worked
full_capture %>%
  ggplot(aes(x=as.numeric(bird_weight_g), color = sex)) + 
  geom_histogram() +
  theme_classic()

