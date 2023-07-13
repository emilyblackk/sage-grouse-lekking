#Emily Black 
#Cleaning TRAN dataset, and comparing with STRUT
#Created: 7 June 2023
#Last modified: 7 June 2023

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

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

tran <- read.csv("prelim_clean/TRAN.csv", 
                    header=FALSE)
head(tran)
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
tran <- fix_column_names(tran)



#We now need to fill blank spaces with NAs
#And remove redundant columns, or replace them
tran <- tran %>% mutate_all(~ifelse(grepl("^\\s*$", .x), NA, .x))
tran <- tran[rowSums(is.na(tran)) != ncol(tran), ]

#The names of the leks should be standardized for analyses
tran$lek_name <- tolower(tran$lek_name) #make lower case
tran$lek_name <-  gsub(" ", "_", tran$lek_name) 
unique(tran$lek_name)
#Looks like there are some spelling mistakes throughout 
#These should be standardized with those in strut
strut <- read.csv("prelim_clean/strut_merged_df_prelim.csv")
unique(strut$lek)

patterns <- c("s\\.","south_fettermna", "windmill", "north_lek", "view_point", 
              'mule_creek_jnct', 'south_fet')
replacements <- c("south", "south_fetterman", "windmill_dam", "north", "viewpoint", 
                  'mule_creek', 'south_fetterman')

# Iterate over each pattern and replacement
for (i in seq_along(patterns)) {
  tran$lek_name <- gsub(patterns[i], replacements[i], tran$lek_name, ignore.case = TRUE)
}
tran$lek_name <- str_replace(tran$lek_name, "south_fettermanterman", "south_fetterman")
tran$lek_name <- str_replace(tran$lek_name, "windmill_dam_dam", "windmill_dam")
unique(tran$lek_name)



#split the tags into colour and number for easier comparison between data
tran$right_tag_colour <- str_extract(tran$tag_numbers_right, "[A-Za-z]+")
tran$right_tag_number <- str_extract(tran$tag_numbers_right, "\\d+")

#split the tags into colour and number for easier comparison between data
tran$left_tag_colour <- str_extract(tran$tag_numbers_left, "[A-Za-z]+")
tran <- tran %>%
  relocate(left_tag_colour, .after = tag_numbers_left)
tran$left_tag_number <- str_extract(tran$tag_numbers_left, "\\d+")
tran$left_tag_number <- as.numeric(tran$left_tag_number)
tran <- tran %>%
  relocate(left_tag_number, .after = tag_numbers_left) %>%
  select(-tag_numbers_left)

tran$right_tag_number <- as.numeric(tran$right_tag_number)





#Compare with the strut dataset by tag number from same year
strut_87 <- strut %>%
  filter(year==1987)

matching_tags <- semi_join( strut_87,tran, by = c("left_tag_number" = "left_tag_number",  
                                                  'right_tag_number'='right_tag_number'))

matching_tags_2 <- filter(strut_87, (left_tag_number %in% tran$left_tag_number |
                                      right_tag_number %in% tran$right_tag_number) |
                           left_tag_colour %in% tran$left_tag_colour, 
                          right_tag_colour %in% tran$right_tag_colour, 
                         lek %in% tran$lek_name)
#When I run this, I get 327 matches - so all but three. 

#It does look like there are additional tags in tran that are not in strut
#Which I suppose could be merged with strut, but worth a think 
#Actually, I'm now thinking they are the same
#There are 300 matches just by number, not including matches by colour 


