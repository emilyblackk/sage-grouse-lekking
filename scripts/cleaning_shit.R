#Emily Black
#Cleaning SHIT from floppy disk
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

shit <- read.csv("prelim_clean/SHIT.csv", 
                    header=FALSE)
head(shit)
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

shit <- fix_column_names(shit)



#We now need to fill blank spaces with NAs
#And remove redundant columns, or replace them
shit <- shit %>% mutate_all(~ifelse(grepl("^\\s*$", .x), NA, .x))
shit <- shit[rowSums(is.na(shit)) != ncol(shit), ]



#We now need to do a LOT of case_when to standardize lek names 

shit <- shit %>%
  mutate(lek = case_when(
    lek=="A" ~ 'unknown', 
    lek=="BW" ~ "broken_wing", 
    lek=="C" ~ 'cordingly', 
    lek=="LL" ~ 'long_lake', 
    lek=="WD" ~ 'windmill_dam', 
    lek=="SF" ~ 'south_fetterman', 
    lek=="MCJ" ~'mule_creek_junction', 
    lek=="NL" ~ 'north_lek', 
    lek=="RP" ~ 'red_post', 
    lek=="SS" ~ 'sybille_springs', 
    lek=="VP" ~ 'viewpoint', 
    lek=="MS" ~ 'mud_springs'
  )) %>%
  rename(lek_name=lek)

shit <- shit %>%
  mutate(sex = case_when(
    sex=="JF" ~ 'juven_female', 
    sex=="JM" ~ 'juven_male', 
    sex=="F" ~ 'adult_female', 
    sex=="M" ~ 'adult_male'
  ))
#Split into two columns, age and sex
shit <- separate(shit, col = sex, into = c("age", "sex"), sep = "_", remove = FALSE)

#looks like the years need a 19 added to them
#let's fix that
shit$year <- ifelse(is.na(shit$year), NA, paste0("19", shit$year))

#make the wing column lowercase
shit$wing <- tolower(shit$wing)

#Make the status column lowercase, and spaces underscores 
shit$status <- tolower(shit$status)
shit$status <- gsub(" ", "_", shit$status)


#There is a stray question mark in allele_pmi
#Should replace with NA

shit$allele_pmi <- ifelse(grepl("\\?", shit$allele_pmi), NA, shit$allele_pmi)
shit$tag <- ifelse(grepl("\\*", shit$tag), NA, shit$tag)


write.csv(shit, 'prelim_clean/blood_enzyme_alleles.csv', 
          row.names=FALSE)



#Can we add this one to the strut dataset? 

strut <-  read.csv("prelim_clean/strut_merged_df_prelim.csv")

shit_noNA <- shit %>%
  filter(!is.na(tag), 
         !is.na(wing))

#Pivot longer so we can get left and right wing numbers
rearranged_df <- pivot_wider(
  data = shit_noNA,
  names_from = wing,
  values_from = tag
) 

