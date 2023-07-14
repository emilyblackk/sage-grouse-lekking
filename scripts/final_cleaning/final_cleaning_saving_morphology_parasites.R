#Emily Black
#Cleaning up, checking all morphology binder sheets
#Created: 13 July 2023
#Last Modified: 13 July 2023


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


morph_1990 <- read.csv("prelim_clean/1990/binder_capture_data_prelim_N_MS_SS_RS.csv")
morph_1989 <- read.csv("prelim_clean/1989/binder_capture_data_1989_prelim_CL_LL_N_VP_MS_SS.csv")
morph_1988 <- read.csv("prelim_clean/1988/binder_capture_data_1988_prelim_CL_RP_LL_WD_MS_N_SS_V_SF_MCJ.csv")
morph_1987 <- read.csv("prelim_clean/1987/binder_capture_data_1987_prelim_CL_BW_MS_MC_N_SF_WD_SS.csv")
tagsort <- read.csv("prelim_clean/1987/morphology_parasites_1987.csv")

head(morph_1990)
head(morph_1989)
head(morph_1988)
head(morph_1987)

#first thing first - for 1990, remove all columns with mean 
morph_1990 <-morph_1990 %>% select(-matches("mean"))


#Let's start by merging 1987 and 1988, as they should be quite similar
morph_1987 <- morph_1987 %>% mutate_all(as.character)
morph_1987 <- morph_1987 %>%
  rename(binder_pdf_scan_page = binder_pdf_scan_page_) %>%
  rename(hematomas_applied = hematomas_applied_) %>%
  select(-num_lice_back_of_head) %>%
  rename(total_num_lice_on_air_sacs =num_lice_on_air_sac) %>%
  rename(comments = notes) %>%
  select(-num_combs)
morph_1987$blood_smear <- ifelse(is.na(morph_1987$blood_smear), morph_1987$blood_smears, ifelse(is.na(morph_1987$blood_smears), morph_1987$blood_smear, paste(morph_1987$blood_smear, morph_1987$blood_smears, sep = " ")))
morph_1987 <- morph_1987 %>%
  select(-blood_smears)

morph_1988 <- morph_1988 %>%
  rename(time_end = time_search_end_24h) %>%
  rename(time_start = time_search_start_24h) 
morph_1988 <- morph_1988 %>% mutate_all(as.character)
morph_1987_1988 <- bind_rows(morph_1987, morph_1988)

colnames(morph_1987_1988)

#start rearranging some columns to see better 
lice_columns <- grep("lice", names(morph_1987_1988), value = TRUE)
hematoma_columns <- grep("hematoma", names(morph_1987_1988), value = TRUE)
left_tag_columns <- grep("left_tag", names(morph_1987_1988), value = TRUE)


morph_1987_1988 <- morph_1987_1988 %>%
  relocate(weight_bird_and_bag_g, .before = bird_weight_g) %>%
relocate(weight_bag_g, .after = weight_bird_and_bag_g) %>%
  relocate(lice_columns, .after = toe_length_mm) %>%
  relocate(total_num_lice_on_air_sacs, .after = num_lice_on_air_sac_right) %>%
  relocate(hematoma_columns, .after = total_num_lice_on_air_sacs) %>%
  relocate(hematomas_air_sac_comments, .after = hematomas_num_air_sacs) %>%
  relocate(hematomas_comb_comments, .after = hematomas_combs) %>%
  relocate(hematomas_air_sacs, .before = hematomas_num_air_sacs) %>% 
  relocate(air_sac_color, .after = toe_length_mm) %>%
  relocate(comb_color, .after = air_sac_color) %>%
  relocate(head_length_mm, .after = tail_length_mm) %>% 
  relocate(time_start, .after = day) %>%
  relocate(time_end, .after = time_start) %>%
  relocate(left_tag_columns, .after = lek_name) %>%
  relocate(wygf_leg_band, .after = right_tag_code) %>%
  relocate(observer, .after = lek_name) %>%
  relocate(capture_technique, .after = observer) %>%
  relocate(num_tail_feathers, .after = tail_length_mm) %>%
  relocate(comments, .after = stresses_while_handling_describe) %>%
  relocate(hematomas_num_combs, .after = hematomas_combs)
  
  
  #Add in tagsort - malaria, leuko, haema
head(tagsort)
tagsort_select<- tagsort %>%
  select(lek_name, year, month, day, left_tag_color, right_tag_color, 
         fecal_samples_mites, fecal_samples_spores, fecal_samples_coccidia,blood_tryps, 
         blood_plasm, blood_leuco, blood_haema, total_volume_mm, blood_pcv_mm)
tagsort_select <- tagsort_select %>%
  rename(hematocrit_total_1 = total_volume_mm) %>%
  rename(hematocrit_cells_1 = blood_pcv_mm) %>%
  rename(blood_malaria = blood_plasm)
tagsort_select <- tagsort_select %>% mutate_all(as.character)
left_join <- left_join(morph_1987_1988, tagsort_select, by=c("lek_name", "year", "month", 
                                                             "day", "left_tag_color", "right_tag_color"))
blood_columns <-  grep("blood", names(left_join), value = TRUE)
fecal_columns <-  grep("fecal", names(left_join), value = TRUE)

#calculate hematocrit plasm 1
left_join <- left_join %>%
  mutate(hematocrit_plasma_1=as.numeric(hematocrit_total_1)-as.numeric(hematocrit_cells_1))
table(left_join$hematocrit_plasma_1)

left_join <- left_join %>%
  relocate(blood_columns, .after = labelled) %>%
  relocate(fecal_columns, .after = blood_haema) %>%
  relocate(hematocrit_total_1, .before = existing_injuries) %>%
  relocate(hematocrit_plasma_1, .before = hematocrit_total_1) %>%
  relocate(hematocrit_cells_1, .before = hematocrit_plasma_1)
morph_1987_1988 <- left_join

#Add in 1989
colnames(morph_1989)
morph_1989 <- morph_1989 %>%
  rename(time_start = time_search_start_24h) %>%
  rename(time_end = time_search_end_24h) %>%
  rename(slide_number = slide_numer) %>%
  rename(hematomas_air_sac_comments = hematomas_air_sacs_comments)

#Fix some naming issues before the join
morph_1987_1988 <- morph_1987_1988 %>%
  rename(total_num_hematomas_on_combs = hematomas_num_combs) %>%
  rename(weight_bird_g = bird_weight_g) %>%
  rename(total_num_hematomas_on_air_sacs = hematomas_num_air_sacs)

morph_1989 <- morph_1989 %>% mutate_all(as.character)
morph_1987_1988 <- morph_1987_1988 %>% mutate_all(as.character)
morph_1987_1988_1989 <- bind_rows(morph_1987_1988, morph_1989)

morph_1987_1988_1989 <- morph_1987_1988_1989 %>%
  relocate(num_hematomas_on_air_sac_left, .after = hematomas_air_sacs) %>%
  relocate(num_hematomas_on_air_sac_right, .after = num_hematomas_on_air_sac_left) %>%
  relocate(num_hematomas_on_comb_left, .after = hematomas_combs) %>%
  relocate(num_hematomas_on_comb_right, .after = num_hematomas_on_comb_left) %>%
  relocate(hematocrit_cells_2, .after = hematocrit_cells_1) %>% 
  relocate(hematocrit_plasma_1, .after = hematocrit_cells_2) %>%
  relocate(hematocrit_plasma_2, .after = hematocrit_plasma_1) %>%
  relocate(hematocrit_total_2, .after = hematocrit_total_1) %>%
  relocate(samples_collected_cecal, .after = samples_collected_ectoparasites) %>%
  relocate(slide_number, .after = hematocrit_total_2)


#merge 1990 into existing columns
#double check column names
compare_column_names <- function(df1, df2) {
  col_names_df1 <- colnames(df1)
  col_names_df2 <- colnames(df2)
  
  unmatched_cols <- setdiff(col_names_df1, col_names_df2)
  
  return(unmatched_cols)
}
compare_column_names(morph_1987_1988_1989, morph_1990)
compare_column_names(morph_1990, morph_1987_1988_1989)

#rename so columns match 
morph_1990 <- morph_1990 %>%
  rename(time_start = time_search_start_24h) %>%
  rename(time_end = time_search_end_24h) %>%
rename(hematocrit_cells_1 = blood_cells_volume_1) %>% 
  rename(hematocrit_cells_2 = blood_cells_volume_2) %>%
rename(hematocrit_plasma_1 = blood_plasma_volume_1) %>% 
  rename(hematocrit_plasma_2 = blood_plasma_volume_2)%>%
rename(hematocrit_total_1 = blood_total_volume_1) %>% 
  rename(hematocrit_total_2 = blood_total_volume_2)%>%
  rename(box_number = box_num) %>%
  rename(slide_number = slide_num ) %>%
  rename(blood_malaria = blood_samples_malaria) %>%
  rename(blood_leuco = blood_samples_leuco) %>%
  rename(blood_haema = blood_samples_haema)  %>%
  rename(hematomas_air_sacs_comments = hematoma_air_sacs_comments)
morph_1987_1988_1989 <- morph_1987_1988_1989 %>%
  rename(hematomas_air_sacs_comments = hematomas_air_sac_comments)

#compare again to check 
compare_column_names(morph_1987_1988_1989, morph_1990)
compare_column_names(morph_1990, morph_1987_1988_1989)

#bind rows 
morph_1990 <- morph_1990 %>% mutate_all(as.character)
morph_1987_1988_1989_1990 <- bind_rows(morph_1987_1988_1989, morph_1990)
  

morph_1987_1988_1989_1990 <- morph_1987_1988_1989_1990 %>%
  select(-source, -blood_samples_other, -lice_present)

morph_1987_1988_1989_1990 <- morph_1987_1988_1989_1990 %>%
  relocate(dominance, .after = wygf_leg_band) %>%
  relocate(fecal_samples_comments, .after = fecal_samples_coccidia) %>%
  relocate(box_number, .after = slide_number) %>%
  relocate(feathers_lice, .after = samples_collected_ectoparasites) %>%
  relocate(feathers_mites, .after = samples_collected_ectoparasites) %>%
  relocate(lice_ID_ifpresent, .after = total_num_lice_on_air_sacs)
  



#start cleaning up the final dataframe
final_morph <- morph_1987_1988_1989_1990


#go column by column and check everything over

#binder scan pdf page
unique(final_morph$binder_pdf_scan_page)
#looks fine!

#year
table(final_morph$year)
#looks like a few years have gone weird, let's fix that
weird_years <- final_morph %>%
  filter(as.numeric(year)<1000)

#all from windmill dam - see if we can find within morph datasets
weird_years <- morph_1987 %>%
  filter(as.numeric(year)<1000)
weird_years <- morph_1988 %>%
  filter(as.numeric(year)<1000)
#they're in 1988
#so we can set the
final_morph$day <- ifelse(final_morph$year == '6' & final_morph$time_start == "4:40", '6', final_morph$day)
final_morph$month <- ifelse(final_morph$year == '6' & final_morph$time_start == "4:40", '4', final_morph$month)
final_morph$year <- ifelse(final_morph$year == '6' & final_morph$time_start == "4:40", '1988', final_morph$year)

final_morph$day <- ifelse(final_morph$year == '6' & final_morph$time_start == "5:00", '6', final_morph$day)
final_morph$month <- ifelse(final_morph$year == '6' & final_morph$time_start == "5:00", '4', final_morph$month)
final_morph$year <- ifelse(final_morph$year == '6' & final_morph$time_start == "5:00", '1988', final_morph$year)

final_morph$day <- ifelse(final_morph$year == '15' & final_morph$time_start == "3:05", '15', final_morph$day)
final_morph$month <- ifelse(final_morph$year == '15' & final_morph$time_start == "3:05", '4', final_morph$month)
final_morph$year <- ifelse(final_morph$year == '15' & final_morph$time_start == "3:05", '1988', final_morph$year)


final_morph$day <- ifelse(final_morph$year == '15' & final_morph$time_start == "3:05", '15', final_morph$day)
final_morph$month <- ifelse(final_morph$year == '15' & final_morph$time_start == "3:05", '4', final_morph$month)
final_morph$year <- ifelse(final_morph$year == '15' & final_morph$time_start == "3:05", '1988', final_morph$year)


final_morph$day <- ifelse(final_morph$year == '21', '21', final_morph$day)
final_morph$month <- ifelse(final_morph$year == '21', '4', final_morph$month)
final_morph$year <- ifelse(final_morph$year == '21', '1988', final_morph$year)

final_morph$day <- ifelse(final_morph$year == '6', '6', final_morph$day)
final_morph$month <- ifelse(final_morph$year == '6', '4', final_morph$month)
final_morph$year <- ifelse(final_morph$year == '6', '1988', final_morph$year)

table(final_morph$year)

#fixed!


#month
table(final_morph$month)
#good

#day
table(final_morph$day)
#replace some of the erroneous errors
final_morph$day[final_morph$day=="05, 03"] <- "3"
#set as numeric to remove some leading zeroes
final_morph$day <- as.numeric(final_morph$day)
table(final_morph$day)
#done!

#time_start
unique(final_morph$time_start)
#remove tildes
#but otherwise looks good!
final_morph$time_start <- gsub("~", "", final_morph$time_start)
#looks good!

#time_end
unique(final_morph$time_end)
#fix one error
final_morph$time_end <- gsub("`", NA, final_morph$time_end)
unique(final_morph$time_end)
#looks good!

#age
unique(final_morph$age)
#good to go

#sex
unique(final_morph$sex)
#good to go

#lek_name
unique(final_morph$lek_name)
#fix that one mule creek junction
final_morph$lek_name[final_morph$lek_name=="mule_creek_junction"] <- "mule_creek"
unique(final_morph$lek_name)
#fixed

#observer
unique(final_morph$observer)
#remove leading and trailing white space
final_morph$observer <- gsub("^\\s+|\\s+$", "", final_morph$observer)
#better

#capture technique
unique(final_morph$capture_technique)
#correct a spelling mistake
final_morph$capture_technique <- gsub("canon", "cannon", final_morph$capture_technique)
#done


#left tag color
unique(final_morph$left_tag_color)
#looks good!

#left tag number
unique(final_morph$left_tag_number)
#replace "no" with NA
final_morph$left_tag_number[final_morph$left_tag_number=="no"]<- NA
#fix one separated by a comma 
#the first values are correct
final_morph$left_tag_number[final_morph$left_tag_number=="285, 279"] <- "285"
final_morph$right_tag_number[final_morph$right_tag_number=="279, 285"] <- "279"
#looks good!


#left tag code
unique(final_morph$left_tag_code)
#replace "" with NA
final_morph$left_tag_code[final_morph$left_tag_code==""]<- NA
#fix one separated by a comma 
#the first values are correct
final_morph$left_tag_code[final_morph$left_tag_code=="B285, B279"] <- "B285"
final_morph$right_tag_code[final_morph$right_tag_code=="B279, B285"] <- "B279"
#remove subordinate and master cock
final_morph$left_tag_code[final_morph$left_tag_code=="Subordinate"]<- NA
final_morph$left_tag_code[final_morph$left_tag_code=="Master cock"]<- NA
#quick fix from samples check: 1704 should be 1706
final_morph$left_tag_number <- ifelse(final_morph$left_tag_color == 'white red' & final_morph$left_tag_number == "1704", '1706', final_morph$left_tag_number)
final_morph$left_tag_code <- ifelse(final_morph$left_tag_color == 'white red' & final_morph$left_tag_code == "WR1704", 'WR1706', final_morph$left_tag_code)


#looks good!

#right tag color
unique(final_morph$right_tag_color)
#one separated by commas - take the first one
final_morph$right_tag_color[final_morph$right_tag_color=="white, yellow"]<- "white"
#looks good!


#right tag number
unique(final_morph$right_tag_number)
#looks good!


#right tag code
unique(final_morph$right_tag_code)
final_morph$right_tag_code[final_morph$right_tag_code=="W1432, Y1432"]<- "W1432"
#looks good!

#wygf_leg_band
unique(final_morph$wygf_leg_band)
#some commas - first one is correct
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
final_morph$wygf_leg_band <- sapply(final_morph$wygf_leg_band, extract_value_before_comma)
final_morph$wygf_leg_band[final_morph$wygf_leg_band=="No tags"] <- NA
#done

#dominance
unique(final_morph$dominance)
#tolower
final_morph$dominance <- tolower(final_morph$dominance)

#weight_bird_and_bag
table(final_morph$weight_bird_and_bag_g)
#don't see any text values
final_morph$weight_bird_and_bag_g <- as.numeric(final_morph$weight_bird_and_bag_g)
hist(final_morph$weight_bird_and_bag_g)
#yay, all values within good range!
#Change name since this is actually kg
final_morph <- final_morph %>%
  rename(weight_bird_and_bag_kg = weight_bird_and_bag_g)

#weight_bag_g
table(final_morph$weight_bag_g)
#don't see any text values
final_morph$weight_bag_g <- as.numeric(final_morph$weight_bag_g)
hist(final_morph$weight_bag_g)
#yay, all values within good range!
#Change name since this is actually kg
final_morph <- final_morph %>%
  rename(weight_bag_kg = weight_bag_g)


#weight_bird_g
table(final_morph$weight_bird_g)
#don't see any text values
final_morph$weight_bird_g <- as.numeric(final_morph$weight_bird_g)
hist(final_morph$weight_bird_g)
#yay, all values within good range!
#Change name since this is actually kg
final_morph <- final_morph %>%
  rename(weight_bird_kg = weight_bird_g)

#total_length_mm
table(final_morph$total_length_mm)
#don't see any text values
final_morph$total_length_mm <- as.numeric(final_morph$total_length_mm)
hist(final_morph$total_length_mm)
#yay, all values within good range!
#the 220 down there is a very small female, 
#still a reasonable weight


#tail_length_mm
table(final_morph$tail_length_mm)
#replace no tail with 0
final_morph$tail_length_mm[final_morph$tail_length_mm=="no tail"] <- 0
#don't see any text values
final_morph$tail_length_mm <- as.numeric(final_morph$tail_length_mm)
hist(final_morph$tail_length_mm)
#yay, all values within good range!

#num_tail_feathers
table(final_morph$num_tail_feathers)
final_morph$num_tail_feathers[final_morph$num_tail_feathers=="18 (gap?)"] <- 18
#don't see any text values
final_morph$num_tail_feathers <- as.numeric(final_morph$num_tail_feathers)
hist(final_morph$num_tail_feathers)
#yay, all values within good range!

#head_length_mm
table(final_morph$head_length_mm)
#don't see any text values
final_morph$head_length_mm <- as.numeric(final_morph$head_length_mm)
hist(final_morph$head_length_mm)
#yay, all values within good range!


#wing_length_mm
table(final_morph$wing_length_mm)
#don't see any text values
final_morph$wing_length_mm <- as.numeric(final_morph$wing_length_mm)
hist(final_morph$wing_length_mm)
#there are a few values out of range, looks like they're in cm
#it's ok, easy fix! 
final_morph$wing_length_mm<- ifelse(final_morph$wing_length_mm < 100, final_morph$wing_length_mm * 10,final_morph$wing_length_mm)
#yay, all values within good range now!


#bill_length_mm
table(final_morph$bill_length_mm)
#don't see any text values
final_morph$bill_length_mm <- as.numeric(final_morph$bill_length_mm)
hist(final_morph$bill_length_mm)
#whoa, there's one in the 400s! That should be divided by 10
final_morph$bill_length_mm<- ifelse(final_morph$bill_length_mm >200, final_morph$bill_length_mm/ 10,final_morph$bill_length_mm)
#yay, all values within good range now!



#bill_depth_mm
table(final_morph$bill_depth_mm)
#don't see any text values
final_morph$bill_depth_mm <- as.numeric(final_morph$bill_depth_mm)
hist(final_morph$bill_depth_mm)
#whoa, there's one in the 200s! That should be divided by 10
final_morph$bill_depth_mm<- ifelse(final_morph$bill_depth_mm >100, final_morph$bill_depth_mm/ 10,final_morph$bill_depth_mm)
#yay, all values within good range now!


#keel_length_mm
table(final_morph$keel_length_mm)
#don't see any text values
final_morph$keel_length_mm <- as.numeric(final_morph$keel_length_mm)
hist(final_morph$keel_length_mm)
#yay, all values within good range!


#comb_length_mm
table(final_morph$comb_length_mm)
#don't see any text values
final_morph$comb_length_mm <- as.numeric(final_morph$comb_length_mm)
hist(final_morph$comb_length_mm)
#whoa, there's one in the 200s! That should be divided by 10
final_morph$comb_length_mm<- ifelse(final_morph$comb_length_mm >100, final_morph$comb_length_mm/ 10,final_morph$comb_length_mm)
#yay, all values within good range now!

#tarsus_length_mm
table(final_morph$tarsus_length_mm)
#don't see any text values
final_morph$tarsus_length_mm <- as.numeric(final_morph$tarsus_length_mm)
#whoa, there's one in the 200s! That should be divided by 10
final_morph$tarsus_length_mm<- ifelse(final_morph$tarsus_length_mm >300, final_morph$tarsus_length_mm/ 10,final_morph$tarsus_length_mm)
hist(final_morph$tarsus_length_mm)
#yay, all values within good range!



#toe_length_mm
table(final_morph$toe_length_mm)
#don't see any text values
final_morph$toe_length_mm <- as.numeric(final_morph$toe_length_mm)
#there are some values that should probably be adjusted - that 6 should be 60
final_morph$toe_length_mm<- ifelse(final_morph$toe_length_mm<10, final_morph$toe_length_mm*10,final_morph$toe_length_mm)
hist(final_morph$toe_length_mm)
#yay, all values within good range!


#air sac color
unique(final_morph$air_sac_color)
#remove leading and trailing white space
final_morph$air_sac_color <- gsub("^\\s+|\\s+$", "", final_morph$air_sac_color)


#comb color
unique(final_morph$comb_color)
#remove leading and trailing white space
final_morph$comb_color <- gsub("^\\s+|\\s+$", "", final_morph$comb_color)

#lice back of head
table(final_morph$lice_back_of_head)
#replace "can't tell" and "too late" with NA
final_morph$lice_back_of_head[final_morph$lice_back_of_head=="Can't tell"] <- NA
final_morph$lice_back_of_head[final_morph$lice_back_of_head=="Too late"] <- NA


#num_lice_on_comb
table(final_morph$num_lice_on_comb)
#replace some erroneous values
final_morph$num_lice_on_comb[final_morph$num_lice_on_comb=="Too late"] <- NA
final_morph$num_lice_on_comb[final_morph$num_lice_on_comb=="no"] <- 0
final_morph$num_lice_on_comb[final_morph$num_lice_on_comb=="No"] <- 0
final_morph$num_lice_on_comb[final_morph$num_lice_on_comb=="0L, 0R"] <- 0
#done


#num lice on air sac left
table(final_morph$num_lice_on_air_sac_left)
final_morph$num_lice_on_air_sac_left <- as.numeric(final_morph$num_lice_on_air_sac_left)
#done

#num lice on air sac right
table(final_morph$num_lice_on_air_sac_right)
final_morph$num_lice_on_air_sac_right <- as.numeric(final_morph$num_lice_on_air_sac_right)
#done

#total num lice on air sacs
table(final_morph$total_num_lice_on_air_sacs)
final_morph$total_num_lice_on_air_sacs[final_morph$total_num_lice_on_air_sacs=="Yes"] <- NA
final_morph$total_num_lice_on_air_sacs <- as.numeric(final_morph$total_num_lice_on_air_sacs)

#lice ID if present
unique(final_morph$lice_ID_ifpresent)
#replaces dashes with NA
final_morph$lice_ID_ifpresent[final_morph$lice_ID_ifpresent=="-"] <- NA
final_morph$lice_ID_ifpresent[final_morph$lice_ID_ifpresent=="- "] <- NA
final_morph$lice_ID_ifpresent[final_morph$lice_ID_ifpresent=="? "] <- NA


#do total number of hematomas first
table(final_morph$total_num_hematomas_on_combs)
final_morph$total_num_hematomas_on_combs <- as.numeric(final_morph$total_num_hematomas_on_combs)

#hematomas_combs
#need to update this with values from total_hematomas combs
table(final_morph$hematomas_combs)
#replace ++
final_morph$hematomas_combs[final_morph$hematomas_combs=="++"] <- "yes"
final_morph$hematomas_combs <- ifelse(is.na(final_morph$hematomas_combs) & final_morph$total_num_hematomas_on_combs > 0, "yes", final_morph$hematomas_combs)
final_morph$hematomas_combs <- ifelse(is.na(final_morph$hematomas_combs) & final_morph$total_num_hematomas_on_combs == 0, "no", final_morph$hematomas_combs)

#hematomas_combs_left
table(final_morph$num_hematomas_on_comb_left)
#looking good
final_morph$num_hematomas_on_comb_left <- as.numeric(final_morph$num_hematomas_on_comb_left)

#hematomas_combs_right
table(final_morph$num_hematomas_on_comb_right)
#looking good
final_morph$num_hematomas_on_comb_right <- as.numeric(final_morph$num_hematomas_on_comb_right)

#hematoma combs commebts
unique(final_morph$hematomas_comb_comments)
final_morph$hematomas_comb_comments <- gsub("^\\s+|\\s+$", "", final_morph$hematomas_comb_comments)
#looks good!



#do total number of hematomas first
table(final_morph$total_num_hematomas_on_air_sacs)
final_morph$total_num_hematomas_on_air_sacs <- as.numeric(final_morph$total_num_hematomas_on_air_sacs)

#hematomas_air_sac
#need to update this with values from total_hematomas air_sac
table(final_morph$hematomas_air_sacs)
#replace ++
final_morph$hematomas_air_sacs[final_morph$hematomas_air_sacs=="++"] <- "yes"
final_morph$hematomas_air_sacs <- ifelse(is.na(final_morph$hematomas_air_sacs) & final_morph$total_num_hematomas_on_air_sacs > 0, "yes", final_morph$hematomas_air_sacs)
final_morph$hematomas_air_sacs <- ifelse(is.na(final_morph$hematomas_air_sacs) & final_morph$total_num_hematomas_on_air_sacs == 0, "no", final_morph$hematomas_air_sacs)

#hematomas_air_sac_left
table(final_morph$num_hematomas_on_air_sac_left)
#looking good
final_morph$num_hematomas_on_air_sac_left <- as.numeric(final_morph$num_hematomas_on_air_sac_left)

#hematomas_air_sac_right
table(final_morph$num_hematomas_on_air_sac_right)
#looking good
final_morph$num_hematomas_on_air_sac_right <- as.numeric(final_morph$num_hematomas_on_air_sac_right)


#hematoma air sacs comments
unique(final_morph$hematomas_air_sacs_comments)
#remove leading and trailing white space 
final_morph$hematomas_air_sacs_comments <- gsub("^\\s+|\\s+$", "", final_morph$hematomas_air_sacs_comments)
#looks good


#hematoma group
unique(final_morph$hematoma_group)
table(final_morph$hematoma_group)
#looks good

#hematoma applied
unique(final_morph$hematomas_applied)
table(final_morph$hematomas_applied)
#looks good

#cut_feathers
table(final_morph$cut_feathers)

#collect
table(final_morph$collect)
#looks good

#labelled
table(final_morph$labelled)
#looks good

#samples_collected_blood
table(final_morph$samples_collected_blood)
#looks good

#blood smear
table(final_morph$blood_smear)
final_morph$blood_smear[final_morph$blood_smear=="FALSE"] <- "no"
#looks good

#blood tryps
table(final_morph$blood_tryps)
#looks good

#blood malaria
table(final_morph$blood_malaria)
#looks good


#blood malaria
table(final_morph$blood_malaria)
#looks good

#blood leuco
table(final_morph$blood_leuco)
#looks good

#blood haema
table(final_morph$blood_haema)
#look good

#samples collected fecal 
table(final_morph$samples_collected_fecal)
#looks

#fecal samples mites
table(final_morph$fecal_samples_mites)

#fecal sample spores
table(final_morph$fecal_samples_spores)

#fecal sample coccidia
table(final_morph$fecal_samples_coccidia)

#fecal sample comments
unique(final_morph$fecal_samples_comments)
#correct a typo
final_morph$fecal_samples_comments <- gsub("coocialia", "coccidia", final_morph$fecal_samples_comments)
#looks good

#samples ectoparasites
table(final_morph$samples_collected_ectoparasites)
#good
#feathers
table(final_morph$feathers_lice)
#nothing in either feather column - remove
final_morph <- final_morph %>%
  select(-feathers_mites, -feathers_lice)

#samples collected cecal
table(final_morph$samples_collected_cecal)
#good

#hematocrit cells 1
table(final_morph$hematocrit_cells_1)
#looks good, make numeric
final_morph$hematocrit_cells_1 <- as.numeric(final_morph$hematocrit_cells_1)



#hematocrit cells 2
table(final_morph$hematocrit_cells_2)
#looks good, make numeric
final_morph$hematocrit_cells_2 <- as.numeric(final_morph$hematocrit_cells_2)



#hematocrit plasma 1
table(final_morph$hematocrit_plasma_1)
#looks good, make numeric
final_morph$hematocrit_plasma_1 <- as.numeric(final_morph$hematocrit_plasma_1)



#hematocrit plasma 2
table(final_morph$hematocrit_plasma_2)
#looks good, make numeric
final_morph$hematocrit_plasma_2 <- as.numeric(final_morph$hematocrit_plasma_2)


#hematocrit total 1
table(final_morph$hematocrit_total_1)
#looks good, make numeric
final_morph$hematocrit_total_1 <- as.numeric(final_morph$hematocrit_total_1)



#hematocrit total 2
table(final_morph$hematocrit_total_2)
#correct error in NA
final_morph$hematocrit_total_2[final_morph$hematocrit_total_2=="nA"] <- NA
#looks good, make numeric
final_morph$hematocrit_total_2 <- as.numeric(final_morph$hematocrit_total_2)

#slide number
unique(final_morph$slide_number)
#looks good

#box number
unique(final_morph$box_number)
#looks good

#existing injuries
unique(final_morph$existing_injuries)
#remove leading trailing white space just in case
final_morph$existing_injuries <- gsub("^\\s+|\\s+$", "", final_morph$existing_injuries)


#handling injury
unique(final_morph$handling_injury)
#remove leading trailing white space just in case
final_morph$handling_injury <- gsub("^\\s+|\\s+$", "", final_morph$handling_injury)


#stresses while handling
unique(final_morph$stresses_while_handling_describe)
#replace No with NA
final_morph$stresses_while_handling_describe[final_morph$stresses_while_handling_describe=="No"] <- NA
final_morph$stresses_while_handling_describe[final_morph$stresses_while_handling_describe=="no"] <- NA
#remove leading trailing white space just in case
final_morph$stresses_while_handling_describe <- gsub("^\\s+|\\s+$", "", final_morph$stresses_while_handling_describe)


#comments
unique(final_morph$comments)
#probably best just to remove leading and trailing white space and be done!
final_morph$comments <- gsub("^\\s+|\\s+$", "", final_morph$comments)


#write to file!
write.csv(final_morph, "clean_data/morphology_parasites_1987_1988_1989_1990.csv", 
          row.names=FALSE)

#test 10 random rows to check sorting
set.seed(4477)
samples <- sample_n(final_morph, 10)