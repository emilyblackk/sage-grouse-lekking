#Emily Black
#Merging strut and tagsort - to see if compatible
#Created: 15 June 2023
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

tagsort <- read.csv('prelim_clean/morphology_parasites_1987.csv')
strut <-  read.csv("prelim_clean/strut_merged_df_prelim.csv")

head(tagsort)
head(strut)


#rename tagsort columns to match strut
tagsort <- tagsort %>%
  rename(left_tag_colour = left_tag_color)
tagsort <- tagsort %>%
  rename(right_tag_colour = right_tag_color)

#Looks like this calls for a full join...
full_join <- full_join(strut, tagsort, by=c('left_tag_colour', 'right_tag_colour', 'lek_name', 
                                            'year', 'month'), 
                       relationship = 'many-to-many', suffix = c('_strutdf', "_tagsortdf"))


#Rename the day columns 
full_join <- full_join %>%
  rename(day_observed = day_strutdf) %>%
  relocate(day_observed, .after = month) %>%
  rename(day_captured = day_tagsortdf) %>%
    relocate(day_captured, .after = day_observed)


# Get column names with "_strut" and "_tagsort" suffixes
strut_cols <- grep("_strutdf$", names(full_join), value = TRUE)
tagsort_cols <- grep("_tagsortdf$", names(full_join), value = TRUE)

full_join <- full_join %>%
  mutate(age_merged = ifelse(is.na(age_strutdf), age_tagsortdf,
                             ifelse(is.na(age_tagsortdf), age_strutdf,
                                    ifelse(age_strutdf == age_tagsortdf, age_strutdf,
                                           paste(age_strutdf, age_tagsortdf, sep = "-")))))
full_join <- full_join %>%
  relocate(age_merged, .after = lice)

full_join <- full_join %>%
  mutate(left_tag_code_merged = ifelse(is.na(left_tag_code_strutdf), left_tag_code_tagsortdf,
                             ifelse(is.na(left_tag_code_tagsortdf), left_tag_code_strutdf,
                                    ifelse(left_tag_code_strutdf == left_tag_code_tagsortdf, left_tag_code_strutdf,
                                           paste(left_tag_code_strutdf, left_tag_code_tagsortdf, sep = "-")))))
full_join <- full_join %>%
  relocate(left_tag_code_merged, .before = left_tag_colour)

full_join <- full_join %>%
  mutate(right_tag_code_merged = ifelse(is.na(right_tag_code_strutdf), right_tag_code_tagsortdf,
                                       ifelse(is.na(right_tag_code_tagsortdf), right_tag_code_strutdf,
                                              ifelse(right_tag_code_strutdf == right_tag_code_tagsortdf, right_tag_code_strutdf,
                                                     paste(right_tag_code_strutdf, right_tag_code_tagsortdf, sep = "-")))))
full_join <- full_join %>%
  relocate(right_tag_code_merged, .before = right_tag_colour)

# full_join <- full_join %>%
#   mutate(day_merged = ifelse(is.na(day_strutdf), day_tagsortdf,
#                                         ifelse(is.na(day_tagsortdf), day_strutdf,
#                                                ifelse(day_strutdf == day_tagsortdf, day_strutdf,
#                                                       paste(day_strutdf, day_tagsortdf, sep = "-")))))
# full_join <- full_join %>%
#   relocate(day_merged, .after= month)

full_join <- full_join %>%
  mutate(breed_merged = ifelse(is.na(breed_strutdf), breed_tagsortdf,
                             ifelse(is.na(breed_tagsortdf), breed_strutdf,
                                    ifelse(breed_strutdf == breed_tagsortdf, breed_strutdf,
                                           paste(breed_strutdf, breed_tagsortdf, sep = "-")))))

full_join <- full_join %>%
  relocate(breed_merged, .after= lice)

full_join <- full_join %>%
  mutate(left_tag_number_merged = ifelse(is.na(left_tag_number_strutdf), left_tag_number_tagsortdf,
                               ifelse(is.na(left_tag_number_tagsortdf), left_tag_number_strutdf,
                                      ifelse(left_tag_number_strutdf == left_tag_number_tagsortdf, left_tag_number_strutdf,
                                             paste(left_tag_number_strutdf, left_tag_number_tagsortdf, sep = "-")))))
full_join <- full_join %>%
  relocate(left_tag_number_merged, .after= left_tag_colour)

full_join <- full_join %>%
  mutate(right_tag_number_merged = ifelse(is.na(right_tag_number_strutdf), right_tag_number_tagsortdf,
                                         ifelse(is.na(right_tag_number_tagsortdf), right_tag_number_strutdf,
                                                ifelse(right_tag_number_strutdf == right_tag_number_tagsortdf, right_tag_number_strutdf,
                                                       paste(right_tag_number_strutdf, right_tag_number_tagsortdf, sep = "-")))))
full_join <- full_join %>%
  relocate(right_tag_number_merged, .after= right_tag_colour)




full_join <- full_join %>%
  select(-ends_with("_strutdf"), -ends_with("_tagsortdf"))



full_join %>%
  ggplot(aes(x=as.character(breed_merged), y=as.numeric(tail_length_mm))) + 
  geom_point() + 
  labs(x="Breed", y="Tail length (mm)") + 
  theme_classic()
