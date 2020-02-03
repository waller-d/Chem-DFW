##########################
################  waller-d
#
# Data cleaning for chemistry course
#        DFW rates, 2008-2019
#
# Contributors: David Waller
# Project: Chem-DFW
#
################
##########################

library(tidyverse)
library(readxl)
rm(list = ls()) # clear workspace

# import data for gender overall
df_gender_all <- read_excel("DFW For UG CHM Courses_Fa08_to_Sum19.xlsx", sheet = 2, col_names = T, skip = 1)
colnames(df_gender_all) <- c("Course","n_grades_female","n_DFW_female", "%_DFW_female","n_grades_male","n_DFW_male","%_DFW_male")

# import data for first-generation overall
df_fgen_all <- read_excel("DFW For UG CHM Courses_Fa08_to_Sum19.xlsx", sheet = 3, col_names = T, skip = 1)
colnames(df_fgen_all) <- c("Course","n_grades_non-fgen","n_DFW_non-fgen", "%_DFW_non-fgen","n_grades_fgen","n_DFW_fgen","%_DFW_fgen")

# import data for ethnicity overall
df_ethnicity_all <- read_excel("DFW For UG CHM Courses_Fa08_to_Sum19.xlsx", sheet = 4, col_names = T, skip = 1)
colnames(df_ethnicity_all) <- c("Course","n_grades_2+","n_DFW_2+", "%_DFW_2+","n_grades_AI-AN","n_DFW_AI-AN","%_DFW_AI-AN",
                         "n_grades_Asian","n_DFW_Asian", "%_DFW_Asian","n_grades_B-AA","n_DFW_B-AA","%_DFW_B-AA",
                         "n_grades_H-L","n_DFW_H-L","%_DFW_H-L","n_grades_Int","n_DFW_Int","%_DFW_Int","n_grades_NH-PI",
                         "n_DFW_NH-PI","%_DFW_NH-PI","n_grades_Unknown","n_DFW_Unknown","%_DFW_Unknown","n_grades_White",
                         "n_DFW_White","%_DFW_White")

# create vectors with proper names for labeling graphs and charts
gender_groups <- c("Female", "Male")
fgen_groups <- c("Non-First Generation","First Generation")
ethnicity_groups <- c("2 or more races","American Indian or Alaska Native","Asian","Black or African American","Hispanic/Latino",
                      "International","Native Hawaiian or Other Pacific Islander","Unknown","White")

# create a column for gender as a factor and simplify measure descriptions
df_gender_all <- gather(df_gender_all, measure, value, n_grades_female:"%_DFW_male")
df_gender_all$gender <- NA
df_gender_all[grepl("male", df_gender_all$measure),4] <- "Male"
df_gender_all[grepl("female", df_gender_all$measure),4] <- "Female"
df_gender_all$gender <- as.factor(df_gender_all$gender)
df_gender_all$measure[df_gender_all$measure=="n_grades_male" | df_gender_all$measure=="n_grades_female"] <- "n_grades"
df_gender_all$measure[df_gender_all$measure=="n_DFW_male" | df_gender_all$measure=="n_DFW_female"] <- "n_DFW"
df_gender_all$measure[df_gender_all$measure=="%_DFW_male" | df_gender_all$measure=="%_DFW_female"] <- "%_DFW"

# create a column for first generation as a factor and simplify measure descriptions
df_fgen_all <- gather(df_fgen_all, measure, value, "n_grades_non-fgen":"%_DFW_fgen")
df_fgen_all$fgen <- NA
df_fgen_all[grepl("fgen", df_fgen_all$measure),4] <- "First Generation"
df_fgen_all[grepl("non-fgen", df_fgen_all$measure),4] <- "Non-First Generation"
df_fgen_all$fgen <- as.factor(df_fgen_all$fgen)
df_fgen_all$measure[df_fgen_all$measure=="n_grades_non-fgen" | df_fgen_all$measure=="n_grades_fgen"] <- "n_grades"
df_fgen_all$measure[df_fgen_all$measure=="n_DFW_non-fgen" | df_fgen_all$measure=="n_DFW_fgen"] <- "n_DFW"
df_fgen_all$measure[df_fgen_all$measure=="%_DFW_non-fgen" | df_fgen_all$measure=="%_DFW_fgen"] <- "%_DFW"

# create a column for ethnicity as a factor and simplify measure descriptions
df_ethnicity_all <- gather(df_ethnicity_all, measure, value, "n_grades_2+":"%_DFW_White")
df_ethnicity_all$ethnicity <- NA
df_ethnicity_all[grepl("2+", df_ethnicity_all$measure),4] <- ethnicity_groups[1]
df_ethnicity_all[grepl("AI-AN", df_ethnicity_all$measure),4] <- ethnicity_groups[2]
df_ethnicity_all[grepl("Asian", df_ethnicity_all$measure),4] <- ethnicity_groups[3]
df_ethnicity_all[grepl("B-AA", df_ethnicity_all$measure),4] <- ethnicity_groups[4]
df_ethnicity_all[grepl("H-L", df_ethnicity_all$measure),4] <- ethnicity_groups[5]
df_ethnicity_all[grepl("Int", df_ethnicity_all$measure),4] <- ethnicity_groups[6]
df_ethnicity_all[grepl("NH-PI", df_ethnicity_all$measure),4] <- ethnicity_groups[7]
df_ethnicity_all[grepl("Unknown", df_ethnicity_all$measure),4] <- ethnicity_groups[8]
df_ethnicity_all[grepl("White", df_ethnicity_all$measure),4] <- ethnicity_groups[9]
df_ethnicity_all$ethnicity <- as.factor(df_ethnicity_all$ethnicity)
df_ethnicity_all$measure[df_ethnicity_all$measure=="n_grades_2+" | df_ethnicity_all$measure=="n_grades_AI-AN"
                         | df_ethnicity_all$measure=="n_grades_Asian" | df_ethnicity_all$measure=="n_grades_B-AA" 
                         | df_ethnicity_all$measure=="n_grades_H-L" | df_ethnicity_all$measure=="n_grades_Int" 
                         | df_ethnicity_all$measure=="n_grades_NH-PI" | df_ethnicity_all$measure=="n_grades_Unknown" 
                         | df_ethnicity_all$measure=="n_grades_White"] <- "n_grades"
df_ethnicity_all$measure[df_ethnicity_all$measure=="n_DFW_2+" | df_ethnicity_all$measure=="n_DFW_AI-AN"
                         | df_ethnicity_all$measure=="n_DFW_Asian" | df_ethnicity_all$measure=="n_DFW_B-AA" 
                         | df_ethnicity_all$measure=="n_DFW_H-L" | df_ethnicity_all$measure=="n_DFW_Int" 
                         | df_ethnicity_all$measure=="n_DFW_NH-PI" | df_ethnicity_all$measure=="n_DFW_Unknown" 
                         | df_ethnicity_all$measure=="n_DFW_White"] <- "n_DFW"
df_ethnicity_all$measure[df_ethnicity_all$measure=="%_DFW_2+" | df_ethnicity_all$measure=="%_DFW_AI-AN"
                         | df_ethnicity_all$measure=="%_DFW_Asian" | df_ethnicity_all$measure=="%_DFW_B-AA" 
                         | df_ethnicity_all$measure=="%_DFW_H-L" | df_ethnicity_all$measure=="%_DFW_Int" 
                         | df_ethnicity_all$measure=="%_DFW_NH-PI" | df_ethnicity_all$measure=="%_DFW_Unknown" 
                         | df_ethnicity_all$measure=="%_DFW_White"] <- "%_DFW"

# import semester data and transform into long form data with gender as a factor
df_gender_semester <- read_excel("DFW For UG CHM Courses_Fa08_to_Sum19.xlsx", sheet = 5, col_names = T, skip = 2)
colnames(df_gender_semester) <- c("Course","Semester_Fall","n_grades_female_Fall","n_DFW_female_Fall", "%_DFW_female_Fall","n_grades_male_Fall","n_DFW_male_Fall","%_DFW_male_Fall",
                                  "Semester_Spring","n_grades_female_Spring","n_DFW_female_Spring", "%_DFW_female_Spring","n_grades_male_Spring","n_DFW_male_Spring","%_DFW_male_Spring",
                                  "Semester_Summer","n_grades_female_Summer","n_DFW_female_Summer", "%_DFW_female_Summer","n_grades_male_Summer","n_DFW_male_Summer","%_DFW_male_Summer")
df_gender_fall_f <- select(df_gender_semester, Course:"%_DFW_female_Fall")
df_gender_fall_f$gender <- "Female"
colnames(df_gender_fall_f) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","gender")
df_gender_spring_f <- select(df_gender_semester, Course, Semester_Spring:"%_DFW_female_Spring")
df_gender_spring_f$gender <- "Female"
colnames(df_gender_spring_f) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","gender")
df_gender_summer_f <- select(df_gender_semester, Course, Semester_Summer:"%_DFW_female_Summer")
df_gender_summer_f$gender <- "Female"
colnames(df_gender_summer_f) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","gender")
df_gender_fall_m <- select(df_gender_semester, Course, Semester_Fall, n_grades_male_Fall:"%_DFW_male_Fall")
df_gender_fall_m$gender <- "Male"
colnames(df_gender_fall_m) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","gender")
df_gender_spring_m <- select(df_gender_semester, Course, Semester_Spring, n_grades_male_Spring:"%_DFW_male_Spring")
df_gender_spring_m$gender <- "Male"
colnames(df_gender_spring_m) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","gender")
df_gender_summer_m <- select(df_gender_semester, Course, Semester_Summer, n_grades_male_Summer:"%_DFW_male_Summer")
df_gender_summer_m$gender <- "Male"
colnames(df_gender_summer_m) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","gender")
df_gender_semester <- rbind(df_gender_fall_f, df_gender_fall_m, df_gender_spring_f, df_gender_spring_m, df_gender_summer_f, df_gender_summer_m)
df_gender_semester$gender <- as.factor(df_gender_semester$gender)
df_gender_semester <- gather(df_gender_semester, measure, value, n_grades:"%_DFW")

# import semester data and transform into long form data with first generation as a factor
df_fgen_semester <- read_excel("DFW For UG CHM Courses_Fa08_to_Sum19.xlsx", sheet = 6, col_names = T, skip = 2)
colnames(df_fgen_semester) <- c("Course","Semester_Fall","n_grades_non-fgen_Fall","n_DFW_non-fgen_Fall", "%_DFW_non-fgen_Fall","n_grades_fgen_Fall","n_DFW_fgen_Fall","%_DFW_fgen_Fall",
                                  "Semester_Spring","n_grades_non-fgen_Spring","n_DFW_non-fgen_Spring", "%_DFW_non-fgen_Spring","n_grades_fgen_Spring","n_DFW_fgen_Spring","%_DFW_fgen_Spring",
                                  "Semester_Summer","n_grades_non-fgen_Summer","n_DFW_non-fgen_Summer", "%_DFW_non-fgen_Summer","n_grades_fgen_Summer","n_DFW_fgen_Summer","%_DFW_fgen_Summer")
df_fgen_fall_f <- select(df_fgen_semester, Course:"%_DFW_non-fgen_Fall")
df_fgen_fall_f$fgen <- "Non-First Generation"
colnames(df_fgen_fall_f) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","fgen")
df_fgen_spring_f <- select(df_fgen_semester, Course, Semester_Spring:"%_DFW_non-fgen_Spring")
df_fgen_spring_f$fgen <- "Non-First Generation"
colnames(df_fgen_spring_f) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","fgen")
df_fgen_summer_f <- select(df_fgen_semester, Course, Semester_Summer:"%_DFW_non-fgen_Summer")
df_fgen_summer_f$fgen <- "Non-First Generation"
colnames(df_fgen_summer_f) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","fgen")
df_fgen_fall_m <- select(df_fgen_semester, Course, Semester_Fall, n_grades_fgen_Fall:"%_DFW_fgen_Fall")
df_fgen_fall_m$fgen <- "First Generation"
colnames(df_fgen_fall_m) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","fgen")
df_fgen_spring_m <- select(df_fgen_semester, Course, Semester_Spring, n_grades_fgen_Spring:"%_DFW_fgen_Spring")
df_fgen_spring_m$fgen <- "First Generation"
colnames(df_fgen_spring_m) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","fgen")
df_fgen_summer_m <- select(df_fgen_semester, Course, Semester_Summer, n_grades_fgen_Summer:"%_DFW_fgen_Summer")
df_fgen_summer_m$fgen <- "First Generation"
colnames(df_fgen_summer_m) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","fgen")
df_fgen_semester <- rbind(df_fgen_fall_f, df_fgen_fall_m, df_fgen_spring_f, df_fgen_spring_m, df_fgen_summer_f, df_fgen_summer_m)
df_fgen_semester$fgen <- as.factor(df_fgen_semester$fgen)
df_fgen_semester <- gather(df_fgen_semester, measure, value, n_grades:"%_DFW")

# import semester data and transform into long form data with ethnicity as a factor
df_ethnicity_semester_fall <- read_excel("DFW For UG CHM Courses_Fa08_to_Sum19.xlsx", sheet = 7, col_names = T, skip = 1)
colnames(df_ethnicity_semester_fall) <- c("Course","Semester","n_grades_2+","n_DFW_2+", "%_DFW_2+","n_grades_AI-AN","n_DFW_AI-AN","%_DFW_AI-AN",
                                "n_grades_Asian","n_DFW_Asian", "%_DFW_Asian","n_grades_B-AA","n_DFW_B-AA","%_DFW_B-AA",
                                "n_grades_H-L","n_DFW_H-L","%_DFW_H-L","n_grades_Int","n_DFW_Int","%_DFW_Int","n_grades_NH-PI",
                                "n_DFW_NH-PI","%_DFW_NH-PI","n_grades_Unknown","n_DFW_Unknown","%_DFW_Unknown","n_grades_White",
                                "n_DFW_White","%_DFW_White")
df_ethnicity_fall_2 <- select(df_ethnicity_semester_fall, Course, Semester, "n_grades_2+":"%_DFW_2+")
df_ethnicity_fall_2$ethnicity <- "2 or more races"
colnames(df_ethnicity_fall_2) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_fall_AIAN <- select(df_ethnicity_semester_fall, Course, Semester, "n_grades_AI-AN":"%_DFW_AI-AN")
df_ethnicity_fall_AIAN$ethnicity <- "American Indian or Alaska Native"
colnames(df_ethnicity_fall_AIAN) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_fall_Asian <- select(df_ethnicity_semester_fall, Course, Semester, "n_grades_Asian":"%_DFW_Asian")
df_ethnicity_fall_Asian$ethnicity <- "Asian"
colnames(df_ethnicity_fall_Asian) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_fall_BAA <- select(df_ethnicity_semester_fall, Course, Semester, "n_grades_B-AA":"%_DFW_B-AA")
df_ethnicity_fall_BAA$ethnicity <- "Black or African American"
colnames(df_ethnicity_fall_BAA) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_fall_HL <- select(df_ethnicity_semester_fall, Course, Semester, "n_grades_H-L":"%_DFW_H-L")
df_ethnicity_fall_HL$ethnicity <- "Hispanic/Latino"
colnames(df_ethnicity_fall_HL) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_fall_Int <- select(df_ethnicity_semester_fall, Course, Semester, "n_grades_Int":"%_DFW_Int")
df_ethnicity_fall_Int$ethnicity <- "International"
colnames(df_ethnicity_fall_Int) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_fall_NHPI <- select(df_ethnicity_semester_fall, Course, Semester, "n_grades_NH-PI":"%_DFW_NH-PI")
df_ethnicity_fall_NHPI$ethnicity <- "Native Hawaiian or Other Pacific Islander"
colnames(df_ethnicity_fall_NHPI) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_fall_Unknown <- select(df_ethnicity_semester_fall, Course, Semester, "n_grades_Unknown":"%_DFW_Unknown")
df_ethnicity_fall_Unknown$ethnicity <- "Unknown"
colnames(df_ethnicity_fall_Unknown) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_fall_White <- select(df_ethnicity_semester_fall, Course, Semester, "n_grades_White":"%_DFW_White")
df_ethnicity_fall_White$ethnicity <- "White"
colnames(df_ethnicity_fall_White) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")

df_ethnicity_semester_spring <- read_excel("DFW For UG CHM Courses_Fa08_to_Sum19.xlsx", sheet = 8, col_names = T, skip = 1)
colnames(df_ethnicity_semester_spring) <- c("Course","Semester","n_grades_2+","n_DFW_2+", "%_DFW_2+","n_grades_AI-AN","n_DFW_AI-AN","%_DFW_AI-AN",
                                          "n_grades_Asian","n_DFW_Asian", "%_DFW_Asian","n_grades_B-AA","n_DFW_B-AA","%_DFW_B-AA",
                                          "n_grades_H-L","n_DFW_H-L","%_DFW_H-L","n_grades_Int","n_DFW_Int","%_DFW_Int","n_grades_NH-PI",
                                          "n_DFW_NH-PI","%_DFW_NH-PI","n_grades_Unknown","n_DFW_Unknown","%_DFW_Unknown","n_grades_White",
                                          "n_DFW_White","%_DFW_White")
df_ethnicity_spring_2 <- select(df_ethnicity_semester_spring, Course, Semester, "n_grades_2+":"%_DFW_2+")
df_ethnicity_spring_2$ethnicity <- "2 or more races"
colnames(df_ethnicity_spring_2) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_spring_AIAN <- select(df_ethnicity_semester_spring, Course, Semester, "n_grades_AI-AN":"%_DFW_AI-AN")
df_ethnicity_spring_AIAN$ethnicity <- "American Indian or Alaska Native"
colnames(df_ethnicity_spring_AIAN) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_spring_Asian <- select(df_ethnicity_semester_spring, Course, Semester, "n_grades_Asian":"%_DFW_Asian")
df_ethnicity_spring_Asian$ethnicity <- "Asian"
colnames(df_ethnicity_spring_Asian) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_spring_BAA <- select(df_ethnicity_semester_spring, Course, Semester, "n_grades_B-AA":"%_DFW_B-AA")
df_ethnicity_spring_BAA$ethnicity <- "Black or African American"
colnames(df_ethnicity_spring_BAA) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_spring_HL <- select(df_ethnicity_semester_spring, Course, Semester, "n_grades_H-L":"%_DFW_H-L")
df_ethnicity_spring_HL$ethnicity <- "Hispanic/Latino"
colnames(df_ethnicity_spring_HL) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_spring_Int <- select(df_ethnicity_semester_spring, Course, Semester, "n_grades_Int":"%_DFW_Int")
df_ethnicity_spring_Int$ethnicity <- "International"
colnames(df_ethnicity_spring_Int) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_spring_NHPI <- select(df_ethnicity_semester_spring, Course, Semester, "n_grades_NH-PI":"%_DFW_NH-PI")
df_ethnicity_spring_NHPI$ethnicity <- "Native Hawaiian or Other Pacific Islander"
colnames(df_ethnicity_spring_NHPI) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_spring_Unknown <- select(df_ethnicity_semester_spring, Course, Semester, "n_grades_Unknown":"%_DFW_Unknown")
df_ethnicity_spring_Unknown$ethnicity <- "Unknown"
colnames(df_ethnicity_spring_Unknown) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_spring_White <- select(df_ethnicity_semester_spring, Course, Semester, "n_grades_White":"%_DFW_White")
df_ethnicity_spring_White$ethnicity <- "White"
colnames(df_ethnicity_spring_White) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")

df_ethnicity_semester_summer <- read_excel("DFW For UG CHM Courses_Fa08_to_Sum19.xlsx", sheet = 9, col_names = T, skip = 1)
colnames(df_ethnicity_semester_summer) <- c("Course","Semester","n_grades_2+","n_DFW_2+", "%_DFW_2+","n_grades_AI-AN","n_DFW_AI-AN","%_DFW_AI-AN",
                                          "n_grades_Asian","n_DFW_Asian", "%_DFW_Asian","n_grades_B-AA","n_DFW_B-AA","%_DFW_B-AA",
                                          "n_grades_H-L","n_DFW_H-L","%_DFW_H-L","n_grades_Int","n_DFW_Int","%_DFW_Int","n_grades_NH-PI",
                                          "n_DFW_NH-PI","%_DFW_NH-PI","n_grades_Unknown","n_DFW_Unknown","%_DFW_Unknown","n_grades_White",
                                          "n_DFW_White","%_DFW_White")
df_ethnicity_summer_2 <- select(df_ethnicity_semester_summer, Course, Semester, "n_grades_2+":"%_DFW_2+")
df_ethnicity_summer_2$ethnicity <- "2 or more races"
colnames(df_ethnicity_summer_2) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_summer_AIAN <- select(df_ethnicity_semester_summer, Course, Semester, "n_grades_AI-AN":"%_DFW_AI-AN")
df_ethnicity_summer_AIAN$ethnicity <- "American Indian or Alaska Native"
colnames(df_ethnicity_summer_AIAN) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_summer_Asian <- select(df_ethnicity_semester_summer, Course, Semester, "n_grades_Asian":"%_DFW_Asian")
df_ethnicity_summer_Asian$ethnicity <- "Asian"
colnames(df_ethnicity_summer_Asian) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_summer_BAA <- select(df_ethnicity_semester_summer, Course, Semester, "n_grades_B-AA":"%_DFW_B-AA")
df_ethnicity_summer_BAA$ethnicity <- "Black or African American"
colnames(df_ethnicity_summer_BAA) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_summer_HL <- select(df_ethnicity_semester_summer, Course, Semester, "n_grades_H-L":"%_DFW_H-L")
df_ethnicity_summer_HL$ethnicity <- "Hispanic/Latino"
colnames(df_ethnicity_summer_HL) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_summer_Int <- select(df_ethnicity_semester_summer, Course, Semester, "n_grades_Int":"%_DFW_Int")
df_ethnicity_summer_Int$ethnicity <- "International"
colnames(df_ethnicity_summer_Int) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_summer_NHPI <- select(df_ethnicity_semester_summer, Course, Semester, "n_grades_NH-PI":"%_DFW_NH-PI")
df_ethnicity_summer_NHPI$ethnicity <- "Native Hawaiian or Other Pacific Islander"
colnames(df_ethnicity_summer_NHPI) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_summer_Unknown <- select(df_ethnicity_semester_summer, Course, Semester, "n_grades_Unknown":"%_DFW_Unknown")
df_ethnicity_summer_Unknown$ethnicity <- "Unknown"
colnames(df_ethnicity_summer_Unknown) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")
df_ethnicity_summer_White <- select(df_ethnicity_semester_summer, Course, Semester, "n_grades_White":"%_DFW_White")
df_ethnicity_summer_White$ethnicity <- "White"
colnames(df_ethnicity_summer_White) <- c("Course","Semester","n_grades","n_DFW", "%_DFW","ethnicity")

df_ethnicity_semester <- rbind(df_ethnicity_fall_2, df_ethnicity_fall_AIAN, df_ethnicity_fall_Asian, df_ethnicity_fall_BAA, df_ethnicity_fall_HL,
                               df_ethnicity_fall_Int, df_ethnicity_fall_NHPI, df_ethnicity_fall_Unknown, df_ethnicity_fall_White, df_ethnicity_spring_2, 
                               df_ethnicity_spring_AIAN, df_ethnicity_spring_Asian, df_ethnicity_spring_BAA, df_ethnicity_spring_HL, 
                               df_ethnicity_spring_Int, df_ethnicity_spring_NHPI, df_ethnicity_spring_Unknown, df_ethnicity_spring_White, df_ethnicity_summer_2, 
                               df_ethnicity_summer_AIAN, df_ethnicity_summer_Asian, df_ethnicity_summer_BAA, df_ethnicity_summer_HL,
                               df_ethnicity_summer_Int, df_ethnicity_summer_NHPI, df_ethnicity_summer_Unknown, df_ethnicity_summer_White)
df_ethnicity_semester$ethnicity <- as.factor(df_ethnicity_semester$ethnicity)
df_ethnicity_semester <- gather(df_ethnicity_semester, measure, value, n_grades:"%_DFW")

semester_levels <- c("Fall 2008", "Spring 2009", "Summer 2009", "Fall 2009", "Spring 2010", "Summer 2010", "Fall 2010", "Spring 2011", "Summer 2011", 
                     "Fall 2011", "Spring 2012", "Summer 2012", "Fall 2012", "Spring 2013", "Summer 2013", "Fall 2013", "Spring 2014", "Summer 2014", 
                     "Fall 2014", "Spring 2015", "Summer 2015", "Fall 2015", "Spring 2016", "Summer 2016", "Fall 2016", "Spring 2017", "Summer 2017", 
                     "Fall 2017", "Spring 2018", "Summer 2018", "Fall 2018", "Spring 2019", "Summer 2019")

df_gender_semester$Semester_level <- factor(df_gender_semester$Semester, levels = semester_levels)
df_fgen_semester$Semester_level <- factor(df_fgen_semester$Semester, levels = semester_levels)
df_ethnicity_semester$Semester_level <- factor(df_ethnicity_semester$Semester, levels = semester_levels)

# Adding variables for semester and year separately
years <- as.character(c(2008:2019))

df_gender_semester$date <- NA
df_gender_semester$date[str_detect(df_gender_semester$Semester, "^Su")] <- "Summer"
df_gender_semester$date[str_detect(df_gender_semester$Semester, "^Sp")] <- "Spring"
df_gender_semester$date[str_detect(df_gender_semester$Semester, "^F")] <- "Fall"

df_fgen_semester$date <- NA
df_fgen_semester$date[str_detect(df_fgen_semester$Semester, "^Su")] <- "Summer"
df_fgen_semester$date[str_detect(df_fgen_semester$Semester, "^Sp")] <- "Spring"
df_fgen_semester$date[str_detect(df_fgen_semester$Semester, "^F")] <- "Fall"

df_ethnicity_semester$date <- NA
df_ethnicity_semester$date[str_detect(df_ethnicity_semester$Semester, "^Su")] <- "Summer"
df_ethnicity_semester$date[str_detect(df_ethnicity_semester$Semester, "^Sp")] <- "Spring"
df_ethnicity_semester$date[str_detect(df_ethnicity_semester$Semester, "^F")] <- "Fall"

df_gender_semester$year <- NA
df_fgen_semester$year <- NA
df_ethnicity_semester$year <- NA

for(i in 1:length(years)){
  df_gender_semester$year[str_detect(df_gender_semester$Semester, years[i])] <- years[i]
  df_fgen_semester$year[str_detect(df_fgen_semester$Semester, years[i])] <- years[i]
  df_ethnicity_semester$year[str_detect(df_ethnicity_semester$Semester, years[i])] <- years[i]
}

df_gender_semester$year <- as.factor(df_gender_semester$year)
df_fgen_semester$year <- as.factor(df_fgen_semester$year)
df_ethnicity_semester$year <- as.factor(df_ethnicity_semester$year)

# Split data based on 100, 200, 300 level courses
df_gender_semester_100 <- df_gender_semester[grepl("CHM1", df_gender_semester$Course),]
df_gender_semester_200 <- df_gender_semester[grepl("CHM2", df_gender_semester$Course),]
df_gender_semester_300 <- df_gender_semester[grepl("CHM3", df_gender_semester$Course),]

df_fgen_semester_100 <- df_fgen_semester[grepl("CHM1", df_fgen_semester$Course),]
df_fgen_semester_200 <- df_fgen_semester[grepl("CHM2", df_fgen_semester$Course),]
df_fgen_semester_300 <- df_fgen_semester[grepl("CHM3", df_fgen_semester$Course),]

df_ethnicity_semester_100 <- df_ethnicity_semester[grepl("CHM1", df_ethnicity_semester$Course),]
df_ethnicity_semester_200 <- df_ethnicity_semester[grepl("CHM2", df_ethnicity_semester$Course),]
df_ethnicity_semester_300 <- df_ethnicity_semester[grepl("CHM3", df_ethnicity_semester$Course),]
