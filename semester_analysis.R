##########################
################  waller-d
#
# Semester analsys of chemistry course
#        DFW rates, 2008-2019
#
# Contributors: David Waller
# Project: Chem-DFW
#
################
##########################

# Run data_cleaning.R script prior to running this script

# Calculate average for each measure (across classes) and count the number of missing values
# The number of missing values is the number of classes that we do not have data for a given group
df_gender_semester %>%
  group_by(gender, Course, measure) %>%
  summarise(Mean = mean(value, na.rm = TRUE), Variance = var(value, na.rm = TRUE), Missing = sum(is.na(value)), n())
df_fgen_all %>%
  group_by(fgen, measure) %>%
  summarise(Mean = mean(value, na.rm = TRUE), Variance = var(value, na.rm = TRUE), Missing = sum(is.na(value)))
df_ethnicity_all %>%
  group_by(ethnicity, measure) %>%
  summarise(Mean = mean(value, na.rm = TRUE), Variance = var(value, na.rm = TRUE), Missing = sum(is.na(value)))

# Split data based on 100, 200, 300 level courses
df_gender_semester_100 <- df_gender_semester[grepl("CHM1", df_gender_semester$Course),]
df_gender_semester_200 <- df_gender_semester[grepl("CHM2", df_gender_semester$Course),]
df_gender_semester_300 <- df_gender_semester[grepl("CHM3", df_gender_semester$Course),]

# Create heat maps for DFW rates by gender
# Combined DFW rates for all 100, 200, 300 level courses
df_gender_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  ggplot(aes(gender, Semester)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red" ) +
  labs(x = "Gender", fill = "DFW Rate", title = "100 Level Chemistry Courses")
ggsave("grades_gender_100_semester.png", units="in", width=6, height=5, dpi=300)
df_gender_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  ggplot(aes(gender, Semester)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Gender", fill = "DFW Rate", title = "200 Level Chemistry Courses")
ggsave("grades_gender_200_semester.png", units="in", width=6, height=5, dpi=300)
df_gender_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  ggplot(aes(gender, Semester)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Gender", fill = "DFW Rate", title = "300 Level Chemistry Courses")
ggsave("grades_gender_300_semester.png", units="in", width=6, height=5, dpi=300)

# Create heat maps for DFW rates by gender
# DFW rates faceted by course
df_gender_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  ggplot(aes(gender, Semester)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red" ) +
  labs(x = "Gender", fill = "DFW Rate", title = "100 Level Chemistry Courses") +
  facet_grid(.~Course)
df_gender_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  ggplot(aes(gender, Semester)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Gender", fill = "DFW Rate", title = "200 Level Chemistry Courses") +
  facet_grid(.~Course)
df_gender_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  ggplot(aes(gender, Semester)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Gender", fill = "DFW Rate", title = "300 Level Chemistry Courses") +
  facet_grid(.~Course)


