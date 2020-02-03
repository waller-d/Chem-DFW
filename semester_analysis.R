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
ggsave("grades_gender_100_semester_course.png", units="in", width=15, height=5, dpi=750)
df_gender_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  ggplot(aes(gender, Semester)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Gender", fill = "DFW Rate", title = "200 Level Chemistry Courses") +
  facet_grid(.~Course)
ggsave("grades_gender_200_semester_course.png", units="in", width=15, height=5, dpi=750)
df_gender_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  ggplot(aes(gender, Semester)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Gender", fill = "DFW Rate", title = "300 Level Chemistry Courses") +
  facet_grid(.~Course)
ggsave("grades_gender_300_semester_course.png", units="in", width=15, height=5, dpi=750)

# Create heat maps for DFW rates by first generation status
# Combined DFW rates for all 100, 200, 300 level courses
df_fgen_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  ggplot(aes(fgen, Semester)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red" ) +
  labs(x = "First Generation Status", fill = "DFW Rate", title = "100 Level Chemistry Courses")
ggsave("grades_fgen_100_semester.png", units="in", width=6, height=5, dpi=300)
df_fgen_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  ggplot(aes(fgen, Semester)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "First Generation Status", fill = "DFW Rate", title = "200 Level Chemistry Courses")
ggsave("grades_fgen_200_semester.png", units="in", width=6, height=5, dpi=300)
df_fgen_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  ggplot(aes(fgen, Semester)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "First Generation Status", fill = "DFW Rate", title = "300 Level Chemistry Courses")
ggsave("grades_fgen_300_semester.png", units="in", width=6, height=5, dpi=300)

# Create heat maps for DFW rates by first generation status
# DFW rates faceted by course
df_fgen_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  ggplot(aes(fgen, Semester)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red" ) +
  labs(x = "First Generation Status", fill = "DFW Rate", title = "100 Level Chemistry Courses") +
  facet_grid(.~Course)
ggsave("grades_fgen_100_semester_course.png", units="in", width=15, height=5, dpi=750)
df_fgen_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  ggplot(aes(fgen, Semester)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "First Generation Status", fill = "DFW Rate", title = "200 Level Chemistry Courses") +
  facet_grid(.~Course)
ggsave("grades_fgen_200_semester_course.png", units="in", width=15, height=5, dpi=750)
df_fgen_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  ggplot(aes(fgen, Semester)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "First Generation Status", fill = "DFW Rate", title = "300 Level Chemistry Courses") +
  facet_grid(.~Course)
ggsave("grades_fgen_300_semester_course.png", units="in", width=15, height=5, dpi=750)

# Create heat maps for DFW rates by ethnicity
# Combined DFW rates for all 100, 200, 300 level courses
df_ethnicity_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  ggplot(aes(ethnicity, Semester)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red" ) +
  labs(x = "Ethnicity", fill = "DFW Rate", title = "100 Level Chemistry Courses")
ggsave("grades_ethnicity_100_semester.png", units="in", width=12, height=5, dpi=450)
df_ethnicity_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  ggplot(aes(ethnicity, Semester)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Ethnicity", fill = "DFW Rate", title = "200 Level Chemistry Courses")
ggsave("grades_ethnicity_200_semester.png", units="in", width=12, height=5, dpi=450)
df_ethnicity_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  ggplot(aes(ethnicity, Semester)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Ethnicity", fill = "DFW Rate", title = "300 Level Chemistry Courses")
ggsave("grades_ethnicity_300_semester.png", units="in", width=12, height=5, dpi=450)

# Create line plots to see change over time
df_gender_semester %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  group_by(Semester_level, gender) %>% summarise(Mean = mean(value)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "DFW Rate")
ggsave("grades_gender_semester_line.png", units="in", width=12, height=5, dpi=450)

df_fgen_semester %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  group_by(Semester_level, fgen) %>% summarise(Mean = mean(value)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "DFW Rate")
ggsave("grades_fgen_semester_line.png", units="in", width=12, height=5, dpi=450)

df_ethnicity_semester %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  group_by(Semester_level, ethnicity) %>% summarise(Mean = mean(value)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "DFW Rate")
ggsave("grades_ethnicity_semester_line.png", units="in", width=12, height=5, dpi=450)

df_ethnicity_semester %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(Semester_level, ethnicity) %>% summarise(Mean = mean(value)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "DFW Rate")
ggsave("grades_ethnicity_semester_line_simple.png", units="in", width=12, height=5, dpi=450)

# Create line plots to see change over time faceted by course and group membership
# Summer semester filtered out
df_gender_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>%
  group_by(Semester_level, Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender") +
  facet_grid(Course~gender) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_100_line_separate.png", units="in", width=12, height=5, dpi=450)

df_gender_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>%
  group_by(Semester_level, Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_100_line_overlay.png", units="in", width=12, height=5, dpi=450)

df_gender_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>%
  group_by(Semester_level, Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender") +
  facet_grid(Course~gender) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_200_line_separate.png", units="in", width=12, height=5, dpi=450)

df_gender_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>%
  group_by(Semester_level, Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_200_line_overlay.png", units="in", width=12, height=5, dpi=450)

df_gender_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>%
  group_by(Semester_level, Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender") +
  facet_grid(Course~gender) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_300_line_separate.png", units="in", width=12, height=5, dpi=450)

df_gender_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>%
  group_by(Semester_level, Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_300_line_overlay.png", units="in", width=12, height=5, dpi=450)

df_fgen_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>% 
  group_by(Semester_level, Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "First Generation") +
  facet_grid(Course~fgen) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_100_line_simple_separate.png", units="in", width=15, height=10, dpi=750)

df_fgen_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>% 
  group_by(Semester_level, Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "First Generation") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_100_line_simple_overlay.png", units="in", width=15, height=10, dpi=750)

df_fgen_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>% 
  group_by(Semester_level, Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "First Generation") +
  facet_grid(Course~fgen) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_200_line_simple_separate.png", units="in", width=15, height=10, dpi=750)

df_fgen_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>% 
  group_by(Semester_level, Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "First Generation") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_200_line_simple_overlay.png", units="in", width=15, height=10, dpi=750)

df_fgen_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>% 
  group_by(Semester_level, Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "First Generation") +
  facet_grid(Course~fgen) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_300_line_simple_separate.png", units="in", width=15, height=10, dpi=750)

df_fgen_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>% 
  group_by(Semester_level, Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "First Generation") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_300_line_simple_overlay.png", units="in", width=15, height=10, dpi=750)

df_ethnicity_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>% 
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(Semester_level, Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "Ethnicity") +
  facet_grid(Course~ethnicity) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_100_line_simple_separate.png", units="in", width=15, height=10, dpi=750)

df_ethnicity_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>% 
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(Semester_level, Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "Ethnicity") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_100_line_simple_overlay.png", units="in", width=15, height=10, dpi=750)

df_ethnicity_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>% 
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(Semester_level, Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "Ethnicity") +
  facet_grid(Course~ethnicity) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_200_line_simple_separate.png", units="in", width=15, height=10, dpi=750)

df_ethnicity_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>% 
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(Semester_level, Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "Ethnicity") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_200_line_simple_overlay.png", units="in", width=15, height=10, dpi=750)

df_ethnicity_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>% 
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(Semester_level, Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "Ethnicity") +
  facet_grid(Course~ethnicity) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_300_line_simple_separate.png", units="in", width=15, height=10, dpi=750)

df_ethnicity_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value), !str_detect(Semester, "^Su")) %>% 
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(Semester_level, Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester_level, y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "Ethnicity") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_300_line_simple_overlay.png", units="in", width=15, height=10, dpi=750)

# Create line plots to see change over time faceted by course and group membership
# Summer semester filtered out
df_gender_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(Semester, Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester, y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender") +
  facet_grid(Course~gender) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_100_line_term.png", units="in", width=12, height=5, dpi=450)

df_gender_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(Semester, Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester , y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_100_line_overlay_term.png", units="in", width=12, height=5, dpi=450)

df_gender_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(Semester , Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester , y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender") +
  facet_grid(Course~gender) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_200_line_term.png", units="in", width=12, height=5, dpi=450)

df_gender_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(Semester , Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester , y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_200_line_overlay_term.png", units="in", width=12, height=5, dpi=450)

df_gender_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(Semester , Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester , y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender") +
  facet_grid(Course~gender) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_300_line_term.png", units="in", width=12, height=5, dpi=450)

df_gender_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(Semester , Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester , y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_300_line_overlay_term.png", units="in", width=12, height=5, dpi=450)

df_fgen_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  group_by(Semester , Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester , y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "First Generation") +
  facet_grid(Course~fgen) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_100_line_simple_term.png", units="in", width=15, height=10, dpi=750)

df_fgen_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  group_by(Semester , Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester , y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "First Generation") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_100_line_simple_overlay_term.png", units="in", width=15, height=10, dpi=750)

df_fgen_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  group_by(Semester , Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester , y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "First Generation") +
  facet_grid(Course~fgen) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_200_line_simple_term.png", units="in", width=15, height=10, dpi=750)

df_fgen_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  group_by(Semester , Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester , y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "First Generation") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_200_line_simple_overlay_term.png", units="in", width=15, height=10, dpi=750)

df_fgen_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  group_by(Semester , Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester , y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "First Generation") +
  facet_grid(Course~fgen) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_300_line_simple_term.png", units="in", width=15, height=10, dpi=750)

df_fgen_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  group_by(Semester , Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester , y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "First Generation") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_300_line_simple_overlay_term.png", units="in", width=15, height=10, dpi=750)

df_ethnicity_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(Semester , Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester , y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "Ethnicity") +
  facet_grid(Course~ethnicity) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_100_line_simple_term.png", units="in", width=15, height=10, dpi=750)

df_ethnicity_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(Semester , Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester , y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "Ethnicity") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_100_line_simple_overlay_term.png", units="in", width=15, height=10, dpi=750)

df_ethnicity_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(Semester , Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester , y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "Ethnicity") +
  facet_grid(Course~ethnicity) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_200_line_simple_term.png", units="in", width=15, height=10, dpi=750)

df_ethnicity_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(Semester , Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester , y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "Ethnicity") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_200_line_simple_overlay_term.png", units="in", width=15, height=10, dpi=750)

df_ethnicity_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(Semester , Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester , y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "Ethnicity") +
  facet_grid(Course~ethnicity) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_300_line_simple_term.png", units="in", width=15, height=10, dpi=750)

df_ethnicity_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>% 
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(Semester , Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = Semester , y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "Ethnicity") +
  facet_grid(Course~.) + # Faceting only by course
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_300_line_simple_overlay_term.png", units="in", width=15, height=10, dpi=750)

# Creating plots for each course separated by term based on gender
df_gender_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(year, date, Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Fall") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender", title = "Fall Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_100_line_simple_overlay_fall.png", units="in", width=15, height=10, dpi=750)
df_gender_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(year, date, Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Spring") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender", title = "Spring Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_100_line_simple_overlay_spring.png", units="in", width=15, height=10, dpi=750)
df_gender_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(year, date, Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Summer") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender", title = "Summer Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_100_line_simple_overlay_summer.png", units="in", width=15, height=10, dpi=750)

df_gender_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(year, date, Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Fall") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender", title = "Fall Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_200_line_simple_overlay_fall.png", units="in", width=15, height=10, dpi=750)
df_gender_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(year, date, Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Spring") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender", title = "Spring Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_200_line_simple_overlay_spring.png", units="in", width=15, height=10, dpi=750)
df_gender_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(year, date, Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Summer") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender", title = "Summer Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_200_line_simple_overlay_summer.png", units="in", width=15, height=10, dpi=750)

df_gender_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(year, date, Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Fall") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender", title = "Fall Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_300_line_simple_overlay_fall.png", units="in", width=15, height=10, dpi=750)
df_gender_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(year, date, Course, gender) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Spring") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = gender, color = gender)) +
  geom_line(aes(group = gender, color = gender)) +
  labs(x = "Semester", y = "DFW Rate", color = "Gender", title = "Spring Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_gender_semester_course_300_line_simple_overlay_spring.png", units="in", width=15, height=10, dpi=750)

# Creating plots for each course separated by term based on first generation status
df_fgen_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(year, date, Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Fall") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "fgen", title = "Fall Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_100_line_simple_overlay_fall.png", units="in", width=15, height=10, dpi=750)
df_fgen_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(year, date, Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Spring") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "fgen", title = "Spring Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_100_line_simple_overlay_spring.png", units="in", width=15, height=10, dpi=750)
df_fgen_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(year, date, Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Summer") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "fgen", title = "Summer Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_100_line_simple_overlay_summer.png", units="in", width=15, height=10, dpi=750)

df_fgen_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(year, date, Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Fall") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "fgen", title = "Fall Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_200_line_simple_overlay_fall.png", units="in", width=15, height=10, dpi=750)
df_fgen_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(year, date, Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Spring") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "fgen", title = "Spring Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_200_line_simple_overlay_spring.png", units="in", width=15, height=10, dpi=750)
df_fgen_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(year, date, Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Summer") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "fgen", title = "Summer Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_200_line_simple_overlay_summer.png", units="in", width=15, height=10, dpi=750)

df_fgen_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(year, date, Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Fall") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "fgen", title = "Fall Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_300_line_simple_overlay_fall.png", units="in", width=15, height=10, dpi=750)
df_fgen_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  group_by(year, date, Course, fgen) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Spring") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = fgen, color = fgen)) +
  geom_line(aes(group = fgen, color = fgen)) +
  labs(x = "Semester", y = "DFW Rate", color = "fgen", title = "Spring Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_fgen_semester_course_300_line_simple_overlay_spring.png", units="in", width=15, height=10, dpi=750)

# Creating plots for each course separated by term based on ethnicity
df_ethnicity_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(year, date, Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Fall") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "ethnicity", title = "Fall Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_100_line_simple_overlay_fall.png", units="in", width=15, height=10, dpi=750)
df_ethnicity_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(year, date, Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Spring") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "ethnicity", title = "Spring Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_100_line_simple_overlay_spring.png", units="in", width=15, height=10, dpi=750)
df_ethnicity_semester_100 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(year, date, Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Summer") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "ethnicity", title = "Summer Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_100_line_simple_overlay_summer.png", units="in", width=15, height=10, dpi=750)

df_ethnicity_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(year, date, Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Fall") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "ethnicity", title = "Fall Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_200_line_simple_overlay_fall.png", units="in", width=15, height=10, dpi=750)
df_ethnicity_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(year, date, Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Spring") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "ethnicity", title = "Spring Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_200_line_simple_overlay_spring.png", units="in", width=15, height=10, dpi=750)
df_ethnicity_semester_200 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(year, date, Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Summer") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "ethnicity", title = "Summer Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_200_line_simple_overlay_summer.png", units="in", width=15, height=10, dpi=750)

df_ethnicity_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(year, date, Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Fall") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "ethnicity", title = "Fall Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_300_line_simple_overlay_fall.png", units="in", width=15, height=10, dpi=750)
df_ethnicity_semester_300 %>%
  filter(measure == "%_DFW", value != is.na(value)) %>%
  filter(ethnicity == "Asian" | ethnicity == "Black or African American" | 
           ethnicity == "Hispanic/Latino" | ethnicity == "International" | 
           ethnicity == "White") %>% 
  group_by(year, date, Course, ethnicity) %>% summarise(Mean = mean(value, na.rm = TRUE)) %>% 
  filter(date == "Spring") %>% 
  ggplot(aes(x = year, y = Mean)) +
  geom_point(aes(group = ethnicity, color = ethnicity)) +
  geom_line(aes(group = ethnicity, color = ethnicity)) +
  labs(x = "Semester", y = "DFW Rate", color = "ethnicity", title = "Spring Term") +
  facet_grid(Course~.) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave("grades_ethnicity_semester_course_300_line_simple_overlay_spring.png", units="in", width=15, height=10, dpi=750)
