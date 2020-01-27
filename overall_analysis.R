##########################
################  waller-d
#
# Overall analsys of chemistry course
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
df_gender_all %>%
  group_by(gender, measure) %>%
  summarise(Mean = mean(value, na.rm = TRUE), Variance = var(value, na.rm = TRUE), Missing = sum(is.na(value)), n())
df_fgen_all %>%
  group_by(fgen, measure) %>%
  summarise(Mean = mean(value, na.rm = TRUE), Variance = var(value, na.rm = TRUE), Missing = sum(is.na(value)))
df_ethnicity_all %>%
  group_by(ethnicity, measure) %>%
  summarise(Mean = mean(value, na.rm = TRUE), Variance = var(value, na.rm = TRUE), Missing = sum(is.na(value)))

# Generate heat maps depicting for DFW rates by gender
# Combined DFW rates for each course
df_gender_all %>%
  filter(measure == "%_DFW") %>% 
  ggplot(aes(gender, Course)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Gender", fill = "DFW Rate")
ggsave("grades_gender_overall.png", units="in", width=6, height=5, dpi=300)
df_fgen_all %>%
  filter(measure == "%_DFW") %>% 
  ggplot(aes(fgen, Course)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "First Generation Status", fill = "DFW Rate")
ggsave("grades_fgen_overall.png", units="in", width=6, height=5, dpi=300)
df_ethnicity_all %>%
  filter(measure == "%_DFW") %>% 
  ggplot(aes(ethnicity, Course)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Ethnicity", fill = "DFW Rate")
ggsave("grades_ethnicity_overall.png", units="in", width=15, height=5, dpi=750)
