library(dplyr)
library(tidyr)

# Sample dataframe (my_data)
my_data <- data.frame(
  ID = 1:5,
  Age = c(25, 30, 40, 35, 28),
  Gender = c("M", "F", "M", "M", "F"),
  Education = c("High School", "Bachelor's", "Master's", "Bachelor's", "Doctorate")
)

# Sample lookup table (lookup_table)
lookup_table <- data.frame(
  variable = c("Gender", "Gender", "Education", "Education", "Education"),
  original_value = c("M", "F", "Bachelor's", "Master's", "Doctorate"),
  new_value = c("Male", "Female", "Bachelors", "Masters", "PhD")
)

# Convert the lookup table into a named vector for easier recoding
lookup_vector <- lookup_table %>%
  pivot_longer(cols = starts_with("original_value"), names_to = "variable", values_to = "original_value") %>%
  pivot_wider(names_from = "original_value", values_from = "new_value") %>%
  select(-variable) %>%
  deframe()

# Recode the values in the dataframe using the lookup vector
recoded_data <- my_data %>%
  mutate(across(everything(), ~ lookup_vector[as.character(.)]))

# Output the recoded dataframe
print(recoded_data)



#####

# Sample dataframe (my_data)
my_data <- data.frame(
  ID = 1:5,
  Age = c(25, 30, 40, 35, 28),
  Gender = c("M", "F", "M", "M", "F"),
  Education = c("High School", "Bachelor's", "Master's", "Bachelor's", "Doctorate")
)

# Sample lookup table (lookup_table)
lookup_table <- data.frame(
  original_value = c("M", "F", "Bachelor's", "Master's", "Doctorate"),
  new_value = c("Male", "Female", "Bachelors", "Masters", "PhD")
)

# Convert the lookup table into a named vector for easier recoding
lookup_vector <- setNames(lookup_table$new_value, lookup_table$original_value)

# Recode the values in the dataframe using the lookup vector
recoded_data <- my_data %>%
  mutate(across(everything(), ~ lookup_vector[as.character(.)]))

# Output the recoded dataframe
print(recoded_data)
