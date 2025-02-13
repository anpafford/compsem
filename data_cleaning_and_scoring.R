
## PILOT PROCESSING AND SCORING

# ----------------------------------------------------------------------------------

# load dependencies
library(dplyr)
library(tidyr)
library(stringr)
library(readr)


# ----------------------------------------------------------------------------------

## LOAD IN DATA

# ----------------------------------------------------------------------------------

# import csv results

# full visibility pilot results
experiment_1a <- read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Full Experiment Data\\experiment_1a_July+2,+2024_15.41.csv")
experiment_1b <- read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Full Experiment Data\\experiment_1b_July+2,+2024_15.40.csv")
experiment_2a <- read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Full Experiment Data\\experiment_2a_July+2,+2024_15.41.csv")
experiment_2b <- read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Full Experiment Data\\experiment_2b_July+2,+2024_15.41.csv")
experiment_3a <- read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Full Experiment Data\\experiment_3a_July+2,+2024_15.53.csv")
experiment_3b <- read_csv("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Full Experiment Data\\experiment_3b_July+2,+2024_15.53.csv")

# Fix the survey order to fit the experiment conditions tracking document

# Below is the matrix for reference

##################################
# Qualtrics Name | Survey Number #
#--------------------------------#
# experiment_1a  | 1             #
# experiment_2a  | 2             #
# experiment_3a  | 3             #
# experiment_1b  | 4             #
# experiment_2b  | 5             #
# experiment_3b  | 6             #
##################################

# put them all in separate lists

# NB: make sure that they are in the right order
experiment_dfs <- list(experiment_1a,experiment_2a,experiment_3a,
                       experiment_1b,experiment_2b,experiment_3b)
experiment_names <- c("experiment_1a",
                           "experiment_2a",
                           "experiment_3a",
                           "experiment_1b",
                           "experiment_2b",
                           "experiment_3b")

# incremental_dfs <- list(incremental_pilot_1,incremental_pilot_2,incremental_pilot_3,
#             incremental_pilot_4,incremental_pilot_5,incremental_pilot_6,incremental_pilot_7,incremental_pilot_8,incremental_pilot_9,
#             incremental_pilot_10,incremental_pilot_11,incremental_pilot_12)

# Incremental data is omitted here because it was part of the pilot

# read in the lookup table csv
# the lookup table contains all of the questions amd their corresponding answer options (coherent and incoherent). we use this to score
lookup_table <- read.csv2("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Code\\experiment_lookup_table.csv")
# print(lookup_table)


# read in the conditions csv
# this shows what each of the experiments look like in terms of the actual questions that were asked 
# this also includes complexity, interval length, sentence-id, etc. all of the variables.
conditions_df <- read.csv2("C:\\Users\\alexa\\OneDrive\\Documents\\MSc Psychology\\MSc Thesis\\Code\\experiment_conditions_tracking_semicolon.csv")

# ----------------------------------------------------------------------------------

## FUNCTIONS

# ----------------------------------------------------------------------------------

# Define a function to clean the dataframe
clean_df <- function(df) {
  cleaned_df <- df[-c(1, 2), ]  # Remove the first two rows (metadata)
  cleaned_df <- cleaned_df[, !duplicated(colnames(cleaned_df))]  # Remove duplicated column names
  return(cleaned_df)
}

# Define a function to reshape the dataframe to long format
reshape_to_long <- function(df, survey_number) {
  # Gather all columns except ResponseId and Q1_1
  long_df <- df %>%
    select(ResponseId, Q1_1, contains("query"), contains("prompt"), contains("context"), contains("update"), contains("interval")) %>%
    pivot_longer(cols = -c(ResponseId, Q1_1), names_to = "question", values_to = "answer") %>%
    mutate(survey = survey_number, row_id = row_number())  # Add survey number and row identifier
  return(long_df)
}

# Define a function to process multiple dataframes
process_multiple_dfs <- function(df_list, survey_numbers) {
  long_dfs <- lapply(seq_along(df_list), function(i) {
    df <- df_list[[i]]
    survey_number <- survey_numbers[i]
    cleaned_df <- clean_df(df)
    long_df <- reshape_to_long(cleaned_df, survey_number)
    return(long_df)
  })
  combined_long_df <- bind_rows(long_dfs)
  combined_long_df <- combined_long_df %>%
    arrange(survey, ResponseId, row_id)  # Arrange by survey, ResponseId, and row_id to preserve order
  return(combined_long_df)
}

# ----------------------------------------------------------------------------------

## Experiment

# ----------------------------------------------------------------------------------

survey_numbers <- 1:6 # Define survey numbers, we have 6

# Process all dataframes
combined_long_df <- process_multiple_dfs(experiment_dfs, survey_numbers)

# Display the combined dataframe
print(combined_long_df[combined_long_df$survey == 3, ], n=600)

# Rename Q1_1 to attention
combined_long_df <- combined_long_df %>%
  rename(attention = Q1_1)

# Rename 'question' to 'sentence_name'
combined_long_df <- combined_long_df %>%
  rename(sentence_name = question)

# Create a new column 'score' and initialize with NA
combined_long_df <- combined_long_df %>%
  mutate(score = NA)

# Iterate through each row in combined_long_df and assign the score
for (i in 1:nrow(combined_long_df)) {
  sentence_name <- combined_long_df$sentence_name[i]
  answer <- combined_long_df$answer[i]
  
  lookup_row <- lookup_table %>%
    filter(sentence_name == !!sentence_name) %>%
    slice(1)  # Select the first matching row
  
  if (nrow(lookup_row) > 0) {
    # Check if the answer matches the coherent or incoherent answer
    if (answer == lookup_row$coherent_answer[1]) {
      combined_long_df$score[i] <- 1
    } else if (answer == lookup_row$incoherent_answer[1]) {
      combined_long_df$score[i] <- 0
    }
  }
}

# Recode 0s in the score for dyingtruthvaluequery1, eatingtruthvalue1, and ToMtruthvaluequery1 to 1s
combined_long_df <- combined_long_df %>%
  mutate(score = if_else(sentence_name %in% c("dyingtruthvaluequery1",
                                              "ToMtruthvaluequery1",
                                              "eatingtruthvaluequery1",
                                              "goingtruthvaluequery1",
                                              "largesttruthvaluequery1") & score == 0, 1, score))

# Exclude rows where sentence_name is "smallestcontinuationprompt1"
combined_long_df <- combined_long_df[combined_long_df$sentence_name != "smallestcontinuationprompt1", ] # messed up this item



# Filter rows where attention score is below 5
low_attention_df <- combined_long_df %>%
  filter(attention < 5)



# Display the rows with attention score 4 or less (this is a check)
nrow(low_attention_df)
# Nothing is returned here, so we're good

# Check to make sure everything is ok
print(combined_long_df, n = 250)

# Look for entries where the scoring function didn't work
na_rows <- combined_long_df[is.na(combined_long_df$score), ]
print(na_rows)
# Note to self: it looks like I messed up in entering the answer options in Qualtrics, hence the mismatch with the dictionary
# I now manually score these as a 1 because they actually got them correct
# Update the score directly for NA values
combined_long_df$score[is.na(combined_long_df$score)] <- 1
# Check again
na_rows <- combined_long_df[is.na(combined_long_df$score), ]
print(na_rows)
# Empty df

# Final check
print(combined_long_df, n = 300)
# Save locally
write.csv(combined_long_df, "experiment_results_v3.csv")

















# ----------------------------------------------------------------------------------

## Full Visibility (pilot)

# ----------------------------------------------------------------------------------

# Create a list of survey numbers
survey_numbers <- 1:12

# Process all dataframes
combined_long_df <- process_multiple_dfs(full_visibility_dfs, survey_numbers)

# Display the combined dataframe
print(combined_long_df, n=100)

# Create a new column 'score' and initialize with NA
combined_long_df <- combined_long_df %>%
  mutate(score = NA)

# Iterate through each row in combined_long_df and assign the score
for (i in 1:nrow(combined_long_df)) {
  sentence_name <- combined_long_df$sentence_name[i]
  answer <- combined_long_df$answer[i]
  
  lookup_row <- lookup_table %>%
    filter(sentence_name == !!sentence_name) %>%
    slice(1)  # Select the first matching row
  
  if (nrow(lookup_row) > 0) {
    # Check if the answer matches the coherent or incoherent answer
    if (answer == lookup_row$coherent_answer[1]) {
      combined_long_df$score[i] <- 1
    } else if (answer == lookup_row$incoherent_answer[1]) {
      combined_long_df$score[i] <- 0
    }
  }
}

# See what it looks like 
print(combined_long_df, n=100)



# Save locally
write.csv(combined_long_df, "full_visibility_results_v1") # Update CSV name as appropriate





