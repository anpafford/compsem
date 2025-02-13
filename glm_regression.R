
# Load necessary libraries
library(lme4)    # For mixed-effects models
library(ggplot2) # For plotting
library(dplyr)   # For data manipulation
library(showtext)

##################################################################################

# 1) First fix the data
# Load Data
human_truth_value <- read.csv('C:/Users/alexa/OneDrive/Documents/MSc Psychology/MSc Thesis/Code/human_truth_value_results_14_11.csv')
human_continuation <- read.csv('C:/Users/alexa/OneDrive/Documents/MSc Psychology/MSc Thesis/Code/human_continuation_results_14_11.csv')
machine_truth_value <- read.csv('C:/Users/alexa/OneDrive/Documents/MSc Psychology/MSc Thesis/Code/model_truth_value_clean.csv')
machine_continuation <- read.csv('C:/Users/alexa/OneDrive/Documents/MSc Psychology/MSc Thesis/Code/model_continuation_melted.csv')

# Preprocess Data
# Create copies of the DataFrames to avoid modifying originals
human_truth_df <- human_truth_value
human_cont_df <- human_continuation
llm_truth_df <- machine_truth_value
llm_cont_df <- machine_continuation

# Add 'group' column to identify the source
human_truth_df$group <- 'human'
human_cont_df$group <- 'human'
llm_truth_df$group <- 'llm'
llm_cont_df$group <- 'llm'

# Rename columns if needed
colnames(llm_truth_df)[colnames(llm_truth_df) == 'both_correct'] <- 'correct'
colnames(llm_truth_df)[colnames(llm_truth_df) == 'sentence.id'] <- 'sentence_id'
colnames(llm_truth_df)[colnames(llm_truth_df) == 'interval.length'] <- 'interval_length'
colnames(llm_cont_df)[colnames(llm_cont_df) == 'sentence.id'] <- 'sentence_id'
colnames(llm_cont_df)[colnames(llm_cont_df) == 'interval.length'] <- 'interval_length'
colnames(human_cont_df)[colnames(human_cont_df) == 'score'] <- 'correct'
colnames(human_truth_df)[colnames(human_truth_df) == 'score'] <- 'correct'

# Fix interval length
llm_truth_df$interval_length <- llm_truth_df$interval_length - 1
llm_cont_df$interval_length <- llm_cont_df$interval_length - 1


# This stuff is only relevant for LLMS

# Standardize concept tags
llm_truth_df$concept_tag <- tolower(llm_truth_df$concept_tag)
llm_cont_df$concept_tag <- tolower(llm_cont_df$concept_tag)

# Add 'probe_type' if not present
llm_truth_df$probe_type <- 'truth_value'
llm_cont_df$probe_type <- 'continuation'

# Replace 1s for TRUE and 0s for FALSE
llm_truth_df$correct[llm_truth_df$correct == "True"] <- 1
llm_truth_df$correct[llm_truth_df$correct == "False"] <- 0
llm_truth_df$correct <- as.numeric(llm_truth_df$correct)
llm_cont_df$correct[llm_cont_df$correct == "True"] <- 1
llm_cont_df$correct[llm_cont_df$correct == "False"] <- 0
llm_cont_df$correct <- as.numeric(llm_cont_df$correct)

################################################################################

# 2) Scale accuracy

# Scale accuracy and add back to the dataframe
chance_level <- 0.5
llm_truth_chance <- 0.25

# Human, truth value
human_truth_df$raw_accuracy <- human_truth_df$correct
# Apply the scaling formula
human_truth_df$scaled_accuracy <- (human_truth_df$raw_accuracy - chance_level) / (1 - chance_level)

# Human, continuation
human_cont_df$raw_accuracy <- human_cont_df$correct
# Apply the scaling formula
human_cont_df$scaled_accuracy <- (human_cont_df$raw_accuracy - chance_level) / (1 - chance_level)

# LLM, truth value
llm_truth_df$raw_accuracy <- llm_truth_df$correct
# Apply the scaling formula
llm_truth_df$scaled_accuracy <- (llm_truth_df$raw_accuracy - chance_level) / (1 - chance_level)

# LLM, continuation
llm_cont_df$raw_accuracy <- llm_cont_df$correct
# Apply the scaling formula
llm_cont_df$scaled_accuracy <- (llm_cont_df$raw_accuracy - chance_level) / (1 - chance_level)

################################################################################

# 3) Scale predictors

# Scale the predictors
# Scale predictors and add them back to the df
human_truth_df$interval_length_scaled <- scale(human_truth_df$interval_length, center = TRUE, scale = TRUE)
human_truth_df$complexity_scaled <- scale(human_truth_df$complexity, center = TRUE, scale = TRUE)
human_cont_df$interval_length_scaled <- scale(human_cont_df$interval_length, center = TRUE, scale = TRUE)
human_cont_df$complexity_scaled <- scale(human_cont_df$complexity, center = TRUE, scale = TRUE)
llm_truth_df$interval_length_scaled <- scale(llm_truth_df$interval_length, center = TRUE, scale = TRUE)
llm_truth_df$complexity_scaled <- scale(llm_truth_df$complexity, center = TRUE, scale = TRUE)
llm_cont_df$interval_length_scaled <- scale(llm_cont_df$interval_length, center = TRUE, scale = TRUE)
llm_cont_df$complexity_scaled <- scale(llm_cont_df$complexity, center = TRUE, scale = TRUE)



# Check summary statistics to verify scaling
summary(human_truth_df$interval_length_scaled)
summary(human_truth_df$complexity_scaled)
# The scaled values are now on a comparable scale (centered around zero and approximately spanning ±1 standard deviation).
# 1 = perfect performance
# 0 = chance
# negative = below chance

##################################################################################

# 4) Hierarchical Means Analysis

# Create a function to calculate hierarchical means
calculate_means <- function(df, group_by, level_name) {
  df %>%
    group_by(across(all_of(group_by))) %>%
    summarise(scaled_accuracy = mean(scaled_accuracy, na.rm = TRUE), .groups = "drop") %>%
    mutate(level = level_name)
}

# Human truth data hierarchical means
human_means_truth_sentid <- calculate_means(human_truth_df, c("sentence_id", "concept_tag"), "sentence_id")
human_means_truth_concept <- calculate_means(human_truth_df, c("concept_tag", "interval_length", "complexity"), "concept_tag")
human_means_truth_interval <- calculate_means(human_truth_df, c("interval_length"), "interval_length")
human_means_truth_complexity <- calculate_means(human_truth_df, c("complexity"), "complexity")

# Add group identifier
human_means_truth_sentid$group <- "human"
human_means_truth_concept$group <- "human"
human_means_truth_interval$group <- "human"
human_means_truth_complexity$group <- "human"

# LLM truth data hierarchical means
llm_means_truth_sentid <- calculate_means(llm_truth_df, c("sentence_id", "concept_tag"), "sentence_id")
llm_means_truth_concept <- calculate_means(llm_truth_df, c("concept_tag", "interval_length", "complexity"), "concept_tag")
llm_means_truth_interval <- calculate_means(llm_truth_df, c("interval_length"), "interval_length")
llm_means_truth_complexity <- calculate_means(llm_truth_df, c("complexity"), "complexity")

# Add group identifier
llm_means_sentid$group <- "llm"
llm_means_concept$group <- "llm"
llm_means_interval$group <- "llm"
llm_means_complexity$group <- "llm"

# Human continuation data hierarchical means
human_means_cont_sentid <- calculate_means(human_cont_df, c("sentence_id", "concept_tag"), "sentence_id")
human_means_cont_concept <- calculate_means(human_cont_df, c("concept_tag", "interval_length", "complexity"), "concept_tag")
human_means_cont_interval <- calculate_means(human_cont_df, c("interval_length"), "interval_length")
human_means_cont_complexity <- calculate_means(human_cont_df, c("complexity"), "complexity")

# Add group identifier
human_means_cont_sentid$group <- "human"
human_means_cont_concept$group <- "human"
human_means_cont_interval$group <- "human"
human_means_cont_complexity$group <- "human"

# LLM continuation data hierarchical means
llm_means_cont_sentid <- calculate_means(llm_cont_df, c("sentence_id", "concept_tag"), "sentence_id")
llm_means_cont_concept <- calculate_means(llm_cont_df, c("concept_tag", "interval_length", "complexity"), "concept_tag")
llm_means_cont_interval <- calculate_means(llm_cont_df, c("interval_length"), "interval_length")
llm_means_cont_complexity <- calculate_means(llm_cont_df, c("complexity"), "complexity")

# Add group identifier
llm_means_cont_sentid$group <- "llm"
llm_means_cont_concept$group <- "llm"
llm_means_cont_interval$group <- "llm"
llm_means_cont_complexity$group <- "llm"

# Combine all dataframes
all_means <- bind_rows(
  human_means_sentid, human_means_concept, human_means_interval, human_means_complexity,
  llm_means_sentid, llm_means_concept, llm_means_interval, llm_means_complexity
)



##################################################################################

# Logistic Mixed Effects Regression

#################################################################################
# Humans regression
#################################################################################

# Define the mixed-effects logistic regression model
formula <- "scaled_accuracy ~ interval_length + complexity + ResponseId + sentence_id" # when I add sentence_id the model doesn't converge

#################################################################################
# Truth value
#################################################################################

# Fit the mixed-effects logistic regression model
truth_model <- lm(formula, data = human_truth_df)

# Display the summary of the model
summary(truth_model)

# Plot with means: truth value/complexity
ggplot(human_truth_df, aes(x = complexity, y = scaled_accuracy)) +
  geom_jitter(aes(color = factor(scaled_accuracy)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "lm", color = "#f28a2f", se = FALSE, size = 1) +  # Line of best fit
  geom_point(data = human_means_truth_complexity, aes(x = complexity, y = scaled_accuracy), color = "black", size = 1) +  # Add hierarchical means as points
  labs(
    x = "Scene Complexity",
    y = "Accuracy (Scaled)",
    title = "Truth Value Prompt: Scene Complexity vs. (Predicted) Accuracy in Humans"
  ) +
  scale_x_continuous(breaks = seq(from = 1, to = 3)) +
  scale_color_manual(values = c("-1" = "brown3", "1" = "darkseagreen")) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend
    text = element_text(family = "serif", size = 11)
  )

# Plot with means: truth value/interval length
ggplot(human_truth_df, aes(x = interval_length, y = scaled_accuracy)) +
  geom_jitter(aes(color = factor(scaled_accuracy)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "lm", color = "#f28a2f", se = FALSE, size = 1) +  # Line of best fit
  geom_point(data = human_means_truth_interval, aes(x = interval_length, y = scaled_accuracy), color = "black", size = 1) +  # Add hierarchical means as points
  labs(
    x = "Interval Length",
    y = "Accuracy (Scaled)",
    title = "Truth Value Prompt: Interval Length vs. (Predicted) Accuracy in Humans"
  ) +
  scale_x_continuous(breaks = seq(from = 0, to = 5)) +
  scale_color_manual(values = c("-1" = "brown3", "1" = "darkseagreen")) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend
    text = element_text(family = "serif", size = 11)
  )

#################################################################################
# Continuation
#################################################################################

# Fit the linear regression model
cont_model <- lm(formula, data = human_cont_df)

# Display the summary of the model
summary(cont_model)

# Plot with means: continuation/complexity
ggplot(human_cont_df, aes(x = complexity, y = scaled_accuracy)) +
  geom_jitter(aes(color = factor(scaled_accuracy)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "lm", color = "#f28a2f", se = FALSE, size = 1) +  # Line of best fit
  geom_point(data = human_means_cont_complexity, aes(x = complexity, y = scaled_accuracy), color = "black", size = 1) +  # Add hierarchical means as points
  labs(
    x = "Scene Complexity",
    y = "Accuracy (Scaled)",
    title = "Continuation Prompt: Scene Complexity vs. (Predicted) Accuracy in Humans"
  ) +
  scale_x_continuous(breaks = seq(from = 1, to = 3)) +
  scale_color_manual(values = c("-1" = "brown3", "1" = "darkseagreen")) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend
    text = element_text(family = "serif", size = 11)
  )

# Plot with means: continuation/interval length
ggplot(human_truth_df, aes(x = interval_length, y = scaled_accuracy)) +
  geom_jitter(aes(color = factor(scaled_accuracy)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "lm", color = "#f28a2f", se = FALSE, size = 1) +  # Line of best fit
  geom_point(data = human_means_cont_interval, aes(x = interval_length, y = scaled_accuracy), color = "black", size = 1) +  # Add hierarchical means as points
  labs(
    x = "Interval Length",
    y = "Accuracy (Scaled)",
    title = "Continuation Prompt: Interval Length vs. (Predicted) Accuracy in Humans"
  ) +
  scale_x_continuous(breaks = seq(from = 0, to = 5)) +
  scale_color_manual(values = c("-1" = "brown3", "1" = "darkseagreen")) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend
    text = element_text(family = "serif", size = 11)
  )

################################################################################
#  Models
################################################################################

llm_formula <- "scaled_accuracy ~ interval_length + complexity + model + sentence_id"

#################################################################################
# Truth value
#################################################################################

# Fit the linear regression model
llm_truth_model <- lm(llm_formula, data = llm_truth_df)

# Display the summary of the model
summary(llm_truth_model)

# Plot with means: truth value/complexity
ggplot(llm_truth_df, aes(x = complexity, y = scaled_accuracy)) +
  geom_jitter(aes(color = factor(scaled_accuracy)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "lm", color = "#5a4c94", se = FALSE, size = 1) +  # Line of best fit
  geom_point(data = llm_means_truth_complexity, aes(x = complexity, y = scaled_accuracy), color = "black", size = 1) +  # Add hierarchical means as points
  labs(
    x = "Scene Complexity",
    y = "Accuracy (Scaled)",
    title = "Truth Value Prompt: Scene Complexity vs. (Predicted) Accuracy in LLMs"
  ) +
  scale_x_continuous(breaks = seq(from = 1, to = 3)) +
  scale_color_manual(values = c("-1" = "brown3", "1" = "darkseagreen")) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend
    text = element_text(family = "serif", size = 11)
  )

# Plot with means: truth value/interval length
ggplot(llm_truth_df, aes(x = interval_length, y = scaled_accuracy)) +
  geom_jitter(aes(color = factor(scaled_accuracy)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "lm", color = "#5a4c94", se = FALSE, size = 1) +  # Line of best fit
  geom_point(data = llm_means_truth_interval, aes(x = interval_length, y = scaled_accuracy), color = "black", size = 1) +  # Add hierarchical means as points
  labs(
    x = "Interval Length",
    y = "Accuracy (Scaled)",
    title = "Truth Value Prompt: Interval Length vs. (Predicted) Accuracy in LLMs"
  ) +
  scale_x_continuous(breaks = seq(from = 0, to = 5)) +
  scale_color_manual(values = c("-1" = "brown3", "1" = "darkseagreen")) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend
    text = element_text(family = "serif", size = 11)
  )

#################################################################################
# Continuation
#################################################################################

# Fit the linear regression model
llm_cont_model <- lm(llm_formula, data = llm_cont_df)

# Display the summary of the model
summary(llm_cont_model)

# Plot with means: truth value/complexity
ggplot(llm_cont_df, aes(x = complexity, y = scaled_accuracy)) +
  geom_jitter(aes(color = factor(scaled_accuracy)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "lm", color = "#5a4c94", se = FALSE, size = 1) +  # Line of best fit
  geom_point(data = llm_means_cont_complexity, aes(x = complexity, y = scaled_accuracy), color = "black", size = 1) +  # Add hierarchical means as points
  labs(
    x = "Scene Complexity",
    y = "Accuracy (Scaled)",
    title = "Continuation Prompt: Scene Complexity vs. (Predicted) Accuracy in LLMs"
  ) +
  scale_x_continuous(breaks = seq(from = 1, to = 3)) +
  scale_color_manual(values = c("-1" = "brown3", "1" = "darkseagreen")) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend
    text = element_text(family = "serif", size = 11)
  )

# Plot with means: truth value/interval length
ggplot(llm_cont_df, aes(x = interval_length, y = scaled_accuracy)) +
  geom_jitter(aes(color = factor(scaled_accuracy)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "lm", color = "#5a4c94", se = FALSE, size = 1) +  # Line of best fit
  geom_point(data = llm_means_cont_interval, aes(x = interval_length, y = scaled_accuracy), color = "black", size = 1) +  # Add hierarchical means as points
  labs(
    x = "Interval Length",
    y = "Accuracy (Scaled)",
    title = "Continuation Prompt: Interval Length vs. (Predicted) Accuracy in LLMs"
  ) +
  scale_x_continuous(breaks = seq(from = 0, to = 5)) +
  scale_color_manual(values = c("-1" = "brown3", "1" = "darkseagreen")) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend
    text = element_text(family = "serif", size = 11)
  )

#################################################################################




## GLMER
#################################################################################
# Humans regression
#################################################################################

# Define the mixed-effects logistic regression model
formula <- "correct ~ interval_length_scaled + complexity_scaled + (1 | ResponseId) + concept_tag" # when I add sentence_id the model doesn't converge

#################################################################################
# Truth value
#################################################################################


# Fit the mixed-effects logistic regression model
truth_model <- glmer(formula, data = human_truth_df, family = binomial(link = "logit"))

# Display the summary of the model
summary(truth_model)

# Generate predictions for plotting
human_truth_df$predicted_probabilities <- predict(truth_model, type = "response")

# Plot
ggplot(human_truth_df, aes(x = complexity, y = correct)) +
  geom_jitter(aes(color = factor(correct)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "glm", method.args = list(family = binomial), color = "#f28a2f", se = FALSE, size = 1) +  # Line of best fit
  labs(
    x = "Scene Complexity",
    y = "Accuracy",
    title = "Truth Value Prompt: Scene Complexity vs. (Predicted) Accuracy in Humans"
  ) +
  scale_color_manual(values = c("0" = "brown3", "1" = "darkseagreen")) +  # Map incorrect (0) to grey and correct (1) to black
  scale_x_continuous(breaks = seq(from = min(human_truth_df$complexity), to = max(human_truth_df$complexity), length.out = 3)) +  # Set 3 ticks on x-axis
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend
    text = element_text(family = "serif", size = 11)
  )


# Plotting the relationship between interval length and accuracy
ggplot(human_truth_df, aes(x = interval_length, y = correct)) +
  geom_jitter(aes(color = factor(correct)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "glm", method.args = list(family = binomial), color = "#f28a2f", se = FALSE, size = 1) +
  labs(
    x = "Interval Length",
    y = "Accuracy",
    title = "Truth Value Prompt: Interval Length vs. (Predicted) Accuracy in Humans"
  ) +
  scale_color_manual(values = c("0" = "brown3", "1" = "darkseagreen")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 11)
  )


#################################################################################
# Continuation
#################################################################################

# Fit the mixed-effects logistic regression model
cont_model <- glmer(formula, data = human_cont_df, family = binomial(link = "logit"))

# Display the summary of the model
summary(cont_model)

# Generate predictions for plotting
human_cont_df$predicted_probabilities <- predict(cont_model, type = "response")

# Plotting the relationship between complexity and accuracy
ggplot(human_cont_df, aes(x = complexity, y = correct)) +
  geom_jitter(aes(color = factor(correct)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "glm", method.args = list(family = binomial), color = "#f28a2f", se = FALSE, size = 1) +
  labs(
    x = "Scene Complexity",
    y = "Accuracy",
    title = "Continuation Coherency Prompt: Scene Complexity vs. (Predicted) Accuracy"
  ) +
  scale_x_continuous(breaks = seq(from = min(human_truth_df$complexity), to = max(human_truth_df$complexity), length.out = 3)) +  # Set 3 ticks on x-axis
  scale_color_manual(values = c("0" = "brown3", "1" = "darkseagreen")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 11)
  )


# Plotting the relationship between Interval Length and observed data with a line of best fit
ggplot(human_cont_df, aes(x = interval_length, y = correct)) +
  geom_jitter(aes(color = factor(correct)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "glm", method.args = list(family = binomial), color = "#f28a2f", se = FALSE, size = 1) +
  labs(
    x = "Interval Length",
    y = "Accuracy",
    title = "Continuation Coherency Prompt: Interval Length vs. (Predicted) Accuracy in Humans"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("0" = "brown3", "1" = "darkseagreen")) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 11)
  )


################################################################################
#  Models
################################################################################

llm_formula <- "correct ~ interval_length_scaled + complexity_scaled + (1 | model)"

#################################################################################
# Truth value
#################################################################################


# Fit the mixed-effects logistic regression model
llm_truth_model <- glmer(llm_formula, data = llm_truth_df, family = binomial(link = "logit"))

# Display the summary of the model
summary(llm_truth_model)

# Generate predictions for plotting
llm_truth_df$predicted_probabilities <- predict(llm_truth_model, type = "response")

# Plotting the relationship between complexity and accuracy
ggplot(llm_truth_df, aes(x = complexity_scaled, y = correct)) +
  geom_jitter(aes(color = factor(correct)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "glm", method.args = list(family = binomial), color = "#5a4c94", se = FALSE, size = 1) +
  labs(
    x = "Scene Complexity",
    y = "Accuracy",
    title = "Truth Value Prompt: Scene Complexity vs. (Predicted) Accuracy in LLMs"
  ) +
  scale_x_continuous(breaks = seq(from = min(llm_truth_df$complexity), to = max(llm_truth_df$complexity), length.out = 3)) +  # Set 3 ticks on x-axis
  theme_minimal() +
  scale_color_manual(values = c("0" = "brown3", "1" = "darkseagreen")) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 11)
  )

# Plotting the relationship between interval length and accuracy with a custom legend
ggplot(llm_truth_df, aes(x = interval_length_scaled, y = correct)) +
  geom_jitter(aes(color = factor(correct)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jittered points with custom legend label
  geom_smooth(method = "glm", method.args = list(family = binomial), color = "#5a4c94", se = FALSE, size = 1) +
  labs(
    x = "Interval Length",
    y = "Accuracy",
    title = "Truth Value Prompt: Interval Length vs. (Predicted) Accuracy in LLMs"
  ) +
  scale_x_continuous(breaks = seq(from = min(llm_truth_df$complexity), to = max(llm_truth_df$complexity), length.out = 3)) +  # Set 3 ticks on x-axis
  theme_minimal() +
  scale_color_manual(values = c("0" = "brown3", "1" = "darkseagreen")) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 11)
  )

#################################################################################
# Continuation
#################################################################################

# Fit the mixed-effects logistic regression model
llm_cont_model <- glmer(llm_formula, data = llm_cont_df, family = binomial(link = "logit"))

# Display the summary of the model
summary(llm_cont_model)

# Generate predictions for plotting
llm_cont_df$predicted_probabilities <- predict(llm_cont_model, type = "response")

# Plotting the relationship between complexity and accuracy
ggplot(llm_cont_df, aes(x = complexity_scaled, y = correct)) +
  geom_jitter(aes(color = factor(correct)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "glm", method.args = list(family = binomial), color = "#5a4c94", se = FALSE, size = 1) +
  labs(
    x = "Scene Complexity",
    y = "Accuracy",
    title = "Continuation Coherency Prompt: Scene Complexity vs. (Predicted) Accuracy in LLMs"
  ) +
  scale_x_continuous(breaks = seq(from = min(llm_cont_df$complexity), to = max(llm_cont_df$complexity), length.out = 3)) +  # Set 3 ticks on x-axis
  theme_minimal() +
  scale_color_manual(values = c("0" = "brown3", "1" = "darkseagreen")) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 11)
  )

# Plotting the relationship between interval length and accuracy with a custom legend
ggplot(llm_cont_df, aes(x = interval_length_scaled, y = correct)) +
  geom_jitter(aes(color = factor(correct)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jittered points with custom legend label
  geom_smooth(method = "glm", method.args = list(family = binomial), color = "#5a4c94", se = FALSE, size = 1) +
  labs(
    x = "Interval Length",
    y = "Accuracy",
    title = "Continuation Coherency Prompt: Interval Length vs. (Predicted) Accuracy"
  ) +
  scale_x_continuous(breaks = seq(from = min(llm_cont_df$complexity), to = max(llm_cont_df$complexity), length.out = 3)) +  # Set 3 ticks on x-axis
  theme_minimal() +
  scale_color_manual(values = c("0" = "brown3", "1" = "darkseagreen")) +
  theme(
    legend.position = "none",
    text = element_text(family = "serif", size = 11)
  )
