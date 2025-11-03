# Load necessary libraries
library(lme4)    # For mixed-effects models
library(ggplot2) # For plotting
library(dplyr)   # For data manipulation
library(showtext)
library(patchwork)
library(purrr)


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

# 2) Scale accuracy (we don't need this anymore)

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
llm_truth_df$scaled_accuracy <- (llm_truth_df$raw_accuracy - llm_truth_chance) / (1 - llm_truth_chance)

# LLM, continuation
llm_cont_df$raw_accuracy <- llm_cont_df$correct
# Apply the scaling formula
llm_cont_df$scaled_accuracy <- (llm_cont_df$raw_accuracy - chance_level) / (1 - chance_level)

################################################################################

# 3) Scale predictors (we don't need this anymore either)

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

# 4) Hierarchical Means Analysis (not needed anymore)

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

# Linear Regression

#################################################################################
# Humans regression
#################################################################################

# Define the mixed-effects logistic regression model
formula <- "correct ~ interval_length + complexity"

#################################################################################
# Truth value
#################################################################################

# Fit the model
truth_model <- glm(formula, data = human_truth_df, family = binomial(link = 'logit'))

# Display the summary of the model
summary(truth_model)

# Check linearity
# Residuals vs Fitted plot
plot(truth_model$fitted.values, resid(truth_model), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Add a smooth line to check for patterns
ggplot(data.frame(Fitted = truth_model$fitted.values, Residuals = resid(truth_model)), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "loess", col = "blue") +
  theme_minimal() +
  ggtitle("Residuals vs Fitted Values")


# Plot with means: truth value/complexity
ggplot(human_truth_df, aes(x = complexity, y = correct)) +
  geom_jitter(aes(color = factor(correct)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "glm", color = "#f28a2f", se = FALSE, size = 1) +  # Line of best fit
  labs(
    x = "Scene Complexity",
    y = "Accuracy",
    title = "Truth Value Prompt: Scene Complexity vs. (Predicted) Accuracy"
  ) +
  scale_x_continuous(breaks = seq(from = 1, to = 3)) +
  scale_color_manual(values = c("0" = "brown3", "1" = "darkseagreen")) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend
    text = element_text(family = "serif", size = 11)
  )

# Plot with means: truth value/interval length
ggplot(human_truth_df, aes(x = interval_length, y = correct)) +
  geom_jitter(aes(color = factor(correct)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "glm", color = "#f28a2f", se = FALSE, size = 1) +  # Line of best fit
  labs(
    x = "Interval Length",
    y = "Accuracy",
    title = "Truth Value Prompt: Interval Length vs. (Predicted) Accuracy"
  ) +
  scale_x_continuous(breaks = seq(from = 0, to = 5)) +
  scale_color_manual(values = c("0" = "brown3", "1" = "darkseagreen")) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend
    text = element_text(family = "serif", size = 11)
  )

#################################################################################
# Continuation
#################################################################################

# Fit the linear regression model
cont_model <- glm(formula, data = human_cont_df, family = binomial(link = 'logit'))

# Display the summary of the model
summary(cont_model)

# Plot with means: continuation/complexity
ggplot(human_cont_df, aes(x = complexity, y = correct)) +
  geom_jitter(aes(color = factor(correct)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "glm", color = "#f28a2f", se = FALSE, size = 1) +  # Line of best fit
  labs(
    x = "Scene Complexity",
    y = "Accuracy",
    title = "Continuation Prompt: Scene Complexity vs. (Predicted) Accuracy"
  ) +
  scale_x_continuous(breaks = seq(from = 1, to = 3)) +
  scale_color_manual(values = c("0" = "brown3", "1" = "darkseagreen")) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend
    text = element_text(family = "serif", size = 11)
  )

# Plot: continuation/interval length
ggplot(human_cont_df, aes(x = interval_length, y = correct)) +
  geom_jitter(aes(color = factor(correct)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "glm", color = "#f28a2f", se = FALSE, size = 1) +  # Line of best fit
  labs(
    x = "Interval Length",
    y = "Accuracy",
    title = "Continuation Prompt: Interval Length vs. (Predicted) Accuracy"
  ) +
  scale_x_continuous(breaks = seq(from = 0, to = 5)) +
  scale_color_manual(values = c("0" = "brown3", "1" = "darkseagreen")) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend
    text = element_text(family = "serif", size = 11)
  )

################################################################################

# Models means (this is to look at effect of individual models)

################################################################################
# Truth value
################################################################################

# Plot with means: truth value/scene complexity
plot_llm_truth_complexity <- ggplot(llm_truth_df, aes(x = complexity, y = correct, color = model, group = model)) +
  stat_summary(
    fun = mean,  # Calculate mean accuracy
    geom = "line",  # Use a line to connect means
    size = 1
  ) +
  stat_summary(
    fun = mean,  # Calculate mean accuracy
    geom = "point",  # Add points for means
    size = 2
  ) +
  scale_color_manual(
    values = c("#d4b9da", "#c994c7", "purple", "darkorchid3", "darkmagenta", "mediumpurple", "#5a4c94")  # Shades of purple
  ) +
  labs(
    x = "Scene Complexity",
    y = "Mean Accuracy",
    title = "Mean Accuracy by Scene Complexity (Truth Value Prompt)",
    color = "LLM Model"
  ) +  # <- Correct placement of the +
  scale_x_continuous(breaks = unique(llm_truth_df$complexity)) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif", size = 12),
    legend.position = "top"
  )

  
# Plot with means: truth value/interval length
plot_llm_truth_interval <- ggplot(llm_truth_df, aes(x = interval_length, y = correct, color = model, group = model)) +
  stat_summary(
    fun = mean,  # Calculate mean accuracy
    geom = "line",  # Use a line to connect means
    size = 1
  ) +
  stat_summary(
    fun = mean,  # Calculate mean accuracy
    geom = "point",  # Add points for means
    size = 2
  ) +
  scale_color_manual(
    values = c("#d4b9da", "#c994c7", "purple", "darkorchid3", "darkmagenta", "mediumpurple", "#5a4c94")  # Shades of purple
  ) +
  labs(
    x = "Interval Length",
    y = "Mean Accuracy",
    title = "Mean Accuracy by Interval Length (Truth Value Prompt)",
    color = "LLM Model"
  ) +  # <- Correct placement of the +
  scale_x_continuous(breaks = unique(llm_truth_df$interval_length)) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif", size = 12),
    legend.position = "top"
  )

################################################################################
# Continuation
###############################################################################

# Plot with means: continuation/scene complexity
plot_llm_cont_complexity <- ggplot(llm_cont_df, aes(x = complexity, y = correct, color = model, group = model)) +
  stat_summary(
    fun = mean,  # Calculate mean accuracy
    geom = "line",  # Use a line to connect means
    size = 1
  ) +
  stat_summary(
    fun = mean,  # Calculate mean accuracy
    geom = "point",  # Add points for means
    size = 2
  ) +
  scale_color_manual(
    values = c("#d4b9da", "#c994c7", "purple", "darkorchid3", "darkmagenta", "mediumpurple", "#5a4c94")  # Shades of purple
  ) +
  labs(
    x = "Scene Complexity",
    y = "Mean Accuracy",
    title = "Mean Accuracy by Scene Complexity (Continuation Prompt)",
    color = "LLM Model"
  ) +  # <- Correct placement of the +
  scale_x_continuous(breaks = unique(llm_truth_df$complexity)) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif", size = 12),
    legend.position = "top"
  )


# Plot with means: continuation/interval length
plot_llm_cont_interval <- ggplot(llm_cont_df, aes(x = interval_length, y = correct, color = model, group = model)) +
  stat_summary(
    fun = mean,  # Calculate mean accuracy
    geom = "line",  # Use a line to connect means
    size = 1
  ) +
  stat_summary(
    fun = mean,  # Calculate mean accuracy
    geom = "point",  # Add points for means
    size = 2
  ) +
  scale_color_manual(
    values = c("#d4b9da", "#c994c7", "purple", "darkorchid3", "darkmagenta", "mediumpurple", "#5a4c94")  # Shades of purple
  ) +
  labs(
    x = "Interval Length",
    y = "Mean Accuracy",
    title = "Mean Accuracy by Interval Length (Continuation Prompt)",
    color = "LLM Model"
  ) + 
  scale_x_continuous(breaks = unique(llm_truth_df$interval_length)) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif", size = 12),
    legend.position = "top"
  )


################################################################################

# Directly compare human vs. LLM(s) using regression and pairwise comparison

################################################################################
# Aggregate the LLM means across model type for Truth Value and Continuation
################################################################################

# Create scaled data frames for comparison plots
scaled_human_truth_df <- human_truth_df %>%
  mutate(
    scaled_accuracy = (correct - chance_level) / (1 - chance_level)
  )

scaled_llm_truth_df <- llm_truth_df %>%
  mutate(
    scaled_accuracy = (correct - llm_truth_chance) / (1 - llm_truth_chance)
  )

# Aggregate LLM scaled data for Truth Value
scaled_agg_truth_df <- scaled_llm_truth_df %>%
  group_by(complexity, interval_length) %>%
  summarise(mean_scaled_accuracy = mean(scaled_accuracy, na.rm = TRUE))

# Aggregate data for Truth Value (Scene Complexity and Interval Length)
agg_truth_df <- llm_truth_df %>%
  group_by(complexity, interval_length) %>%
  summarise(mean_correct = mean(correct, na.rm = TRUE))  # Aggregate by model

# Aggregate data for Continuation (Scene Complexity and Interval Length)
agg_cont_df <- llm_cont_df %>%
  group_by(complexity, interval_length) %>%
  summarise(mean_correct = mean(correct, na.rm = TRUE))  # Aggregate by model

################################################################################
# Truth value
################################################################################
################################################################################
# Plot for Truth Value - Scene Complexity vs. Mean Accuracy with Regression Line
################################################################################

ggplot(agg_truth_df, aes(x = complexity, y = mean_correct)) +
  geom_smooth(method = "lm", color = "#5a4c94", se = FALSE, size = 1) +  # Regression line (predicted line)
  geom_smooth(data = human_truth_df, aes(x = complexity, y = correct), method = "lm", color = "darkorange", se = FALSE, size = 1) +  # Human regression line
  labs(
    x = "Scene Complexity",
    y = "Accuracy",
    title = "Accuracy vs. Scene Complexity (Truth Value)",
    color = "Line Type"
  ) +
  scale_x_continuous(breaks = unique(agg_truth_df$complexity)) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    text = element_text(family = "serif", size = 12),
    legend.position = "top"
  )

# Complexity plot with scaled accuracy
ggplot(scaled_agg_truth_df, aes(x = complexity, y = mean_scaled_accuracy)) +
  geom_smooth(method = "lm", color = "#5a4c94", se = FALSE, size = 1) +  # LLM line
  geom_smooth(data = scaled_human_truth_df, aes(x = complexity, y = scaled_accuracy), 
              method = "lm", color = "darkorange", se = FALSE, size = 1) +  # Human line
  labs(
    x = "Scene Complexity",
    y = "Scaled Accuracy",
    title = "Scaled Accuracy vs. Scene Complexity (Truth Value)",
    color = "Line Type"
  ) +
  scale_x_continuous(breaks = unique(scaled_agg_truth_df$complexity)) +
  scale_y_continuous(limits = c(-1, 1)) +  # Adjust y-axis for scaled accuracy
  theme_minimal() +
  theme(
    text = element_text(family = "serif", size = 12),
    legend.position = "top"
  )

# Interval length plot with scaled accuracy
ggplot(scaled_agg_truth_df, aes(x = interval_length, y = mean_scaled_accuracy)) +
  geom_smooth(method = "lm", color = "#5a4c94", se = FALSE, size = 1) +  # LLM line
  geom_smooth(data = scaled_human_truth_df, aes(x = interval_length, y = scaled_accuracy), 
              method = "lm", color = "darkorange", se = FALSE, size = 1) +  # Human line
  labs(
    x = "Interval Length",
    y = "Scaled Accuracy",
    title = "Scaled Accuracy vs. Interval Length (Truth Value)",
    color = "Line Type"
  ) +
  scale_x_continuous(breaks = unique(scaled_agg_truth_df$interval_length)) +
  scale_y_continuous(limits = c(-1, 1)) +  # Adjust y-axis for scaled accuracy
  theme_minimal() +
  theme(
    text = element_text(family = "serif", size = 12),
    legend.position = "top"
  )

################################################################################
# Plot for Truth Value - Interval Length vs. Mean Accuracy with Regression Line
################################################################################

ggplot(agg_truth_df, aes(x = interval_length, y = mean_correct)) +
  geom_smooth(method = "lm", color = "#5a4c94", se = FALSE, size = 1) +  # Regression line (predicted line)
  geom_smooth(data = human_truth_df, aes(x = interval_length, y = correct), method = "lm", color = "darkorange", se = FALSE, size = 1) +  # Human regression line
  labs(
    x = "Interval Length",
    y = "Mean Accuracy",
    title = "Accuracy vs. Interval Length (Truth Value)",
    color = "Line Type"
  ) +
  scale_x_continuous(breaks = unique(agg_truth_df$interval_length)) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    text = element_text(family = "serif", size = 12),
    legend.position = "top"
  )

################################################################################
# Plot for Continuation - Scene Complexity vs. Mean Accuracy with Regression Line
################################################################################

ggplot(agg_cont_df, aes(x = complexity, y = mean_correct)) +
  geom_smooth(method = "lm", color = "#5a4c94", se = FALSE, size = 1) +  # Regression line (predicted line)
  geom_smooth(data = human_cont_df, aes(x = complexity, y = correct), method = "lm", color = "darkorange", se = FALSE, size = 1) +  # Human regression line
  labs(
    x = "Scene Complexity",
    y = "Mean Accuracy",
    title = "Accuracy vs. Scene Complexity (Continuation)",
    color = "Line Type"
  ) +
  scale_x_continuous(breaks = unique(agg_cont_df$complexity)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif", size = 12),
    legend.position = "top"
  )

################################################################################
# Plot for Continuation - Interval Length vs. Mean Accuracy with Regression Line
################################################################################

ggplot(agg_cont_df, aes(x = interval_length, y = mean_correct)) +
  geom_smooth(method = "lm", color = "#5a4c94", se = FALSE, size = 1) +  # Regression line (predicted line)
  geom_smooth(data = human_cont_df, aes(x = interval_length, y = correct), method = "lm", color = "darkorange", se = FALSE, size = 1) +  # Human regression line
  labs(
    x = "Interval Length",
    y = "Mean Accuracy",
    title = "Accuracy vs. Interval Length (Continuation)",
    color = "Line Type"
  ) +
  scale_x_continuous(breaks = unique(agg_cont_df$interval_length)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif", size = 12)
  )

################################################################################

# Truth value vs Continuation

################################################################################

# Step 1: Aggregate the data
truth_data <- human_truth_df %>%
  mutate(prompt_type = "Truth Value") %>%
  select(correct, prompt_type)
truth_data <- llm_truth_df %>%
  mutate(prompt_type = "Truth Value") %>%
  select(correct, prompt_type)

cont_data <- human_cont_df %>%
  mutate(prompt_type = "Continuation") %>%
  select(correct, prompt_type)
cont_data <- llm_cont_df %>%
  mutate(prompt_type = "Continuation") %>%
  select(correct, prompt_type)

# Combine the data
combined_data <- bind_rows(truth_data, cont_data)

# Step 2: Scale Accuracy
combined_data$scaled_accuracy <- scale(combined_data$correct)

# Step 3: Perform t-test
t_test_result <- t.test(scaled_accuracy ~ prompt_type, data = combined_data)

# Print the t-test result
print(t_test_result)


ggplot(combined_data, aes(x = prompt_type, y = scaled_accuracy, fill = prompt_type)) +
  geom_violin(alpha = 0.6, trim = FALSE) +  # Trimmed violins may misrepresent distribution
  labs(
    title = "Comparison of Scaled Accuracy: Truth Value vs. Continuation",
    x = "Prompt Type",
    y = "Scaled Accuracy"
  ) +
  scale_fill_manual(values = c("Truth Value" = "#f28a2f", "Continuation" = "#4c9ed9")) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif", size = 12),
    legend.position = "none",
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )







################################################################################
#  Models (we don't need this anymore)
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
ggplot(llm_truth_df, aes(x = complexity, y = correct)) +
  geom_jitter(aes(color = factor(correct)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "lm", color = "#5a4c94", se = FALSE, size = 1) +  # Line of best fit
  labs(
    x = "Scene Complexity",
    y = "Accuracy",
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
  labs(
    x = "Interval Length",
    y = "Accuracy",
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
ggplot(llm_cont_df, aes(x = complexity, y = correct)) +
  geom_jitter(aes(color = factor(correct)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "lm", color = "#5a4c94", se = FALSE, size = 1) +  # Line of best fit
  labs(
    x = "Scene Complexity",
    y = "Accuracy",
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
ggplot(llm_cont_df, aes(x = interval_length, y = correct)) +
  geom_jitter(aes(color = factor(correct)), alpha = 0.6, width = 0.05, height = 0.05) +  # Add jitter to the points
  geom_smooth(method = "lm", color = "#5a4c94", se = FALSE, size = 1) +  # Line of best fit
  labs(
    x = "Interval Length",
    y = "Accuracy",
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
## GLMER (OLD)
#################################################################################
# Humans regression
#################################################################################

# Define the mixed-effects logistic regression model
formula <- "correct ~ interval_length + complexity + (1 | ResponseId) + concept_tag" # when I add sentence_id the model doesn't converge

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
