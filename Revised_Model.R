# Load required libraries
library(tidyverse)
library(performance)
library(xgboost)
library(caret)
library(gridExtra)
library(viridis)

# Read in the data
CollegeScorecard <- read_csv("CollegeScorecard.csv")

# First filter the data
filtered_scorecard <- CollegeScorecard %>%
  filter(PREDDEG == 3,  # Bachelor's degree granting
         CCUGPROF != 0, # Remove unclassified
         CCUGPROF != -2) # Remove graduate-only

# Define major groups
stem_cols <- c("PCIP11", "PCIP14", "PCIP15", "PCIP26", "PCIP27", "PCIP40", "PCIP41")
business_cols <- c("PCIP52")
healthcare_cols <- c("PCIP51")
liberal_arts_cols <- c("PCIP23", "PCIP24", "PCIP38", "PCIP39", "PCIP45", "PCIP54")
professional_cols <- c("PCIP04", "PCIP22", "PCIP44")

# Create the second model dataset with explicit type conversion and keeping all original features
second_model_scorecard <- filtered_scorecard %>%
  select(UNITID,
         INSTNM,          # Institution Name
         SAT_AVG,         # Average SAT Score
         SATVRMID,        # SAT Midpoint Critical Reading
         SATMTMID,        # SAT Midpoint Math
         ACTCMMID,        # Cumulative ACT Midpoint
         FAMINC,          # Family Income
         MD_FAMINC,       # Median Family Income
         FIRST_GEN,       # Percent First Generation Students
         FEMALE,          # Percent Female
         MD_EARN_WNE_P10, # Median Income 10 Years After Grad
         # All individual program mix variables
         PCIP01,          # Agriculture
         PCIP03,          # Natural Resources
         PCIP04,          # Architecture
         PCIP05,          # Area Studies
         PCIP09,          # Communication
         PCIP10,          # Communications Tech
         PCIP11,          # Computer Science
         PCIP12,          # Personal/Culinary Services
         PCIP13,          # Education
         PCIP14,          # Engineering
         PCIP15,          # Engineering Tech
         PCIP16,          # Foreign Languages
         PCIP19,          # Family/Consumer Sciences
         PCIP22,          # Legal
         PCIP23,          # English
         PCIP24,          # Liberal Arts
         PCIP25,          # Library Science
         PCIP26,          # Biology
         PCIP27,          # Mathematics
         PCIP29,          # Military Tech
         PCIP30,          # Interdisciplinary
         PCIP31,          # Parks/Recreation
         PCIP38,          # Philosophy
         PCIP39,          # Theology
         PCIP40,          # Physical Sciences
         PCIP41,          # Science Tech
         PCIP42,          # Psychology
         PCIP43,          # Security
         PCIP44,          # Public Admin
         PCIP45,          # Social Sciences
         PCIP46,          # Construction
         PCIP47,          # Mechanic/Repair Tech
         PCIP48,          # Precision Production
         PCIP49,          # Transportation
         PCIP50,          # Visual/Performing Arts
         PCIP51,          # Health
         PCIP52,          # Business
         PCIP54,          # History
         # Student body demographics
         UGDS_WHITE,      # Percent White undergrads
         UGDS_BLACK,      # Percent Black undergrads
         UGDS_HISP,       # Percent Hispanic undergrads
         UGDS_ASIAN,      # Percent Asian undergrads
         UGDS_AIAN,       # Percent American Indian/Alaska Native undergrads
         UGDS_NHPI,       # Percent Native Hawaiian/Pacific Islander undergrads
         UGDS_2MOR,       # Percent Two or more races undergrads
         UGDS_NRA,        # Percent Non-resident alien undergrads
         UGDS_UNKN,       # Percent Race unknown undergrads
         AGE_ENTRY,       # Average age of entry
         AGEGE24,         # Percent of students age 24 or above
         DEPENDENT,       # Percent dependent students
         # Home zip code demographics
         PCT_WHITE,       # Percent White in home zip codes
         PCT_BLACK,       # Percent Black in home zip codes
         PCT_ASIAN,       # Percent Asian in home zip codes
         PCT_HISPANIC,    # Percent Hispanic in home zip codes
         PCT_BA,          # Percent with Bachelor's in home zip codes
         PCT_GRAD_PROF,   # Percent with graduate degree in home zip codes
         PCT_BORN_US,     # Percent born in US in home zip codes
         MEDIAN_HH_INC,   # Median household income in home zip codes
         POVERTY_RATE,    # Poverty rate in home zip codes
         UNEMP_RATE      # Unemployment rate in home zip codes
  ) %>%
  # Convert columns to numeric explicitly
  mutate(across(-INSTNM, ~as.numeric(as.character(.)))) %>%
  # Handle missing values
  filter(!is.na(SAT_AVG), 
         !is.na(MD_FAMINC), 
         FIRST_GEN != 'PrivacySuppressed',
         !is.na(MD_EARN_WNE_P10)) %>%
  # Add major group percentages as additional features (not replacements)
  mutate(
    STEM_PCT = rowSums(across(all_of(stem_cols)), na.rm = TRUE),
    Business_PCT = rowSums(across(all_of(business_cols)), na.rm = TRUE),
    Healthcare_PCT = rowSums(across(all_of(healthcare_cols)), na.rm = TRUE),
    Liberal_Arts_PCT = rowSums(across(all_of(liberal_arts_cols)), na.rm = TRUE),
    Professional_PCT = rowSums(across(all_of(professional_cols)), na.rm = TRUE)
  )
INSTNM,          # Institution Name
SAT_AVG,         # Average SAT Score
SATVRMID,        # SAT Midpoint Critical Reading
SATMTMID,        # SAT Midpoint Math
ACTCMMID,        # Cumulative ACT Midpoint
FAMINC,          # Family Income
MD_FAMINC,       # Median Family Income
FIRST_GEN,       # Percent First Generation Students
FEMALE,          # Percent Female
MD_EARN_WNE_P10, # Median Income 10 Years After Grad
# All program mix variables
starts_with("PCIP"),
# Student body demographics
UGDS_WHITE,      # Percent White undergrads
UGDS_BLACK,      # Percent Black undergrads
UGDS_HISP,       # Percent Hispanic undergrads
UGDS_ASIAN,      # Percent Asian undergrads
UGDS_AIAN,       # Percent American Indian/Alaska Native undergrads
UGDS_NHPI,       # Percent Native Hawaiian/Pacific Islander undergrads
UGDS_2MOR,       # Percent Two or more races undergrads
UGDS_NRA,        # Percent Non-resident alien undergrads
UGDS_UNKN,       # Percent Race unknown undergrads
AGE_ENTRY,       # Average age of entry
AGEGE24,         # Percent of students age 24 or above
DEPENDENT,       # Percent dependent students
# Home zip code demographics
PCT_WHITE,       # Percent White in home zip codes
PCT_BLACK,       # Percent Black in home zip codes
PCT_ASIAN,       # Percent Asian in home zip codes
PCT_HISPANIC,    # Percent Hispanic in home zip codes
PCT_BA,          # Percent with Bachelor's in home zip codes
PCT_GRAD_PROF,   # Percent with graduate degree in home zip codes
PCT_BORN_US,     # Percent born in US in home zip codes
MEDIAN_HH_INC,   # Median household income in home zip codes
POVERTY_RATE,    # Poverty rate in home zip codes
UNEMP_RATE      # Unemployment rate in home zip codes
) %>%
  # Convert columns to numeric explicitly
  mutate(across(-INSTNM, ~as.numeric(as.character(.)))) %>%
  # Handle missing values
  filter(!is.na(SAT_AVG), 
         !is.na(MD_FAMINC), 
         FIRST_GEN != 'PrivacySuppressed',
         !is.na(MD_EARN_WNE_P10)) %>%
  # Now calculate major group percentages safely
  mutate(
    # Use across() to ensure we're working with numeric data
    STEM_PCT = rowSums(across(all_of(stem_cols)), na.rm = TRUE),
    Business_PCT = rowSums(across(all_of(business_cols)), na.rm = TRUE),
    Healthcare_PCT = rowSums(across(all_of(healthcare_cols)), na.rm = TRUE),
    Liberal_Arts_PCT = rowSums(across(all_of(liberal_arts_cols)), na.rm = TRUE),
    Professional_PCT = rowSums(across(all_of(professional_cols)), na.rm = TRUE)
  )

# Let's verify the data types
print("Structure of major percentages:")
str(select(second_model_scorecard, ends_with("_PCT")))

# Now proceed with the model
# Split into training and testing sets
set.seed(123)
train_index <- createDataPartition(second_model_scorecard$MD_EARN_WNE_P10, p = 0.8, list = FALSE)
train <- second_model_scorecard[train_index, ]
test <- second_model_scorecard[-train_index, ]

# Setup cross-validation
ctrl <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE
)

# Define hyperparameter grid
xgb_grid <- expand.grid(
  nrounds = c(50, 100, 150),
  max_depth = c(3, 5, 7),
  eta = c(0.01, 0.1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

# Train model with cross-validation - keep all features except identifying columns
model_features <- train %>%
  select(-c(INSTNM, UNITID, MD_EARN_WNE_P10))  # Only remove identifier columns and target variable

xgb_cv <- train(
  x = as.matrix(model_features),
  y = train$MD_EARN_WNE_P10,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = xgb_grid,
  verbose = FALSE
)