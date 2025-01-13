# Load required packages
library(tidyverse)
library(caret)
library(ranger)  # For random forest
library(xgboost)  # For xgboost
library(kernlab)  # For SVM
library(nnet)     # For neural networks

# Feature descriptions
feature_descriptions <- list(
  # Basic institutional characteristics
  UNITID = "Unique identifier for institution",
  INSTNM = "Institution name",
  
  # Academic preparation metrics
  SAT_AVG = "Average SAT equivalent score (includes converted ACT scores)",
  SATVRMID = "SAT Verbal/Critical Reading midpoint",
  SATMTMID = "SAT Math midpoint",
  ACTCMMID = "ACT Composite midpoint",
  
  # Family background
  FAMINC = "Average family income",
  MD_FAMINC = "Median family income",
  FIRST_GEN = "Percentage of first-generation college students",
  
  # Student body characteristics
  FEMALE = "Percentage of female students",
  AGE_ENTRY = "Average age of entry",
  AGEGE24 = "Percentage of students 24 or older",
  DEPENDENT = "Percentage of dependent students",
  
  # Outcome variable
  MD_EARN_WNE_P10 = "Median earnings of students 10 years after entry (working, not enrolled)",
  
  # Program mix (PCIP) variables
  PCIP01 = "Percentage of degrees awarded in Agriculture",
  PCIP03 = "Percentage of degrees awarded in Natural Resources",
  PCIP04 = "Percentage of degrees awarded in Architecture",
  PCIP05 = "Percentage of degrees awarded in Area Studies",
  PCIP09 = "Percentage of degrees awarded in Communication",
  PCIP10 = "Percentage of degrees awarded in Communications Technology",
  PCIP11 = "Percentage of degrees awarded in Computer Science",
  PCIP12 = "Percentage of degrees awarded in Personal/Culinary Services",
  PCIP13 = "Percentage of degrees awarded in Education",
  PCIP14 = "Percentage of degrees awarded in Engineering",
  PCIP15 = "Percentage of degrees awarded in Engineering Technology",
  PCIP16 = "Percentage of degrees awarded in Foreign Languages",
  PCIP19 = "Percentage of degrees awarded in Family/Consumer Sciences",
  PCIP22 = "Percentage of degrees awarded in Legal Professions",
  PCIP23 = "Percentage of degrees awarded in English Language/Literature",
  PCIP24 = "Percentage of degrees awarded in Liberal Arts/Sciences",
  PCIP25 = "Percentage of degrees awarded in Library Science",
  PCIP26 = "Percentage of degrees awarded in Biological Sciences",
  PCIP27 = "Percentage of degrees awarded in Mathematics/Statistics",
  PCIP29 = "Percentage of degrees awarded in Military Technologies",
  PCIP30 = "Percentage of degrees awarded in Multi/Interdisciplinary Studies",
  PCIP31 = "Percentage of degrees awarded in Parks/Recreation/Leisure",
  PCIP38 = "Percentage of degrees awarded in Philosophy/Religious Studies",
  PCIP39 = "Percentage of degrees awarded in Theology/Religious Vocations",
  PCIP40 = "Percentage of degrees awarded in Physical Sciences",
  PCIP41 = "Percentage of degrees awarded in Science Technologies",
  PCIP42 = "Percentage of degrees awarded in Psychology",
  PCIP43 = "Percentage of degrees awarded in Homeland Security/Law Enforcement",
  PCIP44 = "Percentage of degrees awarded in Public Administration",
  PCIP45 = "Percentage of degrees awarded in Social Sciences",
  PCIP46 = "Percentage of degrees awarded in Construction Trades",
  PCIP47 = "Percentage of degrees awarded in Mechanic/Repair Technologies",
  PCIP48 = "Percentage of degrees awarded in Precision Production",
  PCIP49 = "Percentage of degrees awarded in Transportation",
  PCIP50 = "Percentage of degrees awarded in Visual/Performing Arts",
  PCIP51 = "Percentage of degrees awarded in Health Professions",
  PCIP52 = "Percentage of degrees awarded in Business/Management",
  PCIP54 = "Percentage of degrees awarded in History",
  
  # Student body demographics
  UGDS_WHITE = "Percentage of undergraduate students who are White",
  UGDS_BLACK = "Percentage of undergraduate students who are Black",
  UGDS_HISP = "Percentage of undergraduate students who are Hispanic",
  UGDS_ASIAN = "Percentage of undergraduate students who are Asian",
  UGDS_AIAN = "Percentage of undergraduate students who are American Indian/Alaska Native",
  UGDS_NHPI = "Percentage of undergraduate students who are Native Hawaiian/Pacific Islander",
  UGDS_2MOR = "Percentage of undergraduate students who are Two or More Races",
  UGDS_NRA = "Percentage of undergraduate students who are Non-Resident Aliens",
  UGDS_UNKN = "Percentage of undergraduate students whose race/ethnicity is Unknown",
  
  # Home zip code characteristics
  PCT_WHITE = "Percentage of population in students' home zip codes who are White",
  PCT_BLACK = "Percentage of population in students' home zip codes who are Black",
  PCT_ASIAN = "Percentage of population in students' home zip codes who are Asian",
  PCT_HISPANIC = "Percentage of population in students' home zip codes who are Hispanic",
  PCT_BA = "Percentage of population in students' home zip codes with Bachelor's degree",
  PCT_GRAD_PROF = "Percentage of population in students' home zip codes with Graduate/Professional degree",
  PCT_BORN_US = "Percentage of population in students' home zip codes born in US",
  MEDIAN_HH_INC = "Median household income in students' home zip codes",
  POVERTY_RATE = "Poverty rate in students' home zip codes",
  UNEMP_RATE = "Unemployment rate in students' home zip codes"
)

# Read and filter data
CollegeScorecard <- read_csv("CollegeScorecard.csv")

# First filter the data
filtered_scorecard <- CollegeScorecard %>%
  filter(PREDDEG == 3,  # Bachelor's degree granting
         CCUGPROF != 0, # Remove unclassified
         CCUGPROF != -2) # Remove graduate-only

# Create second model scorecard with specific variable selection
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
         # Student body demographics - specific selection to avoid old categories
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
  # Convert columns to numeric
  mutate(across(-INSTNM, ~as.numeric(as.character(.)))) %>%
  # Handle missing values
  drop_na()

# Split into train/test sets
set.seed(123)
train_index <- createDataPartition(second_model_scorecard$MD_EARN_WNE_P10, p = 0.8, list = FALSE)
train <- second_model_scorecard[train_index, ]
test <- second_model_scorecard[-train_index, ]

# Set up cross-validation
ctrl <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE
)

# Train multiple models
# 1. Linear Model
lm_model <- train(
  MD_EARN_WNE_P10 ~ .,
  data = train %>% select(-c(INSTNM, UNITID)),
  method = "lm",
  trControl = ctrl
)

# 2. Random Forest with importance enabled
rf_model <- train(
  MD_EARN_WNE_P10 ~ .,
  data = train %>% select(-c(INSTNM, UNITID)),
  method = "ranger",
  trControl = ctrl,
  importance = "impurity"
)

# 3. XGBoost
xgb_model <- train(
  MD_EARN_WNE_P10 ~ .,
  data = train %>% select(-c(INSTNM, UNITID)),
  method = "xgbTree",
  trControl = ctrl
)

# 4. Neural Network
nnet_model <- train(
  MD_EARN_WNE_P10 ~ .,
  data = train %>% select(-c(INSTNM, UNITID)),
  method = "nnet",
  trControl = ctrl,
  trace = FALSE
)

# Compare models
model_list <- list(
  LinearModel = lm_model,
  RandomForest = rf_model,
  XGBoost = xgb_model,
  NeuralNet = nnet_model
)

# Get comparison of model performance
model_results <- resamples(model_list)
summary(model_results)

# Plot model comparison
bwplot(model_results)

# Get variable importance from random forest
rf_importance <- varImp(rf_model)
plot(rf_importance, top = 20, main = "Random Forest - Top 20 Important Variables")

# Get variable importance from XGBoost
xgb_importance <- varImp(xgb_model)
plot(xgb_importance, top = 20, main = "XGBoost - Top 20 Important Variables")

# Make predictions on both sets
train$predicted_earnings <- predict(xgb_model, newdata = train %>% select(-c(INSTNM, UNITID)))
train$value_add <- train$MD_EARN_WNE_P10 - train$predicted_earnings
train$dataset <- "Training"

test$predicted_earnings <- predict(rf_model, newdata = test %>% select(-c(INSTNM, UNITID)))
test$value_add <- test$MD_EARN_WNE_P10 - test$predicted_earnings
test$dataset <- "Test"

# Combine datasets and show top 20 overall
combined_results <- bind_rows(train, test) %>%
  select(INSTNM, MD_EARN_WNE_P10, predicted_earnings, value_add, dataset) %>%
  arrange(desc(value_add)) %>%
  # head(20) %>%
  mutate(
    MD_EARN_WNE_P10 = round(MD_EARN_WNE_P10, 0),
    predicted_earnings = round(predicted_earnings, 0),
    value_add = round(value_add, 0)
  ) %>%
  rename(
    "Institution" = INSTNM,
    "Actual_Earnings" = MD_EARN_WNE_P10,
    "Predicted_Earnings" = predicted_earnings,
    "Value_Add" = value_add,
    "Dataset" = dataset
  )

print(combined_results)