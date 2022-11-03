library(tidyverse)

test_college_data <- CollegeScorecard %>%
  dplyr::select(UNITID,          # Institution ID Number
                INSTNM,          # Institution Name
                SAT_AVG,         # Average SAT Overall
                SAT_AVG_ALL,     # SAT Average by Ope ID
                SATVRMID,        # SAT Midpoint Critical Reading
                SATMTMID,        # SAT Midpoint Math
                SATWRMID,        # SAT Midpoint Writing
                ACTCMMID,        # Cumulative ACT Midpoint
                FAMINC,          # Family Income
                MD_FAMINC,       # Median Family Income
                FIRST_GEN,       # Percent First Generation Students
                FEMALE,          # Percent Female
                MD_EARN_WNE_P10,  # Median Income 10 Years After Grad
#                MN_EARN_WNE_P10,  # Mean Income 10 Years After Grad
                PCIP01,
                PCIP03,
                PCIP04,
                PCIP05,
                PCIP09,
                PCIP10,
                PCIP11,
                PCIP12,
                PCIP13,
                PCIP14,
                PCIP15,
                PCIP16,
                PCIP19,
                PCIP22,
                PCIP23,
                PCIP24,
                PCIP25,
                PCIP26,
                PCIP27,
                PCIP29,
                PCIP30,
                PCIP31,
                PCIP38,
                PCIP39,
                PCIP40,
                PCIP41,
                PCIP42,
                PCIP43,
                PCIP44,
                PCIP45,
                PCIP46,
                PCIP47,
                PCIP48,
                PCIP49,
                PCIP50,
                PCIP51,
                PCIP52,
                PCIP54,
                UGDS_WHITE,
                UGDS_BLACK,
                UGDS_HISP,
                UGDS_ASIAN,
                UGDS_AIAN,
                UGDS_NHPI,
                UGDS_2MOR,
                UGDS_NRA,
                UGDS_UNKN,
                AGE_ENTRY,
                AGEGE24,
                DEPENDENT,
#                VETERAN,
                PCT_WHITE,
                PCT_BLACK,
                PCT_ASIAN,
                PCT_HISPANIC,
                PCT_BA,
                PCT_GRAD_PROF,
                PCT_BORN_US,
                MEDIAN_HH_INC,
                POVERTY_RATE,
                UNEMP_RATE
                ) %>%
  mutate_at(vars(-("INSTNM")), as.numeric) %>%
  drop_na()


#define intercept-only model
intercept_only <- lm(MD_EARN_WNE_P10 ~ 1 - UNITID - INSTNM, data=test_college_data)

#define model with all predictors
all <- lm(MD_EARN_WNE_P10 ~ . - UNITID - INSTNM, data=test_college_data)

#perform backward stepwise regression
backward <- step(all, direction='backward', scope=formula(all), trace=0)

#view results of backward stepwise regression
backward$anova
str(test_college_data)

# Plot actual versus predicted
plot(predict(backward),                                # Draw plot using Base R
     test_college_data$MD_EARN_WNE_P10,
     xlab = "Predicted Values",
     ylab = "Observed Values")
abline(a = 0,                                        # Add straight line
       b = 1,
       col = "red",
       lwd = 2)

# New DataSet Showing Predicted And Actual

modeled_results <- cbind(test_college_data,predict(backward)) %>%
  rename("modeled_salary" = "predict(backward)") %>%
  mutate(value_add =MD_EARN_WNE_P10 - modeled_salary) %>%
  select(UNITID,INSTNM,MD_EARN_WNE_P10,modeled_salary,value_add)