##########################
## 0) Load Required Packages
##########################
#install.packages(c("lme4", "broom", "dplyr", "ggforce", "broom.mixed"))
#install.packages("devtools")
#devtools::install_github("NightingaleHealth/ggforestplot")

library(lme4)         # for glmer (if you need to fit the models here)
library(broom)        # for tidy()
library(dplyr)        # data wrangling
library(ggforestplot) # forestplot
library(ggforce)      # facet_col for multi-facet layout

##########################
## 1) Fit or Load Your Models
##########################
## If you've already fitted model_men and model_women, skip this chunk.

# Example placeholders (replace with your actual code and data):
# model_men <- glmer(
#   depressed ~ as.numeric(age) + ... + (1 | Household), 
#   data = your_data_men,
#   family = binomial(link = "logit")
# )
#
# model_women <- glmer(
#   depressed ~ as.numeric(age) + ... + (1 | Household), 
#   data = your_data_women,
#   family = binomial(link = "logit")
# )

load("../Code/Results/model_not_relative_men_appendix.rda")

load("../Code/Results/model_not_relative_women_appendix.rda")

##########################
## 2) Tidy Both Models
##########################
glm_model_men   <- broom::tidy(model_men_not_relative_appendix)
glm_model_women <- broom::tidy(model_women_not_relative_appendix)


# Each is a tibble with: effect, group, term, estimate, std.error, statistic, p.value

##########################
## 3) Rename Terms & Add a Gender Column
##########################
results_model_log_ft_men <- glm_model_men %>%
  mutate(
    variables = case_when(
      term == "(Intercept)"              ~ "Intercept",
      term == "as.numeric(age)"          ~ "Age",
      term == "marital_status"           ~ "Marital status (ref. not married nor in a civil union)",
      term == "friends"                   ~ "Number of friends (degree)",
      term == "avg_neighbors_degree"      ~  "Average degree of friends",
      term == "percentage_depressed"      ~ "% Of friends w/depression",
      term == "intransitivity_values"    ~ "Social intransitivity",
      term == "Adversaries"             ~ "Number of adversaries (degree)",
      term == "religion2"                    ~ "Catholic religion (ref. no religion or other religion)",
      term == "religion1"                    ~ "Protestant religion (ref. no religion or other religion)",
      term == "education"                    ~ "Formal education (years)",
      term == "FI"                       ~ "Food insecurity (ref. has sufficient food)",
      term == "avg_household"         ~ "% Household members w/ depression",
      term == "indigenous_status"     ~ "Ethnicity (ref. not indigenous)",
      term == "network_size"             ~ "Village population",
      term == "access_routes"            ~ "Village isolation (# access routes)",
      term == "network_density"          ~ "Village friend network density",
      term == "Adv_density"              ~ "Village adversary network density",
      term == "number_household"         ~ "Number of household members",
      #term == "household_wealth"                   ~ "Household Wealth",
      #term == "individual_wealth"        ~ "Individual Wealth",
      TRUE ~ term
    ),
    Gender = "Men"  # Label these rows as Men
  )

results_model_log_ft_women <- glm_model_women %>%
  mutate(
    variables = case_when(
      term == "(Intercept)"              ~ "Intercept",
      term == "as.numeric(age)"          ~ "Age",
      term == "marital_status"           ~ "Marital status (ref. not married nor in a civil union)",
      term == "friends"                   ~ "Number of friends (degree)",
      term == "avg_neighbors_degree"      ~  "Average degree of friends",
      term == "percentage_depressed"      ~ "% Of friends w/depression",
      term == "intransitivity_values"    ~ "Social intransitivity",
      term == "Adversaries"             ~ "Number of adversaries (degree)",
      term == "religion2"                    ~ "Catholic religion (ref. no religion or other religion)",
      term == "religion1"                    ~ "Protestant religion (ref. no religion or other religion)",
      term == "education"                    ~ "Formal education (years)",
      term == "FI"                       ~ "Food insecurity (ref. has sufficient food)",
      term == "avg_household"         ~ "% Household members w/ depression",
      term == "indigenous_status"     ~ "Ethnicity (ref. not indigenous)",
      term == "network_size"             ~ "Village isolation (# access routes)",
      term == "access_routes"            ~ "Access routes",
      term == "network_density"          ~ "Village friend network density",
      term == "Adv_density"              ~ "Village adversary network density",
      term == "number_household"         ~ "Number of household members",
      #term == "household_wealth"                   ~ "Household Wealth",
      #term == "individual_wealth"        ~ "Individual Wealth",
      TRUE ~ term
    ),
    Gender = "Women"  # Label these rows as Women
  )

##########################
## 4) Combine the Two Tidy Data Frames
##########################
results_model_log_ft <- bind_rows(
  results_model_log_ft_men,
  results_model_log_ft_women
)

##########################
## 5) Define Groupings
##########################
groupings <- list(
  Individual = c(
    "Age",
    "Marital status (ref. not married nor in a civil union)",
    "Formal education (years)",
    "Catholic religion (ref. no religion or other religion)",
    "Protestant religion (ref. no religion or other religion)",
    "Food insecurity (ref. has sufficient food)",
    "Number of Depressed relatives in Household",
    "Ethnicity (ref. not indigenous)"
    #"Individual Wealth"
  ),
  Network = c(
    "Number of friends (degree)",
    "Average degree of friends",
    "% Of friends w/depression",
    "Difference between Friends",
    "Number of adversaries (degree)",
    "Social intransitivity",
    "Village friend network density",
    "Village adversary network density"
    
  ),
  Household = c(
    "Number of household members",
    "% Household members w/ depression"
  ),
  Village = c(
    "Village population",
    "Village isolation (# access routes)"
  )
)

##########################
## 6) Create a Custom forestplot_fx Function
##########################
forestplot_fx <- function(df, groupings, plot_title) {
  # 1) Add a `groupings` column and remove intercept if desired
  df <- df %>%
    mutate(
      groupings = case_when(
        variables %in% groupings$Individual ~ "Individual",
        variables %in% groupings$Network    ~ "Network Properties",
        variables %in% groupings$Household  ~ "Household Properties",
        variables %in% groupings$Village    ~ "Village Characteristics",
        TRUE ~ NA_character_
      )
    ) %>%
    # Remove intercept and anything that doesn't match a grouping
    filter(
      variables != "Intercept",
      !is.na(groupings)
    ) %>%
    # Factor the grouping to control the facet order
    mutate(
      groupings = factor(
        groupings,
        levels = c("Individual",
                   "Network Properties",
                   "Household Properties",
                   "Village Characteristics")
      )
    )
  
  # 2) Make the forest plot
  ggforestplot::forestplot(
    df       = df,
    name     = variables,
    estimate = estimate,
    se       = std.error,
    pvalue   = p.value,
    psignif  = 0.05,
    title    = plot_title,
    # Color by Gender
    colour   = Gender,
    logodds  = TRUE
  ) +
    # Define your own color scheme for men/women
    scale_color_manual(values = c("Men" = "green", "Women" = "blue")) +
    ggforce::facet_col(
      facets = ~ groupings,
      scales = "free_y",
      space  = "free"
    )
}

##########################
## 7) Plot Men & Women Together
##########################
plot_title <- "Mixed Effects: Individual Level Model (Men vs. Women)"
forest_mod1 <- forestplot_fx(results_model_log_ft, groupings, plot_title)

# Display the plot
forest_mod1



print(forest_mod1)  # <-- Explicitly print the ggplot



dev.off()

