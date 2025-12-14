## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-packages, message=FALSE---------------------------------------------
# First, let's load PowRPriori and other helpful packages
library(PowRPriori)
library(tidyr) # For creating the means table

## ----define-design------------------------------------------------------------
my_design <- define_design(
  id = "subject",
  between = list(group = c("Control", "Treatment")),
  within = list(time = c("pre", "post"))
)

## ----specify-means------------------------------------------------------------
expected_means <- tidyr::expand_grid(
  group = c("Control", "Treatment"),
  time = c("pre", "post")
)

# Assign the means based on our hypothesis
expected_means$mean_score <- c(50, 52, 50, 60)

knitr::kable(expected_means, caption = "Expected Mean Scores for Each Condition")

## ----get-fixed-effects--------------------------------------------------------
my_fixed_effects <- fixed_effects_from_average_outcome(
  formula = score ~ group * time,
  outcome = expected_means
)

#Note the naming of the coefficients is exactly as `lme4` expects them to be. Do not change these names!
print(my_fixed_effects)

## ----get fixed effects structure----------------------------------------------
get_fixed_effects_structure(formula = score ~ group * time + (1 | subject), design = my_design)


## -----------------------------------------------------------------------------
my_fixed_effects <- list(
   `(Intercept)` = 50,
   groupTreatment = 0,
   timepost = 2,
   `groupTreatment:timepost` = 8
)

## ----get-random-effects-------------------------------------------------------
# This helps us get the correct names
get_random_effects_structure(score ~ group * time + (1|subject), my_design)


## ----specify-random-effects---------------------------------------------------
my_random_effects <- list(
  subject = list(
    `(Intercept)` = 8
  ),
  sd_resid = 12
)

## ----run-simulation, eval=FALSE-----------------------------------------------
# # NOTE: This function can take a few minutes to run.
# # For a real analysis, n_sims should be 1000 or higher (default value is 2000 simulations).
# # We use a low n_sims here for a quick example.
# 
# power_results <- power_sim(
#   formula = score ~ group * time + (1 | subject),
#   design = my_design,
#   fixed_effects = my_fixed_effects,
#   random_effects = my_random_effects,
#   test_parameter = "groupTreatment:timepost",
#   n_start = 30,
#   n_increment = 10,
#   n_sims = 200, # Use >= 1000 for real analysis
#   power_crit = 0.80,
#   alpha = 0.05,
#   parallel_plan = "sequential"
# )

## ----load-results, include=FALSE, eval=TRUE-----------------------------------
# This hidden code block loads the pre-computed results
power_results <- readRDS(
  system.file("extdata", "power_results_vignette.rds", package = "PowRPriori")
)

## ----summary------------------------------------------------------------------
summary(power_results)

## ----plot-power-curve, fig.width=6, fig.height=4------------------------------
plot_sim_model(power_results, type = "power_curve")

## ----fig.width=6, fig.height=4------------------------------------------------
plot_sim_model(power_results, type = "data")

## ----cluster-design 1---------------------------------------------------------
cluster_design <- define_design(
  id = "pupil",
  nesting_vars = list(class = 1:20), # 20 classes
  between = list(
    class = list(intervention = c("yes", "no")) # Intervention at class level
  ),
  within = list(
    time = c("pre", "post")
  )
)

# The rest of the workflow (specifying effects, running power_sim)
# would follow the same logic as the main example.

## ----glmm-example-------------------------------------------------------------
# We expect the Control group to have a 50% pass rate at both times.
# The Treatment group starts at 50% but improves to a 75% pass rate.
glmm_probs <- expand_grid(
  group = c("Control", "Treatment"),
  time = c("pre", "post")
)
glmm_probs$pass_prob <- c(0.50, 0.50, 0.50, 0.75)

# The fixed effects are calculated from these probabilities
glmm_fixed_effects <- fixed_effects_from_average_outcome(
  formula = passed ~ group * time,
  outcome = glmm_probs,
  family = "binomial"
)

# Note: For binomial models, sd_resid is not specified in random_effects. You could also use generate_random_effects_structure again as before.
glmm_random_effects <- list(
  subject = list(Intercept = 2.0)
)

# The power_sim() call would then include `family = "binomial"`.

