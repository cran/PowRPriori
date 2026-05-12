## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("PowRPriori")

## ----load-packages, message=FALSE---------------------------------------------
library(PowRPriori)
library(tidyr)

## ----define-design------------------------------------------------------------
my_design <- define_design(
  sample_size = list(subject = 30), 
  between = list(group = c("Control", "Treatment")),
  within = list(time = c("pre", "post"))
)

## ----define formula-----------------------------------------------------------
my_formula <- score ~ group * time + (1 | subject)

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
  formula = my_formula,
  outcome = expected_means
)

# Note the naming of the coefficients is exactly as `lme4` expects them to be. 
# Do not change these names!
print(my_fixed_effects)

## ----get fixed effects structure----------------------------------------------
get_fixed_effects_structure(formula = my_formula, design = my_design)


## -----------------------------------------------------------------------------
my_fixed_effects <- list(
   `(Intercept)` = 50,
   groupTreatment = 0,
   timepost = 2,
   `groupTreatment:timepost` = 8
)

## ----get-random-effects-------------------------------------------------------
# This helps you get the correct names
get_random_effects_structure(formula = my_formula, design = my_design)


## ----specify-random-effects---------------------------------------------------
my_random_effects <- list(
  subject = list(
    `(Intercept)` = 8
  ),
  sd_resid = 12
)

## ----run-simulation, eval=FALSE-----------------------------------------------
# # NOTE: This function can take a few minutes to run, depending on model complexity.
# 
# power_results <- power_sim(
#   formula = my_formula,
#   design = my_design,
#   fixed_effects = my_fixed_effects,
#   random_effects = my_random_effects,
#   test_parameter = "groupTreatment:timepost",
#   along = "subject",
#   n_increment = 10,
#   n_sims = 200, # Use >= 1000 for real analysis
#   power_crit = 0.80,
#   alpha = 0.05,
#   parallel_plan = "sequential"
# )

## ----load-results, include=FALSE, eval=TRUE-----------------------------------
# This hidden code block loads the pre-computed results
power_results <- readRDS(
  system.file("extdata", "power_results_workflow.rds", package = "PowRPriori")
)

## ----summary------------------------------------------------------------------
summary(power_results)

## ----plot-power-curve, fig.width=6, fig.height=4, fig.dpi=600, out.width="600px", out.height="400px"----
plot_sim_model(power_results, type = "power_curve")

## ----fig.width=6, fig.height=4, fig.dpi=600, out.width="600px", out.height="400px"----
plot_sim_model(power_results, type = "data")

## ----prepare plot data by lme formula-----------------------------------------
plot_design <- define_design(
  sample_size = list(subject = 30), 
  between = list(group = c("Control", "Intervention")),
  within = list(measurement = c("Pre", "Post"))
)

plot_formula <- y ~ group*measurement + (1|subject)

get_fixed_effects_structure(plot_formula, plot_design)

plot_fixed_effects <- list(
  `(Intercept)` = 50,
  groupIntervention  = 0,
  measurementPost = 2,
  `groupIntervention:measurementPost` = 8
)

get_random_effects_structure(plot_formula, plot_design)

plot_random_effects <- list(
  subject = list(
    `(Intercept)` = 8
  ),
  sd_resid = 12
)

## ----plot data by lme, fig.width=6, fig.height=4, fig.dpi=600, out.width="600px", out.height="400px"----
plot_sim_model(plot_formula, 
               type="data", 
               design = plot_design, 
               fixed_effects = plot_fixed_effects, 
               random_effects = plot_random_effects)

## ----cluster-design 1---------------------------------------------------------
cluster_design <- define_design(
  sample_size = list(class = 20, pupil = 20),
  between = list(
    # Intervention is assigned at the class level
    class = list(intervention = c("yes", "no")) 
  ),
  within = list(
    pupil = list(time = c("pre", "post"))
  )
)

## ----crossed design-----------------------------------------------------------
item_design <- define_design(
  sample_size = list(subject = 50, item = 20),
  between = list(
    subject = list(
      condition = c("A", "B")
    )
  )
)

## ----crossed design formula---------------------------------------------------
crossed.formula <- response ~ condition + (1 | subject) + (1 | item)

## ----glmm-example-------------------------------------------------------------
glmm_design <- define_design(
  sample_size = list(subject = 30),
  between = list(group = c("Control", "Treatment")),
  within = list(time = c("pre", "post", "follow-up"))
)

glmm_formula <- passed ~ group * time + (1|subject)

glmm_probs <- expand_grid(
  group = c("Control", "Treatment"),
  time = c("pre", "post", "follow-up")
)
glmm_probs$pass_prob <- c(0.50, 0.50, 0.50, 0.50, 0.75, 0.80)

# The fixed effects are calculated from these probabilities
glmm_fixed_effects <- fixed_effects_from_average_outcome(
  formula = glmm_formula,
  outcome = glmm_probs,
  family = "binomial"
)

#Get random effects
get_random_effects_structure(formula = glmm_formula, design = glmm_design, family = "binomial")

# Note: For binomial (and poisson) models, sd_resid is not specified in random_effects. 
#       You can also use generate_random_effects_structure as detailed before.
glmm_random_effects <- list(
  subject = list(
    `(Intercept)` = 2
  )
)

# The power_sim() call would then include `family = "binomial"` (or `family = "poisson"` 
# if you simulated count data), everything else being the same
# as in the workflow example above.

## ----ICC example, eval=FALSE--------------------------------------------------
# my_icc_design <- define_design(
#   sample_size = list(subject = 30),
#   between = list(group = c("Control", "Treatment")),
#   within = list(time = c("pre", "post"))
# )
# 
# #Only random intercept models work with the ICC specification
# my_icc_formula <- score ~ group * time + (1 | subject)
# 
# get_fixed_effects_structure(formula = my_icc_formula, design = my_icc_design)
# 
# my_icc_fixed_effects <- list(
#    `(Intercept)` = 50,
#    groupTreatment = 0,
#    timepost = 2,
#    `groupTreatment:timepost` = 8
# )
# 
# #The values are defined so they mirror the random effects structure from the detailed example above. ICCs need to always be specified as lists as well.
# iccs <- list(`subject` = 0.4)
# overall_var <- 20
# 
# power_results <- power_sim(
#   formula = score ~ group * time + (1 | subject),
#   design = my_design,
#   fixed_effects = my_fixed_effects,
#   icc_specs = iccs,
#   overall_variance = overall_var,
#   test_parameter = "groupTreatment:timepost",
#   n_increment = 10,
#   n_sims = 200,
#   power_crit = 0.80,
#   alpha = 0.05,
#   parallel_plan = "sequential"
# )

