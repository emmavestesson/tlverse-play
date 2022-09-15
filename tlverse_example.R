library(data.table)
library(origami)
library(knitr)
library(kableExtra)
library(tidyverse)
library(tmle3)
library(sl3)
library(Rsolnp)
# load data set and take a peek
washb_data <- fread(
  paste0(
    "https://raw.githubusercontent.com/tlverse/tlverse-data/master/",
    "wash-benefits/washb_data.csv"
  ),
  stringsAsFactors = TRUE
)

# specify folds

cluster_ids <- washb_data %>% 
  mutate(fracode = as.character(fracode)) %>% 
  pull(fracode)
folds <- make_folds( cluster_ids = cluster_ids, V = 10)



# Specify variables for model
node_list <- list(
  W = c(
    "month", "aged", "sex", "momage", "momedu",
    "momheight", "hfiacat", "Nlt18", "Ncomp", "watmin",
    "elec", "floor", "walls", "roof", "asset_wardrobe",
    "asset_table", "asset_chair", "asset_khat",
    "asset_chouki", "asset_tv", "asset_refrig",
    "asset_bike", "asset_moto", "asset_sewmach",
    "asset_mobile" 
  ),
  A = "tr",
  Y = "whz", 
  id = "fracode"
)

# Process data (handle missingness)

processed <- process_missing(washb_data, node_list)
washb_data <- processed$data
node_list <- processed$node_list

# Create spec object

ate_spec <- tmle_ATE(
  treatment_level = "Nutrition + WSH",
  control_level = "Control"
)

# Define the learners

# choose base learners
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_rf <- make_learner(Lrnr_ranger)

# define metalearners appropriate to data types


ls_metalearner <- make_learner(Lrnr_nnls)
mn_metalearner <- make_learner(
  Lrnr_solnp, metalearner_linear_multinomial,
  loss_loglik_multinomial, 
  folds = folds
)
sl_Y <- Lrnr_sl$new(
  learners = list(lrnr_mean, lrnr_rf),
  metalearner = ls_metalearner
)
sl_A <- Lrnr_sl$new(
  learners = list(lrnr_mean, lrnr_rf),
  metalearner = mn_metalearner
)


learner_list <- list(A = sl_A, Y = sl_Y)

# fit TMLE

tmle_fit_id <- tmle3(ate_spec, washb_data, node_list, learner_list)
print(tmle_fit_id)
saveRDS(tmle_fit_id, 'tmle_fit_id.rds')
# Evaluate estimates

estimates <- tmle_fit$summary$psi_transformed
print(estimates)



folds_resubstitution(nrow(washb_data))


