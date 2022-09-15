library(tmle3)
library(sl3)


data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
cpp$parity01 <- as.numeric(cpp$parity > 0)
cpp[is.na(cpp)] <- 0
cpp$haz01 <- as.numeric(cpp$haz > 0)

#  define nodes
npsem <- list(
  define_node("W", c(
    "apgar1", "apgar5", "gagebrth", "mage",
    "meducyrs", "sexn"
  )),
  define_node("A", c("parity01"), c("W")),
  define_node("Y", c("haz01"), c("A", "W")), 
  define_node("id", c("subjid"), c("A", "W"))
)


tmle_task <- tmle3_Task$new(cpp, npsem = npsem)



# set up sl3 learners for tmle3 fit
lrnr_glm_fast <- make_learner(Lrnr_glm_fast)
lrnr_mean <- make_learner(Lrnr_mean)

# define and fit likelihood
factor_list <- list(
  define_lf(LF_emp, "W"),
  define_lf(LF_fit, "A", lrnr_glm_fast),
  define_lf(LF_fit, "Y", lrnr_glm_fast, type="mean")
)

likelihood_def <- Likelihood$new(factor_list)
likelihood <- likelihood_def$train(tmle_task)


# intervention where we set all treatment A=1
intervention <- define_lf(LF_static, "A", value = 1)

# make a counterfactual likelihood

cf_likelihood <- make_CF_Likelihood(likelihood, intervention)


#  update procedure


tsm <- define_param(Param_TSM, likelihood, intervention)


updater <- tmle3_Update$new()
targeted_likelihood <- Targeted_Likelihood$new(likelihood, updater)


#  specify target parameter
tsm <- define_param(Param_TSM, likelihood, intervention)




tmle_fit <- fit_tmle3(tmle_task, targeted_likelihood, tsm, updater)
