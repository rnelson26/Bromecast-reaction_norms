############### Bromecast: 12.2 Run Generate Quantities for fecundity ##########################
############# created 1-6-26 ######################
############# Last modified: 2-9-26 ##########################
######## Extracts generated quantities in R instead of Stan ################################
########### code by R. Nelson ###############

## Running generated quantities for model: full
#→ train
#Error in mu_mat[d, i] <- mu : replacement has length zero

## source other scripts as well
source("scripts/12_Generate_Quantities.R")


seed_table <- data.frame(
  model = c(
    "full", "full", "full", "full", "full", "full"
  ),
  quantity = c(
    "train", "test", "train_fixed",
    "train_full", "train_full_fixed", "test_full"
  ),
  seed = c(
    10101, 10102, 10103,
    10104, 10105, 10106
  ),
  stringsAsFactors = FALSE
)

get_seed <- function(model, quantity) {
  seed_table$seed[
    seed_table$model == model &
      seed_table$quantity == quantity
  ]
}


## load in models 
fecundity_models <- c(
  full       = "output/fit_fecundity_full.rds",
 # climate    = "output/fit_fecundity_climate.rds",
  nogene     = "output/fit_fecundity_nogene.rds",
  nocomp     = "output/fit_fecundity_nocomp.rds",
  nointer    = "output/fit_fecundity_nointer.rds",
  null       = "output/fit_fecundity_null.rds"
)

##### load in any existing models #####
load_fecundity_fits <- function(model_paths) {
  
  fits <- list()
  
  for (nm in names(model_paths)) {
    path <- model_paths[[nm]]
    
    if (!file.exists(path)) {
      message("⚠ Skipping ", nm, " (file not found)")
      next
    }
    
    message("✔ Loading ", nm)
    fits[[nm]] <- readRDS(path)
  }
  
  fits
}

fec_fits <- load_fecundity_fits(fecundity_models)

###### Extract draws with diagnostics ########
extract_all_fecundity_draws <- function(fits, verbose = TRUE) {
  
  draws <- list()
  reports <- list()
  
  for (nm in names(fits)) {
    
    message("🔍 Extracting draws: ", nm)
    
    ext <- extract_draws_generic(fits[[nm]], verbose = verbose)
    
    draws[[nm]]   <- ext$draws
    reports[[nm]] <- ext$status
  }
  
  list(
    draws   = draws,
    reports = reports
  )
}

#extract_all_fecundity_draws <- function(fits, q_X, verbose = TRUE) {
  
 # draws <- list()
  #reports <- list()
  
  #for (nm in names(fits)) {
    
  #  message("🔍 Extracting draws: ", nm)
    
    # get q_X
   # ext <- extract_draws_generic(fits[[nm]], q_X = q_X, verbose = verbose)
    
    #draws[[nm]]   <- ext$draws
    #reports[[nm]] <- ext$status
  #}
  
  #list(
   # draws   = draws,
  #  reports = reports
  #)
#}


fec_extracted <- extract_all_fecundity_draws(fec_fits)
#fec_extracted <- extract_all_fecundity_draws(
 # fec_fits,
  #q_X = 2
#)


fec_draws_by_model <- fec_extracted$draws
fec_safety_reports <- fec_extracted$reports



######## 6 types of Generated Quantities ########

gq_specs <- list(
  train = list(
    data = training_df,
    site_year_mode = "conditional",
    plot_mode      = "conditional"
  ),
  test = list(
    data = testing_df,
    site_year_mode = "noise",
    plot_mode      = "conditional"
  ),
  train_fixed = list(
    data = training_df,
    site_year_mode = "noise",
    plot_mode      = "noise"
  ),
  train_full = list(
    data = training_df_emg,
    site_year_mode = "conditional",
    plot_mode      = "conditional"
  ),
  train_full_fixed = list(
    data = training_df_emg,
    site_year_mode = "noise",
    plot_mode      = "noise"
  ),
  test_full = list(
    data = testing_df_emg,
    site_year_mode = "noise",
    plot_mode      = "conditional"
  )
)

## only use parameters that exist in the model 
safe_draws <- function(draws) {
  list(
    alpha              = draws$alpha,
    
    beta               = draws$beta,
    beta_0             = draws$beta_0,
    mu_beta_soil       = draws$mu_beta_soil,
    
    beta_neighbors     = draws$beta_neighbors,
    beta_annual        = draws$beta_annual,
    beta_perennial     = draws$beta_perennial,
    beta_shrub         = draws$beta_shrub,
    
    site_year_effect   = draws$site_year_effect,
    eta_plot           = draws$eta_plot,
    
    sigma_site_year    = draws$sigma_site_year,
    sigma_plot         = draws$sigma_plot,
    
    theta              = draws$theta,
    W = draws$W,           
    W_soil = draws$W_soil  
  )
}



##### Single Model ####
run_fecundity_gqs <- function(
    model_name,
    draws,
    gq_specs,
    env = parent.frame(),
    mu_cap = 10,
    report = TRUE
) {
  
  if (report) {
    message("Running generated quantities for model: ", model_name)
  }
  
  dlist <- safe_draws(draws)
  out <- list()
  
  for (gq_name in names(gq_specs)) {
    
    spec <- gq_specs[[gq_name]]
    #data_obj <- eval(spec$data, env)
    data_obj <- spec$data
    
    
    if (report) {
      message("  → ", gq_name)
    }
    
    set.seed(get_seed(model_name, gq_name))
    
    out[[gq_name]] <- predict_ztnb_universal(
      draws = dlist,
      data  = data_obj,
      site_year_mode = spec$site_year_mode,
      plot_mode      = spec$plot_mode,
      mu_cap = mu_cap,
      report = FALSE
    )
  }
  
  out
}



##### Multiple Models #######
run_all_fecundity_models <- function(
    fec_draws_list,
    gq_specs,
    env = parent.frame()
) {
  
  results <- list()
  
  for (model_name in names(fec_draws_list)) {
    
    results[[model_name]] <- run_fecundity_gqs(
      model_name = model_name,
      draws      = fec_draws_list[[model_name]],
      gq_specs   = gq_specs,
      env        = env
    )
  }
  
  results
}



####### Examples #######
# one model
gqs_full <- run_fecundity_gqs(
  model_name = "full",
  draws = fec_draws_by_model$full,
  gq_specs = gq_specs
)



# several models
gqs_some <- run_all_fecundity_models(
  fec_draws_list = list(
    full = fec_draws_full,
    null = fec_draws_null
  ),
  gq_specs = gq_specs
)

# all the models
#gqs_all <- run_all_fecundity_models(
 # fec_draws_list = fec_draws_by_model,
#  gq_specs = gq_specs
#)

gqs_all <- lapply(names(fec_draws_by_model), function(m) {
  run_fecundity_gqs(
    model_name = m,
    draws = fec_draws_by_model[[m]],
    gq_specs = gq_specs
  )
})
names(gqs_all) <- names(fec_draws_by_model)

## checks 
#used <- gqs_all$full$train$used_effects
#report <- fec_safety_reports$full

#report$used_in_prediction <- report$param %in% used
#print(report)

