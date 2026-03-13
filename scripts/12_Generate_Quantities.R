############### Bromecast: 12. Generate Quantities ##########################
############# created 1-6-26 ######################
############# Last modified: 2-9-26 ##########################
######## Extracts generated quantities in R instead of Stan ################################
########### code by R. Nelson ###############


########### Fecundity #############

# Stan GQ block         | R call                                                  |
  # `mu_train`            | `site_year_mode="conditional", plot_mode="conditional"` |
  # `mu_test`             | `site_year_mode="noise", plot_mode="conditional"`             |
  # `mu_train_fixed`      | `site_year_mode="noise", plot_mode="noise"`             |
 
 # `mu_train_full`       | `site_year_mode="conditional", plot_mode="conditional"` |
  # `mu_train_full_fixed` | `site_year_mode="noise", plot_mode="noise"`             |
  ## mu_test_full         |site_year =  "noise", plot_mode = "conditional          |

###### load packages and data ############

library(cmdstanr)
library(dplyr)


#source("scripts/04_setup.R")
#source("scripts/05_prepare_data.R")
#source("scripts/06_prepare_standata_emg.R")
#source("scripts/07_prepare_standata_rep.R")
#source("scripts/08_prepare_standata_fec.R")



#############  Functions #############


add_scalar <- function(draws, name, d) {
  if (!is.null(draws[[name]])) draws[[name]][d] else 0
}

add_vector <- function(draws, name, d, idx) {
  if (!is.null(draws[[name]])) draws[[name]][d, idx] else 0
}

add_dot <- function(draws, name, d, idx, X) {
  if (!is.null(draws[[name]]) && !is.null(X)) {
    sum(X[idx, ] * draws[[name]][d, ])
  } else 0
}

add_rng <- function(draws, sd_name, d) {
  if (!is.null(draws[[sd_name]])) rnorm(1, 0, draws[[sd_name]][d]) else 0
}

get_data <- function(data, name, i) {
  if (!is.null(data[[name]])) data[[name]][i] else 0
}

### extract draws for any model structure 
extract_draws_generic <- function(fit, verbose = TRUE) {
  
  # stopifnot(q_X == 2)
  #vars <- fit$metadata()$model_params was not getting transformed parameters correctly
  vars <- fit$summary()$variable #same problem
  
  status <- list()
  
  # Flatten arrays consistently across chains
  flatten_array <- function(x) {
    dims <- dim(x)
    if (length(dims) == 4) {
      # iterations x chain x rows x cols -> (iter*chain) x rows x cols
      dim(x) <- c(dims[1] * dims[2], dims[3], dims[4])
      return(x)
    } else if (length(dims) == 3) {
      # iter x chain x rows -> (iter*chain) x rows
      dim(x) <- c(dims[1] * dims[2], dims[3])
      return(x)
    } else if (length(dims) == 2) {
      # already 2D
      return(x)
    } else {
      stop("Unexpected array shape in flatten_array")
    }
  }
  
  # Safe grab of parameter draws
  safe_grab <- function(name) {
    #present <- name %in% vars
    present <- any(startsWith(vars, paste0(name, "["))) || name %in% vars
    
    if (!present) {
      status[[name]] <<- list(
        present = FALSE,
        extracted = FALSE,
        dim = NA,
        note = "Not in model"
      )
      return(NULL)
    }
    
    x <- tryCatch({
      arr <- posterior::as_draws_array(fit$draws(name))
      dims <- dim(arr)
      
      # Flatten if needed
      if (length(dims) == 4) arr <- array(arr, dim = c(dims[1]*dims[2], dims[3], dims[4]))
      if (length(dims) == 3) arr <- array(arr, dim = c(dims[1]*dims[2], dims[3]))
      
      arr
    }, error = function(e) {
      status[[name]] <<- list(
        present = TRUE,
        extracted = FALSE,
        dim = NA,
        note = paste("Extraction error:", e$message)
      )
      return(NULL)
    })
    
    status[[name]] <<- list(
      present = TRUE,
      extracted = !is.null(x),
      dim = if (!is.null(x)) paste(dim(x), collapse = " × ") else NA,
      note = "OK"
    )
    
    x
  }
  
  ## check dimensions
  #normalize_beta <- function(beta, q_X) {
  #  if (is.null(beta)) return(NULL)
  #  d <- dim(beta)
  #  if (length(d) == 3) {
  #    stopifnot(d[3] == q_X)
  #    return(beta)
  #  }
  #  if (length(d) == 2) {
  #    beta <- array(beta, dim = c(d[1], d[2], q_X))
  #    return(beta)
  #  }
  #  stop("Unexpected beta dimensions")
  #}
  
  draws <- list(
    alpha              = safe_grab("alpha"),
    beta_neighbors     = safe_grab("beta_neighbors"),
    beta_annual        = safe_grab("beta_annual"),
    beta_perennial     = safe_grab("beta_perennial"),
    beta_shrub         = safe_grab("beta_shrub"),
    beta               = safe_grab("beta"),
    #beta = normalize_beta(safe_grab("beta"), q_X),
    beta_0             = safe_grab("beta_0_centered"),
    mu_beta_soil       = safe_grab("mu_beta_soil"),
    site_year_effect   = safe_grab("site_year_effect_train_scaled_centered"),
    eta_plot           = safe_grab("eta_plot_centered"),
    sigma_site_year    = safe_grab("sigma_site_year"),
    sigma_plot         = safe_grab("sigma_plot"),
    theta              = safe_grab("theta"),
    W                  = safe_grab("W"),
    W_soil             = safe_grab("W_soil")
    
    #beta             = safe_grab("beta_gq"),
    #beta_0           = safe_grab("beta_0_centered_gq"),
    #site_year_effect = safe_grab("site_year_effect_train_scaled_centered_gq"),
    #eta_plot         = safe_grab("eta_plot_centered_gq")
  )
  ## rehape theta
  if (!is.null(draws$theta)) {
    dims <- dim(draws$theta)
    if (length(dims) == 3) { # iterations × chains × 1
      draws$theta <- array(draws$theta, dim = c(dims[1]*dims[2], dims[3]))
    }
  }
  # --- Automatically detect and reshape W, W_soil, beta ---
  
  q_X <- 2  # known
  
  # reshape W
  if (!is.null(draws$W)) {
    dims <- dim(draws$W)
    n_draws <- dims[1]
    n_X <- dims[2] / q_X
    draws$W <- array(draws$W, dim = c(n_draws, n_X, q_X))
    message("Reshaped W to: ", paste(dim(draws$W), collapse = " × "))
    status$W$dim <- paste(dim(draws$W), collapse = " × ")
  }
  
  # reshape beta
  if (!is.null(draws$beta)) {
    dims <- dim(draws$beta)
    n_draws <- dims[1]
    n_g <- dims[2] / q_X
    draws$beta <- array(draws$beta, dim = c(n_draws, n_g, q_X))
    message("Reshaped beta to: ", paste(dim(draws$beta), collapse = " × "))
    status$beta$dim <- paste(dim(draws$beta), collapse = " × ")
  }
  
  # reshape W_soil  if needed
  if (!is.null(draws$W_soil)) {
    dims <- dim(draws$W_soil)
    n_draws <- dims[1]
    n_X_soil <- dims[2] / q_X
    draws$W_soil <- array(draws$W_soil, dim = c(n_draws, n_X_soil, q_X))
    message("Reshaped W_soil to: ", paste(dim(draws$W_soil), collapse = " × "))
    status$W_soil$dim <- paste(dim(draws$W_soil), collapse = " × ")
  }
  
  
  # Build status table
  status_df <- do.call(rbind, lapply(names(status), function(nm) {
    cbind(param = nm, as.data.frame(status[[nm]], stringsAsFactors = FALSE))
  }))
  rownames(status_df) <- NULL
  
  if (verbose) {
    message("✔ Draw extraction summary:")
    print(status_df)
  }
  
  list(draws = draws, status = status_df)
}


## predict function for fecundity model of any structure

predict_ztnb_universal <- function(
    draws, data, 
    site_year_mode = c("conditional", "noise", "none"),
    plot_mode      = c("conditional", "noise", "none"),
    mu_cap = 10,
    report = TRUE
) {
  
  site_year_mode <- match.arg(site_year_mode)
  plot_mode      <- match.arg(plot_mode)
  
  used <- logical(0)
  mark_used <- function(name) used[name] <<- TRUE
  
  n_draws <- length(draws$alpha)
  #n_obs   <- data$n_obs
  n_obs <- nrow(data)
  
  
  mu_mat <- matrix(NA_real_, n_draws, n_obs)
  y_mat  <- matrix(NA_integer_, n_draws, n_obs)
  
  if (report && !is.null(draws$beta) && !is.null(draws$W)) {
    message(
      "q_X = ", ncol(draws$W),
      " | beta dim: ",
      paste(dim(draws$beta), collapse = " × "),
      " | W dim: ",
      paste(dim(draws$W), collapse = " × ")
    )
  }
  
  
  for (d in seq_len(n_draws)) {
    for (i in seq_len(n_obs)) {
      

      lp <- draws$alpha[d]
      mark_used("alpha")
      

      if (!is.null(draws$beta) && !is.null(draws$W)) {
        g <- data$genotype[i]
          lp <- lp + sum(draws$W[d, data$idx_plant[i], ] * draws$beta[d, g, ])
        mark_used("beta")
      }
      

      if (!is.null(draws$beta_0)) {
        lp <- lp + draws$beta_0[d, data$genotype[i]]
        mark_used("beta_0")
      }
      

      if (!is.null(draws$mu_beta_soil) && !is.null(draws$W_soil)) {
        lp <- lp + sum(
          draws$W_soil[d, data$idx_plant_site[i], ] *
            draws$mu_beta_soil[d, ]
        )
        mark_used("mu_beta_soil")
      }
      
  
      if (!is.null(draws$beta_neighbors)) {
        lp <- lp +
          draws$beta_neighbors[d] * get_data(data, "neighbors", i)
        mark_used("beta_neighbors")
      }
      
      if (!is.null(draws$beta_annual)) {
        lp <- lp +
          draws$beta_annual[d] * get_data(data, "annual", i)
        mark_used("beta_annual")
      }
      
      if (!is.null(draws$beta_perennial)) {
        lp <- lp +
          draws$beta_perennial[d] * get_data(data, "perennial", i)
        mark_used("beta_perennial")
      }
      
      if (!is.null(draws$beta_shrub)) {
        lp <- lp +
          draws$beta_shrub[d] * get_data(data, "shrub", i)
        mark_used("beta_shrub")
      }
      

      if (site_year_mode == "conditional" &&
          !is.null(draws$site_year_effect)) {
        
        lp <- lp + draws$site_year_effect[d, data$site_year_id[i]]
        mark_used("site_year_effect")
      }
      
      if (site_year_mode == "noise" &&
          !is.null(draws$sigma_site_year)) {
        
        lp <- lp + rnorm(1, 0, draws$sigma_site_year[d])
        mark_used("sigma_site_year")
      }
      
      if (!is.null(data$plot_index) && data$plot_index[i] != 0) {
        
        if (plot_mode == "conditional" &&
            !is.null(draws$eta_plot)) {
          
          lp <- lp + draws$eta_plot[d, data$plot_index[i]]
          mark_used("eta_plot")
        }
        
        if (plot_mode == "noise" &&
            !is.null(draws$sigma_plot)) {
          
          lp <- lp + rnorm(1, 0, draws$sigma_plot[d])
          mark_used("sigma_plot")
        }
      }
      
      mu <- exp(pmin(lp, mu_cap))
      mu_mat[d, i] <- mu
      
      if (!is.null(draws$theta)) {
       # y_mat[d, i] <- rztnegbin(1, mu, draws$theta[d])
        y_mat[d, i] <- rztnegbin(1, mu, as.numeric(draws$theta[d, ]))
        mark_used("theta")
      } else {
        y_mat[d, i] <- mu
      }
    }
  }

  out <- list(mu = mu_mat, y = y_mat)
  
  if (report) {
    out$used_effects <- sort(names(used))
  }
  
  out
}

