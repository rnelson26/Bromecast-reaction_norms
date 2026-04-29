/*  // Full training
for (i in 1:n_train_full) {
  int idx = idx_plant_train_full[i];
  int idx_site = idx_plant_train_site_full[i];
  int g = genotype_plant_train_full[i];
  
  real mu_base = alpha + dot_product(W[idx, ], beta[g]) +
    dot_product(W_soil[idx_site, ], mu_beta_soil)  +
    beta_0_centered[g] +
    site_year_effect_train_scaled_centered[site_year_id_train_full[i]] +
    beta_neighbors * neighbors_train_full[i] +
    beta_annual * annual_train_full[i] +
    beta_perennial * perennial_train_full[i] +
    beta_shrub * shrub_train_full[i];
  
  real mu_final = mu_base + (plot_index_train_full[i] == 0 ? 0 : eta_plot_centered[plot_index_train_full[i]]);
  mu_train_full[i] = exp(mu_final);
  y_train_pred_full[i] = ztnb_rng(mu_train_full[i], theta);
}
// Fixed-effects-only predictions for full training data
for (i in 1:n_train_full) {
  int idx = idx_plant_train_full[i];
  int idx_site = idx_plant_train_site_full[i];
  int g = genotype_plant_train_full[i];
  
  real site_year_noise = normal_rng(0, sigma_site_year);
  real plot_noise =
    (plot_index_train_full[i] == 0)
  ? 0
  : normal_rng(0, sigma_plot);
  
  real mu_base = alpha
  + dot_product(W[idx, ], beta[g])
  + dot_product(W_soil[idx_site, ], mu_beta_soil) 
  + beta_neighbors * neighbors_train_full[i]
  + beta_annual * annual_train_full[i]
  + beta_perennial * perennial_train_full[i] 
  + beta_0_centered[g] +
    // JUSTIN COMMENT, you have a double "+" here. I don't think it effects anything but could to fix 
                    beta_shrub * shrub_train_full[i]  + site_year_noise +
    plot_noise;

     mu_train_full_fixed[i] = exp(mu_base);
     y_train_pred_full_fixed[i] = ztnb_rng(mu_train_full_fixed[i], theta);
    }

  // Full test
    for (i in 1:n_test_full) {
    int idx = idx_plant_test_full[i];
   int idx_site = idx_plant_test_site_full[i];
     int g = genotype_plant_test_full[i];

      real site_year_noise = normal_rng(0, sigma_site_year);
      real mu_base = alpha + dot_product(W[idx, ], beta[g]) +
               dot_product(W_soil[idx_site, ], mu_beta_soil)  +
                beta_0_centered[g] +
               site_year_noise +
               beta_neighbors * neighbors_test_full[i] +
               beta_annual * annual_test_full[i] +
               beta_perennial * perennial_test_full[i] +
                beta_shrub * shrub_test_full[i];
      // JUSTIN COMMENT, I'm not sure if we are treating the plot-level random effects correctly. 
  // let me chat with Mevin and get back to you. 
  real mu_final = mu_base + (plot_index_test_full[i] == 0 ? 0 : eta_plot_centered[plot_index_test_full[i]]);
  // JUSTIN COMMMENT, see above comment about mu_cap 
  mu_test_full[i] = exp(mu_final);
  y_test_pred_full[i] = ztnb_rng(mu_test_full[i], theta);
}
}

*/