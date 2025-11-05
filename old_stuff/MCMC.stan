 // Monte Carlo integration over site-year uncertainty for test
 // int n_mc = 50; // You can increase this if needed
 // array[n_test] real mu_test_mc_sum;

  //for (i in 1:n_test)
    //mu_test_mc_sum[i] = 0;

  //for (m in 1:n_mc) {
    //for (i in 1:n_test) {
      //int idx = idx_plant_test[i];
      //int idx_site = idx_plant_test_site[i];
      //int idx_genotype = genotype_plant_test[i];
      //real site_year_sample = normal_rng(0, sigma_site_year);
     // real site_year_sample = 0;

      real mu_base = alpha + dot_product(W[idx, ], beta[idx_genotype, ]) +
                     dot_product(W_soil[idx_site, ], beta[idx_genotype, ]) +
                     beta_0_centered[idx_genotype] +
                     site_year_sample +
                     // beta_neighbors * neighbors_test[i] +
                     // beta_annual * annual_test[i] +
                     // beta_perennial * perennial_test[i] +
                     // beta_shrub * shrub_test[i];

      //if (plot_index_test[i] == 0)
       // mu_test_mc_sum[i] += exp(mu_base);
     // else
     //   mu_test_mc_sum[i] += exp(mu_base + eta_plot_centered[plot_index_test[i]]);
   // }
 // }
   for (i in 1:n_test)
    mu_test[i] = mu_test_mc_sum[i] / n_mc;