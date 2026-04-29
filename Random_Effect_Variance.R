
rm(list = setdiff(ls(), keep))
gc()
path <- "/Users/Becca/Desktop/Adler Lab/Bromecast-reaction_norms/output"
fit <- readRDS(file.path(path, "fit_emerged_full.rds"))

draws <- fit$draws(variables = c("sigma_plot", "sigma_transect"),
                                format = "df")
draws$var_plot <- draws$sigma_plot^2
draws$var_transect <- draws$sigma_transect^2

summary_stats <- data.frame(
  parameter = c("plot_variance", "transect_variance"),
  mean = c(mean(draws$var_plot), mean(draws$var_transect)),
  sd   = c(sd(draws$var_plot), sd(draws$var_transect)),
  q2.5 = c(quantile(draws$var_plot, 0.025),
           quantile(draws$var_transect, 0.025)),
  q97.5 = c(quantile(draws$var_plot, 0.975),
            quantile(draws$var_transect, 0.975))
)

print(summary_stats)

## rhat
#sum <- fit$summary()

#overall_rhat <- c(
 # max_rhat = max(sum$rhat, na.rm = TRUE),
  #median_rhat = median(sum$rhat, na.rm = TRUE),
  #prop_below_1.01 = mean(sum$rhat < 1.01, na.rm = TRUE)
#)

#overall_rhat