### Extracting contents of stanfit object ###

test_fit <- res_covid[[1]][[1]]$fit

# extract() - returns a list with named components corresponding to the model params
list_of_draws <- extract(test_fit)
print(names(list_of_draws))

# as.matrix, as.data.frame and as.array can be used to retrieve the posterior draws
matrix_of_draws <- as.matrix(test_fit)
print(colnames(matrix_of_draws))

df_of_draws <- as.matrix(test_fit)
print(colnames(df_of_draws))

array_of_draws <- as.array(test_fit)
print(dimnames(array_of_draws))

# Matrix and df returns the same thing essentially, array returns each chain separately
print(dim(matrix_of_draws))
print(dim(df_of_draws))
print(dim(array_of_draws))

## Summary stats + convergence diagnostics
fit_summary <- summary(test_fit)
print(names(fit_summary)) # summary -> all chains merged. c_summary -> each chain individually

# fit_summary contains ESS and R_hat
rownames(fit_summary$summary)

## Sampler diagnostics
sampler_params <- get_sampler_params(test_fit, inc_warmup=TRUE)

sampler_params_chain1 <- sampler_params[[1]]
colnames(sampler_params_chain1)

# Can use sapply to get diagnostics per chain
mean_accept_stat_by_chain <- sapply(sampler_params, function(x) mean(x[, "accept_stat__"]))
print(mean_accept_stat_by_chain)

max_treedepth_by_chain <- sapply(sampler_params, function(x) max(x[, "treedepth__"]))
print(max_treedepth_by_chain)

divergent_by_chain <- sapply(sampler_params, function(x) max(x[, "divergent__"]))
print(divergent_by_chain)

bfmi_by_chain <- sapply(sampler_params, function(x) max(x[, "energy__"]))
print(bfmi_by_chain)

