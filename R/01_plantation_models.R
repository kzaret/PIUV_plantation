## PIUV PLANTATION EXPERIMENT
## MODELING SPATIAL AND TEMPORAL EFFECTS ON SEEDLING GROWTH AND SURVIVAL
##
## Use (G)LMMs to estimate differences in seedling performance by patch and microhabitat, 
## accounting for hierarchical spatial structure (transect / group / seedling / observation)
## and temporal autocorrelation (repeated measurements of individuals seedlings).

#===============================================================================
# SETUP
#===============================================================================

library(matrixStats)
library(dplyr)
library(rstanarm)
library(bayesplot)
library(shinystan)
library(ggplot2)
library(here)
options(mc.cores = parallel::detectCores(logical = FALSE) - 1)
if(.Platform$OS.type == "windows") options(device = windows)

#===========================================================================
# DATA
#===========================================================================

epl_raw <- read.csv(here("data", "ExperimentalPlantation_long.csv"), header = TRUE)

# function to diff a vector while leapfrogging NAs (in that or another vector of equal length)
diffNA <- function(x, y = NULL) {
  if(is.null(y)) y <- x else stopifnot(length(x) == length(y))
  indx <- !is.na(y)
  dx <- x[indx] - lag(x[indx])
  replace(rep(NA, length(x)), indx, dx)
}

# add column for transect
# calculate annual growth rate in total height and height above substrate
epl <- epl_raw %>% rename(microhabitat = treat_simp) %>% 
  mutate(transect = sapply(strsplit(group, "_"), function(x) x[1]), .after = patch) %>% 
  arrange(seedling, year) %>% group_by(seedling) %>%
  mutate(growth_height = diffNA(total_height) / diffNA(year, total_height), 
    .after = total_height) %>% 
  mutate(growth_subsurf = diffNA(subsurf_to_tip) / diffNA(year, subsurf_to_tip), 
         .after = subsurf_to_tip) %>% 
  ungroup() %>% as.data.frame()
  

#===========================================================================
# (GENERALIZED) LINEAR MIXED-EFFECTS MODELS
#===========================================================================

# Base models include known hierarchical spatial (and possibly temporal?) structure
# Larger candidate models add covariates (patch, microhabitat, distance on gradient?)

#---------------------------------------------------------------------------
# TOTAL HEIGHT
#---------------------------------------------------------------------------

# Base model

lmer_gtht0 <- stan_lmer(growth_height ~ 1 + (1 | transect) + (1 | group) + (1 | seedling),
                       data = epl, 
                       chains = getOption("mc.cores"), iter = 2000, warmup = 1000)

print(lmer_gtht0)
summary(lmer_gtht0)

# Patch effect

lmer_gtht1 <- stan_lmer(growth_height ~ patch + (1 | transect) + (1 | group) + (1 | seedling),
                       data = epl, 
                       chains = getOption("mc.cores"), iter = 2000, warmup = 1000,
                       control = list(adapt_delta = 0.95))

print(lmer_gtht1)
summary(lmer_gtht1)

# Microhabitat effect

lmer_gtht2 <- stan_lmer(growth_height ~ microhabitat + (1 | transect) + (1 | group) + (1 | seedling),
                       data = epl, 
                       chains = getOption("mc.cores"), iter = 2000, warmup = 1000,
                       control = list(adapt_delta = 0.95))

print(lmer_gtht2)
summary(lmer_gtht2)


#---------------------------------------------------------------------------
# DIAGNOSTIC PLOTS
#---------------------------------------------------------------------------

# Simulate draws from posterior predictive distribution
mod <- lmer_gtht1
yrep <- posterior_predict(mod)
yrep <- yrep[sample(nrow(yrep)),]

# Posterior predictive check: density overlay
ppc_dens_overlay(mod$y, yrep[1:100,])

# Posterior predictive check: histograms
ppc_hist(mod$y, yrep[sample(nrow(yrep), 3),])

# Posterior predictive check: mean
ppc_stat_grouped(mod$y, yrep, group = mod$glmod$fr$transect, stat = mean)

# Posterior predictive check: SD
ppc_stat_grouped(mod$y, yrep, group = mod$glmod$fr$transect, stat = sd)

# Normal QQ plot of transect-level random intercept point estimates, grouped by patch
ranef(mod)$transect %>% rename(intercept = `(Intercept)`) %>% 
  mutate(patch = factor(tapply(epl$patch, epl$transect, unique))) %>% 
  ggplot(aes(sample = intercept)) + stat_qq(size = 2) + geom_qq_line() +
  theme_bw() + theme(panel.grid = element_blank()) + facet_wrap(vars(patch), ncol = 2)

# Normal QQ plot of seedling group-level random intercept point estimates, grouped by patch
ranef(mod)$group %>% rename(intercept = `(Intercept)`) %>% 
  mutate(patch = epl$patch[match(row.names(.), epl$group)]) %>% 
  ggplot(aes(sample = intercept)) + stat_qq(size = 2) + geom_qq_line() +
  theme_bw() + theme(panel.grid = element_blank()) + facet_wrap(vars(patch), ncol = 2)

# Normal QQ plot of seedling-level random intercept point estimates, grouped by patch
ranef(mod)$seedling %>% rename(intercept = `(Intercept)`) %>% 
  mutate(patch = epl$patch[match(row.names(.), epl$seedling)]) %>% 
  ggplot(aes(sample = intercept)) + stat_qq(size = 2) + geom_qq_line() +
  theme_bw() + theme(panel.grid = element_blank()) + facet_wrap(vars(patch), ncol = 2)

# Normal QQ plot of observation-level residuals point estimates, grouped by patch
data.frame(mod$glmod$fr, resid = resid(mod)) %>% 
  ggplot(aes(sample = resid)) + stat_qq(size = 2) + geom_qq_line() +
  theme_bw() + theme(panel.grid = element_blank()) + facet_wrap(vars(patch), ncol = 2)



