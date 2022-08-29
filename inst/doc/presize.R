## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(presize)

## -----------------------------------------------------------------------------
(ss <- prec_sens(sens   = .75,  # sensitivity 
                 prev   = .15,  # prevalence
                 ntot   = 250,  # sample size
                 method = "wilson"))

## -----------------------------------------------------------------------------
prec_spec(spec   = .75,  # specificity 
          prev   = .15,  # prevalence
          ntot   = 250,  # sample size
          method = "exact")

## -----------------------------------------------------------------------------
prec_mean(60, sd = 10, n = 40)

## -----------------------------------------------------------------------------
(ss <- prec_sens(sens       = .75,   # sensitivity 
                 prev       = .15,   # prevalence
                 conf.width = .1,    # CI width
                 method     = "wilson"))

## -----------------------------------------------------------------------------
prec_sens(.6, n = 50, method = "wilson")

## -----------------------------------------------------------------------------
prec_prop(.6, n = 50, method = "wilson")

## -----------------------------------------------------------------------------
prec_mean(60, sd = 10, conf.width = 5)

## -----------------------------------------------------------------------------
(scenario_data <- prec_sens(sens = seq(.5, .95, .05), 
                            prev = .15, 
                            ntot = 250, 
                            method = "wilson"))


## -----------------------------------------------------------------------------
scenarios <- expand.grid(sens = seq(.5, .95, .1),
                         prev = seq(.1, .2, .04),
                         ntot = c(250, 350))

(scenario_data <- prec_sens(sens = scenarios$sens, 
                            prev = scenarios$prev, 
                            ntot = scenarios$ntot, 
                            method = "wilson"))

## ---- fig.width=7-------------------------------------------------------------
scenario_df <- as.data.frame(scenario_data)

library(ggplot2)

ggplot(scenario_df, 
       aes(x = sens, 
           y = conf.width, 
           # convert colour to factor for distinct colours rather than a continuum
           col = as.factor(prev), 
           group = prev)) +
  geom_line() +
  labs(x = "Sensitivity", y = "CI width", col = "Prevalence") + 
  facet_wrap(vars(ntot))


## -----------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(magrittr)
library(gt)

scenario_df %>% 
  # create the values needed specifically for the table
  mutate(
    txt = sprintf("%3.1f - %3.1f", lwr * 100, upr * 100),
    `Prevalence (%)` = prev * 100,
    Sensitivity = sens * 100,
    ntot = sprintf("N = %1.0f", ntot)) %>% 
  # select particular scenarios and variables to keep
  filter(sens > .7) %>% 
  select(ntot, Sensitivity, `Prevalence (%)`, txt) %>% 
  # reshape
  pivot_wider(
    names_from = Sensitivity, 
    values_from = txt, 
    id_cols = c(`Prevalence (%)`, ntot)) %>% 
  # group by ntot to split the table a little
  group_by(ntot) %>%
  # create the table
  gt() %>% 
  # add a header
  tab_spanner(
    label = "Sensitivity (%)",
    columns = 2:4
  ) %>% 
  cols_align("center", columns = 2:4) %>% 
  # increase the spacing between cells
  tab_style(
    style = "padding-left:12;padding-right:12;",
    locations = cells_body()
  )


