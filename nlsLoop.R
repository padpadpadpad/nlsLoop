## ----load_in data, tidy=TRUE---------------------------------------------
# load in package
library(nlsLoop)

# load in data
data('Chlorella_TRC')

# look at column names
names(Chlorella_TRC)

## ----schoolfield_high, tidy = TRUE---------------------------------------
# define the Sharpe-Schoolfield equation
schoolfield_high <- function(lnc, E, Eh, Th, temp, Tc) {
  Tc <- 273.15 + Tc
  k <- 8.62e-5
  boltzmann.term <- lnc + log(exp(E/k*(1/Tc - 1/temp)))
  inactivation.term <- log(1/(1 + exp(Eh/k*(1/Th - 1/temp))))
  return(boltzmann.term + inactivation.term)
  }

## ----nlsLoop, message=FALSE, warning=FALSE, results='hide'---------------
fits <- nlsLoop(ln.rate ~ schoolfield_high(lnc, E, Eh, Th, temp = K, Tc = 20),
                     data = Chlorella_TRC,
                     tries = 500,
                     id_col = 'curve_id',
                     param_bds = c(-10, 10, 0.1, 2, 0.5, 10, 285, 330),
                     r2 = 'Y',
                     supp_errors = 'Y',
                     AICc = 'Y',
                     na.action = na.omit,
                     lower = c(lnc = -10, E = 0, Eh = 0, Th = 0))

## ---- look_at_fits-------------------------------------------------------
# look at parameter values
head(fits$params)

# look at fits
head(fits$params)

## ----first_fit_plot, fig.height=5,  fig.width = 7------------------------
plot_id_nlsLoop(data = Chlorella_TRC, param_data = fits, id = '1')

## ----pdf_fits, eval=FALSE------------------------------------------------
plot_all_nlsLoop('~/Desktop/test.pdf', data = Chlorella_TRC, param_data = fits)

## ----data_wrangling, fig.height=7,  fig.width = 7------------------------
# get distinct values of process, flux and growth.temp for each value of curve_id
d_treatment <- Chlorella_TRC[,c('curve_id','process', 'growth.temp', 'flux')]
d_treatment <- d_treatment[!duplicated(d_treatment),]

# merge with predictions by curve_id
fits$predictions <- merge(fits$predictions, d_treatment, by = 'curve_id')

# plot every curve
library(ggplot2)
ggplot() +
  geom_point(aes(K - 273.15, ln.rate, col = flux), size = 2, Chlorella_TRC) +
  geom_line(aes(K - 273.15, ln.rate, col = flux, group = curve_id), alpha = 0.5, fits$predictions) +
  facet_wrap(~ growth.temp + process, labeller = labeller(.multi_line = F)) +
  scale_colour_manual(values = c('green4', 'black')) +
  theme_bw(base_size = 12, base_family = 'Helvetica') +
  ylab('log Metabolic rate') +
  xlab('Assay temperature (ºC)') +
  theme(legend.position = c(0.9, 0.15))


## ----parameter_plots, fig.height=5, fig.width=6--------------------------
# merge params with d_treatment by curve_id
fits$params <- merge(fits$params, d_treatment, by = 'curve_id')

library(tidyr)

gather(fits$params, 'parameter', 'value', c(lnc, E, Eh, Th)) %>%
  ggplot(., aes(flux, value, col = flux)) +
  geom_boxplot(fill = 'white', outlier.shape = NA) +
  geom_point(position = position_jitter(height = 0, width = 0.1)) +
  facet_wrap(~ parameter, scales = 'free_y') +
  scale_color_manual(values = c('green4', 'black')) +
  scale_shape_manual(values = c(21, 24)) +
  theme_bw(base_size = 12, base_family = 'Helvetica') +
  theme(legend.position = 'top')

## ---- confint_nlsLoop, fig.width = 7, fig.height = 8---------------------
# calculate confidence intervals for each fit
CIs <- confint_nlsLoop(Chlorella_TRC, fits)

# bind with factors dataframe
CIs <- merge(CIs, d_treatment, by = 'curve_id')

# plot
ggplot(CIs, aes(col = flux)) +
  geom_point(aes(curve_id, mean)) +
  facet_wrap(~ param, scale = 'free_x', ncol = 4) +
  geom_linerange(aes(curve_id, ymin = CI_lwr, ymax = CI_upr)) +
  coord_flip() +
  scale_color_manual(values = c('green4', 'black')) +
  theme_bw(base_size = 12, base_family = 'Helvetica') +
  theme(legend.position = 'top') +
  xlab('curve') +
  ylab('parameter estimate')




