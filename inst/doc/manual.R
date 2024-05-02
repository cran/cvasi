## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.path = "../doc/figures/manual-",
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(cvasi)

## ---- eval=FALSE--------------------------------------------------------------
#  library(cvasi)
#  
#  ## Sample base R workflow ##
#  # Create a scenario object of model GUTS-RED-IT
#  my_it <- GUTS_RED_IT()
#  # Set model parameters
#  my_it <- set_param(my_it, c(kd=1.2, hb=0, alpha=9.2, beta=4.3))
#  # Print scenario details
#  my_it

## ---- eval=FALSE--------------------------------------------------------------
#  ## Sample tidy R workflow ##
#  # the pipeline (%>%) symbol passes results to the next statement
#  GUTS_RED_IT() %>%
#    set_param(c(kd=1.2, hb=0, alpha=9.2, beta=4.3))

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("cvasi", dependencies=TRUE)

## -----------------------------------------------------------------------------
library(cvasi)

# Create a new and empty GUTS-RED-IT scenario and set its parameters
GUTS_RED_IT() %>%
  set_param(c(kd=1.2, hb=0, alpha=9.2, beta=4.3))

## -----------------------------------------------------------------------------
# Sample GUTS-RED-IT scenario derived from an acute fish toxicity study
# of the fathead minnow and Chlorpyrifos (Geiger et al. 1988)
minnow_it %>%
  simulate()

## ---- eval=FALSE--------------------------------------------------------------
#  # Access the package help on GUTS-RED type models
#  ?"GUTS-RED-models"

## -----------------------------------------------------------------------------
# Define an exposure time-series
myexposure <- data.frame(time = c(0, 1, 1.01, 5), conc = c(10, 10, 0, 0))

# Create and parameterize a scenario
GUTS_RED_IT() %>%
  set_param(c(kd=1.2, hb=0, alpha=9.2, beta=4.3)) %>%
  set_exposure(myexposure) -> myscenario

## -----------------------------------------------------------------------------
# Print information about the scenario
myscenario


## -----------------------------------------------------------------------------
# Update the exposure time-series but keep former output time points
myscenario %>%
  set_exposure(no_exposure(), reset_times=FALSE)

## -----------------------------------------------------------------------------
# Selected model parameters
myparam <- c(p_M=3211, v=0.023, k_J=0.63)
# Initial body length L
myinit <- c(L=0.02)
# Constant non-zero exposure
myexposure <- data.frame(time=0, conc=1.72)

DEB_abj() %>%
  set_param(myparam) %>%
  set_init(myinit) %>%
  set_exposure(myexposure) %>%
  set_times(0:10) %>%             # Output times 0,1,2,...,10
  set_mode_of_action(4) %>%       # Method of Action #4 to be activated
  set_window(length=3) -> mydeb   # Using moving exposure windows of length 3 days

## -----------------------------------------------------------------------------
# Sample scenario of the Lemna TKTD model
metsulfuron %>%
  set_times(0:7) %>%
  simulate()

## -----------------------------------------------------------------------------
# Same simulation period, but with a smaller step length of 0.1
metsulfuron %>%
  set_times(seq(0, 7, 0.1)) %>%
  simulate() %>%
  tail()

## -----------------------------------------------------------------------------
# Original simulation period, but with a maximum solver step length of hmax=0.01
metsulfuron %>%
  set_times(0:7) %>%
  simulate(hmax = 1)

## ---- eval=FALSE--------------------------------------------------------------
#  ?simulate

## -----------------------------------------------------------------------------
# GUTS-RED-IT scenario of the fathead minnow and chlorpyrifos
minnow_it %>% effect()

## ---- include=FALSE-----------------------------------------------------------
# make sure that value in text are still up to date
testthat::expect_equal(minnow_it %>% effect() %>% dplyr::pull(L), 6.297e-5, tolerance=0.001)

## -----------------------------------------------------------------------------
# Setting up a custom scenario with a total simulation period of 14 days and
# an exposure window length of 7 to assess a trapezoidal exposure pattern
americamysis %>%
  set_window(7) %>%
  set_exposure(data.frame(t=c(0,3,4,7,8), c=c(0,0,3,3,0))) %>%
  set_times(0:14) -> mydeb

# Derive maximum effect level of all exposure windows
mydeb %>% effect()

## ---- include=FALSE-----------------------------------------------------------
# make sure that value in text are still up to date
testthat::expect_equal(mydeb %>% effect() %>% dplyr::pull(L), 0.0521, tolerance=0.001)

## -----------------------------------------------------------------------------
# Restrict assessed endpoints to structural length (L)
mydeb %>% 
  set_endpoints("L") %>%
  epx()

## ---- include=FALSE-----------------------------------------------------------
# make sure that value in text are still up to date
testthat::expect_equal(mydeb %>%  set_endpoints("L") %>% epx(ep_only=TRUE) %>% unlist(), c(1.162598, 1.711914), ignore_attr=TRUE, tolerance=0.01)

## -----------------------------------------------------------------------------
# Examine how the EP23 value is derived
minnow_it %>% epx(level=20, verbose=TRUE)

## -----------------------------------------------------------------------------
# Exposure data in the highest treatment level
exp_df <- Schmitt2013 %>% 
  dplyr::filter(ID == "T5.6") %>%
  dplyr::select(t, conc) 
# Exposure scenario containing the exposure data in the highest treatment level
exp_scen <- metsulfuron %>% 
     set_exposure(exp_df, reset_times = FALSE)
# Create corresponding effect data
# Observed effects in the highest treatment level   
eff_df <- Schmitt2013 %>%   
  dplyr::filter(ID == "T5.6") %>%
  dplyr::select(t, obs)
# calibration 
fit1 <- calibrate(
  x = exp_scen, 
  par = c(KiN = 300),
  data = eff_df, 
  endpoint = "BM"
  )

## -----------------------------------------------------------------------------
fit1$fit[[1]]$par
fit1$fit

## -----------------------------------------------------------------------------
fit1$scenario

## -----------------------------------------------------------------------------
## Display selected scenario properties
metsulfuron@times  # Output times
metsulfuron@init   # Initial state

## Simulate the sample scenario
metsulfuron %>% simulate()

## -----------------------------------------------------------------------------
metsulfuron %>%
  simulate(nout=0) %>%
  head(5)

## -----------------------------------------------------------------------------
metsulfuron %>%
  simulate(nout=8) %>%
  head(5)

## -----------------------------------------------------------------------------
metsulfuron %>%
  set_times(0:7) %>%  # restrict scenario to the period [0,7]
  effect()

## -----------------------------------------------------------------------------
metsulfuron %>%
  set_window(length=7, interval=1) %>%   # enable moving exposure windows
  effect(max_only=FALSE)                 # return effects of all windows

## -----------------------------------------------------------------------------
metsulfuron %>%
  set_window(length=7, interval=1) %>%         # enable moving exposure windows
  effect(max_only=FALSE, marginal_effect=0.01) # return effects of all windows

## -----------------------------------------------------------------------------
metsulfuron %>% epx()

## -----------------------------------------------------------------------------
metsulfuron %>%
  set_endpoints("BM") %>%
  epx(verbose=TRUE)

