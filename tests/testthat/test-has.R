test_that("has_forcings", {
  scenario <- Lemna_Schmitt() %>% set_forcings(temp=10)
  expect_true(has_forcings(scenario))
  expect_equal(has_forcings(c(scenario,scenario)), c(TRUE,TRUE))
  # no forcings
  expect_false(has_forcings(GUTS_RED_IT()))
  expect_false(has_forcings(Lemna_Schmitt()))
  # invalid arguments
  expect_error(has_forcings(NA))
  expect_error(has_forcings(NULL))
  expect_error(has_forcings(1))
  expect_error(any(has_forcings(1:5)))
})

test_that("has_exposure", {
  # exposure is set
  scenario <- new("EffectScenario", exposure=ExposureSeries(series=data.frame(t=1,c=1)))
  expect_true(has_exposure(scenario))
  expect_true(has_exposure(new("EffectScenario", exposure=ExposureSeries(series=data.frame(t=1:5,c=1:5)))))
  expect_equal(c(TRUE,TRUE), has_exposure(c(scenario, scenario)))

  # no exposure
  scenario <- new("EffectScenario")
  expect_false(has_exposure(scenario))
  expect_equal(c(FALSE,FALSE), has_exposure(c(scenario,scenario)))

  # invalid arguments
  expect_error(has_exposure(NA))
  expect_error(has_exposure(NULL))
  expect_error(has_exposure(1))
  expect_error(any(has_exposure(1:5)))
})

test_that("has_constant_forcings", {
  expect_true(has_constant_forcings(Lemna_Schmitt() %>%
                                      set_exposure(data.frame(t=0,c=0))))
  expect_true(has_constant_forcings(Lemna_Schmitt() %>%
                                      set_exposure(data.frame(t=0,c=0)) %>%
                                      set_forcings(temp=1,rad=1)))
  # non-constant forcings
  expect_false(has_constant_forcings(Lemna_Schmitt() %>%
                                       set_exposure(data.frame(t=0:1,c=0:1))))
  expect_false(has_constant_forcings(Lemna_Schmitt() %>%
                                       set_exposure(data.frame(t=0:1,c=0:1)) %>%
                                       set_forcings(temp=1,rad=1)))
  expect_false(has_constant_forcings(Lemna_Schmitt() %>%
                                       set_exposure(data.frame(t=0,c=0)) %>%
                                       set_forcings(temp=1,rad=data.frame(t=0:1,r=0:1))))
  # invalid arguments
  expect_error(has_constant_forcings(NA))
  expect_error(has_constant_forcings(NULL))
  expect_error(has_constant_forcings(1))
  expect_error(any(has_constant_forcings(1:5)))
})

test_that("has_controls", {
  expect_true(has_controls(new("EffectScenario", control=c(1))))
  expect_true(has_controls(new("EffectScenario", control=list(1))))
  # on controls
  expect_false(has_controls(new("EffectScenario")))
  # invalid arguments
  expect_error(has_controls(NA))
  expect_error(has_controls(NULL))
  expect_error(has_controls(1))
  expect_error(any(has_controls(1:5)))
})

test_that("has_windows", {
  expect_true(has_windows(new("EffectScenario", window.length=1)))
  # on controls
  expect_false(has_windows(new("EffectScenario")))
  # invalid arguments
  expect_error(has_windows(NA))
  expect_error(has_windows(NULL))
  expect_error(has_windows(1))
  expect_error(any(has_windows(1:5)))
})

test_that("has_regular_transfer", {
  expect_true(has_regular_transfer(Lemna_Schmitt() %>% set_transfer(interval=1)))
  expect_true(has_transfer(Lemna_Schmitt() %>% set_transfer(interval=1)))
  # on controls
  expect_false(has_regular_transfer(Lemna_Schmitt() %>% set_transfer(interval=-1)))
  expect_false(has_regular_transfer(Lemna_Schmitt() %>% set_transfer(times=1)))
  # invalid arguments
  expect_error(has_regular_transfer(NA))
  expect_error(has_regular_transfer(NULL))
  expect_error(has_regular_transfer(1))
  expect_error(any(has_regular_transfer(1:5)))
})

test_that("has_irregular_transfer", {
  expect_true(has_irregular_transfer(Lemna_Schmitt() %>% set_transfer(times=1)))
  expect_true(has_transfer(Lemna_Schmitt() %>% set_transfer(times=1)))
  # on controls
  expect_false(has_irregular_transfer(Lemna_Schmitt() %>% set_transfer(interval=-1)))
  expect_false(has_irregular_transfer(Lemna_Schmitt() %>% set_transfer(interval=1)))
  # invalid arguments
  expect_error(has_irregular_transfer(NA))
  expect_error(has_irregular_transfer(NULL))
  expect_error(has_irregular_transfer(1))
  expect_error(any(has_irregular_transfer(1:5)))
})
