context("Basic Plots")

test_that("Basic plot stills work",{
  create_supply_and_demand() %>%
    shift_demand(outwards = TRUE) %>%
    plot_supply_and_demand(consumer_surplus = TRUE) -> g1
  vdiffr::expect_doppelganger("first_example", g1)
})

test_that("Second plot stills word", {
  create_supply_and_demand() %>%
    shift_demand(outwards = TRUE) %>%
    shift_supply(outwards = FALSE) %>%
    plot_supply_and_demand(consumer_surplus = TRUE) -> g2
  vdiffr::expect_doppelganger("second-example", g2)
})

test_that("Third plot stills work", {
  create_supply_and_demand() %>%
    shift_supply() %>%
    shift_supply(shifter = 250) %>%
    shift_demand(outwards = FALSE, shifter = 400) %>%
    plot_supply_and_demand() -> g3
  vdiffr::expect_doppelganger("third-example", g3)
})

