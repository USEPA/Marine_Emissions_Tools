context("Test Ship Power Calculation for Admiralty law")

test_that("calcAdmPwr works for shipSpeed == serviceSpeed",{
  expect_equal( calcAdmPwr (shipSpeed = 10, actualDraft = 10,
                                refSpeed  = 10, maxDraft    = 10,
                                totalInstalledPwr = 100,
                                n=3, serviceMargin = 0
                                ),
                100*0.94^3
              )

})

test_that("calcAdmPwr works for shipSpeed == maxSpeed",{
  expect_equal( calcAdmPwr (shipSpeed = 10, actualDraft = 10,
                                refSpeed  = 10, maxDraft    = 10,
                                totalInstalledPwr = 100,
                                n=3, serviceMargin = 0,
                                refSpeedType = "maxSpeed"
                                ),
                100
  )

})


test_that("serviceMargin has the intended effect",{
  expect_equal( calcAdmPwr (shipSpeed = 9, actualDraft = 10,
                                refSpeed  = 10, maxDraft    = 10,
                                totalInstalledPwr = 100,
                                n=3, serviceMargin = 15,
                                refSpeedType = "maxSpeed"
  ),
  100*(0.9^3)*1.15
  )

})


test_that("maximum power output is 100% load",{
  expect_equal( calcAdmPwr (shipSpeed = 20, actualDraft = 15,
                                refSpeed  = 10, maxDraft    = 10,
                                totalInstalledPwr = 100,
                                n=3, serviceMargin = 15),
                100
  )

})


test_that("minimum non-zero power is 2% load",{
  expect_equal( calcAdmPwr (shipSpeed = 0.2, actualDraft = 10,
                                refSpeed  = 10,  maxDraft    = 10,
                                totalInstalledPwr = 100,
                                n=3, serviceMargin = 0),
                2
              )

})



test_that("changing speed generates the correct power",{
  shipSpeed   <- 5
  actualDraft <- 10
  refSpeed    <- 10
  maxDraft    <- 10
  totalInstalledPwr <- 100
  n <- 3
  serviceMargin <- 0

  expect_equal( calcAdmPwr (totalInstalledPwr,
                                shipSpeed, refSpeed,
                                actualDraft, maxDraft,
                                serviceMargin,
                                n,
                                refSpeedType = "maxSpeed"
                                ),
                totalInstalledPwr*((shipSpeed/refSpeed)^n)*((actualDraft/maxDraft)^(2/3))
  )

})


test_that("changing draft generates the correct power",{
  shipSpeed   <- 10
  actualDraft <- 5
  refSpeed    <- 10
  maxDraft    <- 10
  totalInstalledPwr <- 100
  n <- 3
  serviceMargin <- 0

  expect_equal( calcAdmPwr (totalInstalledPwr,
                                shipSpeed, refSpeed,
                                actualDraft, maxDraft,
                                serviceMargin,
                                n,
                                refSpeedType = "maxSpeed"
                                ),
                totalInstalledPwr*((shipSpeed/refSpeed)^n)*((actualDraft/maxDraft)^(2/3))
  )

})
