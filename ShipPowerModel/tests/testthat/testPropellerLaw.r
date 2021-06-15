
context("Test Ship Power Calculation for propeller law")

test_that("calcPropPwr works for shipSpeed == serviceSpeed",{
  expect_equal( calcPropPwr(shipSpeed=10, refSpeed=10, totalInstalledPwr=100, n=3, serviceMargin=0),
                100*0.94^3
              )

})

test_that("calcPropPwr works for shipSpeed == maxSpeed",{
  expect_equal( calcPropPwr(shipSpeed=10, refSpeed=10, totalInstalledPwr=100, n=3, serviceMargin=0, refSpeedType="maxSpeed"),
                100
  )

})

test_that("serviceMargin has the intended effect",{
  expect_equal( calcPropPwr(shipSpeed=9, refSpeed=10, totalInstalledPwr=100, n=3, serviceMargin=15, refSpeedType="maxSpeed"),
                100*(0.9^3)*1.15
  )

})


test_that("maximum power output is 100% load",{
  expect_equal( calcPropPwr(shipSpeed=20, refSpeed=10, totalInstalledPwr=100, n=3, serviceMargin=15),
                100
              )

})


test_that("minimum non-zero power is 2% load",{
  expect_equal( calcPropPwr(shipSpeed=0.2, refSpeed=10, totalInstalledPwr=100, n=3, serviceMargin=0),
                2
              )

})



test_that("changing speed generates the correct power",{
  shipSpeed <- 5
  refSpeed  <- 10
  totalInstalledPwr <- 100
  n <- 3
  serviceMargin <- 0

  expect_equal( calcPropPwr(totalInstalledPwr, shipSpeed, refSpeed, serviceMargin, n),
                totalInstalledPwr*(0.94*shipSpeed/refSpeed)^n
              )

})
