rm(list=ls())
library(testthat)
library(ShipPowerModel)

context("Test unit conversion functions")

test_that("calcSpeedUnitConversion works",
          {
            expect_equal( calcSpeedUnitConversion(10),
                          5.144
                         )
          }
         )

test_that("calcDispUnitConversion works",
          {
           expect_equal(calcDispUnitConversion(1),
                        1/1.025)

            expect_equal(calcDispUnitConversion(tonnageDisp = c(1,2,3,4),
                                                seawaterDensity = c(1, 2,0.5,2)
                                                ),
                         c(1,1,6,2)
                         )
          }
         )
