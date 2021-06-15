rm(list=ls())
library(testthat)
library(ShipPowerModel)



NewValues<- data.frame(matrix(nrow = nrow(TestShipsKrist),ncol=0))


context("Test Ship Power Calculation for Kristensen method")
#===========================================
NewValues$ship.speed<- calcSpeedUnitConversion(TestShipsKrist$shipSpeed)

NewValues$displacement<- calcDispUnitConversion(TestShipsKrist$Displacement, seawaterDensity = 1.025)

NewValues$nProp<- calcPropNum(TestShipsKrist$ship.type)

NewValues$Cb<- calcCb(NewValues$displacement, TestShipsKrist$lwl, TestShipsKrist$breadth, TestShipsKrist$maxDraft)

#==============================================
NewValues$actual.disp<-calcActualDisp(TestShipsKrist$Cb,
                                   TestShipsKrist$Cbw,
                                   TestShipsKrist$actualDraft,
                                   TestShipsKrist$maxDraft,
                                   NewValues$displacement)
test_that("calcActualDisp works",{

  expect_equal(NewValues$actual.disp,
               TestShipsKrist$actual.disp
  )
})
#==============================================

NewValues$froudeNum<- calcFroudeNum(NewValues$ship.speed,
                                    TestShipsKrist$lwl)
test_that("calcFroudeNum works",{

  expect_equal(NewValues$froudeNum,
               TestShipsKrist$froudeNum
  )
})
#==============================================
NewValues$Cm<-calcCm(TestShipsKrist$ship.type,
           TestShipsKrist$Cbw,
           TestShipsKrist$maxDraft,
           TestShipsKrist$actualDraft,
           CmEquationType="kristensen")

test_that("calcCm works",{

  expect_equal(NewValues$Cm,
               TestShipsKrist$Cm
  )
})
#==============================================
NewValues$Cp<-calcCp(NewValues$Cm,
                     TestShipsKrist$Cbw,
                     TestShipsKrist$ship.type,
                     bounds="none")

test_that("calcCp works",{

  expect_equal(NewValues$Cp,
               TestShipsKrist$Cp
  )
})
#==============================================
NewValues$propDiam<-calcPropDia(TestShipsKrist$ship.type,
                      TestShipsKrist$maxDraft)

test_that("calcPropDia works",{

  expect_equal(NewValues$propDiam,
               TestShipsKrist$propDiam
  )
})
#==============================================

NewValues$M<- calcShipM(NewValues$actual.disp,
                            TestShipsKrist$lwl)

test_that("calcPropDia works",{

  expect_equal(NewValues$M,
               TestShipsKrist$M
  )
})

#=============================================================
#Frictional Resistance
NewValues$Cf<-calcCf(NewValues$ship.speed,
                     TestShipsKrist$lwl,
                     15,
                     seawaterDensity=1.025)

test_that("calcCf works",{

  expect_equal(NewValues$Cf,
               TestShipsKrist$Cf
  )
})
#=============================================================
#Wetted Surface Area
NewValues$wettedSA<-calcKristWettedSA(TestShipsKrist$ship.type,
                            NewValues$displacement,
                            TestShipsKrist$maxDraft,
                            TestShipsKrist$actualDraft,
                            TestShipsKrist$lwl,
                            TestShipsKrist$breadth,
                            TestShipsKrist$Cbw,
                            seawaterDensity=1.025)

test_that("calcKristWettedSA works",{

  expect_equal(NewValues$wettedSA,
               TestShipsKrist$wettedSA
  )
})
#==========================================
#Incremental Resistance
NewValues$Ca<-calcKristCa(TestShipsKrist$ship.type,
                          NewValues$actual.disp)

test_that("calcKristCa works",{

  expect_equal(NewValues$Ca,
               TestShipsKrist$Ca,
               tolerance=1e-07
  )
})
#==========================================
#Air Resistance
NewValues$Caa<-calcKristCaa(TestShipsKrist$ship.type,
                            TestShipsKrist$dwt)

test_that("calcKristCaa works",{

  expect_equal(NewValues$Caa,
               TestShipsKrist$Caa
  )
})
#==========================================
#Residual Resistance
NewValues$Cr<-calcKristCr(TestShipsKrist$ship.type,
                          NewValues$M,
                          NewValues$froudeNum,
                          TestShipsKrist$actualDraft,
                          TestShipsKrist$breadth,
                          NewValues$Cp)

test_that("calcKristCr works",{

  expect_equal(NewValues$Cr,
               TestShipsKrist$Cr
  )
})
#=============================================================
#Thrust Deduction Factor
NewValues$t<-calcKristThrustFactor(TestShipsKrist$ship.type,
                                   TestShipsKrist$breadth,
                                   TestShipsKrist$lwl,
                                   TestShipsKrist$Cbw,
                                   NewValues$propDiam,
                                   NewValues$M,
                                   NewValues$nProp
                              )

test_that("calcKristThrustFactor works",{

  expect_equal(NewValues$t,
               TestShipsKrist$t
  )
})
#=============================================================
#Wake Fraction

NewValues$w<-calcKristWakeFrac(TestShipsKrist$ship.type,
                          TestShipsKrist$breadth,
                          TestShipsKrist$lwl,
                          TestShipsKrist$Cbw,
                          NewValues$propDiam,
                          NewValues$M,
                          NewValues$nProp)

test_that("calcKristWakeFrac works",{

  expect_equal(NewValues$w,
               TestShipsKrist$w
  )
})
#==========================================================
#Total Resistance
NewValues$Rtot<-calcKristTotalRes(NewValues$wettedSA,
                               NewValues$Cf,
                               NewValues$Cr,
                               NewValues$Ca,
                               NewValues$Caa,
                     seawaterDensity=1.025,
                     NewValues$ship.speed,
                     serviceMargin=15)

test_that("calcKristTotalRes works",{

  expect_equal(NewValues$Rtot,
               TestShipsKrist$Rtot,
               tolerance=0.01
  )
})
#==========================================================
# Open Water Efficiency
NewValues$openwatereff<-calcOpenWaterEff(NewValues$Rtot,
                               NewValues$t,
                               NewValues$nProp,
                               NewValues$w,
                               NewValues$propDiam,
                               NewValues$ship.speed,
                     seawaterDensity=1.025)

test_that("calcOpenWaterEff works",{

  expect_equal(NewValues$openwatereff,
               TestShipsKrist$openwatereff,
               tolerance=1e-05
  )
})
#=============================================================
NewValues$Power<-calcResistanceShipPwr(Rtot=NewValues$Rtot,
                                       shipSpeed=NewValues$ship.speed,
                             hullEff=calcHullEff(NewValues$t, NewValues$w),
                             openWaterEff=NewValues$openwatereff,
                             totalInstalledPwr =  TestShipsKrist$MCR,
                             shaftEff=0.98,
                             relRotationEff=1,
                              pwrUpperBoundPercent = 2)

test_that("calcResistanceShipPwr works",{

  expect_equal(NewValues$Power,
               TestShipsKrist$Power
  )
})


test_that("calcKristPwr works",
          {
          expect_equal(
                       calcKristPwr(totalInstalledPwr = TestShipsKrist$Power,
                                    shipSpeed         = NewValues$ship.speed,
                                    actualDraft       = TestShipsKrist$actualDraft,
                                    maxDraft          = TestShipsKrist$maxDraft,
                                    shipType          = TestShipsKrist$ship.type,
                                    lwl               = TestShipsKrist$lwl,
                                    breadth           = TestShipsKrist$breadth,
                                    maxDisplacement   = NewValues$displacement,
                                    Cb                = NewValues$Cb,
                                    nProp             = NewValues$nProp,
                                    dwt               = TestShipsKrist$dwt
                                      ),
                       TestShipsKrist$Power
                       )
          }
          )


#=============================================================
# names<- names(TestShipsKrist)[names(TestShipsKrist)%in%names(NewValues)]
# TestShipsKrist[,names]<-NewValues[,names]
# write.csv(TestShipsKrist, "data/Sample.Ships.Updated.Kristensen.csv", row.names=FALSE)
