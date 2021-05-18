rm(list=ls())
library(testthat)


NewValues<-data.frame(matrix(nrow = nrow(TestShipsHM), ncol=0))


context("Test Ship Power Calculation Scripts for Holtrop Mennen method")
#==========================================
NewValues$ship.speed<- calcSpeedUnitConversion(TestShipsHM$V)

NewValues$displacement<- calcDispUnitConversion(TestShipsHM$Displacement,1.025)

NewValues$nProp<- calcPropNum(TestShipsHM$ship.type)

NewValues$Cb<- calcCb(NewValues$displacement,TestShipsHM$lwl,TestShipsHM$Breadth,TestShipsHM$Draft)

NewValues$Cbw<- calcCbw(NewValues$Cb, TestShipsHM$Draft, TestShipsHM$Draft)




#==========================================
NewValues$Fn<-calcFroudeNum(NewValues$ship.speed,TestShipsHM$lwl)
test_that("calcFroudeNum works",{
  expect_equal(NewValues$Fn,
               TestShipsHM$Fn
               ,
               tolerance = 1.5e-4
  )
})
#==========================================

NewValues$Cm<-calcCm(TestShipsHM$ship.type,
                     NewValues$Cbw,
                     TestShipsHM$Draft,
                     TestShipsHM$Draft,
                     CmEquationType="kristensen")

test_that("calcCm works",{
  expect_equal(NewValues$Cm,
               TestShipsHM$Cm
               # ,
               # tolerance = 1.5e-6
  )
})
#==========================================
NewValues$Cp<-calcCp(NewValues$Cm,
                     NewValues$Cbw,
                     TestShipsHM$ship.type,
                     bounds="holtrop mennen")

test_that("calcCp works",{
  expect_equal(NewValues$Cp,
               TestShipsHM$Cp
                ,
                tolerance = 1.5e-4
  )
})
#==========================================
NewValues$Cwp<-calcCwp(
  NewValues$Cb,
  CwpEquationType="kristensen")

test_that("calcCwp works",{

  expect_equal(NewValues$Cwp,
               TestShipsHM$Cwp
               # ,
               # tolerance = 1.5e-6
  )

  expect_equal(calcCwp( NewValues$Cb,
                        CwpEquationType="ScHnEeKlUtH"),
               (1+2*NewValues$Cb)/3
              )


})



#==========================================
NewValues$At<-calcAt(NewValues$Cm,
                     TestShipsHM$Breadth,
                     TestShipsHM$Draft)

test_that("calcAt works",{
  expect_equal(NewValues$At,
               TestShipsHM$At
               # ,
               # tolerance = 1.5e-6
  )
})
#==========================================
NewValues$hb<-calchb(TestShipsHM$Draft)

test_that("calchb works",{
  expect_equal(NewValues$hb,
               TestShipsHM$hb
               # ,
               # tolerance = 1.5e-6
  )
})
#==========================================
NewValues$Abt<-calcAbt(NewValues$Cm,
                       TestShipsHM$Breadth,
                       TestShipsHM$Draft)

test_that("calcAbt works",{
  expect_equal(NewValues$Abt,
               TestShipsHM$Abt
               # ,
               # tolerance = 1.5e-6
  )
})
#==========================================
NewValues$propDiam<-calcPropDia(TestShipsHM$ship.type,
                                TestShipsHM$Draft)

test_that("calcPropDia works",{
  expect_equal(NewValues$propDiam,
               TestShipsHM$propDiam
               # ,
               # tolerance = 1.5e-6
  )
})
#==========================================
NewValues$lcb<-calclcb(TestShipsHM$lwl)

test_that("calclcb works",{
  expect_equal(NewValues$lcb,
               TestShipsHM$lcb
               # ,
               # tolerance = 1.5e-6
  )
})
#===========================================
NewValues$Cf<-calcCf(NewValues$ship.speed,
                     TestShipsHM$lwl,
                     15,
                     1.025)

test_that("CalcCf works",{
  expect_equal(NewValues$Cf,
               TestShipsHM$Cf
               # ,
               # tolerance = 1.5e-6
  )
})
#=============================================
NewValues$Rw<-calcHMWaveMakingRes(lwl=TestShipsHM$lwl,
                                   breadth=TestShipsHM$Breadth,
                                   Cp=NewValues$Cp,
                                   Cwp=NewValues$Cwp,
                                   Cm=NewValues$Cm,
                                   maxDisplacement=NewValues$displacement,
                                   maxDraft=TestShipsHM$Draft,
                                   froudeNum= NewValues$Fn,
                                   At=NewValues$At,
                                   hb=NewValues$hb,
                                   Abt=NewValues$Abt,
                                   seawaterDensity=1.025,
                                   forwardDraft=TestShipsHM$ForwardDraft,
                                   lcb=NewValues$lcb)


test_that("calcRw works",{
  expect_equal(NewValues$Rw,
               TestShipsHM$Rw
               # ,
               # tolerance=1.5e-2
               )
})
#========================================
NewValues$FormFactor<-calcHMFormFactor(TestShipsHM$Draft,
                                       TestShipsHM$lwl,
                                       TestShipsHM$Breadth,
                                       NewValues$displacement,
                                       NewValues$Cp,
                                       TestShipsHM$Cstern,
                                       NewValues$lcb

)
test_that("calcHMFormFactor works",{
  expect_equal(NewValues$FormFactor,
               TestShipsHM$FormFactor
               # ,
               # tolerance=1.5e-2
               )
})

#==========================================
NewValues$S<-calcHMWettedSA(TestShipsHM$lwl,
                            TestShipsHM$Draft,
                            TestShipsHM$Breadth,
                            NewValues$Cm,
                            NewValues$Cb,
                            NewValues$Cwp,
                            NewValues$Abt
)
test_that("calcHMWettedSA works",{
  expect_equal(NewValues$S,
               TestShipsHM$S
               # ,
               # tolerance = 1.5e-6
  )
})
#==============================================
NewValues$Rtr<-calcHMImmersedTransomRes(TestShipsHM$V,
                                        TestShipsHM$Breadth,
                                        NewValues$Cwp,
                                        TestShipsHM$Draft,
                                        NewValues$At,
                                        1.025
)

test_that("calcHMImmersedTransomRes works",{
  expect_equal(
    NewValues$Rtr,
  TestShipsHM$Rtr
  # ,
  # tolerance = 1.5e-6
  )
})
#==============================================
NewValues$Rb<-calcHMBulbousBowRes(NewValues$ship.speed,
                                  TestShipsHM$Draft,
                                  TestShipsHM$ForwardDraft,
                                  NewValues$Abt,
                                  NewValues$hb,
                                  1.025)

test_that("calcHMBulbousBowRes works",{
  expect_equal(NewValues$Rb,
               TestShipsHM$Rb
               # ,
               # tolerance = 1.5e-3
  )
})
#=============================================================
NewValues$Ca<-calcHMCa(TestShipsHM$Draft,
                       TestShipsHM$lwl,
                       NewValues$Cb,
                       TestShipsHM$Breadth,
                       TestShipsHM$ForwardDraft,
                       NewValues$Abt,
                       NewValues$hb

)

test_that("calcHMCa works",{
  expect_equal(NewValues$Ca,
               TestShipsHM$Ca ,
               tolerance = 1.5e-6
  )
})
#=============================================================
NewValues$w<- calcHMWakeFraction(TestShipsHM$Breadth,
                                 NewValues$S,
                                 TestShipsHM$Draft,
                                 NewValues$nProp,
                                 TestShipsHM$lwl,
                                 NewValues$propDiam,
                                 NewValues$FormFactor,
                                 NewValues$Cf,
                                 NewValues$Ca,
                                 NewValues$Cb,
                                 NewValues$Cp,
                                 NewValues$Cm,
                                 TestShipsHM$AftDraft,
                                 TestShipsHM$Cstern,
                                 NewValues$lcb)
test_that("calcHMWakeFraction works",{
  expect_equal(NewValues$w,
               TestShipsHM$w,
               tolerance = 1.5e-3
  )
})
#=============================================================
NewValues$t<-calcHMThrustFactor(TestShipsHM$Breadth,
                                TestShipsHM$lwl,
                                TestShipsHM$Draft,
                                NewValues$displacement,
                                NewValues$nProp,
                                TestShipsHM$Cp,
                                NewValues$propDiam,
                                NewValues$Cb,
                                TestShipsHM$Cstern,
                                NewValues$lcb,
                                1.025)

test_that("calcHMThrustFactor work",{
  expect_equal(NewValues$t,
               TestShipsHM$t,
               tolerance=1.4e-4
  )
})
#=============================================================
NewValues$Rapp<- calcHMAppendageRes(NewValues$ship.speed,
                                    NewValues$Cf,
                                    "rudder behind skeg",
                                    50,
                                    1.025)

test_that("calcHMAppendageRes works",{
  expect_equal(NewValues$Rapp,
               TestShipsHM$Rapp,
               tolerance=1.4e-4
              )

  expect_equal(calcHMAppendageRes(10,1,"zamboni", 1,seawaterDensity=1.025),
               0)
})
#=============================================================
NewValues$Rtot<- calcHMTotalRes(seawaterDensity = 1.025,
                                wettedSA = NewValues$S,
                                shipSpeed = NewValues$ship.speed,
                                formFactor= NewValues$FormFactor,
                                Cf=NewValues$Cf,
                                Rapp=NewValues$Rapp,
                                Rw=NewValues$Rw,
                                Rb=NewValues$Rb,
                                Rtr=NewValues$Rtr,
                                Ca=NewValues$Ca,
                                serviceMargin=0)
test_that("calcHMTotalRes",{
  expect_equal(NewValues$Rtot,
               TestShipsHM$Rtot,
               tolerance= 1.5e-2)

}

)
#=============================================================
NewValues$no<- calcOpenWaterEff(Rtot=NewValues$Rtot,
   thrustFactor=NewValues$t,
   nProp=NewValues$nProp,
   wakeFrac=NewValues$w,
   propDiam=NewValues$propDiam,
   shipSpeed=NewValues$ship.speed,
   seawaterDensity=1.025)


test_that("calcOpenWaterEff works",{
  expect_equal(NewValues$no,
               TestShipsHM$no,
               tolerance= 1e-4)
})
#=============================================================
NewValues$Pwr<- calcResistanceShipPwr(Rtot=NewValues$Rtot,
                                      shipSpeed=NewValues$ship.speed,
                                      hullEff=calcHullEff(NewValues$t,
                                                          NewValues$w),
                                      openWaterEff=NewValues$no,
                                      totalInstalledPwr=100000,
                                      shaftEff=0.98,
                                      relRotationEff=1)

test_that("calcResistanceShipPwr works",{
  expect_equal(NewValues$Pwr,
               TestShipsHM$Pwr
               # ,
               # tolerance= 1e-2
               )
})


test_that("calcHMPwr works",
          {
           expect_equal(calcHMPwr(totalInstalledPwr = TestShipsHM$Pwr,
                                  shipSpeed=NewValues$ship.speed,
                                  actualDraft = TestShipsHM$Draft,
                                  maxDraft = TestShipsHM$Draft,
                                  shipType = TestShipsHM$ship.type,
                                  lwl = TestShipsHM$lwl,
                                  breadth = TestShipsHM$Breadth,
                                  maxDisplacement = NewValues$displacement,
                                  Cb = NewValues$Cb,
                                  nProp = NewValues$nProp),
                        TestShipsHM$Pwr

           )
          }
         )

#===============================================================
# calcHMPwr(shipType=TestShipsHM$ship.type,
#           shipSpeed =  NewValues$ship.speed,
#           lwl =  TestShipsHM$lwl,
#           seawaterTemp = 15,
#           seawaterDensity = 1.025,
#           breadth =  TestShipsHM$Breadth,
#           maxDisplacement = NewValues$displacement,
#           maxDraft =  TestShipsHM$Draft,
#           actualDraft =  TestShipsHM$Draft,
#           Cbw =  NewValues$Cbw,
#           nProp =  NewValues$nProp,
#           totalInstalledPwr =  NewValues$Pwr*0.02,
#           serviceMargin = 0,
#           Cstern = TestShipsHM$Cstern,
#           forwardDraft = TestShipsHM$Draft,
#           aftDraft = TestShipsHM$Draft,
#           AppendagesList = "rudder behind skeg",
#           wettedAppSAList = 50
#           )

# names<-names(TestShipsHM)[names(TestShipsHM)%in%names(NewValues)]
#
# TestShipsHM[,names]<-NewValues[,names]
#
# write.csv(TestShipsHM, "data/Sample.Ships.Updated.Holtrop.Mennen.csv")
