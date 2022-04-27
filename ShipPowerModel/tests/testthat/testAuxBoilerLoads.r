rm(list=ls())
library(testthat)
library(ShipPowerModel)

context("Test aux boiler load functions")


#calcAuxBoilerLoad=============================================================
#IMO Auxiliary and Boiler

test_that("calcAuxBoilerLoad IMO works",
          {
            #create Benchmark
            testIMOAuxBoilerLoad<-data.table::data.table(
              shipType=c("bulk.carrier","chemical.tanker","container.ship",
                         "general.cargo","liquified.gas.tanker","oil.tanker","other.tanker",
                         "ferry.pax","cruise","ferry.ro.pax","reefer","ro.ro","vehicle.carrier",
                         "yacht","service.tug","miscellaneous.fishing","offshore","service.other","miscellaneous.other"
              ),
              subType=c("bulk.carrier.handymax","chemical.tanker.small","container.ship.8000",
                        "general.cargo.5000","liquified.gas.tanker.20000","oil.tanker.aframax","other.tanker",
                        "ferry.pax.largest","cruise.60000","ferry.ro.pax.2000","reefer","ro.ro.largest","vehicle.carrier.30000",
                        "yacht","service.tug","miscellaneous.fishing","offshore","service.other","miscellaneous.other"
              ),
              opMode=c(rep(c("Anchorage","Berth","Maneuvering","Transit"),4),
                       "Anchorage","Berth","Maneuvering"
              ),
              AuxLoad=c(260,490,2600,60,1710,1000,750,524,3500,105,1150,950,500,130,50,200,320,220,190),
              BoilerLoad=c(100,250,450,0,300,2000,200,0,1000,0,270,0,268,0,0,0,0,0,0)
            )

            #Run Calculation
            #IMO Aux
            testIMOAuxBoilerLoad[,newAuxLoad:=calcAuxBoilerLoad(opMode = opMode,
                                                                shipType = shipType,
                                                                subType = subType,
                                                                method= "imo",
                                                                output = "aux"
            )
            ]

            #IMO Boiler
            testIMOAuxBoilerLoad[,newBoilerLoad:=calcAuxBoilerLoad(opMode = opMode,
                                                                   shipType = shipType,
                                                                   subType = subType,
                                                                   method= "imo",
                                                                   output = "boiler"
            )
            ]

            #Compare output against benchmark
            expect_equal(testIMOAuxBoilerLoad$AuxLoad,
                         testIMOAuxBoilerLoad$newAuxLoad
            )
            expect_equal(testIMOAuxBoilerLoad$BoilerLoad,
                         testIMOAuxBoilerLoad$newBoilerLoad
            )
          })


# #Starcrest Auxiliary
test_that("calcAuxBoilerLoad Starcrest works",
          {
            #create Benchmark
            testStarcrestAuxBoilerLoad<- data.table::data.table(
              shipType=c("container.ship","cruise.ed","vehicle.carrier","tanker"),
              subType=c("container.ship.11000","cruise.ed.3500","vehicle.carrier","tanker.panamax"),
              opMode=c("Maneuvering","Anchorage","Berth","Transit"),
              AuxLoad=c(3500,10500,859,561),
              BoilerLoad=c(575,0,314,167)
            )

            #Run Calculation
            #Starcrest Aux
            testStarcrestAuxBoilerLoad[,newAuxLoad:=calcAuxBoilerLoad(opMode = opMode,
                                                                      shipType = shipType,
                                                                      subType = subType,
                                                                      method= "starcrest",
                                                                      output = "aux"
            )
            ]

            #Starcrest Boiler
            testStarcrestAuxBoilerLoad[,newBoilerLoad:=calcAuxBoilerLoad(opMode = opMode,
                                                                         shipType = shipType,
                                                                         subType = subType,
                                                                         method= "starcrest",
                                                                         output = "boiler"
            )
            ]

            #Compare output against benchmark
            expect_equal(testStarcrestAuxBoilerLoad$newAuxLoad,
                         testStarcrestAuxBoilerLoad$AuxLoad
            )
            expect_equal(testStarcrestAuxBoilerLoad$BoilerLoad,
                         testStarcrestAuxBoilerLoad$newBoilerLoad
            )
          }
)

# custom input table
test_that("calcAuxBoilerLoad works with inputTable",
          {
            #create Benchmark
            tbl<-data.table(shipType = c("fruit","fruit","boat","boat"),
                            subType = c("apple","pear","canoe","kayak"),
                            Transit = c(1,1,10,10),
                            Maneuvering = c(.5,.5,5,5),
                            Berth = c(0,0,0,0),
                            Anchorage = c(.1,.1,1,1)
                            )

            tmpName<-tempfile()
            fwrite(tbl, file = tmpName)

            #Run Calculation
            testOut<-data.table()
            testOut$aux<-calcAuxBoilerLoad(opMode = c("Transit","Maneuvering","Berth","Anchorage"),
                                           shipType = c("boat", "fruit", "boat", "fruit"),
                                           subType = c("kayak", "pear", "canoe", "apple"),
                                           output = "aux",
                                           inputTableLocation = tmpName
                                           )

            testOut$boiler<-calcAuxBoilerLoad(opMode = c("Transit","Maneuvering","Berth","Anchorage"),
                                              shipType = c("boat", "fruit", "boat", "fruit"),
                                              subType = c("kayak", "pear", "canoe", "apple"),
                                              output = "boiler",
                                              inputTableLocation = tmpName
                                              )

            #Compare output against benchmark
            expect_equal(testOut$aux,
                         c(10,0.5,0,0.1)
            )

            expect_equal(testOut$boiler,
                         c(10,0.5,0,0.1)
            )

            #delete that temporary file
            file.remove(tmpName)

          }

)
