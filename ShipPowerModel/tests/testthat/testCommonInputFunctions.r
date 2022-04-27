rm(list=ls())
library(testthat)
library(ShipPowerModel)

context("Test common input functions for resistance-power methods")

test_that("calclwl works",
          {
            shipType<-c("tanker", "ro.ro", "misc")
            customTypes<-c("zamboni","penguin","apple")
            tankerBulkCarrierGCargoShipTypes<-c("zamboni","sled")
            roroPaxShipTypes<-c("penguin", "duck", "robin")
            lbp<-rep(100,3)
            lwl<-lbp*c(1.02,1.035,1.01)


            expect_equal(calclwl(shipType,lbp),
                         lwl)

            expect_equal(calclwl(customTypes,
                                 lbp,
                                 tankerBulkCarrierGCargoShipTypes,
                                 roroPaxShipTypes),
                         lwl)

          }
        )


test_that("calcCb works",
          {
            maxDisplacement<-c(1,0.5,2,4)
            lwl<-c(1,1,1,2)
            breadth<-c(1,1,1,2)
            maxDraft<-c(1,1,1,2)

            cb<-c(1,0.5,1,0.5)

            expect_equal(calcCb(maxDisplacement,
                                lwl,
                                breadth,
                                maxDraft),
                         cb)
          }
        )

test_that("calcPropNum works",
          {
            expect_equal(calcPropNum(c("tanker","passenger","fish")),
                         c(1,2,1)
                         )

            expect_equal(calcPropNum(shipType = c("tanker","passenger","fish"),
                                     paxTugShipTypes = c("tanker","fish")
                                     ),
                         c(2,1,2)
                        )
          }
         )

test_that("calcCbw Works",
          {
           expect_equal(calcCbw(Cb=c(1, 0.75, 0.5),
                                actualDraft=c(1,.75,0.5),
                                maxDraft=c(1, 1, 1)
                                ),
                        c(0.99,
                          1-(1-0.75)*(1/0.75)^(1/3),
                          1-(1-0.5)*(1/0.5)^(1/3)
                          )
                        )
          }
         )

test_that("calcCm works",
          {
            shipType<-shipType<-c("tanker","tug", "passenger", "misc")
            Cbw<-rep(0.25,0.5,0.75, 0.99)
            maxDraft<-rep(1,4)
            actualDraft<-c(0.25,0.5,0.75,1)
            kirstCoefs<-c(0.995, 0.92, 0.95, 0.98)

            tankerBulkCarrierShipTypes=c("octopus","squid","snail","clam")
            tugShipTypes=c("aligator","snake", "turtle")
            roroPaxShipTypes=c("apple","cucumber","squash")
            altShips<- c("octopus", "turtle", "cucumber","zamboni")


            expect_equal(calcCm(shipType = shipType,
                                Cbw = Cbw,
                                maxDraft = maxDraft,
                                actualDraft = actualDraft,
                                CmEquationType = "kristensen"
                                ),
                         1-(maxDraft/actualDraft)*(1 - kirstCoefs)

                        )

            expect_equal(calcCm(shipType = altShips,
                                Cbw = Cbw,
                                maxDraft = maxDraft,
                                actualDraft = actualDraft,
                                CmEquationType = "kristensen",
                                tankerBulkCarrierShipTypes,
                                tugShipTypes,
                                roroPaxShipTypes
                               ),
                         1-(maxDraft/actualDraft)*(1 - kirstCoefs)
                        )

            expect_equal(calcCm(shipType = shipType,
                                Cbw = Cbw,
                                maxDraft = maxDraft,
                                actualDraft = actualDraft,
                                CmEquationType = "BeNfOrD"
                                ),
                         0.977+0.085*(Cbw-0.6)
                        )

            expect_equal(calcCm(shipType = shipType,
                                Cbw = Cbw,
                                maxDraft = maxDraft,
                                actualDraft = actualDraft,
                                CmEquationType = "ScHnEeKlUtH"
                               ),
                         1.006-0.0056*Cbw^-3.56
                        )

})

test_that("calcTEU works",
          {
            shipType<-c("zamboni", rep("container.ship",3))
            dwt<-c(10000,35000,60000, 60001)

            output<-c(0,
                      (dwt[2]/15.19)^(1/0.9814),
                      (dwt[3]/28.81)^(1/0.902),
                      (dwt[4]/37)^(1/0.875)
                      )

            expect_equal(calcTEU(shipType = shipType,
                                 dwt = dwt),
                         output
                         )
          }
         )



#calcSubType====================================================================

test_that("calcSubType works for IMO ship types",
          {
            # Create Benchmark
            testIMOGHG3SubTypeDF<-data.table::data.table(
              shipType=c("bulk.carrier","chemical.tanker","container.ship",
                         "general.cargo","liquified.gas.tanker","oil.tanker","other.tanker",
                         "ferry.pax","cruise","ferry.ro.pax","reefer","ro.ro","vehicle.carrier",
                         "yacht","service.tug","miscellaneous.fishing","offshore","service.other","miscellaneous.other"
              ),
              DWT=c(50000,9000,NA,3000,15600, 119000,1000,NA,NA,NA,1000,NA, 24000,rep(NA,6)),
              Gross_Tonnage=c(rep(NA,7),3700,43000,1500,NA,6000,NA,10000,1000,300,4000,560,790),
              TEU=c(rep(NA,2),7900,rep(NA,16)),
              subType=c("bulk.carrier.handymax","chemical.tanker.small","container.ship.8000",
                        "general.cargo.5000","liquified.gas.tanker.20000","oil.tanker.aframax","other.tanker",
                        "ferry.pax.largest","cruise.60000","ferry.ro.pax.2000","reefer","ro.ro.largest","vehicle.carrier.30000",
                        "yacht","service.tug","miscellaneous.fishing","offshore","service.other","miscellaneous.other"
              )
            )

            #Run Calculation
            testIMOGHG3SubTypeDF[,newSubType:=calcSubType(shipType = shipType,
														  DWT=DWT,
                                                          GT= Gross_Tonnage,
                                                          TEU = TEU
            )
            ]

            #compare output against benchmark
            expect_equal(testIMOGHG3SubTypeDF$newSubType,
                         testIMOGHG3SubTypeDF$subType
            )
          }
)

test_that("calcSubType works for Starcrest ship types",
          {
            # Create Benchmark
            testSCSubTypeDF<-data.table::data.table(
              shipType=c("vehicle.carrier", "bulk.carrier","container.ship",
                         "cruise", "general.cargo", "service.tug",
                         "miscellaneous", "reefer", "ro.ro",
                         "tanker", "cruise.ed"
              ),
              DWT=c(1000, 1000, 1000,
                    1000, 1000, 1000,
                    1000, 1000, 1000,
                    45000, 500
              ),
              Gross_Tonnage=rep(NA,11),
              TEU=c(NA,NA, 3500, rep(NA,8)),
              nPassengers = c(rep(NA,10),3200),
              subType=c("vehicle.carrier", "bulk.carrier","container.ship.3000",
                        "cruise", "general.cargo", "tug",
                        "miscellaneous", "reefer", "ro.ro",
                        "tanker.handysize", "cruise.ed.3500"
              )
            )

            #Run Calculation
            testSCSubTypeDF[,newSubType:=calcSubType(shipType = shipType,
                                                     DWT = DWT,
                                                     GT= Gross_Tonnage,
                                                     TEU = TEU,
                                                     nPassengers = nPassengers,
                                                     method = "starcrest"
            )
            ]

            #compare output against benchmark
            expect_equal(testSCSubTypeDF$newSubType,
                         testSCSubTypeDF$subType
            )
          }
)

test_that("calcSubType works with input file",
          {
            # Create Benchmark
            tbl<-data.table(shipType = c("fruit","fruit","boat","boat"),
                            subType = c("apple","watermelon","canoe","kayak"),
                            sizeMin = c(1,2.1,2,0),
                            sizeMax = c(2,5,3,2),
                            sizeUnits = c("Gross_Tonnage","Gross_Tonnage","Number_of_Passengers","Number_of_Passengers")
                            )

            tmpName<-tempfile()
            fwrite(tbl, file = tmpName)

            # Run Calculation
            testOut<-calcSubType(shipType = c("boat","boat","fruit","fruit"),
                                 GT = c(NA, NA, 1.2,3),
                                 nPassengers = c(1, 2, NA, NA),
                                 DWT = c(NA,NA,NA,NA),
                                 TEU = c(NA,NA,NA,NA),
                                 inputTableLocation = tmpName
                                 )

            # Compare output against benchmark
            expect_equal(testOut$subType,
                         c("kayak","canoe","apple","watermelon"))

            #delete that temporary file
            file.remove(tmpName)
            }
          )


#calcShipType==================================================================
test_that("calcShipType works",{
  # Create Benchmark
  testShipTypeDF<-data.table::data.table(
    Vessel_Type=c("Chip Carrier","Fully Cellular Container","Cruise Ship","Ferry","Ore/Oil Carrier",
                  "General Cargo/Passenger (Inland)","LPG Carrier","Tug, Naval Auxiliary","Multi-Purpose",
                  "Replenishment Tanker","Pipe Laying Barge","Reefer","Ro-Ro Cargo (Inland)","Small Harbor Tug",
                  "Chemical & Oil Carrier","Asphalt & Bitumen Carrier","FPSO","Oil & Liquid Gas Carrier","Recreational"
    ),
    imoShipType=c("bulk.carrier","container.ship","cruise","ferry.ro.pax","general.cargo","general.cargo",
                  "liquified.gas.tanker","service.tug","miscellaneous.other","other.tanker","offshore","reefer",
                  "ro.ro","service.tug","chemical.tanker","other.tanker","offshore","oil.tanker","yacht"
    ),
    starcrestShipType=c("bulk.carrier","container.ship","cruise","cruise.ed","general.cargo","general.cargo",
                        "tanker","service.tug","miscellaneous","tanker","miscellaneous","reefer","ro.ro","service.tug",
                        "tanker","tanker","tanker","tanker","cruise.ed"
    )
  )

  #Run Calculation
  testShipTypeDF[,newIMOShipType:=calcShipType(Vessel_Type,method="imo")]

  testShipTypeDF[,newStarcrestShipType:=calcShipType(Vessel_Type,method="starcrest")]

  #compare output against benchmark
  #IMO Method
  expect_equal(testShipTypeDF$newIMOShipType,
               testShipTypeDF$imoShipType
  )

  #Starcrest Method
  expect_equal(testShipTypeDF$newStarcrestShipType,
               testShipTypeDF$starcrestShipType
  )
}
)


test_that("calcShipType works with input file",
          {
            # Create Benchmark
            tbl<-data.table(Vessel_Type = c("canoe","kayak","apple","pear"),
                            shipType = c("boat","boat","fruit","fruit")
                            )

            tmpName<-tempfile()
            fwrite(tbl, file = tmpName)

            #Run Calculation
            testOut<-calcShipType(c("apple","canoe"),
                                  inputTableLocation = tmpName)

            #compare output against benchmark
            expect_equal(testOut$shipType,
                         c("fruit","boat")
                         )

            #delete that temporary file
            file.remove(tmpName)

          }
         )


#calcOperatingMode=============================================================

test_that("calcOperatingMode works",
          {
            #create Benchmark
            testOperatingModes<-c("Berth","Anchorage","Maneuvering","Transit")
            #Run Calculation
            testOperatingModes_out<-calcOperatingMode(shipSpeed = c(0.5, 2, 7, 16),
                                                      loadFactor = c(0.02, 0.17, 0.19, 0.75),
                                                      berthArea = c(TRUE, FALSE, FALSE, FALSE),
                                                      anchorageArea = c(FALSE, TRUE, FALSE, FALSE)
            )
            #Compare output against benchmark
            expect_equal(testOperatingModes_out,
                         testOperatingModes
            )
          }
)
