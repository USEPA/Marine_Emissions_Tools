################################################################################
context("Test Emission Factor Assignments")
################################################################################

#calcEF_CO====================================================================

test_that("calcEF_CO works for Main engine",
          {
            #Create Benchmark
            testcalcCO<-data.table::data.table(engineType =  c("SSD","MSD","MSD-ED","ST","GT","GT-ED","LNG"),
                                               CO = c(1.4, 1.1, 1.1, 0.2, 0.2, 0.2, 1.3)
            )

            #Run Calculation
            testcalcCO[,newCO:=calcEF_CO(engineType=engineType)]

            #Compare output against benchmark
            expect_equal(testcalcCO$newCO,
                         testcalcCO$CO
            )
          }
)

test_that("calcEF_CO works for Aux engine",
          {
            #Create Benchmark
            testcalcAuxCO<-data.table::data.table(engineType = c("MSD","HSD","LNG"),
                                                  CO = c(1.1,0.9,1.3)
            )
            #Run Calculation
            testcalcAuxCO[,newCO:=calcEF_CO(engineType=engineType,
                                             main_aux_boiler = "aux")
            ]

            #Compare output against benchmark
            expect_equal(testcalcAuxCO$CO,
                         testcalcAuxCO$newCO
            )
          }
)

test_that("calcEF_CO works for boilers",
          {
            #Create Benchmark
            testcalcBoilerCO<-data.table::data.table(engineType = c("MSD","HSD","LNG"),
                                                     CO = c(0.2,0.2,0.2)
            )
            #Run Calculation
            testcalcBoilerCO[,newCO:=calcEF_CO(engineType=engineType,
                                                main_aux_boiler = "boiler")
            ]

            #Compare output against benchmark
            expect_equal(testcalcBoilerCO$newCO,
                         testcalcBoilerCO$CO
            )
          }
)

#calcEF_NOx===================================================================

#TODO This needs to be fixed!!!
#Main Engines
test_that("calcEF_NOx works for Main engines",
          {
            #Create Benchmark
            testcalcNox<-data.table::data.table(
              engineType=rep(c("SSD","MSD","ST","GT","LNG"),each=12),
              location=rep(c("ECA","OutsideECA","GreatLakes"),20),
              tier=rep(c(rep("Tier 0",3),
                         rep("Tier 1",3),
                         rep("Tier 2",3),
                         rep("Tier 3",3))
                       ,5),
              loadFactor=rep(c(0.22,0.9,0.83,0.63,0.1,0.48,0.96,0.34,0.27,0.45,0.93,0.03),5),
              nox=c( #T0              #T1              #T2                #T3 (last number is under low load, so reverts to T2)
                     17, 18.1, 17,    16, 17, 16,      14.4, 15.3, 14.4,  3.4, 3.6, 14.4,
                     13.2, 14, 13.2,  12.2, 13, 12.2,  10.5, 11.2, 10.5,  2.6, 2.8, 10.5,
                     rep(2, 3),       rep(NA,3),       rep(NA,3),         rep(NA,3),
                     rep(5.7,3),      rep(NA,3),       rep(NA,3),         rep(NA,3),
                     rep(1.3,3),      rep(NA,3),       rep(NA,3),         rep(NA,3)
              )
            )

            testcalcNox<-testcalcNox[is.na(nox)==FALSE]

            #Run Calculation
            testcalcNox[,newNOx:= calcEF_NOx(engineType = engineType,
                                             location = location,
                                             tier = tier,
                                             loadFactor = loadFactor
                                             )
            ]

            #Compare output against benchmark
            expect_equal(testcalcNox$newNOx,
                         testcalcNox$nox
            )
          }
)


#Auxiliary Engines
test_that("calcEF_NOx works for Aux Engines",
          {
            #Create Benchmark
            testcalcAuxNox<-data.table::data.table(
              engineType=rep(c("HSD","MSD","LNG"),
                             each=12),
              location=rep(c("ECA","OutsideECA","GreatLakes"),
                           12),
              tier=rep(c(rep("Tier 0",3),
                         rep("Tier 1",3),
                         rep("Tier 2",3),
                         rep("Tier 3",3)),
                       3),
              auxNox=c(13.8,11.6,13.8,12.2,10.4,12.2,10.5,8.2,10.5,2.6,2.6,2.6,10.9,14.7,
                       10.9,9.8,13,9.8,7.7,11.2,7.7,2,2,2,1.3,1.3,1.3,
                       rep(NA,9)
              ),
              boilerNOx=rep(c(2,2.1,2),12)
            )
            testcalcAuxNox<-testcalcAuxNox[is.na(auxNox)==FALSE]

            #Run Calculation
            testcalcAuxNox[,newAuxNOx:= calcEF_NOx(engineType = engineType, location = location, tier = tier, main_aux_boiler = "aux")]

            #Compare output against benchmark
            expect_equal(testcalcAuxNox$newAuxNox,
                         testcalcAuxNox$auxNOx
            )
          }
)

#Boiler
test_that("calcEF_NOx works for Boilers",
          {
            #Create Benchmark
            testcalcAuxNox<-data.table::data.table(
              engineType=rep(c("HSD","MSD","LNG"),
                             each=12),
              location=rep(c("ECA","OutsideECA","GreatLakes"),
                           12),
              tier=rep(c(rep("Tier 0",3),
                         rep("Tier 1",3),
                         rep("Tier 2",3),
                         rep("Tier 3",3)),
                       3),
              boilerNOx=rep(c(2,2.1,2),12)
            )

            #Run Calculation
            testcalcAuxNox[,newBoilerNOx:= calcEF_NOx(engineType = engineType,
                                                       location = location,
                                                       tier = tier,
                                                       main_aux_boiler = "boiler")
            ]

            #Compare output against benchmark
            expect_equal(testcalcAuxNox$newBoilerNOx,
                         testcalcAuxNox$boilerNOx
            )
          }
)

#calcEF_HC====================================================================
#Main Engine
test_that("calcEF_HC works for Main engines",
          {
            #Create Benchmark
            testcalcHC<-data.table::data.table(engineType =  c("SSD","MSD","MSD-ED","ST","GT","GT-ED","LNG"),
                                               HC = c(0.6,0.5,0.5,0.1,0.1,0.1,0)
            )

            #Run Calculation
            testcalcHC[,newHC:=calcEF_HC(engineType=engineType)]

            #Compare output against benchmark
            expect_equal(testcalcHC$newHC,
                         testcalcHC$HC
            )
          }
)

#Auxiliary
test_that("calcEF_HC Auxiliary works for Aux engines",
          {
            #Create Benchmark
            testcalcAuxHC<-data.table::data.table(engineType =  c("MSD","HSD","LNG"),
                                                  HC = c(0.4,0.4,0)
            )

            #Run Calculation
            testcalcAuxHC[,newHC:=calcEF_HC(engineType=engineType,
                                             main_aux_boiler = "aux")
            ]

            #Compare output against benchmark
            expect_equal(testcalcAuxHC$newHC,
                         testcalcAuxHC$HC
            )
          }
)

#Boiler
test_that("calcEF_HC Boiler works for Boilers",
          {
            #Create Benchmark
            testcalcBoilerHC<-data.table::data.table(engineType =  c("MSD","HSD","LNG"),
                                                     HC = c(0.1,0.1,0.1)
            )

            #Run Calculation
            testcalcBoilerHC[,newHC:=calcEF_HC(engineType=engineType, main_aux_boiler = "boiler")]

            #Compare output against benchmark
            expect_equal(testcalcBoilerHC$newHC,
                         testcalcBoilerHC$HC
            )
          }
)

#calcLoadBSFC===================================================================
test_that("calcLoadBSFC works",
          {
            #Create Benchmark
            testLoad<-data.table(loadFactor = c(NA,0.5, 0.7),
                                 BSFC_Baseline = c(185,195,300),
                                 loadBSFC = c(185, 202.5563, 301.785)
            )

            #Run Calculation
            testLoad[,newLoadBSFC := calcLoadBSFC(loadFactor,
                                                  BSFC_Baseline
            )
            ]

            #Compare output against benchmark
            expect_equal(testLoad$newLoadBSFC,
                         testLoad$loadBSFC,
                         tolerance=1e-5
            )
          }
)

#calcEF_CO2====================================================================

#Main
test_that("calcEF_CO2 Baseline works for Main engines",
          {
            #Create Benchmark
            testcalcCO2<-data.table::data.table(engineType=rep(c("SSD", "MSD", "MSD-ED", "GT", "ST", "GT-ED", "LNG"), each=3),
                                                location=rep(c("ECA", "OutsideECA", "GreatLakes"), 7),
                                                loadFactor=rep(c(0.06, 0.54, 0.79), 7),
                                                co2=c(593.11, 607.23, 593.11,
                                                      rep(c(657.23, 669.51, 657.23), 2),
                                                      rep(c(961.8, 961.8, 961.8), 3),
                                                      rep(456.5, 3)),
                                                loadCO2=c(734.8858, 625.0085, 594.9282,
                                                          rep(c(814.3329, 689.1119, 659.2447), 2),
                                                          rep(c(1191.7067, 989.9596, 964.7484), 3),
                                                          565.6208, 469.8654, 457.8994)
            )

            #Run Calculation
            testcalcCO2[,newCO2:=calcEF_CO2(engineType = engineType, location=location)]
            testcalcCO2[,newLoadCO2:=calcEF_CO2(engineType = engineType, location=location,loadFactor = loadFactor)]

            #Compare output against benchmark
            # baseline
            expect_equal(testcalcCO2$co2,
                         testcalcCO2$newCO2,
                         tolerance=1e-04
            )
            #load based
            expect_equal(testcalcCO2$loadCO2,
                         testcalcCO2$newLoadCO2,
                         tolerance=1e-04
            )
          }
)

#Auxiliary and Boiler
test_that("calcEF_CO2 Auxiliary works",
          {
            #Create Benchmark
            testcalcAuxCO2<-data.table::data.table(engineType=rep(c("MSD","HSD","LNG"),each=3),
                                                   location=rep(c("ECA", "OutsideECA", "GreatLakes"),3),
                                                   AuxCO2=c(695.702, 706.878, 695.702, 695.702, 706.878, 695.702, 456.5, 456.5, 456.5)
            )

            #Run Calculation
            testcalcAuxCO2[,newAuxCO2:=calcEF_CO2(engineType,
                                                   location,
                                                   main_aux_boiler = "aux")
            ]

            #Compare output against benchmark
            expect_equal(testcalcAuxCO2$AuxCO2,
                         testcalcAuxCO2$newAux
            )
          }
)

test_that("calcEF_CO2 Boiler works",
          {
            #Create Benchmark
            testcalcBoilerCO2<-data.table::data.table(engineType=rep(c("MSD","HSD","LNG"),each=3),
                                                      location=rep(c("ECA","OutsideECA","GreatLakes"),3),
                                                      BoilerCO2=c(961.8,949.77,961.8,961.8,949.77,961.8,961.8,949.77,961.8)
            )

            #Run Calculation
            testcalcBoilerCO2[,newBoilerCO2:=calcEF_CO2(engineType,
                                                         location,
                                                         main_aux_boiler = "boiler")
            ]

            #Compare output against benchmark
            expect_equal(testcalcBoilerCO2$BoilerCO2,
                         testcalcBoilerCO2$newBoilerCO2
            )
          }
)

#calcEF_PM=====================================================================
#Main
test_that("calcEF_PM works for Main engines",
          {
            #Create Benchmark inputs
            engineTypes<-c("SSD","MSD","MSD-ED","GT","ST","GT-ED","LNG")
            locations<- c("ECA","OutsideECA","GreatLakes")
            loadFactors<-c(0.06, 0.54, 0.79)

            #manually calculate outputs
            # EF = PM_base + (S_act*BSFC*FSC*MWR*0.0001)
            SSD_ECA<-0.1545+(0.1*185*2.247*7*0.0001)
            SSD_HFO<-0.5761+(0.5*195*2.247*7*0.0001)
            MSD_ECA<-0.1545+(0.1*205*2.247*7*0.0001)
            MSD_HFO<-0.5761+(0.5*215*2.247*7*0.0001)
            GT <-0.01
            ST <-0.16
            LNG<-0.03

            SSD_ECA_l<-0.1545+(0.1*calcLoadBSFC(loadFactors[1],185)*2.247*7*0.0001)
            SSD_HFO_l<-0.5761+(0.5*calcLoadBSFC(loadFactors[2],195)*2.247*7*0.0001)
            SSD_GL_l <-0.1545+(0.1*calcLoadBSFC(loadFactors[3],185)*2.247*7*0.0001)

            MSD_ECA_l<-0.1545+(0.1*calcLoadBSFC(loadFactors[1],205)*2.247*7*0.0001)
            MSD_HFO_l<-0.5761+(0.5*calcLoadBSFC(loadFactors[2],215)*2.247*7*0.0001)
            MSD_GL_l <-0.1545+(0.1*calcLoadBSFC(loadFactors[3],205)*2.247*7*0.0001)

            testcalcPM<-data.table::data.table(engineType=rep(engineTypes,each=3),
                                               location=rep(locations,7),
                                               loadFactor=rep(loadFactors,7),
                                               pm=c(SSD_ECA,SSD_HFO,SSD_ECA, #SSD
                                                    rep(c(MSD_ECA,MSD_HFO,MSD_ECA),2), #MSD, MSD-ED
                                                    rep(GT,3), #GT
                                                    rep(ST,3), #ST
                                                    rep(GT,3), #GT-ED
                                                    rep(LNG,3)  #LNG
                                               ),
                                               loadPM=c(SSD_ECA_l,SSD_HFO_l,SSD_GL_l, #SSD
                                                        rep(c(MSD_ECA_l,MSD_HFO_l,MSD_GL_l),2),#MSD, MSD-ED
                                                        rep(GT,3), #GT
                                                        rep(ST,3), #ST
                                                        rep(GT,3), #GT-ED
                                                        rep(LNG,3)  #LNG
                                               )
            )

            #Run Calculation
            testcalcPM[,newPM:=calcEF_PM(engineType = engineType,
                                          location = location,
                                          pmSize="pm10")
            ]
            testcalcPM[,newLoadPM:=calcEF_PM(engineType = engineType,
                                              location = location,
                                              loadFactor = loadFactor,
                                              pmSize="pm10")
            ]

            #Compare output against benchmark
            #Baseline
            expect_equal(testcalcPM$pm,
                         testcalcPM$newPM,
                         tolerance=1e-07
            )
            #load-based
            expect_equal(testcalcPM$loadPM,
                         testcalcPM$newLoadPM,
                         tolerance=1e-07
            )
          }
)


#Auxiliary
test_that("calcEF_PM works for Aux engines",
          {
            #Create Benchmark inputs
            engineType<-c("MSD","HSD","LNG")
            location<-c("ECA","OutsideECA","GreatLakes")

            #calculate Benchmark outputs
            # EF = PM_base + (S_act*BSFC*FSC*MWR*0.0001)
            MSD_ECA<-0.1545+(0.1*217*2.247*7*0.0001)
            MSD_HFO<-0.5761+(0.5*227*2.247*7*0.0001)
            HSD_ECA<-0.1545+(0.1*217*2.247*7*0.0001)
            HSD_HFO<-0.5761+(0.5*227*2.247*7*0.0001)
            LNG<-0.03

            testcalcAuxPM<-data.table::data.table(engineType=rep(engineType,each=3),
                                                  location=rep(location,3),
                                                  AuxPM=c(MSD_ECA, MSD_HFO, MSD_ECA, #MSD
                                                          HSD_ECA, HSD_HFO, HSD_ECA, #HSD
                                                          rep(LNG,3) #LNG
                                                  )
            )

            #Run Calculation
            testcalcAuxPM[,newAuxPM:=calcEF_PM(engineType = engineType,
                                                location = location,
                                                main_aux_boiler = "aux")]

            #Compare output against benchmark
            expect_equal(testcalcAuxPM$newAuxPM,
                         testcalcAuxPM$AuxPM
            )
          }
)

#Boiler
test_that("calcEF_PM works for Boilers",
          {
            #calculate Benchmark outputs
            # EF = PM_base + (S_act*BSFC*FSC*MWR*0.0001)
            ECA<-0.1545+(0.1*300*2.247*7*0.0001)
            HFO<-0.5761+(0.5*305*2.247*7*0.0001)


            testcalcBoilerPM<-data.table::data.table(engineType=rep(c("MSD","HSD","LNG"),each=3),
                                                     location=rep(c("ECA","OutsideECA","GreatLakes"),3),
                                                     BoilerPM=c(ECA, HFO, ECA,#MSD
                                                                ECA, HFO, ECA,#HSD
                                                                ECA, HFO, ECA)#LNG
            )

            #Run Calculation
            testcalcBoilerPM[,newBoilerPM:=calcEF_PM(engineType = engineType,
                                                      location = location,
                                                      main_aux_boiler = "boiler"
            )
            ]

            #Compare output against benchmark
            expect_equal(testcalcBoilerPM$newBoilerPM,
                         testcalcBoilerPM$BoilerPM
            )
          }
)

#calcEF_SO2====================================================================

# TODO FuelSulfur algorithm might be broken - these tests should probably fail
# TODO no special handling for fuel type assignment to GT/ST

# Main Engine
test_that("calcEF_SO2 works for Main engines",
          {
            #create Benchmark
            engineType<-c("SSD","MSD","MSD-ED","GT","ST","GT-ED","LNG")
            location<-c("ECA","OutsideECA","GreatLakes")
            loadFactor<-c(0.06,0.54,0.79)
            fuelSulfurLevel<-c(0.1, 0.5, 0.1)

            # so2=BSFC_LoadFactor*2*0.97753*(fuelSulfurLevel/100)
            SSD<-c(185, 195, 185)*2*0.97753*(fuelSulfurLevel/100)
            MSD<-c(205, 215, 205)*2*0.97753*(fuelSulfurLevel/100)
            GT <-c(300, 300, 300)*2*0.97753*(0.1/100) #gas turbines only run MDO/MGO
            LNG<-rep(166*2e-4,3)

            SSD_load<-calcLoadBSFC(loadFactor,c(185, 195, 185))*2*0.97753*(fuelSulfurLevel/100)
            MSD_load<-calcLoadBSFC(loadFactor,c(205, 215, 205))*2*0.97753*(fuelSulfurLevel/100)
            GT_load <-calcLoadBSFC(loadFactor,c(300, 300, 300))*2*0.97753*(0.1/100)
            LNG_load<-calcLoadBSFC(loadFactor,c(166, 166, 166))*2e-4

            testcalcSO2<-data.table::data.table(engineType=rep(engineType,each=3),
                                                location=rep(location,7),
                                                loadFactor=rep(loadFactor,7),
                                                SO2=c(SSD, #SSD
                                                      rep(MSD,2), #MSD, MSD-ED
                                                      rep(GT,3), #GT, ST, GT-ED
                                                      LNG), #LNG

                                                loadSO2=c(SSD_load,
                                                          rep(MSD_load,2),
                                                          rep(GT_load,3),
                                                          LNG_load)

            )

            #Run Calculation
            testcalcSO2[,newSO2:=calcEF_SO2(engineType = engineType,
                                             location=location)
            ]

            testcalcSO2[,newLoadSO2:=calcEF_SO2(engineType = engineType,
                                                 location=location,
                                                 loadFactor = loadFactor)
            ]

            #Compare output against benchmark
            #Baseline
            expect_equal(testcalcSO2$newSO2,
                         testcalcSO2$SO2
            )

            #Load Based
            expect_equal(testcalcSO2$newLoadSO2,
                         testcalcSO2$loadSO2
            )
          }
)


#Auxiliary and Boiler

test_that("calcEF_SO2 Auxiliary works",
          {
            #create Benchmark
            engineType<-c("MSD","HSD","LNG")
            location<-c("ECA","OutsideECA","GreatLakes")
            loadFactor<-c(0.06,0.54,0.79)
            fuelSulfurLevel<-c(0.1, 0.5, 0.1)

            # so2=BSFC*2*0.97753*(fuelSulfurLevel/100)
            MSD<-c(217,227,217)*2*0.97753*(fuelSulfurLevel/100)
            HSD<-c(217,227,217)*2*0.97753*(fuelSulfurLevel/100)
            LNG<-rep(166*2e-4,3)

            testcalcAuxSO2<-data.table::data.table(engineType=rep(c("MSD","HSD","LNG"),each=3),
                                                   location=rep(c("ECA","OutsideECA","GreatLakes"),3),
                                                   AuxSO2=c(MSD, HSD, LNG)
            )

            #Run Calculation
            testcalcAuxSO2[,newAuxSO2:=calcEF_SO2(engineType = engineType,
                                                   location = location,
                                                   main_aux_boiler = "aux"
            )
            ]

            #Compare output against benchmark
            expect_equal(testcalcAuxSO2$newAuxSO2,
                         testcalcAuxSO2$AuxSO2
            )
          }
)

test_that("calcEF_SO2 Boiler works",
          {
            #create Benchmark
            engineType<-c("MSD","HSD","LNG")
            location<-c("ECA","OutsideECA","GreatLakes")
            loadFactor<-c(0.06,0.54,0.79)
            fuelSulfurLevel<-c(0.1, 0.5, 0.1)

            # so2=BSFC*2*0.97753*(fuelSulfurLevel/100)
            EF<-c(300,305,300)*2*0.97753*(fuelSulfurLevel/100)

            testcalcBoilerSO2<-data.table::data.table(engineType=rep(c("MSD","HSD","LNG"),each=3),
                                                      location=rep(c("ECA","OutsideECA","GreatLakes"),3),
                                                      BoilerSO2=c(EF, #MSD
                                                                  EF, #HSD
                                                                  EF) #LNG
            )

            #Run Calculation
            testcalcBoilerSO2[,newBoilerSO2:=calcEF_SO2(engineType = engineType,
                                                         location = location,
                                                         main_aux_boiler = "boiler"
            )
            ]

            #Compare output against benchmark
            expect_equal(testcalcBoilerSO2$newBoilerSO2,
                         testcalcBoilerSO2$BoilerSO2
            )
          }
)
