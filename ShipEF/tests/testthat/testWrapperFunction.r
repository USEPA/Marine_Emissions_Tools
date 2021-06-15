
################################################################################
context("Test wrapper function (calcEF)")
################################################################################

 #calcEF==================================================================

#Main
test_that("calcEF EF Baseline works for Main Engines",
          {
            #create Benchmark
            testEFBaseline<-data.table::data.table(
              engineType=c("SSD","MSD","ST","GT","LNG"),
              location=c("OutsideECA","ECA","GreatLakes","ECA","OutsideECA"),
              tier=c("Tier 2","Tier 3","Tier 0","Tier 0","Tier 0")
            )

            testEFBaseline[,hc:=calcEF_HC(engineType = engineType,
                                           main_aux_boiler = "main")
                          ]

            testEFBaseline[,co2:=calcEF_CO2(engineType = engineType,
                                            location = location,
                                            main_aux_boiler = "main")
                          ]

            testEFBaseline[,co:=calcEF_CO(engineType = engineType,
                                          main_aux_boiler = "main")
                          ]

            testEFBaseline[,nox:=calcEF_NOx(engineType = engineType,
                                           location = location,
                                           tier = tier,
                                           main_aux_boiler = "main")
                          ]

            testEFBaseline[,so2:=calcEF_SO2(engineType = engineType,
                                            location = location,
                                            ECAfuelSulfurPercentage = 0.1,
                                            GlobalfuelSulfurPercentage = 0.5,
                                            main_aux_boiler = "main")
                          ]

            testEFBaseline[,pm2.5:=calcEF_PM(engineType = engineType,
                                             location = location,
                                             ECAfuelSulfurPercentage = 0.1,
                                             GlobalfuelSulfurPercentage = 0.5,
                                             pmSize = "pm2.5",
                                             main_aux_boiler = "main"),
                           ]

            testEFBaseline[,pm10:=calcEF_PM(engineType = engineType,
                                             location = location,
                                             ECAfuelSulfurPercentage = 0.1,
                                             GlobalfuelSulfurPercentage = 0.5,
                                             pmSize="pm10",
                                             main_aux_boiler = "main")
                          ]

             #Run Calculation
             calcEF_out<-data.table(
                                      calcEF(engineType = testEFBaseline$engineType,
                                                              location = testEFBaseline$location,
                                                              tier = testEFBaseline$tier,
                                                              pollutants = "ALL",
                                                              loadBasedBSFC = "N",
                                                              output="EF",
                                                              main_aux_boiler = "main")
                                    )

             #Compare output against benchmark
             expect_equal(calcEF_out$hc,
                          testEFBaseline$hc
                         )

             expect_equal(calcEF_out$co2,
                          testEFBaseline$co2
                          )

             expect_equal(calcEF_out$co,
                          testEFBaseline$co
                         )

             expect_equal(calcEF_out$nox,
                         testEFBaseline$nox
                        )

             expect_equal(calcEF_out$pm2.5,
                          testEFBaseline$pm2.5
                         )

             expect_equal(calcEF_out$so2,
                          testEFBaseline$so2
                         )

             expect_equal(calcEF_out$pm10,
                          testEFBaseline$pm10
                         )

          }
         )


test_that("calcEF EF Load Based works for Main Engines",
          {
            #create Benchmark
            testEFLoad<-data.table::data.table(
              engineType=c("SSD","MSD","ST","GT","LNG"),
              location=c("OutsideECA","GreatLakes","ECA","GreatLakes","ECA"),
              tier=c("Tier 1","Tier 3","Tier 0","Tier 0","Tier 0"),
              loadFactor=c(0.284596685,0.521665062,0.766855145,0.078203076,0.708915957249701)
              )

            testEFLoad[,hc:=calcEF_HC(engineType = engineType,
                                       main_aux_boiler = "main")
                      ]

            testEFLoad[,co2:=calcEF_CO2(engineType = engineType,
                                         location = location,
                                         loadFactor = loadFactor,
                                         main_aux_boiler = "main")
                      ]

            testEFLoad[,co:=calcEF_CO(engineType = engineType,
                                       main_aux_boiler = "main")
                      ]

            testEFLoad[,nox:=calcEF_NOx(engineType = engineType,
                                        location = location,
                                        tier = tier,
                                        main_aux_boiler = "main")
                      ]

            testEFLoad[,pm2.5:=calcEF_PM(engineType = engineType,
                                          location = location,
                                          loadFactor = loadFactor,
                                          ECAfuelSulfurPercentage = 0.1,
                                          GlobalfuelSulfurPercentage = 0.5,
                                          pmSize = "pm2.5",
                                          main_aux_boiler = "main")
                      ]

            testEFLoad[,so2:=calcEF_SO2(engineType = engineType,
                                         location = location,
                                         loadFactor = loadFactor,
                                         ECAfuelSulfurPercentage = 0.1,
                                         GlobalfuelSulfurPercentage = 0.5,
                                         main_aux_boiler = "main")
                      ]

            testEFLoad[,pm10:=calcEF_PM(engineType = engineType,
                                         location = location,
                                         loadFactor = loadFactor,
                                         ECAfuelSulfurPercentage = 0.1,
                                         GlobalfuelSulfurPercentage = 0.5,
                                         pmSize = "pm10",
                                         main_aux_boiler = "main")
                      ]

            #Run Calculation
            EF_LLAF_out<-calcEF(engineType = testEFLoad$engineType,
                                      location = testEFLoad$location,
                                      tier = testEFLoad$tier,
                                      loadFactor = testEFLoad$loadFactor,
                                      loadBasedBSFC = "Y",
                                      pollutants = "ALL",
                                      output="EF",
                                      main_aux_boiler = "main")

            #Compare output against benchmark
            expect_equal(EF_LLAF_out$hc,
                         testEFLoad$hc
                         )

            expect_equal(EF_LLAF_out$co2,
                         testEFLoad$co2
                        )

            expect_equal(EF_LLAF_out$co,
                         testEFLoad$co
                        )

            expect_equal(EF_LLAF_out$nox,
                         testEFLoad$nox
                        )

            expect_equal(EF_LLAF_out$pm2.5,
                         testEFLoad$pm2.5
                        )

            expect_equal(EF_LLAF_out$pm10,
                         testEFLoad$pm10
                        )

            expect_equal(EF_LLAF_out$so2,
                         testEFLoad$so2
                        )

            }
           )

test_that("calcEF EF Load range works for Main Engines",
          {
            testEFLoad<-data.table::data.table(
              engineType=c("SSD","MSD","ST","GT","LNG"),
              location=c("OutsideECA","GreatLakes","ECA","GreatLakes","ECA"),
              tier=c("Tier 1","Tier 3","Tier 0","Tier 0","Tier 0"),
              loadFactor=c(0.078203076,0.284596685,0.521665062,0.708915957249701,0.766855145),
              loadRange = "0.0,0.5"
            )

            testEFLoad[,hc:=calcEF_HC(engineType = engineType,
                                       main_aux_boiler = "main")
            ]

            testEFLoad[,#loadFactor>=0.5,
                       co2:=calcEF_CO2(engineType = engineType,
                                         location = location,
                                         main_aux_boiler = "main")
            ]

            testEFLoad[loadFactor<0.5,
                       co2:=calcEF_CO2(engineType = engineType,
                                        location = location,
                                        loadFactor = loadFactor,
                                        main_aux_boiler = "main")
            ]

            testEFLoad[,co:=calcEF_CO(engineType = engineType,
                                       main_aux_boiler = "main")
            ]

            testEFLoad[,nox:=calcEF_NOx(engineType = engineType,
                                         location = location,
                                         tier = tier,
                                         main_aux_boiler = "main")
            ]

            testEFLoad[loadFactor>=0.5
                       ,pm2.5:=calcEF_PM(engineType = engineType,
                                          location = location,
                                          ECAfuelSulfurPercentage = 0.1,
                                          GlobalfuelSulfurPercentage = 0.5,
                                          pmSize = "pm2.5",
                                          main_aux_boiler = "main")
            ]

            testEFLoad[loadFactor<0.5
                       ,pm2.5:=calcEF_PM(engineType = engineType,
                                          location = location,
                                          loadFactor = loadFactor,
                                          ECAfuelSulfurPercentage = 0.1,
                                          GlobalfuelSulfurPercentage = 0.5,
                                          pmSize = "pm2.5",
                                          main_aux_boiler = "main")
            ]

            testEFLoad[loadFactor >= 0.5,
                       so2:=calcEF_SO2(engineType = engineType,
                                         location = location,
                                         ECAfuelSulfurPercentage = 0.1,
                                         GlobalfuelSulfurPercentage = 0.5,
                                         main_aux_boiler = "main")
            ]

            testEFLoad[loadFactor < 0.5
                       ,so2:=calcEF_SO2(engineType = engineType,
                                         location = location,
                                         loadFactor = loadFactor,
                                         ECAfuelSulfurPercentage = 0.1,
                                         GlobalfuelSulfurPercentage = 0.5,
                                         main_aux_boiler = "main")
            ]

            testEFLoad[loadFactor >=0.5
                       ,pm10:=calcEF_PM(engineType = engineType,
                                         location = location,
                                         ECAfuelSulfurPercentage = 0.1,
                                         GlobalfuelSulfurPercentage = 0.5,
                                         pmSize = "pm10",
                                         main_aux_boiler = "main")
            ]

            testEFLoad[loadFactor <0.5,
                       pm10:=calcEF_PM(engineType = engineType,
                                         location = location,
                                         loadFactor = loadFactor,
                                         ECAfuelSulfurPercentage = 0.1,
                                         GlobalfuelSulfurPercentage = 0.5,
                                         pmSize = "pm10",
                                         main_aux_boiler = "main")
            ]

            #Run Calculation
            EF_LLAF_out<-calcEF(engineType = testEFLoad$engineType,
                                      location = testEFLoad$location,
                                      tier = testEFLoad$tier,
                                      loadFactor = testEFLoad$loadFactor,
                                      pollutants = "ALL",
                                      loadBasedBSFC = testEFLoad$loadRange[1],
                                      output="EF",
                                      main_aux_boiler = "main")

            #Compare output against benchmark
            expect_equal(EF_LLAF_out$hc,
                         testEFLoad$hc
            )

            expect_equal(EF_LLAF_out$co2,
                         testEFLoad$co2
            )

            expect_equal(EF_LLAF_out$co,
                         testEFLoad$co
            )

            expect_equal(EF_LLAF_out$nox,
                         testEFLoad$nox
            )

            expect_equal(EF_LLAF_out$pm2.5,
                         testEFLoad$pm2.5
            )

            expect_equal(EF_LLAF_out$pm10,
                         testEFLoad$pm10
            )

            expect_equal(EF_LLAF_out$so2,
                         testEFLoad$so2
            )
          }
        )

test_that("calcEF EF Load LLAF works",
          {
            #create Benchmark
            testEFLoadLLAF<-data.table::data.table(
              engineType=c("SSD", "MSD", "ST", "GT", "LNG"),
              location=c("OutsideECA", "GreatLakes", "ECA", "GreatLakes", "ECA"),
              tier=c("Tier 1","Tier 3","Tier 0","Tier 0","Tier 0"),
              loadFactor=c(0.284596685, 0.521665062, 0.766855145, 0.078203076, 0.708915957249701)
            )

            #The previous tests check the accuracy of the EF and LLAF components,
            #so we just need to make sure they get multiplied at the end for this test
            testEFs<-calcEF(engineType = testEFLoadLLAF$engineType,
                                 location = testEFLoadLLAF$location,
                                 tier = testEFLoadLLAF$tier,
                                 loadFactor = testEFLoadLLAF$loadFactor,
                                 pollutants = "ALL",
                                 output="EF",
                                 main_aux_boiler = "main")

            testLLAFs<-calcLLAF(engineType = testEFLoadLLAF$engineType,
                                location = testEFLoadLLAF$location,
                                loadFactor = testEFLoadLLAF$loadFactor,
                                pollutants = "ALL")

            #Run Calculation
            EF_LLAF_out<-calcEF(engineType = testEFLoadLLAF$engineType,
                                     location = testEFLoadLLAF$location,
                                     tier = testEFLoadLLAF$tier,
                                     loadFactor = testEFLoadLLAF$loadFactor,
                                     pollutants = "ALL",
                                     output="EF_LLAF",
                                     main_aux_boiler = "main")

            #Compare output against benchmark
            expect_equal(EF_LLAF_out$hc,
                         testEFs$hc*testLLAFs$hc,
                         tolerance=1e-07
            )

            expect_equal(EF_LLAF_out$co2,
                         testEFs$co2*testLLAFs$co2,
                         tolerance=1e-07
            )

            expect_equal(EF_LLAF_out$nox,
                         testEFs$nox*testLLAFs$nox,
                         tolerance=1e-07
            )

            expect_equal(EF_LLAF_out$so2,
                         testEFs$so2*testLLAFs$so2,
                         tolerance=1e-07
            )

            expect_equal(EF_LLAF_out$pm2.5,
                         testEFs$pm2.5*testLLAFs$pm2.5,
                         tolerance=1e-07
            )

            expect_equal(EF_LLAF_out$pm10,
                         testEFs$pm10*testLLAFs$pm10,
                         tolerance=1e-07
            )

          }
)

test_that("calcEF EF Load LLAF works with input LLAF table",
          {
            #create Benchmark
            testEFLoadLLAF<-data.table::data.table(
              engineType=c("SSD", "MSD", "ST", "GT", "LNG"),
              location=c("OutsideECA", "GreatLakes", "ECA", "GreatLakes", "ECA"),
              tier=c("Tier 1","Tier 3","Tier 0","Tier 0","Tier 0"),
              loadFactor=c(0.284596685, 0.521665062, 0.766855145, 0.078203076, 0.708915957249701)
            )

            #create LLAF table
            tbl<-data.table(load=c(1:100))

            tbl[,co:=load*2]
            tbl[,pm2.5:=load*2]
            tbl[,nox:=load*2]

            #create a temporary file!!!
            tmpName<-tempfile()

            fwrite(tbl, file = tmpName)


            #The previous tests check the accuracy of the EF and LLAF components,
            #so we just need to make sure they get multiplied at the end for this test
            testEFs<-calcEF(engineType = testEFLoadLLAF$engineType,
                                 location = testEFLoadLLAF$location,
                                 tier = testEFLoadLLAF$tier,
                                 loadFactor = testEFLoadLLAF$loadFactor,
                                 pollutants = c("co","pm2.5","nox"),
                                 output="EF",
                                 main_aux_boiler = "main")

            testLLAFs<-calcLLAF(engineType = testEFLoadLLAF$engineType,
                                loadFactor = testEFLoadLLAF$loadFactor,
                                pollutants = c("co","pm2.5","nox"),
                                inputTableLocation = tmpName)


            #Run Calculation
            EF_LLAF_out<-calcEF(engineType = testEFLoadLLAF$engineType,
                                     location = testEFLoadLLAF$location,
                                     tier = testEFLoadLLAF$tier,
                                     loadFactor = testEFLoadLLAF$loadFactor,
                                     pollutants = c("co","pm2.5","nox"),
                                     output="EF_LLAF",
                                     main_aux_boiler = "main",
                                     inputTableLocation = tmpName)

            file.remove(tmpName)

            #Compare output against benchmark

            expect_equal(EF_LLAF_out$nox,
                         testEFs$nox*testLLAFs$nox,
                         tolerance=1e-07
            )

            expect_equal(EF_LLAF_out$co,
                         testEFs$co*testLLAFs$co,
                         tolerance=1e-07
            )

            expect_equal(EF_LLAF_out$pm2.5,
                         testEFs$pm2.5*testLLAFs$pm2.5,
                         tolerance=1e-07
            )

          }
)

#Aux
test_that("calcEF EF Baseline works for Aux Engines",
          {
            #create Benchmark
            testEFAux<-data.table::data.table(
              engineType=c("SSD","MSD","ST","GT","LNG"),
              location=c("OutsideECA","ECA","GreatLakes","ECA","OutsideECA"),
              tier=c("Tier 2","Tier 3","Tier 0","Tier 0","Tier 0")
            )

            testEFAux[,hc:=calcEF_HC(engineType = engineType,
                                           main_aux_boiler = "aux")
            ]

            testEFAux[,co2:=calcEF_CO2(engineType = engineType,
                                             location = location,
                                             main_aux_boiler = "aux")
            ]

            testEFAux[,co:=calcEF_CO(engineType = engineType,
                                           main_aux_boiler = "aux")
            ]

            testEFAux[,nox:=calcEF_NOx(engineType = engineType,
                                             location = location,
                                             tier = tier,
                                             main_aux_boiler = "aux")
            ]

            testEFAux[,so2:=calcEF_SO2(engineType = engineType,
                                             location = location,
                                             ECAfuelSulfurPercentage = 0.1,
                                             GlobalfuelSulfurPercentage = 0.5,
                                             main_aux_boiler = "aux")
            ]

            testEFAux[,pm2.5:=calcEF_PM(engineType = engineType,
                                              location = location,
                                              ECAfuelSulfurPercentage = 0.1,
                                              GlobalfuelSulfurPercentage = 0.5,
                                              pmSize = "pm2.5",
                                              main_aux_boiler = "aux"),
            ]

            testEFAux[,pm10:=calcEF_PM(engineType = engineType,
                                             location = location,
                                             ECAfuelSulfurPercentage = 0.1,
                                             GlobalfuelSulfurPercentage = 0.5,
                                             pmSize="pm10",
                                             main_aux_boiler = "aux")
            ]

            #Run Calculation
            calcEF_out<-data.table(
              calcEF(engineType = testEFAux$engineType,
                           location = testEFAux$location,
                           tier = testEFAux$tier,
                           pollutants = "ALL",
                           loadBasedBSFC = "N",
                           output="EF",
                           main_aux_boiler = "aux")
            )

            #Compare output against benchmark
            expect_equal(calcEF_out$hc,
                         testEFAux$hc
            )

            expect_equal(calcEF_out$co2,
                         testEFAux$co2
            )

            expect_equal(calcEF_out$co,
                         testEFAux$co
            )

            expect_equal(calcEF_out$nox,
                         testEFAux$nox
            )

            expect_equal(calcEF_out$pm2.5,
                         testEFAux$pm2.5
            )

            expect_equal(calcEF_out$so2,
                         testEFAux$so2
            )

            expect_equal(calcEF_out$pm10,
                         testEFAux$pm10
            )

          }
)

#Boiler
test_that("calcEF EF Baseline works for Aux Engines",
          {
            #create Benchmark
            testEFBoiler<-data.table::data.table(
              engineType=c("SSD","MSD","ST","GT","LNG"),
              location=c("OutsideECA","ECA","GreatLakes","ECA","OutsideECA"),
              tier=c("Tier 2","Tier 3","Tier 0","Tier 0","Tier 0")
            )

            testEFBoiler[,hc:=calcEF_HC(engineType = engineType,
                                      main_aux_boiler = "boiler")
            ]

            testEFBoiler[,co2:=calcEF_CO2(engineType = engineType,
                                        location = location,
                                        main_aux_boiler = "boiler")
            ]

            testEFBoiler[,co:=calcEF_CO(engineType = engineType,
                                      main_aux_boiler = "boiler")
            ]

            testEFBoiler[,nox:=calcEF_NOx(engineType = engineType,
                                        location = location,
                                        tier = tier,
                                        main_aux_boiler = "boiler")
            ]

            testEFBoiler[,so2:=calcEF_SO2(engineType = engineType,
                                        location = location,
                                        ECAfuelSulfurPercentage = 0.1,
                                        GlobalfuelSulfurPercentage = 0.5,
                                        main_aux_boiler = "boiler")
            ]

            testEFBoiler[,pm2.5:=calcEF_PM(engineType = engineType,
                                         location = location,
                                         ECAfuelSulfurPercentage = 0.1,
                                         GlobalfuelSulfurPercentage = 0.5,
                                         pmSize = "pm2.5",
                                         main_aux_boiler = "boiler"),
            ]

            testEFBoiler[,pm10:=calcEF_PM(engineType = engineType,
                                        location = location,
                                        ECAfuelSulfurPercentage = 0.1,
                                        GlobalfuelSulfurPercentage = 0.5,
                                        pmSize="pm10",
                                        main_aux_boiler = "boiler")
            ]

            #Run Calculation
            calcEF_out<-data.table(
              calcEF(engineType = testEFBoiler$engineType,
                           location = testEFBoiler$location,
                           tier = testEFBoiler$tier,
                           pollutants = "ALL",
                           loadBasedBSFC = "N",
                           output="EF",
                           main_aux_boiler = "boiler")
            )

            #Compare output against benchmark
            expect_equal(calcEF_out$hc,
                         testEFBoiler$hc
            )

            expect_equal(calcEF_out$co2,
                         testEFBoiler$co2
            )

            expect_equal(calcEF_out$co,
                         testEFBoiler$co
            )

            expect_equal(calcEF_out$nox,
                         testEFBoiler$nox
            )

            expect_equal(calcEF_out$pm2.5,
                         testEFBoiler$pm2.5
            )

            expect_equal(calcEF_out$so2,
                         testEFBoiler$so2
            )

            expect_equal(calcEF_out$pm10,
                         testEFBoiler$pm10
            )

          }
)
