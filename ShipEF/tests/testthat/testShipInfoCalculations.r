#library(testthat)

################################################################################
context("Test Ship Information Calculations")
################################################################################

#Test calcTier
test_that("calcTier works",
          {
            #create test benchmark
            testTierDF<- data.frame(
              engineType=rep(c("SSD","MSD","MSD-ED","LNG","GT","ST","GT-ED"),4),
              keelLaidYear=rep(c(1999,2005,2014,2017),each=7),
              shipCategory=rep(3,28),
              tier=c(rep("Tier 0",7),
                     rep("Tier 1",3),rep("Tier 0",4),
                     rep("Tier 2",3),rep("Tier 0",4),
                     rep("Tier 3",3),rep("Tier 0",4)
              )
            )

            #run calculation
            testTierDF$newTier<-calcTier(engineType = testTierDF$engineType,
                                         keelLaidYear = testTierDF$keelLaidYear,
                                         shipCategory = testTierDF$shipCategory)

            #compare output against benchmark
            expect_equal(testTierDF$newTier,as.factor(testTierDF$tier))
          }
)


#calcTestIMO====================================================================
test_that("calcTestIMO works",
          {
            #Create benchmark
            testIMODF<-data.frame(IMO = c(6605022,7417159,9999999,0,3254,NA),
                                  IMOFlag=c("Correct","Correct","Incorrect","Incorrect","Incorrect","Incorrect")
            )
            #run calculation
            testIMODF$NewIMOFlag<-calcTestIMO(IMO = testIMODF$IMO)

            #compare output against benchmark
            expect_equal(testIMODF$NewIMOFlag,as.factor(testIMODF$IMOFlag))
          }
)


#calcShipCategory===============================================================

test_that("calcShipCategory works",
          {
            # Create Benchmark
            testShipCategory<-c(rep(3,4),
                                rep(2,4),
                                rep(1,4)
            )

            #Run Calculation
            testShipCategory_out<-calcShipCategory(mainEngineBore = c(320,500,420,400,
                                                                      280,250,270,300,
                                                                      165,142,190,152
            ),
            mainEngineStroke = c(440,2000,1764,460,
                                 390,320,380,300,
                                 137,128,165,128
            )
            )

            #compare output against benchmark
            expect_equal(testShipCategory_out,
                         testShipCategory
            )
          }
)


#calcEngineType================================================================

test_that("calcEngineType works for Main Engines",
          {
            #create benchmark
            testEngineType<- c("MSD","SSD","SSD","MSD-ED","SSD","MSD-ED","ST","ST","ST","GT","GT","GT")

            #Run Calculation
            testEngineType_out<-calcEngineType(propulsionType = rep(c("Oil Engine(s), Direct Drive",
                                                                      "Oil Engine(s), Electric Drive",
                                                                      "Steam Recip(s), Direct Drive",
                                                                      "Gas Turbine(s), Geared Drive"
            ),
            each=3
            ),
            mainEngineStrokeType = rep(c(4,2,NA),4),
            mainEngineRPM = c(NA,NA,350,NA,NA,600,450,NA,500,NA,NA,NA)
            )

            #compare output against benchmark
            expect_equal(testEngineType_out,
                         testEngineType
            )
          }
)

test_that("calcEngineType works for Aux Engines",
          {
            #create benchmark
            testEngineType<- rep("MSD",12)

            #Run Calculation
            testEngineType_out<-calcEngineType(propulsionType = rep(c("Oil Engine(s), Direct Drive",
                                                                      "Oil Engine(s), Electric Drive",
                                                                      "Steam Recip(s), Direct Drive",
                                                                      "Gas Turbine(s), Geared Drive"
                                                                      ),
                                                                    each=3
                                                                    ),
                                               main_aux_boiler = "aux"
                                               )

            #compare output against benchmark
            expect_equal(testEngineType_out,
                         testEngineType
            )
          }
)

test_that("calcEngineType works for Boiler Engines",
          {
            #create benchmark
            testEngineType<- rep("Boiler",12)

            #Run Calculation
            testEngineType_out<-calcEngineType(propulsionType = rep(c("Oil Engine(s), Direct Drive",
                                                                      "Oil Engine(s), Electric Drive",
                                                                      "Steam Recip(s), Direct Drive",
                                                                      "Gas Turbine(s), Geared Drive"
                                                                      ),
                                                                    each=3
                                                                    ),
                                               main_aux_boiler = "boiler"
                                               )

            #compare output against benchmark
            expect_equal(testEngineType_out,
                         testEngineType
            )
          }
)

test_that("calcEngineType works for mix of Main, Aux, and Boiler Engines",
          {
            #create benchmark
            testEngineType<- c("MSD","SSD","SSD","MSD-ED","SSD","MSD-ED",rep("MSD",3),rep("Boiler",3))

            #Run Calculation
            testEngineType_out<-calcEngineType(propulsionType = rep(c("Oil Engine(s), Direct Drive",
                                                                      "Oil Engine(s), Electric Drive",
                                                                      "Steam Recip(s), Direct Drive",
                                                                      "Gas Turbine(s), Geared Drive"
                                                                      ),
                                                                    each=3
                                                                    ),
                                               mainEngineStrokeType = rep(c(4,2,NA),4),
                                               mainEngineRPM = c(NA,NA,350,NA,NA,600,450,NA,500,NA,NA,NA),
                                               main_aux = c(rep("main",6),rep("aux",3),rep("boiler",3))
                                               )

            #compare output against benchmark
            expect_equal(testEngineType_out,
                         testEngineType
            )
          }
)
