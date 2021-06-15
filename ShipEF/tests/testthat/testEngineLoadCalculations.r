################################################################################
context("Test Engine load Calculations")
################################################################################

#calcLLAF=======================================================================

test_that("calcLLAF LLAF works",
          {
            #create Benchmark
            testLLAF<-data.table::data.table(
              engineType=c("SSD","MSD","ST","GT","LNG"),
              location=c("OutsideECA","ECA","GreatLakes","ECA","OutsideECA"),
              fuelSulfur=c(0.5, 0.1, 0.1, 0.1, 0.5),
              loadFactor=c(0.0284596685,0.0521665062,0.1766855145,0.078203076,0.708915957249701)
            )
            coefs<-ShipEF::LLAFCoeff

            testLLAF[loadFactor>0.2, so2:= 1]
            testLLAF[loadFactor<=0.2,
                     so2:=(coefs[pollutant=="so2",a]*( (14.1205/loadFactor + 205.7169) * fuelSulfur)+coefs[pollutant=="so2",b])/
                       (coefs[pollutant=="so2",a]*( (14.1205/0.2 + 205.7169) * fuelSulfur)+coefs[pollutant=="so2",b])
            ]

            testLLAF[loadFactor>0.2, hc:= 1]
            testLLAF[loadFactor<=0.2,
                     hc:=(coefs[pollutant=="hc",a]*(loadFactor^-coefs[pollutant=="hc",x])+coefs[pollutant=="hc",b])/
                       (coefs[pollutant=="hc",a]*(0.2^-coefs[pollutant=="hc",x])+ coefs[pollutant=="hc",b])
            ]

            testLLAF[loadFactor>0.2, co2:= 1]
            testLLAF[loadFactor<=0.2,
                     co2:=(coefs[pollutant=="co2",a]*(loadFactor^-coefs[pollutant=="co2",x])+coefs[pollutant=="co2",b])/
                       (coefs[pollutant=="co2",a]*(0.2^-coefs[pollutant=="co2",x])+ coefs[pollutant=="co2",b])
            ]

            testLLAF[loadFactor>0.2, co:= 1]
            testLLAF[loadFactor<=0.2,
                     co:=(coefs[pollutant=="co",a]*(loadFactor^-coefs[pollutant=="co",x])+coefs[pollutant=="co",b])/
                       (coefs[pollutant=="co",a]*(0.2^-coefs[pollutant=="co",x])+ coefs[pollutant=="co",b])
            ]

            testLLAF[loadFactor>0.2, nox:= 1]
            testLLAF[loadFactor<=0.2,
                     nox:=(coefs[pollutant=="nox",a]*(loadFactor^-coefs[pollutant=="nox",x])+coefs[pollutant=="nox",b])/
                       (coefs[pollutant=="nox",a]*(0.2^-coefs[pollutant=="nox",x])+ coefs[pollutant=="nox",b])
            ]

            testLLAF[loadFactor>0.2, pm2.5:= 1]
            testLLAF[loadFactor<=0.2,
                     pm2.5:=(coefs[pollutant=="pm2.5",a]*(loadFactor^-coefs[pollutant=="pm2.5",x])+coefs[pollutant=="pm2.5",b])/
                       (coefs[pollutant=="pm2.5",a]*(0.2^-coefs[pollutant=="pm2.5",x])+ coefs[pollutant=="pm2.5",b])
            ]

            testLLAF[loadFactor>0.2, pm10:= 1]
            testLLAF[loadFactor<=0.2,
                     pm10:=(coefs[pollutant=="pm10",a]*(loadFactor^-coefs[pollutant=="pm10",x])+coefs[pollutant=="pm10",b])/
                       (coefs[pollutant=="pm10",a]*(0.2^-coefs[pollutant=="pm10",x])+ coefs[pollutant=="pm10",b])
            ]


            #Run Calculation
            calcLLAF_out<-data.table(
              calcLLAF(engineType = testLLAF$engineType,
                          location = testLLAF$location,
                          loadFactor = testLLAF$loadFactor,
                          pollutants = "ALL")
            )

            #Compare output against benchmark
            expect_equal(calcLLAF_out$hc,
                         testLLAF$hc
            )

            expect_equal(calcLLAF_out$co2,
                         testLLAF$co2
            )

            expect_equal(calcLLAF_out$co,
                         testLLAF$co
            )

            expect_equal(calcLLAF_out$nox,
                         testLLAF$nox
            )

            expect_equal(calcLLAF_out$pm2.5,
                         testLLAF$pm2.5
            )

            expect_equal(calcLLAF_out$so2,
                         testLLAF$so2
            )

            expect_equal(calcLLAF_out$pm10,
                         testLLAF$pm10
            )

          }
)



test_that("calcLLAF LLAF works for input LLAF tables",
          {

            tbl<-data.table(load=c(1:100))

            # make up adjustment factors (2x the load in percent space,
            # or 200x the load in fraction space)
            tbl[,co:=load*2]
            tbl[,pm2.5:=load*2]
            tbl[,nox:=load*2]

            #create a temporary file and write the made up adjustment factors
            tmpName<-tempfile()
            fwrite(tbl, file = tmpName)

            # subset only some of the pollutants available
            pollutants<-c("co","nox")

            # this creates 18 different loads to test with
            loadFactor<-seq(0.1,1, by=0.051)

            # this creates 18 engine types to test with
            engineType<-c(rep("MSD-ED",6),
                          rep("GT-ED", 6),
                          rep("SSD", 6))

            # run the function using the input table
            calcLLAF_out<-calcLLAF(engineType,
                                   rep("ECA", 18),
                                   loadFactor,
                                   pollutants=c("co","nox"),
                                   inputTableLocation=tmpName)

            #delete that temporary file
            file.remove(tmpName)

            # first 12 should be 1 because they are electric drive engines
            # last 6 should be the load factor (which is in fraction space) times
            # 200 (see above comment where the adjustment factors are made up)
            expect_equal(calcLLAF_out$co,
                         c(rep(1,12),loadFactor[13:18]*200)
                        )

            expect_equal(calcLLAF_out$nox,
                         c(rep(1,12),loadFactor[13:18]*200)
                         )


})
