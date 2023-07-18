test_data <- test_data("input3.rds")


test_that("timefilter works", {
  
  actual <- rFunction(data = test_data, 
                      timefilter = 5,
                      bind_times = FALSE,
                      createUTMs = FALSE,
                      EPSG = 32733,
                      bind_kmph = FALSE,
                      bind_dist = FALSE,
                      bind_timediff = FALSE,
                      bind_study = FALSE,
                      idcol = NULL,
                      altitudecol = NULL,
                      tempcol = NULL,
                      headingcol = NULL,
                      keepessentials = FALSE)
  expect_condition(min(mt_time_lags(actual)) > lubridate::minutes(5))
  
})


test_that("generated columns are included", {
  
  actual <- rFunction(data = test_data, 
                      timefilter = 5,
                      bind_times = TRUE,
                      createUTMs = TRUE,
                      EPSG = 32733,
                      bind_kmph = TRUE,
                      bind_dist = TRUE,
                      bind_timediff =TRUE,
                      bind_study = TRUE,
                      idcol = NULL,
                      altitudecol = NULL,
                      tempcol = NULL,
                      headingcol = NULL,
                      keepessentials = FALSE)
  
  expectcols <- c("altitude", 
                  "heading",
                  "temperature",
                  "index",
                  "x",
                  "y",
                  "study",
                  "hour",
                  "min",
                  "secs",
                  "hourmin",
                  "yearmonthday",
                  "gap_mins",
                  "kmph",
                  "dist_m",
                  "geometry",
                  "lon",
                  "lat")
  expect_contains(colnames(actual), expectcols)
  
  
}) 

test_that("alternative EPSGs don't throw errors", {
  
  actual <- rFunction(data = test_data, 
                                        timefilter = 5,
                                        bind_times = TRUE,
                                        createUTMs = TRUE,
                                        EPSG = 2039,
                                        bind_kmph = TRUE,
                                        bind_dist = TRUE,
                                        bind_timediff =TRUE,
                                        bind_study = TRUE,
                                        idcol = NULL,
                                        altitudecol = NULL,
                                        tempcol = NULL,
                                        headingcol = NULL,
                                        keepessentials = FALSE)
  expect_contains(colnames(actual), c("x", "y"))

  
}) 