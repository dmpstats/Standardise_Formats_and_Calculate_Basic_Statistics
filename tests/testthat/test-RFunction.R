library(units)

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
                      idcol = "",
                      altitudecol = "",
                      tempcol = "",
                      headingcol = "",
                      keepessentials = FALSE)
  expect_condition(min(mt_time_lags(actual)) > lubridate::minutes(5))
  
})



test_that("`speedcut` works", {
  
  speed_thresh <- 100
  
  actual <- rFunction(data = test_data, 
                      timefilter = 5,
                      bind_times = FALSE,
                      createUTMs = FALSE,
                      EPSG = 32733,
                      bind_kmph = TRUE,
                      speedcut = speed_thresh,
                      bind_dist = FALSE,
                      bind_timediff =FALSE,
                      idcol = "",
                      altitudecol = "",
                      tempcol = "",
                      headingcol = "",
                      keepessentials = FALSE)
  
  expect_lt(max(actual$kmph, na.rm = TRUE), units::set_units(speed_thresh, "km/h"))
  expect_lt(nrow(actual), nrow(test_data))
})





test_that("generated columns are binded", {
  
  actual <- rFunction(data = test_data, 
                      timefilter = 5,
                      bind_times = TRUE,
                      createUTMs = TRUE,
                      EPSG = 32733,
                      bind_kmph = TRUE,
                      bind_dist = TRUE,
                      bind_timediff =TRUE,
                      idcol = "",
                      altitudecol = "argos.altitude",
                      tempcol = "",
                      headingcol = "",
                      keepessentials = FALSE)
  
  expectcols <- c("altitude", 
                  "heading",
                  "temperature",
                  "index",
                  "x",
                  "y",
                  "hour",
                  "min",
                  "secs",
                  "hourmin",
                  "yearmonthday",
                  "timediff_hrs",
                  "kmph",
                  "dist_m",
                  "geometry",
                  "lon",
                  "lat")
  expect_contains(colnames(actual), expectcols)
  expect(all(is.numeric(actual$kmph)), failure_message = "Output speeds (kmph) are non-numeric")
  expect(all(is.numeric(actual$dist_m)), failure_message = "Output distances between locations are non-numeric")
  
}) 


test_that("alternative EPSGs can still be handled", {
  
  actual <- rFunction(data = test_data, 
                                        timefilter = 5,
                                        bind_times = TRUE,
                                        createUTMs = TRUE,
                                        EPSG = 2048,
                                        bind_kmph = TRUE,
                                        bind_dist = TRUE,
                                        bind_timediff =TRUE,
                                        idcol = "",
                                        altitudecol = "",
                                        tempcol = "",
                                        headingcol = "",
                                        keepessentials = FALSE)
  expect_contains(colnames(actual), c("x", "y"))
  expect(all(is.double(c(actual$x, actual$y))), failure_message = "Output UTMs are non-numeric")
  expect(dplyr::between(sf::st_coordinates(actual)[1,]["X"], 473631, 473632), failure_message = "UTM X-coordinate is incorrectly transformed")
  expect(dplyr::between(sf::st_coordinates(actual)[1,]["Y"], -5799990, -5799989), failure_message = "UTM Y-coordinate is incorrectly transformed")
  
})


test_that("no IDs are removed", {
  
  actual <- rFunction(data = test_data, 
                      timefilter = 20,
                      bind_times = TRUE,
                      createUTMs = TRUE,
                      EPSG = 32733,
                      bind_kmph = TRUE,
                      bind_dist = TRUE,
                      bind_timediff =TRUE,
                      idcol = "",
                      altitudecol = "",
                      tempcol = "",
                      headingcol = "",
                      keepessentials = FALSE)
  
  expect(unique(move2::mt_track_id(test_data)) == unique(move2::mt_track_id(actual)), failure_message = "IDs in the input data are not accounted for in the output")
  
})


test_that("misnamed columns throw error message", {
  
  expect_error(actual <- rFunction(data = test_data, 
                                   timefilter = 5,
                                   bind_times = TRUE,
                                   createUTMs = TRUE,
                                   EPSG = 32733,
                                   bind_kmph = TRUE,
                                   bind_dist = TRUE,
                                   bind_timediff =TRUE,
                                   idcol = "",
                                   altitudecol = "THIS_IS_NOT_A_COLUMN",
                                   tempcol = "",
                                   headingcol = "",
                                   keepessentials = FALSE))
  
})





test_that("App throws error if input move2 object has no CRS specified", {
  
  dt <- test_data
  sf::st_crs(dt) <- NA
  
  expect_error(
    rFunction(data = dt), 
    regexp = "App requires input data with a specified Coordinate Reference System."
  )
  
})



test_that("App throws error if specified EPSG code is non-valid", {

  expect_error(
    suppressWarnings( # really not good to suppress warnings in testing, but just trying to avoid having to built a withCallingHandlers() here. The warning message is a nuisance here
      rFunction(data = test_data, EPSG = 155567717)
    ), 
    regexp = "Can't find the Coordinate Reference System for the provided `EPSG` code"
  )
  
})
