library('move2')
library('dplyr')
library('stringr')
library('magrittr')
require('lubridate')
library('ggplot2')
library('units')
library('sf')
library("cli")

# Call useful function for later
`%!in%` <- Negate(`%in%`)
not_null <- Negate(is.null)

# MoveApp settings
rFunction = function(data, 
                     timefilter = 0, 
                     bind_times = TRUE, 
                     bind_timediff = TRUE,
                     bind_dist = TRUE,
                     bind_kmph = TRUE,
                     createUTMs = TRUE,
                     EPSG = 32733,
                     idcol = "", 
                     altitudecol = "", 
                     tempcol = "", 
                     headingcol = "", 
                     keepessentials = TRUE,
                     outlier_thresh = NULL
                     ) {


  # Check inputs ---------------------------------------------------------------------
  
  # Assert that CRS is set
  if(is.na(sf::st_crs(data))){
    stop(
      "App requires input data with a specified Coordinate Reference System.", 
      call. = FALSE)
  }
  
  
  if(timefilter < 0 & timefilter > 60) {
    logger.fatal("Time interval for filtering is outside the accepted range. Please provide a valid number of minutes in the range 0 < t < 60. Returning data") # BC: maybe throw an error instead?
    return(data)
  }
  
  # assert validity of EPSG
  if(createUTMs){
    if(is.na(sf::st_crs(EPSG))){
      stop("Can't find the Coordinate Reference System for the provided `EPSG` code.", call. = FALSE)
    }
  }
  
  
  logger.info("Buckle up... starting data processing")
  
  # Filter to predefined intervals --------------------------------------------------
  
  if(timefilter != 0) {
    
    logger.info(paste0("Filtering to bins of duration ", timefilter, " minutes"))
    timeunit <- paste0(as.character(timefilter), " mins")
    data %<>% mt_filter_per_interval(criterion = "first", unit = timeunit)
    
    logger.info("Filtering completed")
  }
  
  if(!mt_is_move2(data)) {logger.fatal("Data is no longer move2 object after filtering - can't be passed onto next MoveApp")}
  
  
  
  # Detect and remove outliers (locations above speed threshold)  ----------------
  
  if (not_null(outlier_thresh)){
    logger.info("Detecting and removing potential outliers...")
    #units(outlier_thresh) <- units::as_units("km/h")
    data <- data |>  remove_outliers(kmph_thresh = outlier_thresh)
  }
  
  
  # Append speed, distance and time data ------------------------------------------------------
  
  if(bind_kmph == TRUE) {
    logger.info("Binding speed column")
    
    data$kmph <- mt_speed(data) %>%
      units::set_units("km/h") %>%
      as.vector() # convert to kmph    
  }
  
  if(bind_dist == TRUE) {
    logger.info("Binding distance column")
    
    data$dist_m <- mt_distance(data) |>   
      units::set_units("m") |>
      as.vector()
  }
  
  
  if(bind_timediff == TRUE) {
    logger.info("Binding time difference column")
    
    data$timediff_hrs <- mt_time_lags(data) %>%
      units::set_units("hours") %>%
      as.vector()
  }
  
  # Generate optional columns --------------------------------------------------------

  
  logger.trace(paste0("Provided column names are: \n idcol: ", idcol,
                      "\n altitudecol: ", altitudecol,
                      "\n tempcol: ", tempcol, 
                      "\n headingcol: ", headingcol))
  logger.trace(paste0("Column names in the dataset are: \n", toString(colnames(data))))
  colheadings <- c(altitudecol, tempcol, idcol, headingcol)
  colheadings <- colheadings[colheadings != ""] # remove empty inputs
  
  if(any(colheadings %!in% colnames(data))) {
    missing <- colheadings[which(colheadings %!in% colnames(data))]
    logger.fatal(paste0("One of the input column names is not present in this dataset. Please check input settings carefully. Missing column(s): ", toString(missing)))
    stop(paste0("One of the input column names is not present in this dataset. Please check input settings carefully. Missing column(s): ", toString(missing)))
  }
  

  # Process altitude data
  if(altitudecol == "") {
    data %<>% dplyr::mutate(altitude = NA)
  } else {
    # If alt column in input is identified, rename as 'altitude'
    data <- data |> 
      dplyr::rename(altitude = dplyr::all_of(altitudecol)) |> 
      dplyr::mutate(altitude = as.numeric(altitude)) # remove units
  }
  
  
  # Process temperature data
  if(tempcol == "") {
    data %<>% dplyr::mutate(temperature = NA)
  } else {
    # If temp col in input is identified, rename as 'temperature'
    data <- data |> 
      dplyr::rename(temperature = dplyr::all_of(tempcol)) |> 
      dplyr::mutate(temperature = as.numeric(temperature)) # remove units
    
  }
  
  
  # Process heading data
  if(headingcol == "") {
    data %<>% dplyr::mutate(heading = NA)
  } else {
    # If heading col in input is identified, rename as 'heading'
    data <- data |> 
      dplyr::rename(heading = dplyr::all_of(headingcol)) |> 
      dplyr::mutate(heading = as.numeric(heading)) # remove units
    }
  
  
  # Define trackID 
  if(idcol != "") {
    logger.info(paste0("Changing primary ID column to ", idcol))
    mt_track_data(data)
    data <- mt_set_track_id_column(data, idcol)
  }
  
  
  # The following is deprecated until later changes
  # Calling movebank study name requires a Movebank handle input
  
  # Add study name
  # if(bind_study == TRUE) {
  #   
  #   if("study.id" %!in% colnames(mt_track_data(data)) & "study_id" %!in% colnames(mt_track_data(data))) {
  #     logger.warn("study.id and study_id are not columns in the track data. Unable to bind study column")
  #   } else {
  #     
  #     if("study.id" %in% colnames(mt_track_data(data))) {
  #       study_id <- mt_track_data(data)$study.id
  #     } else {
  #       study_id <- mt_track_data(data)$study_id
  #     }
  #     
  #     studyname <- movebank_download_study_info(id = study_id)$name
  #     
  #     # CC: I'll update this to work on >1 studies later
  #     if(length(unique(study_id)) > 1) {
  #       logger.warn("Input contains data from several studies. Study ID will not be appended.")
  #     } else {
  #       data %<>% mutate(study = rep(studyname, nrow(data)))
  #       logger.info("Study column appended")
  #     }
  #     
  #   }
    
  #}
  
  # Add indexes -----------------------------------------------------------------

  logger.info("Binding indexes for each tag")
  data %<>% mutate(
    index = paste0(mt_track_id(data), " ", mt_time(data))
  )

  
  # Bind time columns ----------------------------------------------------------

  logger.info("Generating additional timestamp data")
  data <- data %>%
    dplyr::mutate(
      hour = hour(mt_time(data)),
      min = minute(mt_time(data)),
      secs = second(mt_time(data)),
      hourmin = hour + min/60 + secs/3600,
      day = day(mt_time(data)),
      month = month(mt_time(data)),
      year = year(mt_time(data)),
      yearmonthday = stringr::str_replace_all(str_sub(mt_time(data), 1, 10), "-", "")
    )  
  
  

  # Append UTM data --------------------------------------------------------------
  

  if(createUTMs == TRUE) {
    logger.info("Binding UTM data.")
    
    # Filter out points without attached geometry
    latlons <- sf::st_coordinates(data)
    data %<>% mutate(lon = latlons[, 1],
                     lat = latlons[, 2])
    remove <- which(is.na(latlons), arr.ind=TRUE)[, 1]
    removeindex <- data$index[remove]
    
    # Check for points without associated geometry and report:
    if(identical(remove, integer(0))) {
      logger.info("No empty points located. Proceeding with UTMs.")
    } else {
      logger.warn(paste0("Empty point(s) located, with indexes: ", 
                         toString(removeindex)
      ))

      logger.warn("Removing empty points and continuing with UTMs.")
      data %<>% filter(index %!in% removeindex)


    }
    
    # Make UTMs primary data geometry
    logger.info("Changing primary coordinates to UTM data")
    crscode <- sf::st_crs(EPSG) # retrieve CRS code for given EPSG
    data %<>% sf::st_transform(crscode)
    
    newcoords <- sf::st_coordinates(data) %>% as.data.frame()
    data %<>% mutate(x = newcoords[, 1],
                     y = newcoords[, 2])
    


  }
  
  if(!mt_is_move2(data)) {logger.fatal("Data is no longer move2 object after UTM data appending - can't be passed onto next MoveApp")}


  
  # Generate summary stats and plots ---------------------------------------------------
  
  logger.info("Plotting and summarising")
  
  if(nrow(data) > 1){
    
    # Summary table by ID:
    summarystats <- data %>%
      bind_cols(
        ID2 = mt_track_id(.),
        TIME2 = mt_time(.),
        TIMEDIFF2 = mt_time_lags(.),
        SPEED2 = mt_speed(.),
        DIST2 = mt_distance(.) %>% as.vector()) %>%
      as.data.frame() %>%
      group_by(ID2) %>%
      summarise(first_obs = min(TIME2, na.rm = TRUE),
                last_obs = max(TIME2, na.rm = TRUE),
                total_obs = n(),
                max_kmph = max(SPEED2, na.rm = TRUE),
                mean_kmph = mean(SPEED2, na.rm = TRUE),
                med_kmph = median(SPEED2, na.rm = TRUE),
                max_gap_mins = max(TIMEDIFF2, na.rm = TRUE),
                max_alt = ifelse(altitudecol != "", max(altitude, na.rm = TRUE), NA),
                min_alt = ifelse(altitudecol != "", min(altitude, na.rm = TRUE), NA),
                total_km = sum(DIST2, na.rm = TRUE) / 1000
      )
    
    write.csv(summarystats, file = appArtifactPath("summarystats.csv"))
    
    
    # Generate density plots:
    
    png(appArtifactPath("times.png"))
    times <- ggplot(data, aes(x = mt_time(data), y = factor(mt_track_id(data)))) +
      geom_point() +
      xlab("timestamp") +
      ylab("trackID") +
      ggtitle("Location timestamps by ID") +
      theme_bw() +
      scale_x_datetime(
        date_labels = "%Y (%b)") 
    print(times)
    dev.off()
    
    if (bind_dist == TRUE) {
      png(appArtifactPath("distances.png"))
      adjData <- data %>% filter(dist_m < quantile(data$dist_m, 0.9, na.rm = TRUE)) 
      dists <- ggplot(adjData, 
                      aes(x = dist_m, fill = mt_track_id(adjData))) +
        facet_wrap(~ mt_track_id(adjData)) +
        geom_density(alpha = 0.4)+
        xlab("Distance travelled (m)") +
        ggtitle("Distribution of Distance Travelled by ID (up to 95th percentile)") +
        guides(fill=guide_legend(title="Track ID"))
      print(dists)
      dev.off()
    }
    
    if(bind_kmph == TRUE) {
      png(appArtifactPath("speeds.png"))
      adjData <- data %>% filter(kmph < quantile(data$kmph, 0.9, na.rm = TRUE)) 
      speeds <- ggplot(adjData, 
                       aes(x = kmph, fill = mt_track_id(adjData))) + 
        facet_wrap(~ mt_track_id(adjData)) +
        geom_density(alpha = 0.4) +
        ggtitle("Distribution of speed by ID (up to 95th percentile)") +
        xlab("Speed (km/h)") +
        guides(fill=guide_legend(title="Track ID"))
      print(speeds)
      dev.off()
    }
    
  }else{
    warning("nrow(data) <= 1 after all processing steps applied. Skipping the creation of diagnostic plots.")
  }
  
  
  
  

  # Select only essential columns --------------------------------------------------------
  
  essentialcols <- c(mt_track_id_column(data), 
                     mt_time_column(data), 
                     "temperature", 
                     "heading", 
                     "altitude", 
                     "import_marked_outlier", 
                     "index", 
                     "hour", 
                     "min",
                     "secs",
                     "hourmin",
                     "yearmonthday",
                     "timediff_hrs",
                     "kmph",
                     "dist_m",
                     "x",
                     "y",
                     "geometry",
                     "lon", 
                     "lat",
                     "sunrise_timestamp", 
                     "sunset_timestamp",
                     "acc_dt"
  )

  if(keepessentials == TRUE) {
    data %<>% dplyr::select(any_of(essentialcols))
  }
  

  # Remove times if desired
  if(bind_times == FALSE) {
    data %<>% dplyr::select(-any_of(c("hour", "min", "secs", "hourmin", "yearmonthday", "timediff_hrs")))
  }
  
  # Return --------------------------------------------------------------------------
  logger.info("Job completed!")
  
  result <- data
  return(result)
}






# //////////////////////////////////////////////////////////////////////////////
# using speed (i.e. a combination of time and location) as a metric to identify
# outliers in movement data (due to e.g. GPS glitches). All location events
# associated with speeds above the user-defined threshold (given by some
# sensible estimate of abnormal speeds for the tagged species) are removed
# from the data.

remove_outliers <- function(data, kmph_thresh){
  
  if(!mt_is_move2(data))  stop("`data` must be a move2 object")
  if(!mt_is_time_ordered(data)) stop("`data` must ordered by time")
  
  n_start <- nrow(data)
  
  # compute initial speeds
  kmph <- mt_speed(data) |> units::set_units("km/h") |> as.vector()  

  # counter to keep track of loop iterations
  i <- 1
  
  cli::cli_progress_bar("Sweeping")
  # `while` loop to drop speeds above the threshold. Removals are done
  # iteratively relative to time (i.e chronologically) with speeds re-calculated
  # between iterations so that offending locations are dropped in the right
  # order
  while(any(kmph > kmph_thresh, na.rm = TRUE)){
    
    # find the index of the location causing the 1st offending speed.
    # `mt_speed()` calculates speed to next location, so the offending location
    # (i.e. outlier) is in the subsequent entry
    fastindex <- which(kmph > kmph_thresh)[1] + 1 
    # remove the guilty location
    data <- data[-fastindex,]
    
    # recalculate speed
    kmph <- mt_speed(data) |> units::set_units("km/h") |> as.vector()
    
    # avoid infinite looping
    i <- i + 1
    if(i == 1e6){
      warning("Jumping out of while loop as condition not being met within reasonable number of iterations")
      break
    } 
    
    cli::cli_progress_update()
  }
  
  n_end <- nrow(data)
  n_rows_dropped <- n_start - n_end
  
  if(n_rows_dropped > 0){
    logger.info(paste0("Found ", n_rows_dropped, " locations associated with speeds greater than the threshold of ", kmph_thresh, "kmph. Offending locations were removed"))
  }else{
    logger.info("No outliers detected")
  }
  
  return(data)
}