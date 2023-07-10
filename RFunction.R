library('move2')
library('dplyr')
library('stringr')
library('magrittr')
require('lubridate')

# Call useful function for later
`%!in%` <- Negate(`%in%`)

# MoveApp settings
rFunction = function(data, timefilter = 5, 
                     bind_times = TRUE, 
                     createUTMs = TRUE,
                     EPSG = 32733,
                     bind_index = TRUE, 
                     bind_kmph = TRUE,
                     bind_dist = TRUE,
                     bind_timediff = TRUE,
                     bind_study = TRUE,
                     idcol = NULL, 
                     altitudecol = NULL, 
                     tempcol = NULL, 
                     headingcol = NULL, 
                     keepessentials = TRUE) {


  # Generate optional columns --------------------------------------------------------

  colheadings <- c(altitudecol, tempcol, idcol, headingcol)
  colheadings <- Filter(Negate(is.null), colheadings)
  
  if(any(colheadings %!in% colnames(data))) {
    missing <- colheadings[which(colheadings %!in% colnames(data))]
    logger.fatal(paste0("One of the input column names is not present in this dataset. Please check input settings carefully. Missing column(s): ", toString(missing)))
    return(data)
  }
  

  # Process altitude data
  if(is.null(altitudecol)) {
    data %<>% mutate(altitude = NA)
  } else {
    # If altitude is present, rename the column
    try(data %<>% rename(altitude = altitudecol))

  }
  
  # Process temperature data
  if(is.null(tempcol)) {
    data %<>% mutate(temperature = NA)
  } else {
    # If altitude is present, rename the column
    try(data %<>% rename(temperature = tempcol))
  }
  
  # Process heading data
  if(is.null(headingcol)) {
    data %<>% mutate(heading = NA)
  } else {
    # If altitude is present, rename the column
    try(data %<>% rename(heading = headingcol))
  }
  
  # Define trackID 
  if(!is.null(idcol)) {
    logger.info(paste0("Changing primary ID column to ", idcol))
    mt_track_data(data)
    data <- mt_set_track_id_column(data, idcol)
  }
  
  # Add study name
  if(bind_study == TRUE) {
    
    if("study.id" %!in% colnames(mt_track_data(data)) & "study_id" %!in% colnames(mt_track_data(data))) {
      logger.warn("study.id and study_id are not columns in the track data. Unable to bind study column")
    } else {
      
      if("study.id" %in% colnames(mt_track_data(data))) {
        study_id <- mt_track_data(data)$study.id
      } else {
        study_id <- mt_track_data(data)$study_id
      }
      
      studyname <- movebank_download_study_info(id = study_id)$name
      
      # CC: I'll update this to work on >1 studies later
      if(length(unique(study_id)) > 1) {
        logger.warn("Input contains data from several studies. Study ID will not be appended.")
      } else {
        data %<>% mutate(study = rep(studyname, nrow(data)))
        logger.info("Study column appended")
      }
      
    }
    
  }
  
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
      yearmonthday = stringr::str_replace_all(str_sub(mt_time(data), 1, 10), "-", ""),
      gap_mins = difftime(mt_time(data), lag(mt_time(data)), units = "mins"))  
  
  
  # Filter to predefined intervals --------------------------------------------------
  
  # We should tr mt_filter_by_interval in place of this section
  # Hopefully less computationally demanding

  if(!is.null(timefilter)) {

    logger.info(paste0("Filtering to bins of duration ", timefilter, " minutes"))
    timeunit <- paste0(as.character(timefilter), " mins")
    
    data %<>% mt_filter_per_interval(criterion = "first", unit = timeunit)
    
    # # generate necessary cols (if don't already exist)
    # if("gap_mins" %!in% colnames(data)) {
    #   gap_mins = difftime(mt_time(data), lag(mt_time(data)), units = "mins")
    # }
    # 
    # 
    # 
    # data <- data %>% 
    #   mutate(temptag = mt_track_id(data)) %>% # used to sort below
    #   dplyr::mutate(binmin = ceiling_date(mt_time(data), unit = minutes(timefilter))) # create time bins for filtering
    # 
    # logger.info("Bins created. Grouping and summarising")
    # # The following is slow. We should try to find alternatives
    # data <- data %>%
    #   group_by(temptag, binmin) %>% 
    #   summarise_all(.funs = first) # need original obj names
    # 
    # logger.info("Returning filtered data to move2 object")
    # # Return object to move2 class, as summarise() changes to sf
    # drop.cols <- c('binmin', 'temptag', 'gap_mins')
    # data <- mt_as_move2(data, track_id_column = trackname, time_column = timename) %>%
    #   select(-one_of(drop.cols))
    

    logger.info("Filtering completed.")
  }
  
  if(!mt_is_move2(data)) {logger.fatal("Data is no longer move2 object after filtering - can't be passed onto next MoveApp")}
  

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
                         ifelse(bind_index, removeindex, "[INDEXES UNAVAILABLE. PLEASE USE 'bind_index' PROPERTY IN SETTINGS]")
      ))

      logger.warn("Removing empty points and continuing with UTMs.")
      data %<>% filter(index %!in% removeindex)


    }
    
    # Make UTMs primary data geometry
    logger.info("Changing primary coordinates to UTM data")
    crscode <- sf::st_crs(EPSG) # retrieve CRS code for given EPSG
    data %<>% sf::st_transform(crscode)
    
    newcoords <- sf::st_coordinates(data)
    data %<>% mutate(x = newcoords[, 1],
                     y = newcoords[, 2])
    


  }
  
  if(!mt_is_move2(data)) {logger.fatal("Data is no longer move2 object after UTM data appending - can't be passed onto next MoveApp")}

  
  # Append speeds and final data ------------------------------------------------------
  
  if(bind_kmph == TRUE) {
    data$kmph <- mt_speed(data) %>%
      units::set_units("km/h") %>%
      as.vector() # convert to kmph
  }
  if(bind_dist == TRUE) {
    data$dist_m <- as.vector(mt_distance(data))
  }
  if(bind_timediff == TRUE) {
    data %<>% mutate(
      timediff_hrs =   as.numeric(difftime(mt_time(data), lag(mt_time(data), default = mt_time(data)[1]), units = "hours"))
    )
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
                     "gap_mins",
                     "kmph",
                     "dist_m",
                     "x",
                     "y",
                     "geometry",
                     "lon", 
                     "lat",
                     "study"
  )
  if(keepessentials == TRUE) {
    data %<>% dplyr::select(any_of(essentialcols))
  }
  
  # Remove index if desired
  if(bind_index == FALSE) {
    data %<>% dplyr::select(any_of(-index))
  }
  
  # Remove times if desired
  if(bind_times == FALSE) {
    data %<>% dplyr::select(any_of(-hour, -min, -secs, -hourmin, -yearmonthday, -gap_mins))
  }
  
  # Return --------------------------------------------------------------------------
  
  result <- data
  return(result)
}
