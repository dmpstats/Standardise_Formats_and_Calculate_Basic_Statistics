# histograms + outputs
require(move2)
require(dplyr)
require(ggplot2)

data <- readRDS("data/output/workflow.rds")

onebird <- data %>% filter(mt_track_id(.) == mt_track_id(data)[1])

plotdat <- as.data.frame(altitude = onebird$altitude,
                         dist_m = onebird$dist_m,
                         )


times <- ggplot(data, aes(x = mt_time(data), fill = mt_track_id(data))) +
  geom_density(alpha = 0.4) +
  ggtitle("Timing of observations by ID") +
  xlab("Date") +
  #theme(legend.position = "none") +
  scale_fill_discrete(name = "Track ID")
times

distances <- ggplot(data, aes(x = dist_m, fill = mt_track_id(data))) +
  geom_density(alpha = 0.4)+
  xlab("Distance travelled (m)") +
  ggtitle("Distribution of Distance Travelled by ID") +
  guides(fill=guide_legend(title="Track ID"))
distances

altitudes <- ggplot(data, aes(x = altitude, fill = mt_track_id(data))) +
  geom_density(alpha = 0.4) +
  xlab("Altitude") +
  ggtitle("Distribution of Altitude by ID") +
  guides(fill=guide_legend(title="Track ID"))
altitudes

kmph <- ggplot(data, aes(x = kmph, fill = mt_track_id(data))) + 
  geom_density(alpha = 0.4) +
  ggtitle("Distribution of speed by ID") +
  xlab("Speed (km/h)") +
  guides(fill=guide_legend(title="Track ID"))
kmph

summarystats <- data %>%
  bind_cols(TIME2 = mt_time(.),
            TIMEDIFF2 = mt_time_lags(.),
            SPEED2 = mt_speed(.),
            DIST2 = mt_distance(.) %>% as.vector()) %>%
  as.data.frame() %>%
  group_by(individual_local_identifier) %>%
  summarise(first_obs = min(TIME2),
            last_obs = max(TIME2),
            max_kmph = max(SPEED2, na.rm = TRUE),
            mean_kmph = mean(SPEED2, na.rm = TRUE),
            med_kmph = median(SPEED2, na.rm = TRUE),
            max_gap_mins = max(TIMEDIFF2, na.rm = TRUE),
            max_alt = ifelse("altitude" %in% colnames(data), max(altitude, na.rm = TRUE), NA),
            min_alt = ifelse("altitude" %in% colnames(data), min(altitude, na.rm = TRUE), NA),
            total_km = sum(DIST2, na.rm = TRUE) / 1000
            )
summarystats
