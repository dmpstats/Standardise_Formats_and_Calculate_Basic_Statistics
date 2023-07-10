---
editor_options: 
  markdown: 
    wrap: sentence
---

# Basic Data Processing for Merging Studies

MoveApps

Github repository: [*https://github.com/callumjclarke/preprocessing*](https://github.com/callumjclarke/preprocessing){.uri}

## Description

This MoveApp contains settings to perform various basic cleaning and binding processes, ranging from binding and renaming columns using standardized names to filtering the data to intervals of predefined duration.
Any combination of the processes detailed below can be selected.

## Documentation

This MoveApp fulfills several purposes in a workflow.
Its overall aim is to standardize any input study into a form that can be merged at a later stage in a workflow with additional studies (that have also been processed) to avoid discrepancies in study names, units, data volume, and/or column names.

It contains options to: \* Filter the data to intervals of X minutes \* Append columns containing Universal Transverse Mercator (UTM) data \* Append an *index* column, consisting of the animal's ID and timestamp concatenated \* Append columns containing *speed*, *distance traveled* and/or *time since previous point* data \* Generate columns containing specific timestamp data (hour, minute and second, independently) \* Standardize column names for *altitude*, *temperature* and *heading* data across studies

### Input data

Move2 location object

### Output data

Move2 location object

### Artefacts

None.

### Settings

`Filter by Time Interval` (integer): The length of interval, in minutes, to which to filter data.
NULL means no filtering will take place.
Defaults to 5-minute intervals.
`move2::mt_filter_per_interval(criterion = first)` is used for this filtering.

`Bind additional timestamp columns` (logical): Determines whether to append specific timestamp data (*hour,* *minute,* *second,* *hourmin,* and *yearmonthday*) to the output.
If FALSE, overrides *Keep Essential Columns*.

`Bind UTM location data` (logical): Determines whether to generate and append Universal Transverse Mercator (UTM) data to the output.
If TRUE, the primary geometry will become UTM data.
NOTE: This will also remove points without any attached geometry.

`EPSG` (integer): If *Bind UTM location data* is selected, please provide a valid EPSG code for the transformed coordinate system.
Defaults to EPSG:32733 (UTM zone 33S).

`Bind index` (logical): Determines whether to append an event 'index' to the data, consisting of the tag's ID concatenated with its timestamp.
If FALSE, overrides *Keep Essential Columns*.
`Bind km/h` (logical): Determines whether to append a speed column (units: km/h).

`Bind distance` (logical): Determines whether to append a *distance travelled* from previous event column (units: metres).

`Bind time difference` (logical): Determines whether to append a *time difference* from previous event column (units: hours).

`Bind study` (logical): determines whether to append a *study name* column for each animal.
This is recommended if merging with other studies later in this workflow

`ID Column` (character): The name of the column to be used as primary identification.
NULL means the default (taken from Movebank) will be used.

`Altitude Column` (character): The name of the column containing altitude data (if any).
NULL means no altitude data is available.

`Temperature Column` (character): The name of the column containing temperature data (if any).
NULL means no temperature data is available.

`Heading Column` (character): The name of the column containing heading data (if any).
NULL means no heading data is available.

`Keep Essential Columns` (logical): If TRUE, the output data contains only the following columns present: *temperature*, *heading*, *altitude*, *import_marked_outlier*, *index*, *hour*, *min*, *secs*, *hourmin*, *yearmonthday*, *gap_mins*, *kmph*, *dist_m*, *x (UTM)*, *y (UTM)*, *geometry (sf)*, *lon*, *lat*, *study*.

### Most common errors

-   `bind study` setting currently works only if all tracks in the data come from the same study, and if `study.id` or `study_id` is a column name in the track data. It will be unable to perform the operation if not
-   Any `column` settings can be bugged by a column name with two periods: for example, 'altitude.col.xyz' could not be recognized. *Please describe shortly what most common errors of the App can be, how they occur and best ways of solving them.*

### Null or error handling

*Please indicate for each setting as well as the input data which behaviour the App is supposed to show in case of errors or NULL values/input. Please also add notes of possible errors that can happen if settings/parameters are improperly set and any other important information that you find the user should be aware of.*

*Example:* **Setting `radius`:** If no radius AND no duration are given, the input data set is returned with a warning.
If no radius is given (NULL), but a duration is defined then a default radius of 1000m = 1km is set.
