# Standardise Formats and Calculate Basic Statistics

MoveApps

Github repository:
<https://github.com/dmpstats/Standardise_Formats_and_Calculate_Basic_Statistics>


## Description

This MoveApp contains settings to perform several basic cleaning processes and generation of extra columns of data. For example, using standardized names, thinning the data and adding a speed column. This processing may also be useful for merging data from multiple studies of differing tag types. The user may specify any combination of the processes detailed below.

## Documentation

This MoveApp fulfills several purposes in a workflow. The overall aim is to standardize any input study into a format that can be merged at a later stage in a workflow with additional studies (that have also been processed) to avoid discrepancies in study names, units, data volume, and/or column names. In addition extra information such as location in UTMs, speed, distance travelled and time between points may be appended.

The user may select any of the following settings:

-   Thinning the data by filtering one event per defined time window
-   Create and append columns containing: 
      - Universal Transverse Mercator (UTM) data
      - An *index* column, consisting of the animal's ID and timestamp concatenated
      - *speed*, *distance traveled* and/or *time* to next location point data
      - Specific time-related variables (hour, minute and second, independently)
-   Standardize column names for *altitude*, *temperature* and *heading* data across studies

Density plots displaying the distribution of timestamps, distances travelled, and speeds will be outputted as artifacts. A table detailing summary statistics for each ID will also be generated as an artifact.

### Input data

Move2 location object

### Output data

Move2 location object

### Artefacts

-   `summarystats.csv`: A summary table detailing key metrics for each ID

-   `times.png`: A dot-plot displaying the frequency of timestamps, to identify any anomalies/gaps within the dataset

-   `distances.png`: A density plot displaying the distances travelled by each ID

-   `speeds.png`: A density plot displaying the speeds travelled by each ID

### Settings

`Filter by Time Interval` (integer): The length of interval, in minutes, to which to filter data. Must be between 0 and 60. Setting this equal to 0 means no filtering will take place. Defaults to 0 (no filtering). The function `move2::mt_filter_per_interval` is used for this filtering with the parameter `criterion = first`.

`Bind additional timestamp columns` (logical): Determines whether to append specific timestamp data (*hour,* *minute,* *second,* *hourmin,* and *yearmonthday*) to the output. Default: `TRUE`.

`Bind time difference` (logical): Determines whether to append a column with the *time difference* to the next location (units: hours). Default: `TRUE`.

`Bind distance` (logical): Determines whether to append a column with the *distance travelled* from the previous location (units: metres). Default: `TRUE`.

`Bind speed` (logical): Determines whether to append a speed column (units: km/h).

`Bind UTM location data` (logical): Determines whether to generate and append Universal Transverse Mercator (UTM) data to the output. If TRUE, the primary geometry will become UTM data. NOTE: This will also remove points without any attached geometry. Default: `TRUE`.

`EPSG` (integer): If *Bind UTM location data* is selected, a valid EPSG code for the transformed coordinate system. Defaults to EPSG:32733 (UTM zone 33S).

`ID Column` (character): Name of the column in the input data to reset as primary identification. If no column is named, the default identifier column defined within Movebank (or alternative data source) remains the primary identifier.

`Altitude Column` (character): Column name of the input data containing altitude data (if any). The column must already be contained within the input dataset. Leaving this empty generates an empty `altitude` column. 

`Temperature Column` (character): Column name of the input data containing temperature data (if any). The column must already be contained within the input dataset. Leaving this empty generates an empty `temperature` column. 

`Heading Column` (character): Column name of the input data containing heading data (if any). The column must already be contained within the input dataset. Leaving this empty generates an empty `heading` column. 

`Upper Threshold for Movement Speed` (numeric): Outlier detection based on highest acceptable speeds (units: km/h). Any location with a speed exceeding this value will be removed as an outlier. Default: `NULL`, i.e. outliers detection and removal step is skipped.


`Keep Essential Columns` (logical): If TRUE, the output data contains only the following columns (but not all, depending on which settings above are used): *temperature*, *heading*, *altitude*, *import_marked_outlier*, *index*, *hour*, *min*, *secs*, *hourmin*, *yearmonthday*, *gap_mins*, *kmph*, *dist_m*, *x (UTM)*, *y (UTM)*, *geometry (sf)*, *lon*, *lat*, *study*.

### Most common errors

-   For any `column name` settings, please ensure that:

    1.  the named column is **contained within the input dataset**
    2.  the column name has been **spelled with complete accuracy**

    or an error will be returned and processing will stop. Check the logs for the list of column names in the input dataset.

-   Any `column` settings can be bugged by a column name with two (and, sometimes, one) periods: for example, 'altitude.col.xyz' could not be recognized. To solve this bug, any column containing a period must be duplicated (instead of renamed) with the new name `altitude/temperature/heading`.

-   `Filter by Time Interval` must be numeric, expressing the number of minutes. If the provided interval is greater than 60, the data will be returned with a warning.

### Null or error handling

-   `[ID/Altitude/Temp/Heading] Column`: If any of these options are left blank, the respective column is not renamed. Instead, a `NA` column of the desired name is appended.
-   `[ID/Altitude/Temp/Heading] Column`: For any of these settings, if a column name is provided but isn't found in the input dataset, a warning is provided and a NA column of the desired name is appended.
-   `EPSG`: If no EPSG code is provided, defaults to EPSG:32733 (UTM zone 33S). If the input is an invalid EPSG, the App will throw an error.
