# Cyclistic Bike-Share Data Cleaning and Manipulation Report

## 1. Data Inspection
- The datasets from different years were inspected to identify inconsistencies in column names, data types, and missing values.

## 2. Column Renaming
- The column names in the 2019 dataset were renamed to match the 2020 dataset for consistency, as the latter follows the latest schema.

## 3. Data Type Standardization
To ensure uniformity across datasets, the data types were adjusted:
- `ride_id` and `rideable_type` were converted to character types.

## 4. Data Combination
- The datasets were merged into a single dataframe to facilitate analysis.

## 5. Column Removal
Due to changes in data availability post-2020, certain demographic details were no longer included in the dataset. The following columns were removed:
- `gender`
- `birth_year`
- `start_lat`
- `start_lng`
- `end_lat`
- `end_lng`

## 6. Cleaning Categorical Data
The `member_casual` column had inconsistencies in membership labels:
- "subscriber" and "member" referred to annual members.
- "customer" and "casual" referred to casual riders.

To maintain consistency:
- "subscriber" was replaced with "member."
- "customer" was replaced with "casual."

## 7. Feature Engineering
To enable deeper analysis, new columns were added:
- `day_of_week` (Extracted from the start time)
- `month` (Extracted from the start time)
- `date` (Extracted from the start time)
- `year` (Extracted from the start time)
- `ride_length` (Calculated as the difference between `end_time` and `start_time`)

## 8. Handling Invalid Ride Lengths
- Upon calculating `ride_length`, negative values were discovered.
- Investigation revealed that these rides were for maintenance and QA purposes.
- These invalid trips were removed, as they do not contribute to user behavior analysis.

## 9. Final Integrity Check
- The cleaned dataset was re-inspected to ensure integrity, correct formats, and absence of erroneous data.