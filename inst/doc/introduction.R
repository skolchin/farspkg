## ----setup, include = FALSE, echo = FALSE, message = FALSE---------------
library(farspkg)

## ----load_year-----------------------------------------------------------
fars_2014 <- fars_read(make_filename(2014))

## ----load_years----------------------------------------------------------
fars_2014_2015 <- fars_read_years(years = c(2014, 2015))
fars_2014_2015[[1]]

## ----summarize_data------------------------------------------------------
fars_summary <- fars_summarize_years(2013:2015)
fars_summary

## ----mapping_crashes-----------------------------------------------------
# Alabama
fars_map_state(1, 2014)
# New York
fars_map_state(36, 2014)

