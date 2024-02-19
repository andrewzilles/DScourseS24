url <- "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"
download.file(url, "dates.json", method = "auto")
library(jsonlite)
library(tidyverse)
mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])
class(mydf)
