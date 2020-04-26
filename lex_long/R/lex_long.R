if(!require("pacman"))install.packages("pacman")
pacman::p_load(here, tidyverse, data.table, janitor)


download.file("https://github.com/COVIDExposureIndices/COVIDExposureIndices/archive/master.zip", 
              destfile = "COVIDExposureIndices.zip")

unzip("COVIDExposureIndices.zip")

fl <- 
  list.files(here("covidexposureindices-master", "lex_data"))
county_files <- fl[str_detect(fl, "county_lex")]


# choose your counties!
nyc_county <- c(36005, 36047, 36081,36085, 36061)




# takes a path for a lex matrix and specefied counties
# returns df of those counties in long format
lex_long <- function(path, origin_counties){
  
  
  dir <- here("covidexposureindices-master", "lex_data", path)
  
  # i use fread b/c it can read .gz files
  dt <- fread(dir)
  
  dt2 <- dt[COUNTY_PRE %in% origin_counties]
  tbl1 <- as_tibble(dt2)
  
  dt <- get_date(path)
  
  tbl2 <- 
    tbl1 %>% 
    pivot_longer(-COUNTY_PRE, names_to = "county") %>% 
    filter(value>0) %>% 
    # add date from filepath
    mutate(date = as.Date(dt))
  return(tbl2)
  
}

# get date from filename
get_date <- function(zip_path){
  pth <- str_remove(zip_path, "county_lex_")
  pth <- str_split_fixed(pth, "\\.", n = 2)[1]
  return(pth)
}


nyc_long <- county_files %>% map_dfr(~lex_long(., nyc_county))




