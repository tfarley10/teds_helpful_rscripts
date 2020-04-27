######################################## TO DO: FIX DIFFERENCES IN COLUMN NAMES ########################################

library(rvest)
library(tidyverse)
library(glue)
library(readxl)


url <- "https://www1.nyc.gov/site/finance/taxes/property-annualized-sales-update.page"

# get names of files   
files <- read_html(url) %>% html_nodes("a") %>% html_attr("href")

# only those of annualized sales
files <- 
  files[str_detect(files, "annualized.+xls")]
paths <- glue("https://www1.nyc.gov{files}")

file_names <- str_extract(files, "/(.[^/]+)$")



# empty temp_dir
unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)

# set temporary destination path download files to that path
dest <- paste0(tempdir(),"/", file_names)

walk2(paths, dest, ~download.file(.x, .y))




process <- function(path){
  df1 <- 
    read_excel(path, col_names = F)
  df1 <- 
    df1 %>% 
    filter(!is.na(...2))
  
  names(df1) <- df1[1,]
  df1 <- 
    df1 %>% 
    slice(2:n()) %>% 
    janitor::clean_names()
  return(df1)
}


# final, long dataframe
dat3 <- 
  map_dfr(dest, ~process(.x))








