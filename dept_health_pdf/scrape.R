# largely based off of this blog post: https://www.brodrigues.co/blog/2018-06-10-scraping_pdfs/

# read libraries
pacman::p_load(tidyverse,
               here,
               glue,
               pdftools,
               lubridate,
               ggplot2)


# seperate text file with the dates i want to read in
dates <-
  read_csv(here("data", "dates.txt"), col_names = c("date"))

# url for the pdfs i want to read in
pdf_url <- "https://www1.nyc.gov/assets/doh/downloads/pdf/imm/covid-19-daily-data-summary-deaths-{dates$date}-1.pdf"

# names of the pdfs
pdf_names <- "nyc_deaths_{dates$date}.pdf"

# names of the pdfs in temp directory
dir <- glue(tempdir(), "/", pdf_names)

# download pdfs into a temporary directory
walk2(glue(pdf_url), dir, download.file, mode = "wb" )



# the function pdf_text turns a pdf into one long string, so raw_text will be a list of strings
raw_text <- map(dir, pdf_text)

# name the list with the dates
names(raw_text) <- dates$date

# i just want the past month
raw <- 
raw_text[1:28]

# takes a pdf, returns a dataframe
read_pdf <- function(file){
    
    # break up the table by new lines
    df <- 
       str_split(file, "\n", simplify = T)
    
      # set flags for where the stable starts, and ends
      table_start <- stringr::str_which(df, "Borough")
      table_end <- stringr::str_which(df, "Total\\s+\\d")
      table <- df[1, (table_start +1 ):(table_end)]
      
      # collapse double spaces
      table <- str_replace_all(table, "\\s{2,}", "|")
      table <- read_delim(table, delim = "|", col_names = F)
    return(table)
}


# combine all dataframes into one, dates are in the file name
deaths_long <- 
  map_dfr(raw, read_pdf, .id = "file") %>% 
  mutate(date = mdy(str_remove_all(file, "nyc_deaths|\\.pdf")))


# rename columns of dataframe
col_names <- c("f", "f2",
               "Borough",
               "underlying", 
               "no_underlying", "pending", "total", "date")

colnames(deaths_long) <- col_names

# drop some columns
deaths_long <- 
deaths_long %>% 
  select(-starts_with("f")) %>% 
  mutate(Borough = str_remove(Borough, "- "))

# wide to long
deaths_long <- 
deaths_long %>% 
  select(-total) %>% 
  pivot_longer(cols = c(underlying, no_underlying, pending), 
               names_to = "underlying_status")

# save(deaths_long, file = here("data", "nyc_covid_deaths.rdata"))

