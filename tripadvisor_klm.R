# References 
# [Vignette: Scraping Amazon Reviews in R](https://martinctc.github.io/blog/vignette-scraping-amazon-reviews-in-r/)
# [Scraping Tripadvisor reviews using R](https://www.cahoover.com/blog/marketing-analytics/scraping-tripadvisor-reviews-using-r/)
# [RSelenium Package in R (Example) | Automate Web Browsers](https://www.youtube.com/watch?v=WRjKyCZsbE4)

# load packages
library(tidyverse) # collection of packages for data science
library(rvest) # for scraping data from web pages
library(RSelenium) # for automating web browsers
library(netstat) # for retreving and parsing network statistics
library(sys) # for running system command is R 
library(dplyr)
# Create a new project (or set working directory in a new folder)
getwd()

# Create a folder to save the data
dir.create("data")

# Open the KLM page on tripadvisor and seehow the url changes when browse through the reviews
# 1) Open the KLM page
# https://www.tripadvisor.com/Airline_Review-d8729104-Reviews-KLM-Royal-Dutch-Airlines.html
# 2) Click on 'Reviews"
# https://www.tripadvisor.com/Airline_Review-d8729104-Reviews-KLM-Royal-Dutch-Airlines.html#REVIEWS
# 3) Click on a different review pages to see how the url changes
# https://www.tripadvisor.com/Airline_Review-d8729104-Reviews-or5-KLM-Royal-Dutch-Airlines.html#REVIEWS
# https://www.tripadvisor.com/Airline_Review-d8729104-Reviews-or10-KLM-Royal-Dutch-Airlines.html#REVIEWS

# For most websites, one can simply use use the 'rvest' package to download and extract data from them (Martin Chan blog post for a neat example and explanation -reference # 1 above)
# However tripavisor's anti-spam system makes it a bit more challenging. If one uses 'rvest' directly on tripadvisor, they get the following error message ""Error in open.connection(x, "rb") : HTTP error 403."
# A potential solution to this challenge involves using 'RSelenium' to automate a web browser and mimic a 'real connection (see Chris Hoover post - reference  #2). 

# Scraping function
# Scrape function combines code from references 1, 2 and 3
scrape_trip <- function(page_num){
  url <- paste0("https://www.tripadvisor.com/Airline_Review-d8729104-Reviews-or",page_num,"-KLM-Royal-Dutch-Airlines.html#REVIEWS")
  
  remDr$navigate(url)
  
  Sys.sleep(2) # First wait for the DOM to load by pausing for a couple seconds.
  
  # Now click all the elements with class "expand review"  
  remDr$findElements("xpath", ".//div[contains(@data-test-target, 'expand-review')]")[[1]]$clickElement()
  html <- remDr$getPageSource()[[1]]
  
  # Use read_html() from xml2 to save url content as a R object
  html_data <- read_html(html)
  
  # Review Title
  review_title <- html_data %>% 
    html_elements(".KgQgP") %>%
    html_text()
  
  # Review Text
  review_text <- html_data %>% 
    html_elements(".QewHA") %>%
    html_text()
  
  # Travel Date  
  travel_date <- html_data %>% 
    html_elements(".teHYY") %>%
    html_text() %>%
    str_remove_all("Date of travel: ") %>% # Remove unwanted strings
    
    # Review Score 
    review_score <-html_data %>% 
    html_elements(".Hlmiy") %>%
    html_children() %>% # Look at the child of the named class
    html_attr("class") %>% # Grab the name of the class of the child
    str_remove_all("ui_bubble_rating bubble_") # Remove unwanted strings
  
  # Return a tibble
  tibble(review_title,
         review_text,
         travel_date,
         review_score
  ) %>% return()
}

# Some reviews don't have a 'Travel date'. 
# This function breaks when there is a missing data in one of the variables. It cannot return when the vectors have different lenght.
# "Error:! Tibble columns must have compatible sizes."

# Solution:
# https://stackoverflow.com/questions/45901532/inputting-na-where-there-are-missing-values-when-scraping-with-rvest

scrape_trip_final <- function(page_num){
  url <- paste0("https://www.tripadvisor.com/Airline_Review-d8729104-Reviews-or",page_num,"-KLM-Royal-Dutch-Airlines.html#REVIEWS")
  
  remDr$navigate(url)
  
  Sys.sleep(2) # First wait for the DOM to load by pausing for a couple seconds.
  
  # Now click all the elements with class "expand review"  
  remDr$findElements("xpath", ".//div[contains(@data-test-target, 'expand-review')]")[[1]]$clickElement()
  html <- remDr$getPageSource()[[1]]
  
  # Use read_html() from xml2 to save url content as a R object
  html_data <- read_html(html)
  
  df <- html_data %>% 
    html_nodes("[class='WAllg _T']") %>%    # select enclosing nodes
    # iterate over each, pulling out desired parts and coerce to data.frame
    map_df(~list(review_title = html_nodes(.x, '.KgQgP') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .}, # replace length-0 elements with NA
                 review_text = html_nodes(.x, '.QewHA') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 review_score = html_nodes(.x, '.Hlmiy') %>% 
                   html_children() %>% # Look at the child of the named class
                   html_attr("class") %>% # Grab the name of the class of the child
                   str_remove_all("ui_bubble_rating bubble_"), # Remove unwanted strings,
                 travel_date = html_nodes(.x, '.teHYY') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .}%>%
                   str_remove_all("Date of travel: ")))
  
  # Return a tibble
  tibble(df
  ) %>% return()
}

# Create a function that scrapes pages in batches and do it politely (taking breaks and selecting pages randomly)
# This function builds on code from Reference 1 

# Test function to get sequential url pages (i.e. batches)
test_url_seq <- function(x){
  url_reviews <- paste0("https://www.tripadvisor.com/Airline_Review-d8729104-Reviews-or",x,"-KLM-Royal-Dutch-Airlines.html#REVIEWS")
  
  # Return a tibble
  tibble(url_reviews) %>% return()
}

review_range <- seq(5, 100, by = 5) # Let's say we want to scrape pages 1 to 10
test_url_seq(review_range)


# Now wrap this in a faction and add the random assignmentand breaks part
scrap_batch <- function(str_pg, end_pg){
  page_range <- seq(str_pg, end_pg, by = 5) # Let's say we want to scrape 100 reviews from 20 pages
  
  match_key <- tibble(
    n = page_range,
    key = sample(page_range, length(page_range))
  )
  
  output_list <- lapply(page_range, function(i) {
    j <- match_key[match_key$n == i, ]$key
    
    message("Getting page ", i, " of ", length(page_range), "; Actual: page ", j) # Progress bar
    
    Sys.sleep(3) # Take a three second break
    
    if ((i %% 3) == 0) { # After every three scrapes... take another two second break
      
      message("Taking a break...") # Prints a 'taking a break' message on your console
      
      Sys.sleep(2) # Take an additional two second break
    }
    scrape_trip_final(page_num = j) # Scrape
  })
  
  output_df <- output_list %>% bind_rows()
  
  # Return a tibble
  tibble(output_df
  ) %>% return()
}

# Boot Selenium (You need to have java development kit installed in your machine to run Selenium)
rs_drive_object <- rsDriver(
  browser = "firefox",
  verbose = F,
  port = free_port()
)
remDr <- rs_drive_object$client
# remDr <- rD[["client"]] 
# remDr$open
remDr$navigate("https://www.tripadvisor.com/Airline_Review-d8729104-Reviews-or5-KLM-Royal-Dutch-Airlines.html#REVIEWS")


# Date of collection 25/08/2022
# Get reviews from pages 5 to 100
reviews_5to100 <- scrap_batch(5,100)
# Get reviews from pages 105 to 200
reviews_105to200 <- scrap_batch(105,200)
reviews_205to300 <- scrap_batch(205,300)
reviews_305to400 <- scrap_batch(305,400)
reviews_405to500 <- scrap_batch(405,500)

# Date of collection 26/08/2022
# Get reviews from pages 505 to 100
reviews_505to600 <- scrap_batch(505,600)
reviews_605to700 <- scrap_batch(605,700)
reviews_705to800 <- scrap_batch(705,800)
reviews_805to900 <- scrap_batch(805,900)
reviews_905to1000 <- scrap_batch(905,1000)

# get the reviews dfs and put them in a list stack all the different lists into one table 
ls(pattern = "reviews_")
rev_list <- mget(ls(pattern = "reviews_"))

#remove individual review files from memory
rm(list = ls(pattern = "reviews_"))

# Combining a list of data frames into a single data frame 
reviews_5to1000 <- do.call(rbind, rev_list)
save(reviews_5to1000, file = "/data/reviews_klm_5to1000.RData")
