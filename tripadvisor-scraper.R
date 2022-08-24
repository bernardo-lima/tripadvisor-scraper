# References 
# https://www.youtube.com/watch?v=WRjKyCZsbE4 > RSelenium Package in R (Example) | Automate Web Browsers | Introduction & Tutorial for Beginners
# https://martinctc.github.io/blog/vignette-scraping-amazon-reviews-in-r/
# https://www.cahoover.com/blog/marketing-analytics/scraping-tripadvisor-reviews-using-r/

# load packges
library(RSelenium)
library(tidyverse)
library(netstat)
library(sys)
library(rvest)

# Pages for the hotel one wants to extract data  
# https://www.tripadvisor.com/Hotel_Review-g60982-d209422-Reviews-Hilton_Waikiki_Beach-Honolulu_Oahu_Hawaii.html#REVIEWS
# https://www.tripadvisor.com/Hotel_Review-g60982-d209422-Reviews-or10-Hilton_Waikiki_Beach-Honolulu_Oahu_Hawaii.html#REVIEWS
# https://www.tripadvisor.com/Hotel_Review-g60982-d209422-Reviews-or20-Hilton_Waikiki_Beach-Honolulu_Oahu_Hawaii.html#REVIEWS

# test function to get sequential url pages 
test_url_seq <- function(x){
  url_reviews <- paste0("https://www.tripadvisor.com/Hotel_Review-g60982-d209422-Reviews-or",x,"-Hilton_Waikiki_Beach-Honolulu_Oahu_Hawaii.html")
  
  # Return a tibble
  tibble(url_reviews) %>% return()
}

review_range <- seq(10, 100, by = 10) # Let's say we want to scrape pages 1 to 10
test_url_seq(review_range)

# Boot Selenium
rs_drive_object2 <- rsDriver(
  browser = "firefox",
  verbose = F,
  port = free_port()
)

remDr <- rs_drive_object2$client
# remDr <- rD[["client"]] 
# remDr$open

# Scrape function

scrape_trip <- function(page_num){
  url <- paste0("https://www.tripadvisor.com/Hotel_Review-g60982-d209422-Reviews-or",page_num,"-Hilton_Waikiki_Beach-Honolulu_Oahu_Hawaii.html")
  
  remDr$navigate(url)
  
  Sys.sleep(2) # First wait for the DOM to load by pausing for a couple seconds.
  
  # Now click all the elements with class "exapand review"  
  remDr$findElements("xpath", ".//div[contains(@data-test-target, 'expand-review')]")[[1]]$clickElement()
  html <- remDr$getPageSource()[[1]]
  
  # Use rvest 
  doc <- read_html(html)
  
  # Review Text
  doc %>% 
    html_elements(".QewHA") %>% 
    html_text() -> review_text
  
  # Review Title
  doc %>% 
    html_elements(".KgQgP") %>%
    html_text() -> review_title
  
  # Review Date  
  doc %>% 
    html_elements(".teHYY") %>%
    html_text() %>%
    str_remove_all("Date of stay: ") -> review_date # Remove unwanted strings
  
  # Review Score 
  doc %>% 
    html_elements(".Hlmiy") %>%
    html_children() %>% # Look at the child of the named class
    html_attr("class") %>% # Grab the name of the class of the child
    str_remove_all("ui_bubble_rating bubble_") -> review_score
  
  # Return a tibble
  tibble(review_title,
         review_text,
         review_date,
         review_score
  ) %>% return()
}

scrape_trip(20)

# Create a table that scrambles page numbers using `sample()`
# For randomising page reads!

page_range <- seq(10, 100, by = 10) # Let's say we want to scrape pages 1 to 10

match_key <- tibble(n = page_range,
                    key = sample(page_range,length(page_range)))

lapply(page_range, function(i){
  j <- match_key[match_key$n==i,]$key
  
  message("Getting page ",i, " of ",length(page_range), "; Actual: page ",j) # Progress bar
  
  Sys.sleep(3) # Take a three second break
  
  if((i %% 3) == 0){ # After every three scrapes... take another two second break
    
    message("Taking a break...") # Prints a 'taking a break' message on your console
    
    Sys.sleep(2) # Take an additional two second break
  }
  scrape_trip(page_num = j) # Scrape
}) -> output_list

review_text_100 <- output_list %>% bind_rows() 

# Saving on object in RData format
getwd()
save(output_list, review_text_100, file = "/data/tripadvisor_data.RData")
