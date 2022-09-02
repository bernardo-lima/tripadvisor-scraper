# Scrape user reviews in Tripadvisor using R

This repository contains R code for scraping user reviews in Tripadvisor. 

For most websites, one can simply use the *rvest* package to scrape data from them. Martin Chan has a nice [blog post](https://martinctc.github.io/blog/vignette-scraping-amazon-reviews-in-r/) explaining how to *rvest* to scrape user reviews from Amazon. 

Tripavisor anti-spam system makes scraping data from it a bit more challenging. If you simply use *rvest* directly on it, you’re likely to get an error message like this:
"Error in open.connection(x, "rb") : HTTP error 403."

One way to circumvent this problem involves using the *RSelenium* package to automate a web browser and mimic a ‘real connection”. Chris Hoover has a [post]( https://www.cahoover.com/blog/marketing-analytics/scraping-tripadvisor-reviews-using-r/) about this approach. If you’re new to RSelenium check this [beginners guide]( https://www.youtube.com/watch?v=WRjKyCZsbE4) by Samer Hijjazi. 

Building on these ideas, I wrote an [R script](tripadvisor-scraper.R) with two main functions to scrape user reviews from Tripadvisor pages in batches. 
The function *scrape_trip_final* can be used to scrap different fields from user reviews (e.g. review title, review text, review score, etc.) This function is also robust against missing data in one of the fields. To use this function, you need to customize it with the url of the Tripadvisor page you want to scrape and the correct HTML elements or nodes you want. 
The function *scrape_batch* uses *scrape_trip_final* to politely scrape batches of multiple review pages. I recommend to scrape up to 20 pages at time to avoid connection time out. 

