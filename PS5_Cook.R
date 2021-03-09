#loading the rvest package so I can use it to scrape from a URL
library(rvest)

#reading the HTML code from the website
#in this case, the top 100 rom coms according to IBDb
webpage <- read_html('https://www.imdb.com/list/ls053605210/')

#using CSS selector to scrape the ordinal ranking values
rank_data_html <- html_nodes(webpage,'.text-primary')
#converting ranking data to text
rank_data <- html_text(rank_data_html)
#converting text to numerical
rank_data <- as.numeric(rank_data)
#check that it worked
head(rank_data)

#using CSS selector to scrape the movie titles
title_data_html <- html_nodes(webpage,'.lister-item-header a')
#converting title data to text
title_data <- html_text(title_data_html)
#checking 
head(title_data)

#using CSS selector to scrape the movie descriptions
description_data_html <- html_nodes(webpage,'.ipl-rating-widget+ p , .ratings-metascore+ p') 
#converting description data to text
description_data <- html_text(description_data_html)
#checking
head(description_data)
#getting rid of the "\n" at the begining of every review
description_data <- gsub("\n","",description_data)
#checking again
head(description_data)

#using CSS selector to scrape runtime
runtime_data_html <- html_nodes(webpage,'.runtime')
#converting the runtime data to text
runtime_data <- html_text(runtime_data_html)
#checking
head(runtime_data)

#combining these lists to make a data frame
romcom_df <- data.frame(Rank=rank_data, Title=title_data, Description=description_data, Runtime=runtime_data)

#inspecting the structure
str(romcom_df)
head(romcom_df)


#now to grab data using an API, here: the STL Fed using the fredr package
library(fredr)

#pull some monthly unemployment data 
unemployment_data <- fredr(
  series_id = "UNRATE",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2021-01-01"))


