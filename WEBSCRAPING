##############################################################################
# IMDb webscraping Script
###############################################################################
# 
# There are two steps performed in this script:
# 
# 1. Scrape IMDb movie id from main imdb website
# - In this script date range is from 2021-12-31 to 2023-01-01, 100 movies per page
# - Store in 'all_movies.csv' file
# 
# 2. Scrape reviews for each movie
# - For each movie id, run over the list and scrape top 25 reviews for each movie.
# - Store in 'all_reviews.csv' file
# 
###############################################################################

if (!require("rvest")) install.packages("rvest", quiet=TRUE) ; require("rvest")
if (!require("XML")) install.packages("XML", quiet=TRUE) ; require("XML")
if (!require("stringr")) install.packages("stringr", quiet=TRUE) ; require("stringr")
if (!require("tidyverse")) install.packages("tidyverse", quiet=TRUE) ; require("tidyverse")
#setwd("D:/jntua_project/my/project")
# Function to retrieve Review Score ######
getReviewScore <- function(x){
  stars_html <- html_nodes(x,'.ratings-imdb-rating')
  stars <- html_text(stars_html)
  stars <- str_extract(stars[1], "\\d\\.\\d")
  return(stars)
}

# Initialize dataframe to store the movie id ######
all_movies <- data.frame(titles=character(),
                         imdb_ratings=integer(),
                         movie_id=character(),
                         stringsAsFactors=FALSE)

# scrape movie infos, return to all_movies dataframe ######
# output column names: titles	imdb_ratings	movie_id
# loop through 154 pages
for (i in 1:155){
  url <- paste0("https://www.imdb.com/search/title?title_type=feature&release_date=2021-12-31,2023-01-01&count=100&start=",i,"01&ref_=adv_nxt")
  url <- URLdecode(url)
  webpage <- read_html(url)
  # get a list of all the films in this page (list of 100)
  films_html <- html_nodes(webpage,'.mode-advanced')
  print(i)
  # set sleep time 5 seconds for each page
  Sys.sleep(5)
  #loop for all pages
  for (k in 1:length(films_html)){
    
    # extract movie title
    titles_html <- html_nodes(films_html[[k]],'.lister-item-header a')
    titles <- html_text(titles_html)
    
    # extract movie average rating (stars)
    stars_html <- html_nodes(films_html[[k]],'.ratings-imdb-rating')
    stars <- html_text(stars_html)
    imdb_ratings <- str_extract(stars[1], "\\d\\.\\d")
    
    # extract IMDb movie id
    href_html <- html_nodes(films_html[[k]],'a')%>% html_attr('href')
    movie_id <- strsplit(href_html[[1]],"/")[[1]][3]
    
    # append to dataframe
    this_movie <- as.data.frame(cbind(titles,imdb_ratings,movie_id))
    all_movies <- rbind(all_movies,this_movie)
  }
  # # periodically save the file every 1000 entries
  if(nrow(all_movies)%%1000==0){write.csv(all_movies,'all_movies.csv')}
}
# export to csv
write.csv(all_movies,'all_movies.csv')

# read in all_movies.csv file
library(readr)
all_movies <- read_csv("all_movies.csv")


# scrape movie review using movie id as a key, return data to all_reviews dataframe ######
# output names: id	comment_titles	ratings	comments_body
all_reviews <- data.frame(id=character(),
                          comment_titles=character(),
                          ratings=integer(),
                          comments_body=character(), 
                          stringsAsFactors=FALSE)

# get all films reviews, return to all_reviews dataframe
all_ids <- as.character(all_movies$movie_id)
# loop through all movie id
for (id in all_ids){
  # this url is sorted by 'helpfulness of the review', 25 reviews per movie.
  url <- paste0("https://www.imdb.com/title/",id,"/reviews?ref_=tt_urv")
  url <- URLdecode(url)
  webpage <- read_html(url)
  # some movies do not have any review, check for 0 Reviews
  check_review<- html_nodes(webpage,'.article')[[1]]%>%html_text()
  zero_review<- str_detect(check_review,"0 Reviews")
  if(zero_review==TRUE)
    # set sleep time 5 seconds for each page
  {Sys.sleep(5)}
  else{
    films_html <- html_nodes(webpage,'.lister-item-content')
    for (k in 1:length(films_html)){
      # extract review title
      comment_titles_html <- html_nodes(films_html[[k]],'.title')
      comment_titles <- html_text(comment_titles_html)
      comment_titles <-str_trim(gsub("\r?\n|\r", " ", comment_titles))
      
      # extract comment rating
      ratings_html <- html_nodes(films_html[[k]],'.ipl-ratings-bar')
      ratings <- html_text(ratings_html)
      ratings<- str_extract(ratings, "(\\d)+")
      if(identical(ratings, character(0))){ratings<-0} #replace missing rating with 0
      
      # extract review content
      comments_body_html <- html_nodes(films_html[[k]],'.show-more__control')
      comments_body <- html_text(comments_body_html)
      comments_body <-str_trim(gsub("\r?\n|\r", " ", comments_body))
      
      # combine into dataframe and append
      this_review <- as.data.frame(cbind(id,comment_titles,ratings,comments_body))
      all_reviews <- rbind(all_reviews,this_review)
      # keep track of movie id, in case the script crashes
      write.csv(c(id,match(id,all_ids)),'last_id.csv')
    }
  }
  # periodically save the file every 500 reviews
  if(nrow(all_reviews)%%500==0){write.csv(all_reviews,'all_reviews.csv')}
}
# export to csv
write.csv(all_reviews,'all_reviews_movie_0-1866.csv')

