#Loading the rvest package
library('rvest')
library('ggplot2')

#Specifying the url for desired website to be scrapped
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)

#Rank data

#Using CSS selectors to scrap the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Data-Preprocessing: Converting rankings to numerical
rank_data<-as.numeric(rank_data)

#Title
title_data_html <- html_nodes(webpage,'.lister-item-header a')
title_data <- html_text(title_data_html)

#Description
description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')
description_data <- html_text(description_data_html)
description_data <-gsub("\n ","",description_data)

#Runtime
runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')
runtime_data <- html_text(runtime_data_html)

runtime_data<-gsub(" min","",runtime_data)
runtime_data<-as.numeric(runtime_data)

#Genre
genre_data_html <- html_nodes(webpage,'.genre')
genre_data <- html_text(genre_data_html)
genre_data<-gsub("\n","",genre_data)
genre_data<-gsub(" ","",genre_data)
genre_data<-gsub(",.*","",genre_data)
genre_data<-as.factor(genre_data)

#Rating
rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')
rating_data <- html_text(rating_data_html)
rating_data<-as.numeric(rating_data)

#Votes
votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')
votes_data <- html_text(votes_data_html)
votes_data<-gsub(",","",votes_data)
votes_data<-as.numeric(votes_data)

#Directors
directors_data_html <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')
directors_data <- html_text(directors_data_html)
directors_data<-as.factor(directors_data)

#Actors
actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')
actors_data <- html_text(actors_data_html)
actors_data<-as.factor(actors_data)

#Metascore
metascore_data_html <- html_nodes(webpage,'.metascore')
metascore_data <- html_text(metascore_data_html)
metascore_data<-gsub(" ","",metascore_data)

for (i in c(15,52,65,70)){
  a<-metascore_data[1:(i-1)]
  b<-metascore_data[i:length(metascore_data)]
  metascore_data<-append(a,list("NA"))
  metascore_data<-append(metascore_data,b)
}

metascore_data<-as.numeric(metascore_data)

#GrossData
gross_data_html <- html_nodes(webpage,'.ghost~ .text-muted+ span')
gross_data <- html_text(gross_data_html)
gross_data<-gsub("M","",gross_data)
gross_data<-substring(gross_data,2,6)

for (i in c(3,15,52,56,65,67,70,74,79,95)){
  a<-gross_data[1:(i-1)]
  b<-gross_data[i:length(gross_data)]
  gross_data<-append(a,list("NA"))
  gross_data<-append(gross_data,b)
}

gross_data<-as.numeric(gross_data)

#Combining all the lists to form a data frame
movies_df<-data.frame(Rank = rank_data, Title = title_data,
Description = description_data, Runtime = runtime_data,
Genre = genre_data, Rating = rating_data,
Metascore = metascore_data, Votes = votes_data,
  Gross_Earning_in_Mil = gross_data,
Director = directors_data, Actor = actors_data)

#Structure of the data frame
str(movies_df)

length(metascore_data)
summary(metascore_data)

qplot(data = movies_df,Runtime,fill = Genre,bins = 30)
ggplot(movies_df,aes(x=Runtime,y=Rating))+geom_point(aes(size=Votes,col=Genre))
ggplot(movies_df,aes(x=Runtime,y=Gross_Earning_in_Mil))+geom_point(aes(size=Rating,col=Genre))
