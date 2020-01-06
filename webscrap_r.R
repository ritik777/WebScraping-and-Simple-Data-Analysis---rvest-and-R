install.packages("rvest")
library("rvest")

#Specifying the url for desired website to be scraped
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)

descript_html = html_nodes(webpage,'.ratings-bar+.text-muted') 
descript_data = html_text(descript_html)

descript_data

runtime_data_html <- html_nodes(webpage,' .runtime')
runtime_data = html_text(runtime_data_html)
runtime_data

#removing mins to convert into numerical

runtime_data=gsub("min","",runtime_data)
head(runtime_data)
runtime_data = as.integer(runtime_data)
runtime_data

# getting ranking
ranking_html = html_nodes(webpage,'.text-primary')
ranking_data = html_text(ranking_html)
ranking_data
ranking_data = as.integer(ranking_data)

# getting title
title_html = html_nodes(webpage,'.lister-item-header a')
title_data = html_text(title_html)
head(title_data)

#genre
genre_html = html_nodes(webpage, '.genre')
genre_data= html_text(genre_html)
genre_data

#removing \na
genre_data = gsub("\n","",genre_data)
head(genre_data)

#removing whitespaces
genre_data= gsub(" ","",genre_data)

#taking only the first genre
genre_data = gsub(",.*","",genre_data)

genre_data<-as.factor(genre_data)
genre_data

#ratings
rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')
ratings_data= html_text(rating_data_html)
head(ratings_data)

ratings_data = as.numeric(ratings_data)

votes_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')
votes_html
votes_data = html_text(votes_html)
head(votes_data)

votes_data= gsub(",","",votes_data)
votes_data

votes_data = as.numeric(votes_data)
head(votes_data)

#Using CSS selectors to scrape the directors section
directors_data_html <- html_nodes(webpage,'.text-muted + p a:nth-child(1)')

#Converting the directors data to text
directors_data <- html_text(directors_data_html)
head(directors_data)

#Data-Preprocessing: converting directors data into factors
directors_data<-as.factor(directors_data)

#actors
actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')
actors_data_html <- html_nodes(webpage,'.ghost+ a')
#Converting the directors data to text
actors_data <- html_text(actors_data_html)
actors_data

#Data-Preprocessing: converting directors data into factors
actors_data<-as.factor(actors_data)

metascore_html =  html_nodes(webpage,'.metascore')
metascore_data = html_text(metascore_html)
metascore_data = gsub(" ","",metascore_data)
metascore_data

length(metascore_data)

for (i in c(45,69,93)){
   first<- metascore_data[1:(i-1)]
   second<- metascore_data[i:length(metascore_data)]
   metascore_data<- append(first,list("NA"))
   metascore_data<- append(metascore_data,second)
  
}

length(metascore_data)
metascore_data

metascore_data= as.numeric(metascore_data)
metascore_data

summary(metascore_data)

gross_html <- html_nodes(webpage,'.ghost~ .text-muted+ span')
gross_data<- html_text(gross_html)
head(gross_data)

#Removing M and $
gross_data = gsub("M","",gross_data)
gross_data=substring(gross_data,2,7)



for (i in c(11,45,46,48,71,85,90,92,93,97)){
  first<- gross_data[1:(i-1)]
  second<- gross_data[i:length(gross_data)]
  gross_data<- append(first,list("NA"))
  gross_data<- append(gross_data,second)
}
length(gross_data)
head(gross_data)

gross_data = as.numeric(gross_data)
summary(gross_data)

#Combining all the lists to form a data frame
movies_df<-data.frame(Rank = ranking_data, Title = title_data,
                      
                      Description = descript_data, Runtime = runtime_data,
                      
                      Genre = genre_data, Rating = ratings_data,
                      
                      Metascore = metascore_data, Votes = votes_data,                                                             Gross_Earning_in_Mil = gross_data,
                      
                      Director = directors_data, Actor = actors_data)

movies_df

library('ggplot2')

qplot(data = movies_df,Gross_Earning_in_Mil,fill = Genre,bins = 30)

ggplot(movies_df,aes(x=Runtime,y=Rating))+
  geom_point(aes(size=Votes,col=Genre))

ggplot(movies_df,aes(x=Runtime,y=Gross_Earning_in_Mil))+
  geom_point(aes(size=Rating,col=Genre))
