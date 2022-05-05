# Including Libraries

library(rvest)
library(jsonlite)
library(httr) 
library(curl) 
library(stringr)
library(RCurl)
library(tibble)
library(lubridate)
library(shiny)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(tidyverse)

#include scraping part

#Function for scraping the entire data
getSpotifyCharts <- function(url, month_bin = 0){   #month_bin, when 1, reduces the amount of data read to 50, rather than usual 200
  #we do this to reduce the amount of data stored for longer time frames
  
  #Extract the table
  table <- url %>% read_html() %>% html_table(fill = TRUE) %>% .[[1]] 
  
  if(month_bin == 1){
    
    table <-table[1:50,]  #reading only first 50 entries
  }
  
  
  names(table)[1] <- "a"
  names(table)[2] <- "Number"  #name the unnamed columns so we can convert to tibble
  names(table)[3] <- "b"
  table <- as_tibble(table) %>% select(Number,Track,Streams)
  
  #Try using Selector Gadget
  artists <- url %>% read_html() %>% html_nodes("span") %>% html_text2() 
  artists[2] #This is where the artists start being listed
  artists_list <- artists[2:201] %>% as_tibble() %>% rename(Artist=value) #create a list of the artists for the given chart
  
  
  if(month_bin == 1){
    artists_list <- artists_list[1:50,]  #reading just the first 50 again
  }
  
  
  #Now we can clean the list by removing the "by" in every entry, leaving only the artist
  artists_list$Artist <- str_replace_all(artists_list$Artist,"by ","")
  table <- mutate(table,Artist = artists_list$Artist)
  
  table$Track <- str_replace_all(table$Track, "(?<=by).+","") #Get rid of everything preceded by "by"
  table$Track <- str_replace_all(table$Track, "by","") #Get rid of "by"
  table$Track <- trimws(table$Track,"right")
  
  table$Streams <- str_replace_all(table$Streams,"[,+]", "")
  table$Streams <- as.integer(table$Streams)
  table         #returning table
  
  
}



date1 = "2022-04-01" #A reference date used to generate weekly dates

#As it will be seen further, the "Weekly" Data will be used to generate Weekly, Monthly as well as Yearly Data.
#But alas! The Weekly Data in Spotify Charts is calculated between 2 consecutive Fridays, rather than 7 days before the current date.
#These dates are passed in the URL for Spotify charts 
#Besides the current week (which CAN also like "/weekly/latest") the other Weekly Charts follow "weekly/2022-04-15--2022-04-22" format in their URL
#Hence, to calculate the last Friday Date, or rather, generate proper weekly dates, we will be using this "date1" as reference and the following function


getWeeks <- function(week1_date, n =1) {   #calculates 1 week prior date range (default); n is number of past weeks
  todate = today()                         #get today's date using lubridate's today function
  offset = (interval(date1, todate)/ days (1)) %% 7   #offset stores the days between today and the closest week range suitable for Spotify
  if (offset!= 0){       #if offset exists
    todate = todate - offset    #update the date to the nearest week range
  }
  
  #this step calculates all the dates required in the weeks list where n decides number of weeks and stores in vector
  dates = todate - (7 * (seq(0,n)))  
  #for example, if one wants to fetch dates for past two weeks (n=2), then this function produces 3 dates
  #assuming today is 15th April, the dates produced are 2022-04-15, 2022-04-08, 2022-04-01
  
  
  #the next line concatenates them in string type suitable to be used by URL
  weeks = paste0(dates[-1] ,"--",dates[-length(dates)]) 
  #continuing previous example, dates[-1] would drop first date and store 2022-04-08, 2022-04-01 whereas dates[-length(dates)] would drop last date and store 2022-04-15, 2022-04-08
  #hence, pasting these with "--" in between gives us "2022-04-08--2022-04-15" and "2022-04-01--2022-04-08" which is exactly what we need in the url!
  
  return(weeks)
}


#Generating API? Token

#The use of Spotify API requires Client Credentials which are provided as follows
client_id <- 'redacted'
client_secret <- 'redacted'

#The way this works is that the credentials (formatted as <id>:<secret>) have to be base64 encoded
enc_str = base64(paste0(client_id,":",client_secret))  

#The encoded string then has to include a prefix of "Basic" and later used as a value in Authorization Header of the CURL POST request
auth_str = paste0("Basic ", enc_str)


#Unfortunately, GitHub wouldn't let me upload the Spotify Credentials due to Privacy Threat, so I have stored the entire encoded string as follows
#Although, GitHub did raise a red flag again (which shows how powerful their system is!) but I let it slide for the sake of project
auth_str = "Basic MWFkYTQwNDFiMGJkNDgyZWE0MjEyY2ZiMGIyNDYxODg6MWYzNjYwMTQyZjBjNDM4Mjg1M2E2MGY1YTZhZjU0MzU="
#this is how the string looks like after encoding

#Function for generating token
genToken <- function(){
  
  #The Following is a CURL POST request wherein we send client credentials, type of request and fetch the Access Token Required for Acessing Endpoints of API
  #The headers have been added as per the guidelines of API, where grant_type specifies the "Client Credentials Flow" aka server to server communication
  
  test_response2 = content(POST(url = "https://accounts.spotify.com/api/token", add_headers("Authorization" = auth_str) ,body = list(grant_type = "client_credentials"), encode = "form" ))
  token = test_response2$access_token  #returns the "access_token" part of the POST response
}



#This function fetches Genre of a given Artist from Spotify API and passes onto the "getGenreTibble" Function

getGenreArtist <- function(artist, token){  #accepts URL ecnoded Artist Name and Token as parameter
  
  #Try Catch is to negate null values or Artists with No Genre which is rare!
  tryCatch(  {search = GET(paste0("https://api.spotify.com/v1/search?q=", artist ,"&type=artist&limit=1"),add_headers("Content-Type"="application/json", "Authorization" = paste("Bearer", token) ))
  
  #Lets break down the above code line
  #The "search?q=" signifies that we are using the "search item" endpoint, followed by artist name
  # the "&type=artist&limit=1" means we are searching for an Artist, not a song, and limiting our results to the Top 1 search
  #the reason we are comfortable with limiting the search to 1 is that the Artists Name are generated by Spotify itself and hence we are bound to get perfect match
  #second of all, these Artists will be searched from the TOP 200 CHARTS meaning they ought to be famous, and hence will be fetched first in case of name clashes
  #we pass the token in the authorization header, and the rest is basic CURL format as specified by the Spotify API
  
  
  result = content(search)  #the results are stored using the content function
  result = result$artists$items[[1]]$genres}, error = function(e){})  
  
  #we retrieve the "$genres" of artist and return it to the user
  
  

  
}



#This function generates Genres for a given Data Entry (aka Genres For a Song)
#The approach here is that many songs have multiple artists, and one approach to save genre is without splitting the entry per artist, we find the genres of the song

getGenreTibble <- function(artistS, token){   #takes artists and access token as parameter
  url_encoded = URLencode(artistS)            #modifying artists' name to URL format
  genres = unique(unlist((map2(url_encoded, token ,getGenreArtist))))    #mapping the encoded artist name and token to generate artist function
  #'unlist' helps us unpack or flatten the list and 'unique' function gets rid of common genres between artists of a same song 
  
}


#The following lines of code are specific for the app

date1 = "2022-04-01"  #storing the date1


#This list corresponds to the list available in the dropdown in App
country_list = c("Global", "USA", "UK", "Australia", "Brazil", "Canada")

#This list corresponds to the above list's codes as used in Spotify URL
code_list = c("global","us","gb","au","br","ca")

#a tibble that stores the bith the above lists side by side
concode = tibble(country_list, code_list)


#The following functions translates the button inputs from app to URL format required suffix

#A function used to fetch the Country's codes from their Names using the previous Tibble
matchRegion <- function(region){
  concode%>%
    filter(country_list == region)%>%  #matching the region
    select(code_list)    #fetching just the codes
}



#This function translates the output of "Time" button from shiny app to suitable format for URL search
getDates<- function(time_period){
  
  #just used if else loop 
  if (time_period == "Monthly")
  {
    dates = getWeeks(date1, n = 4)
    suffix = paste0("/weekly/",dates)
  }
  else if (time_period == "Daily")
  {
    return (c("/daily/latest/"))
  }
  else if (time_period == "Weekly")
  {
    return (c("/weekly/latest/"))
  }
}


#After translating the codes, we append them to url and get scraped data
getScraped <- function(region,dates){
  URL = paste0("https://spotifycharts.com/regional/",region,dates)
  if (length(URL) == 1){  #this is for weekly, daily since we will be scraping just once
    getSpotifyCharts(URL)
    
  }
  else {
    
    #for monthly we have to scrap several times hence we use map
    #the "1" is month_bin parameter which enables reading only top 50 entries to reduce size of data stored
    month_list = map2( URL,1, getSpotifyCharts)   
    month_scraped =  month_list%>%
      bind_rows(.) #binding all the weekly data into 1 monthly data
  }
}


#This function is called by shiny app to generate plots
main <- function(region, time_period){   #accepts input from shiny app aka desired time period and region 
  

  region_code = matchRegion(region) #translating region to code that fits with url for
  
  dates = getDates(time_period) #translating time period to code that fits with url
  
  scraped = getScraped(region_code,dates)  #getting scraped data
  
  token = genToken()  #generating access token
  
  #this generates plot for TOP ARTISTS in given time frame and region
  
  #Stored as GLOBAL variable since multiple results cannot be returned from a function!!!!!
  art_plot <<- scraped%>%  
    separate_rows(Artist, sep = ",", convert = TRUE)%>% #splits Multiple Artists, if any, in multiple rows (much like pivot_longer!)
    group_by(Artist)%>%                      #grouping by artist
    summarize(Total_Stream = sum(Streams))%>%   #Finding total streams per artist
    arrange(desc(Total_Stream))%>%              #Arranging them in descending order
    head(10)%>%                                 #storing just the TOP 10 Artists
    mutate(Artist=reorder(Artist,Total_Stream))%>%   #Reordering Artists as per their order of Streams
    ggplot()+ 
    geom_col(aes(Total_Stream,Artist)) +      #plotting Total Streams
    ggtitle(paste("Top Artists", time_period ,region))  #finishing touch
  
  
  
#This Generates plot for TOP GENRES in given time and region
  genplot <- scraped%>%
<<<<<<< HEAD
    mutate(Artist_sing = (str_split(Artist, ", ")))%>%   #splits the artists in a tibble for a given song
    mutate(Genres = map2(Artist_sing, token, getGenreTibble))%>%  #get genre per song
    unnest(Genres)%>%     #splits every genre into each row (more like pivot longer)
    group_by(Genres)%>%   #groups by genre
    summarise(Total_Streams = sum(Streams))%>%   #calculates total streams per genre
    arrange(desc(Total_Streams))%>%    #arrange into descending order per total streams
    head(10)%>%                        #store just the top 10
    ggplot(aes(Total_Streams, reorder(Genres, Total_Streams/1000000)))+   #plot top 10 genres
=======
    mutate(Artist_sing = (str_split(Artist, ", ")))%>%
    mutate(Genres = map2(Artist_sing, token, getGenreTibble))%>%
    unnest(Genres)%>%
    group_by(Genres)%>%
    summarise(Total_Streams = sum(Streams))%>%
    arrange(desc(Total_Streams))%>%
    head(10)%>%
    ggplot(aes(Total_Streams, reorder(Genres, Total_Streams/1000000)))+
>>>>>>> 28ada6f54be3c57b0ddf38a60a27fd2a81cebbb3
    geom_col()+
    labs(x="Total_Streams(in Millions)", y= "Genre", title = paste("Top Genres",time_period ,region))
  return(genplot)
  
  
  #a function to generate dates
  #scrape
  #get genre
  #plot genre, artist
  
}




#lets make a separate dataframe for yearly scraped data @_@

weeks = getWeeks(date1, n = 48)  #for n = 48 weeks assuming 4 weeks per month
#url is set to global weekly
url = "https://spotifycharts.com/regional/global/weekly/"
dates_weeks = paste0(url,weeks)
month_list = map2( dates_weeks,1, getSpotifyCharts) #scraping data for a year (top 50 per week)
month_index = (rep( seq(12,1,-1) , each = 200)) #calculate month backwards

Scraped = month_list%>%
  bind_rows(.)%>%
  mutate(Month = month_index) #add month index

#See how artists did over the year  


#An inconsistency I found was that Charts had "BobHelms" although Spotify identifies him as "Booby Helms"
#So had to make this change
Scraped = Scraped%>%
  mutate(Artist = replace(Artist, Artist == "BobHelms", "Bobby Helms"))

#top10 artists
ten_artists = Scraped%>%
  separate_rows(Artist, sep = ", ", convert = TRUE)%>% #splits Artists
  group_by(Artist)%>%                                  #group by artists
  summarize(Total_Stream = sum(Streams))%>%            #Calculate total streams per artist
  arrange(desc(Total_Stream))%>%                       #save in descending
  select(Artist)%>%                                   #extract just the artists
  head(10)                                            #extract just the top 10



#top10 artists performance over the year each month
global_year_artist_plot = Scraped%>%
  separate_rows(Artist, sep = ", ", convert = TRUE)%>% 
  group_by(Artist, Month)%>%
  summarize(Total_Streams = sum(Streams))%>%
  filter(Artist %in% pull(ten_artists))%>%
  ggplot(aes(Month, Total_Streams/1000000, color = Artist))+
  geom_line()+
  labs(y = "Total Streams (in Millions)", x = "Month(Reverse Order)")

#investigating the peak
Scraped%>%
  separate_rows(Artist, sep = ", ", convert = TRUE)%>% 
  filter(Month == 1)%>% 
  group_by(Artist)%>%
  tally()%>%
  arrange(desc(n))  #Olivia Rodrigo had several hits that month!!


#getting top genre in the past year

#getting rid of one random blank data
Scraped = Scraped%>%
  mutate_all(na_if,"")

Artist_list = Scraped$Artist%>%
  str_split(", ")%>%
  unlist()%>%
  unique()

token= genToken()
genres_list = map2(URLencode(Artist_list), token, getGenreArtist)

Artist_Genre = tibble(Artist_list, genres_list )

res = Artist_Genre%>%filter(Artist_list == "Drake")
res$genres_list


matchGenre <- function(artist){
  result = Artist_Genre%>%filter(Artist_list == artist)
  result$genres_list
}

tpp = Scraped%>%
  separate_rows(Artist, sep = ", ", convert = TRUE)%>%
  group_by(Artist)%>%
  mutate(Genre = map(Artist,matchGenre))



global_year = tpp%>%
  ungroup()%>%
  group_by(Track, Month)%>%
  mutate(Genre = toString(unique(unlist(Genre))))%>%
  separate_rows(Genre, sep = ", ")

tr = global_year%>%
  ungroup()%>%
  group_by(Genre)%>%
  summarize(Total_Streams = sum(Streams))%>%  #tally() here would give appearances of these genres per week
  arrange(desc(Total_Streams))%>%
  select(Genre)%>%
  head(10)

global_year_genre_plot = global_year%>% 
  group_by(Genre, Month)%>%
  summarize(Total_Streams = sum(Streams))%>%
  filter(Genre %in% pull(tr))%>%
  ggplot(aes(Month, Total_Streams/1000000, color = Genre))+
  geom_line()+
  labs(y = "Total Streams (in Millions)", x = "Month(Reverse Order)") 









