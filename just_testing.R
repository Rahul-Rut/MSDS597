library(rvest)
library(jsonlite)
library(httr) 
library(curl) 
library(stringr)
library(RCurl)
library(tibble)
library(lubridate)
library(purrr)
library(tidyverse)
library(shiny)

url_test <- "https://api.spotify.com/v1/artists/3pc0bOVB5whxmD50W79wwO" 
test = url_test %>% fromJSON()%>%.[[1]]%>%as_tibble()  #gives an error so don't bother running

curl_fetch_memory(url_test) #Forgot what this was for lol

#An example of what an API request looks like
GET("https://api.spotify.com/v1/artists/3pc0bOVB5whxmD50W79wwO",add_headers("Content-Type"="application/json", "Authorization" = paste("Bearer", "BQAZKcbzm_5o2ppsrRmzTOp8TZZMZDbnji62-fPp60bv0m-Fv5cWWbjhRPHzU433din119O3-Z5RB7fAY80hFxZmgxsp8W2u2T_79kwOco_lLhT3PIBgZaWHpEjnPz3VO4EaF592_M33YKolUg9GT8Tf0GmF0QU") ))


#THESE ARE VERY PERSONAL CREDENTIALS, PLEASE DO NOT SHARE :(

client_id <- 'redacted'
client_secret <- 'redacted'


#CORRECT: encoding credentials to be used later
enc_str = base64(paste0(client_id,":",client_secret))
auth_str = paste0("Basic ", enc_str)
auth_str = "Basic MWFkYTQwNDFiMGJkNDgyZWE0MjEyY2ZiMGIyNDYxODg6MWYzNjYwMTQyZjBjNDM4Mjg1M2E2MGY1YTZhZjU0MzU="


#CORRECT: 
#Generates token to be used as authorization for further API requests
#AMBITIOUS: Check status of current token and only generate token if the token has expired (Hint: )


test_response2 = content(POST(url = "https://accounts.spotify.com/api/token", add_headers("Authorization" = auth_str) ,body = list(grant_type = "client_credentials"), encode = "form", verbose() ))
token = test_response2$access_token


#artist using our VERY OWN SELF-GENERATED token
#same can be done for several artists, track and several tasks
artist = GET("https://api.spotify.com/v1/artists/3pc0bOVB5whxmD50W79wwO",add_headers("Content-Type"="application/json", "Authorization" = paste("Bearer", token) ))
artist%>% fromJSON()%>%.[[1]]%>%as_tibble()
content(artist)




#To do:
#1) Think of a way to fetch artist id (use various packages)
#2) GET only certain attributes (like genre in this case) from API response instead of storing all redundant info
#3) Time the process of these many fetches and make it efficient



#date

library(lubridate)

#2022-04-01--2022-04-01
date1 = "2022-04-01"
day(date1) %% 7
date0 = floor_date( ymd(date1), 'month')
date0

(interval(date0,date1) / days (1))%%7
#logic: diff mod 7 gives the number of days from start where to calculate daily

li = seq(0, 3)
offset = 1
date0
dl = as.Date(date0) + li
dl
date_k = c()
date_k = append(date_k, dl)
date_k[length(date_k)]

tes = ymd(paste0("2022-",5, "-01"))
offset = (interval(date1,tes) / days (1))%%7
offset
date_list = c()
total_days = days_in_month(tes)
total_days
back_off = (total_days - offset - 1)%%7
li = seq(0, offset-1) 
som_dates = as.Date(tes) + li     #last_date is week1 date for weekly
date_list = append(date_list, som_dates) 

weeks = (31 - offset  - 1) / 7
weekly = date_list[length(date_list)]+1 + (7 * seq(0,weeks))
weekly_list = paste0(weekly[-length(weekly)],"--",weekly[-1])
date_list = append(date_list, weekly_list)
todate = today()
seq(0,4)

weekly = month_date + (7 * seq(0,weeks))
dates = todate - (7 * (seq(0,4)))
weeks = paste0(dates[-length(dates)],"--",dates[-1])
offset = (interval(date1, todate)/ days (1)) %% 7
todate - offset
weeks = list(paste0(dates[-1] ,"--",dates[-length(dates)]))
date_list
append(weeks, date_list)
list(date0, weeks)


as.list(weeks)

seq(0,1)


getWeeks <- function(week1_date, n =1) {  #calculates 1 week prior dates; n is number of weeks
  todate = today()
  offset = (interval(week1_date, todate)/ days (1)) %% 7
  if (offset!= 0){
    todate = todate - offset
  }
  
  
  dates = todate - (7 * (seq(0,n)))
  weeks = paste0(dates[-1] ,"--",dates[-length(dates)])
  return(as.list(weeks))
}

getWeeks(date1,53)

getToken <- function(){
  test_response2 = content(POST(url = "https://accounts.spotify.com/api/token", add_headers("Authorization" = auth_str) ,body = list(grant_type = "client_credentials"), encode = "form", verbose() ))
  token = test_response2$access_token
}


fnCurrentMonth(date1)
in_month = 3
as.Date(paste0("2022-", in_month, "-01"))

fnMonthly <- function(week1_date, in_month) {  #this generates dates (daily, weekly) dates for API (won't use)
  month_date = as.Date(paste0("2022-", in_month, "-01"))
  offset = (interval(week1_date,month_date) / days (1))%%7
  date_list = c()
  total_days = days_in_month(month_date)
  if (offset!= 0)  #if start of URLweek does not match with the month's week
  {
    back_off = (total_days - offset - 1)%%7
    li = seq(0, offset-1)
    som_dates = as.Date(month_date) + li     #last_date is week1 date for weekly
    date_list = append(date_list, som_dates) #appended first few dates
    if (back_off != 0)    #if last week of month doesn't match the URL week
    {
      li2 = seq(total_days-1,(total_days - (back_off)%%7), -1)
      las_dates = as.Date(month_date) + li2   
      date_list = append(date_list, las_dates) #appended last few days
      weeks = (total_days - offset - length(li2) - 1) / 7
      weekly = date_list[length(date_list)]+1 + (7 * seq(0,weeks))
      weekly_list = paste0(weekly[-length(weekly)],"--",weekly[-1])
 
      date_list = append(date_list, weekly_list)
    }  
    else{    #last week of month matches URL week
      weeks = (total_days - offset  - 1) / 7
      weekly = date_list[length(date_list)]+1 + (7 * seq(0,weeks))
      weekly_list = paste0(weekly[-length(weekly)],"--",weekly[-1])

      date_list = append(date_list, weekly_list)
    }
  }
  else{    #first week of month matches URL week
    weeks = floor(total_days/7)
    weekly = month_date + (7 * seq(0,weeks))
    reve = month_date + seq(total_days-1,(weeks*7)+1 ,-1)
    date_list = append(date_list, reve)
    weekly_list = paste0(weekly[-length(weekly)],"--",weekly[-1])
    date_list = append(date_list, weekly_list)
    
  }
  
}

#change1



result = fnMonthly(date1, 4) #call the function directly whenever dates are generated
result





#args = commandArgs(trailingOnly=TRUE)
#metaSource <- args[1]

#args = commandArgs(trailingOnly=TRUE)
#metaSource <- args[1]
#Rscript <myscript.r> arg1 arg2



#TESTING FR
#TO BE REPLACED
#------------------------------------------------------------------------------#
url <- "https://spotifycharts.com/regional/us/weekly/"

concat.url<- function(x){
  full_url <- paste0(url, x)
  full_url
}


#make a nested function which gets parameters set from buttons in shiny
#region, regional/[us,ar], global
#dates : daily: today(), weekly : offset, (maybe change month function to weekly, and run it 4 times)
week_list = fnCurrentMonth(date1)
finalurl = lapply(getWeeks(date1), concat.url)
finalurl[[1]]

url <- "https://spotifycharts.com/regional/global/weekly/2022-04-08--2022-04-15"
#Extract the table
table <- finalurl[[1]] %>% read_html() %>% html_table(fill = TRUE) %>% .[[1]] 
names(table)[1] <- "a"
names(table)[2] <- "Number"  #name the unnamed columns so we can convert to tibble
names(table)[3] <- "b"
table <- as_tibble(table) %>% select(Number,Track,Streams)

#Try using Selector Gadget
artists <- url %>% read_html() %>% html_nodes("span") %>% html_text2() 
artists[2] #This is where the artists start being listed
artists_list <- artists[2:201] %>% as_tibble() %>% rename(Artist=value) #create a list of the artists for the given chart

#Now we can clean the list by removing the "by" in every entry, leaving only the artist
artists_list$Artist <- str_replace_all(artists_list$Artist,"by ","")
table <- mutate(table,Artist = artists_list$Artist)



table$Track <- str_replace_all(table$Track, "(?<=by).+","") #Get rid of everything preceded by "by"
table$Track <- str_replace_all(table$Track, "by","") #Get rid of "by"
table$Track <- trimws(table$Track,"right")

table$Streams <- str_replace_all(table$Streams,"[,+]", "")
table$Streams <- as.integer(table$Streams)

#-----------------------------------------------------------------------------#

spotify$Artist%>% 
dim(spotify)
token
str(table$Artist)

getURLsuffix <- function(){
  
}

genURL <- function(){
  
}

getSpotifyCharts <- function(url, month_bin = 0){

  #Extract the table
  table <- url %>% read_html() %>% html_table(fill = TRUE) %>% .[[1]] 
  
  if(month_bin == 1){
    
    table <-table[1:50,]
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
    artists_list <- artists_list[1:50,]
  }
  
  
  #Now we can clean the list by removing the "by" in every entry, leaving only the artist
  artists_list$Artist <- str_replace_all(artists_list$Artist,"by ","")
  table <- mutate(table,Artist = artists_list$Artist)
  
  table$Track <- str_replace_all(table$Track, "(?<=by).+","") #Get rid of everything preceded by "by"
  table$Track <- str_replace_all(table$Track, "by","") #Get rid of "by"
  table$Track <- trimws(table$Track,"right")
  
  table$Streams <- str_replace_all(table$Streams,"[,+]", "")
  table$Streams <- as.integer(table$Streams)
  table
  
  
}
url = "https://spotifycharts.com/regional/global/weekly/2022-04-08--2022-04-15"
ScrapedData <- getSpotifyCharts(url, month_bin = 1)


weeks = getWeeks(date1)

lapply(weeks, getSpotifyCharts())

 
weeks = getWeeks(date1)
month_list = map2( map(weeks, concat.url),1, getSpotifyCharts) 
month_list%>%
  bind_rows(.)%>%
  group_by(Track, Artist )%>%
  summarize(Streams = sum(Streams))

for (i in weeks){
  url = "https://spotifycharts.com/regional/global/weekly/"
  i = paste0(url,i)
  getSpotifyCharts(i, month_bin = 1)
}


#The following two functions are meant for extracting Genre

getGenreTibble <- function(artistS){ 
  url_encoded = URLencode(artistS)
  genres = unique(unlist((map(url_encoded, getGenre))))
  
}


getGenreArtist <- function(artist){
  
 tryCatch(  {search = GET(paste0("https://api.spotify.com/v1/search?q=", artist ,"&type=artist&limit=1"),add_headers("Content-Type"="application/json", "Authorization" = paste("Bearer", token) ))
  result = content(search)
  message(artist)
  message(result$artists$items[[1]]$genres)
  result = result$artists$items[[1]]$genres}, error = function(e){})
  #genre_list = append(genre_list, result$artists$items[[1]]$genres)
  
}

token = getToken()

test = table%>%
  mutate(Artist_sing = (str_split(Artist, ", ")))%>%
  mutate(Genres = map(Artist_sing, getGenreTibble))


#Generates Top Artist in the Specific Period
table%>%
  separate_rows(Artist, sep = ",", convert = TRUE)%>% #splits Artists
  group_by(Artist)%>%
  summarize(Total_Stream = sum(Streams))%>%
  arrange(desc(Total_Stream))

#generates top genre
test%>%
  unnest(Genres)%>%
  group_by(Genres)%>%
  summarise(Total_Streams = sum(Streams))%>%
  arrange(desc(Total_Streams))%>%
  head(10)%>%
  ggplot(aes(Total_Streams, reorder(Genres, Total_Streams)))+
  geom_col()
  

#lets make a separate dataframe for yearly scraped data @_@

weeks = getWeeks(date1, n = 48)  #for n = 48 weeks assuming 4 weeks per month
#url is set to global weekly
month_list = map2( map(weeks, concat.url),1, getSpotifyCharts) #scraping data for a year (top 50 per week)

month_index = (rep( seq(12,1,-1) , each = 200)) #calculate month backwards

Scraped = month_list%>%
  bind_rows(.)%>%
  mutate(Month = month_index) #add month index

#See how artists did over the year  

Scraped = Scraped%>%
  mutate(Artist = replace(Artist, Artist == "BobHelms", "Bobby Helms"))

#top10 artists
ten_artists = Scraped%>%
  separate_rows(Artist, sep = ", ", convert = TRUE)%>% #splits Artists
  group_by(Artist)%>%
  summarize(Total_Stream = sum(Streams))%>%
  arrange(desc(Total_Stream))%>%
  select(Artist)%>%
  head(10)

pull(ten_artists)

#top10 artists performance over the year each month
Scraped%>%
  separate_rows(Artist, sep = ", ", convert = TRUE)%>% 
  group_by(Artist, Month)%>%
  summarize(Total_Streams = sum(Streams))%>%
  filter(Artist %in% pull(ten_artists))%>%
  ggplot(aes(Month, Total_Streams, color = Artist))+
  geom_line()
  
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

token= getToken()
genres_list = map(URLencode(Artist_list), getGenreArtist)

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



tppp = tpp%>%
  ungroup()%>%
  group_by(Track, Month)%>%
  mutate(Genre = toString(unique(unlist(Genre))))%>%
  separate_rows(Genre, sep = ", ")

tr = tppp%>%
  ungroup()%>%
  group_by(Genre)%>%
  summarize(Total_Streams = sum(Streams))%>%  #tally() here would give appearances of these genres per week
  arrange(desc(Total_Streams))%>%
  head(10)
tr$Genre


dfd = tppp%>%
  filter(Track == "Enemy (with JID) - from the series Arcane League of Legends")%>%
  head(48)

dfd

Artist_Genre%>%filter(Artist_list == "Imagine Dragons")%>%select(genres_list)

toString(unique(unlist(tpp[141,]$Genre)))



  #mutate(Genre = unique((Genre)))

  #mutate(all_genres = paste(Genre, collapse = " "))

tppp['all_genres']

tp = Scraped%>%
  mutate(Artist_sing = (str_split(Artist, ", ")))%>%
  mutate(genre = map(Artist_sing, matchGenre))

unlique <- function(artist){
  unique(unlist(artist))
}


tp = tp%>%mutate(gen = map(genre, unlique))



  token = getToken()
top_genres = Scraped%>%
  mutate(Artist_sing = (str_split(Artist, ", ")))%>%
  mutate(Genres = map(Artist_sing, getGenreTibble))

  #group_by(Genres)%>%
  #summarise(Total_Streams = sum(Streams))%>%
  #arrange(desc(Total_Streams))%>%
  #head(10)

  
  #group_by(Track, Artist )%>%
  #summarize(Streams = sum(Streams))





#Workflow 
table[1:50,]








#JUNK CODE
test2 = table%>%
  separate_rows(Artist, sep = ",", convert = TRUE)%>%
  group_by(Artist)%>%
  mutate(gen = map(Artist, getGenreArtist))

test2%>%group_by(Track)%>%mutate(gen = toString(gen))%>%filter(Track == "'Till I Collapse")%>%select(gen)


test2%>%mutate(gen = map(gen,unique))%>%filter(Track == "'Till I Collapse")
test2%>%filter(Track == "'Till I Collapse")

test = table%>%
  separate_rows(Artist, sep = ",", convert = TRUE)%>%
  group_by(Artist)%>%
  mutate(gen = map(Artist, getGenreArtist))%>%
  unnest(c= (gen)) %>%
  mutate(gen = unlist(gen))


test%>%
  group_by(gen)%>%
  summarize(Strea = sum(Streams))%>%
  arrange(Strea)
  arrange(desc(Streams))
  group_by(gen)%>%
  ggplot(aes(  Streams, reorder(gen, -Streams)))+
  geom_col()


#generate functions for plotting trending genres, artists, top 10 artists in the past month











  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
#TEST CASES
  
lis = URLencode(unlist(str_split(table$Artist,",")))
lis[1]
URLencode("we don't talk about bruno")

test = table%>%
  mutate(Artist_sing = (str_split(Artist, ", ")))%>%
  mutate(Genres = map(Artist_sing, getGenreTibble))

unlist(test$Genres[199])

test$Artist_sing[[181]]

test3 = (separate_rows(test, Artist, sep = ", ", convert = TRUE))
token = getToken()


unnest(test3, cols = c(gen))

test3 = test3%>%
  group_by(Artist)%>%
  mutate(gen = map(Artist, getGenreArtist))
unlist(test3$gen[[247]])

test3%>%
  filter(Number == 181)

unitedArtists = test3 %>%
  group_by(Track) %>%
  summarize(Artist = toString(Artist))

test3%>%
  inner_join(unitedArtists, by = "Track")
 
test3 %>%
  group_by(Track) %>%
  nest %>%
  ungroup


test3%>%
  mutate(Artist = str_extract(Artist, ".*(?= \\(|\\) )"))
#165 has braces

test3%>%
  filter(str_detect(Artist, "[(]"))

NAME = "4*TOWN (From Disney and Pixar’s Turning Red)"

str_extract(NAME, ".*(?= County)")

#RHIS WORKS
str_extract(NAME, ".*(?= \\(|\\) )")


test3 %>% 
  mutate(Artist =)

square <- function(li){
  
}
data("mtcars") 
mtcars %>%
  group_by(cyl) %>%
  group_modify()

test2 = test%>%
  mutate(Gen = map(Genres, unlist))
test2$Gen[[199]]

URLencode(test[[5]][181][[1]])

unique(unlist((map(URLencode(test[[5]][181][[1]]), getGenre))))

test[[5]][[181]]


token = getToken()
genre_list = c()

map(test[[5]][181],getGenreTibble)[[1]]


lapply(lis, genre_f)
lis[1]
search = GET(paste0("https://api.spotify.com/v1/search?q=", lis[1],"&type=artist&limit=1"),add_headers("Content-Type"="application/json", "Authorization" = paste("Bearer", token) ))
result = content(search)
result$artists$items[[1]]$genres

items[[1]]$genres[[1]]

search = GET("https://api.spotify.com/v1/search?q=we%20don't%20talk%20about%20bruno&type=track&limit=1",add_headers("Content-Type"="application/json", "Authorization" = paste("Bearer", token) ))
result = content(search)
result$tracks$items[[1]]$artists
"https://api.spotify.com/v1/search?q=first%20class&type=track&limit=1"
