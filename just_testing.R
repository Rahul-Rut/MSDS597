library(rvest)
library(jsonlite)
library(httr) 
library(curl) 
library(stringr)
library(RCurl)
library(tibble)

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
test_response = content(POST(url = "https://accounts.spotify.com/api/token", accept_json(), add_headers("Authorization" = auth_str) ,body = list(grant_type = "client_credentials"), encode = "form", verbose() ))
test_response

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







fnCurrentMonth <- function(week1_date) {
  todate = today()
  offset = (interval(date1, todate)/ days (1)) %% 7
  if (offset!= 0){
    todate = todate - offset
  }
  dates = todate - (7 * (seq(0,4)))
  weeks = paste0(dates[-1] ,"--",dates[-length(dates)])
  return(weeks)
}

fnCurrentMonth(date1)


fnMonthly <- function(week1_date, in_month) {  #this generates dates (daily, weekly) dates for API (won't use)
  month_date = ymd(paste0("2022-", in_month, "-01"))
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
  
  date_list
}

#change1



result = fnMonthly(date1, 4) #call the function directly whenever dates are generated
result





#args = commandArgs(trailingOnly=TRUE)
#metaSource <- args[1]

#args = commandArgs(trailingOnly=TRUE)
#metaSource <- args[1]
#Rscript <myscript.r> arg1 arg2