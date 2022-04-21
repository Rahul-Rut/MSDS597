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
client_id <- '1ada4041b0bd482ea4212cfb0b246188'
client_secret <- '1f3660142f0c4382853a60f5a6af5435'


#INCORRECT: Doesn't work
unserialize(base64_dec(base64_enc(client_id))) #from jsonlite
str <- base64_enc(serialize(client_id, NULL))
out <- unserialize(base64_dec(str))
serialize(client_id, NULL)

#INCORRECT: Do not run
client_enc = base64(client_id)
secret_enc = base64(client_secret)
id_enc <- base64_enc(serialize(client_id, NULL))
secret_enc <- base64_enc(serialize(client_secret, NULL))
paste("Basic",id_enc,":", secret_enc)

#CORRECT: encoding credentials to be used later
enc_str = base64(paste0(client_id,":",client_secret))
auth_str = paste0("Basic ", enc_str)
auth_str


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
