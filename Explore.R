library(rvest)
library(tidyverse)

#Specify the link
url <- "https://spotifycharts.com/regional/global/weekly/2022-04-08--2022-04-15"
#Extract the table
table <- url %>% read_html() %>% html_table(fill = TRUE) %>% .[[1]] 
names(table)[1] <- "a"
names(table)[2] <- "Number"  #name the unnamed columns so we can convert to tibble
names(table)[3] <- "b"
table <- as_tibble(table) %>% select(Number,Track,Streams)
# This gives us the title of the track and number of streams, but doesn't list the artist!


#Try using Selector Gadget
artists <- url %>% read_html() %>% html_nodes("span") %>% html_text()
#My RStudio keeps freezing when i run the above code. Doing it step by step, it appears the problem happens after running html_text()
#Let's try to extract without using html_text()
artists <- url %>% read_html() %>% html_nodes("span") %>% html_text2() # This seems to work for some reason!
artists[2] #This is where the artists start being listed
artists_list <- artists[2:201] %>% as_tibble() %>% rename(Artist=value) #create a list of the artists for the given chart

#Now we can clean the list by removing the "by" in every entry, leaving only the artist
artists_list$Artist <- str_replace_all(artists_list$Artist,"by ","")

#Add the artists to the original table
table <- mutate(table,Artist = artists_list$Artist)

#Fix the track names
table$Track <- str_replace_all(table$Track, "(?<=by).+","") #Get rid of everything preceded by "by"
table$Track <- str_replace_all(table$Track, "by","") #Get rid of "by"
table$Track <- trimws(table$Track,"right") #Get rid of trailing white space






