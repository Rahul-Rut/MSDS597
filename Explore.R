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
artists <- url %>% read_html() %>% html_nodes("span") %>% html_text2() 
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
#Convert streams to numbers
table$Streams <- str_replace_all(table$Streams,"[,+]", "")
table$Streams <- as.integer(table$Streams)

#Issue: Dealing with tracks that have multiple artists
#Can we split the Artists column somehow?



#Exploring what we have so far
top <- table %>% mutate(Track=reorder(Track,Streams)) %>% head(10)
ggplot(top) + geom_col(aes(Streams,Track)) + ggtitle("Top Tracks This Week")

#Top artists this week
topartists <- table %>% group_by(Artist) %>% summarise(n=n()) %>% mutate(Artist=reorder(Artist,n)) %>% filter(n>1)
ggplot(topartists) + geom_col(aes(n,Artist)) + ggtitle("Top Artists This Week") + xlab("Number of Songs")

#What about artists with only one song on the chart?
singles <- table %>% group_by(Artist) %>% summarise(n=n()) %>% filter(n==1)
table %>% semi_join(singles,by="Artist") %>% mutate(Track=reorder(Track,Streams)) %>% head(10) %>%
  ggplot() + geom_col(aes(Streams,Track)) + ggtitle("Top Tracks By Artists w/ One Song")




