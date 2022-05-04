# MSDS597
Repository for Final Project

## Spotify Analysis of Charts

In this project, we scrape "Top 200 Charts" data from [Spotify Charts](https://spotifycharts.com/regional/us/weekly/) over different regions and time periods, which was later used to discover trends like top artists and genres for those specific conditions (time and region). The charts, however, do not contain genres and hence, [Spotify API](https://developer.spotify.com/documentation/web-api/) following [Client Credentails Flow](https://developer.spotify.com/documentation/general/guides/authorization/client-credentials/) using [various endpoints](https://developer.spotify.com/documentation/web-api/reference/#/operations/search) were used.


### Explore.R



### FINAL_CODE.R

* This file contains functions and dependencies which will later be used by the shiny app to render the plots. 
* It also contains independent plots graphed over the span of the past year. This was not included in the shiny app since it would take longer to scrape data and genres over such a huge time frame. Also, this would allow analyzing the data separately


### App.R

* Standalone Shiny App implemented, which provides the User with the option of choosing a specific region and time frame, and generates plots of Top Artists and Genres in real time dynamically. 
* Note: This app uses functions defined in the FINAL_CODE.R file and hence, it is necessary to have those functions stores in environment


### just_testing.R

*Origin file where all (and many more!) functions were developed. Feel free to overlook the junk file wherein the code was developed and is in a messy state. The author prefers to log all these lines of code and hence, left this file as it is.



