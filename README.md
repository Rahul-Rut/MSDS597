# MSDS597
Repository for Final Project

## Spotify Analysis of Charts

In this project, we scrape "Top 200 Charts" data from [Spotify Charts](https://spotifycharts.com/regional/us/weekly/) over different regions and time periods, which was later used to discover trends like top artists and genres for those specific conditions (time and region). The charts, however, do not contain genres and hence, [Spotify API](https://developer.spotify.com/documentation/web-api/) following [Client Credentails Flow](https://developer.spotify.com/documentation/general/guides/authorization/client-credentials/) using [various endpoints](https://developer.spotify.com/documentation/web-api/reference/#/operations/search) were used.


### Report.rmd
* The final report which needs to be executed by the User in order to access the Shiny App since a Shiny Application cannot be present in a static HTML page. It also contains all the required functions combined from the following R Scripts in one
* Running this takes a really long time mostly due to the calculation of Yearly Data dynamically. Using a data store might be a better alternative, at least for the yearly data



### Explore.R

* This file contains the initial exploration of the Spotify Charts data, including scraping the data from the website, cleaning, and basic visualizations.

### FINAL_CODE.R

* This file contains functions and dependencies which will later be used by the shiny app to render the plots. 
* It also contains independent plots graphed over the span of the past year. This was not included in the shiny app since it would take longer to scrape data and genres over such a huge time frame. Also, this would allow analyzing the data separately


### App.R

* Standalone Shiny App implemented, which provides the User with the option of choosing a specific region and time frame, and generates plots of Top Artists and Genres in real time dynamically. 
* Note: This app uses functions defined in the FINAL_CODE.R file and hence, it is necessary to have those functions stored in environment


### just_testing.R


* Origin file where all (and many more!) functions were developed. Feel free to overlook the junk file wherein the code was developed and is in a messy state. The author prefers to log all these lines of code and hence, left this file as it is.





