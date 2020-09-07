#Author: A.Link
#Date: 9.7.2020
#Packages: rvest
#Notes: NOAA seems to be the only site that provides adequate amounts of data

#How to extract certain data - Dev tools does not seem like it gives much on NOAA

#Try to extract states:
install.packages("rvest")
library(rvest)
list=read_html("https://www.ncdc.noaa.gov/cdo-web/datatools/normals")

#Precipitation transfers (temps-times ratio and measurments: https://www.weather.gov/gsp/snow)

html_nodes(list, 'US States#title)
