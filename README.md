# Baltimore Homicide Inequality

There is evidence that [poverty and violence go hand-in-hand](http://www.apa.org/pi/ses/resources/publications/violence.aspx) for a number of reasons. Those who are poor
may come into friction with those who are not. Or the lack of wealth is systemic, learing to a poor public
safety infrasctructure. Or those who are poor do things deemed criminal by society in order to survive.

In this project, I take data from Baltimore on homicides in 2016 and data from the Baltimore Neighborhood
Indicators Alliance (http://bniajfi.org) and see if homicides were inequitably distributed in Baltimore.

This is work that Public Health professionals can reproduce in their own locals (or even in Baltimore)
using their own data and looking at other social and health indicators.

For more information, please feel free to contact me via Twitter: [@EpiRen](http://twitter.com/epiren)

## Data Sources

For my doctoral dissertation, I received a list of homicide cases from [Justin Fenton](https://twitter.com/justin_fenton) at _The Baltimore Sun_. That list contained the names, locations, and some demographic (age, gender, race) information on homicides reported in Baltimore between 2005 and 2016. I added to those data the same data that I had collected from 2017. In order to validate the dataset, I randomly pulled **400 cases** and searched for news archives, obituaries, and official sources to confirm them. All 400 cases were confirmed this way. I then geocoded these data to get the latitude and longitude of the incident location through _ArcGIS_.

The information on the _Community Statistical Areas_ (CSA) came from the [Baltimore Neighborhood Indicators Alliance](http://bniajfi.org). BNIA collects and cleans data about Baltimore every year. For this project, I've used their _Vital Signs 16_ data, which you can explore too by [clicking here](https://bniajfi.org/vital_signs/). I obtained the **percent of households under the poverty level** by (CSA).

All of the data and code that you need for this project are in the [project's depository](https://github.com/RFNajera/baltimore-homicide-inequality). The [R code](/Concentration Curve.R) has within it all the commenting you'll need to understand what is happening with each line of code, as well as the questions that we will be answering below.

## The Main Aim of This Project

The main aim of this project is to give Public Health professionals a primer on using R programming to determine if a health indicator (homicides, in this case) is equitably distributed in a location (Baltimore) according to that location's indicator of wealth (percent of households under the poverty level, in this case).

## Step One: Are Homicides and Poverty Distributed in a Spatially Similar Way in Baltimore?

For this first part, we bring in all the data of homicides from 2005 to 2017. I've already geocoded these data. If it only had street addresses (at the block level) of homicides, they would need to be geocoded into latitude-longitude coordinates. But that's for a different project at a different time. What is also included in the dataset are the names of the Community Statistical Areas (CSA) they belong to. CSAs are a way to bring together neighborhoods into bigger areas while maintaining as much as possible of the neighborhoods' geographic and sociodemographic characteristics.

```r
# Read the homicides between 2005 and 2017 into a dataframe. Since we're using 2016 CSA data, we'll only use 2016 homicides.
# The data we will use was obtained from The Baltimore Sun and cleaned up by the author for work in his doctoral (DrPH) dissertation.
homicides_all <- read.csv("data/baltimore_homicides_2005_2017.csv")

#Subset only the homicides in 2016
library(tidyr)
library(dplyr)
homicides_2016 <- homicides_all %>% filter(year==2016)

# Save this subset as a .csv file if you want to use it later
write.csv(homicides_2016, file = "data/baltimore_homicides_2016.csv")
```
### Question 1
**How many homicides were reported in 2016?

Now we're ready to bring in a shapefile from BNIA which contains the shape of the CSAs in Baltimore for mapping purposes as well as other information on the CSAs, like population, percent of households under the poverty line, etc... All data we'll use.

```r
# Bring in shapefile with population data obtained from the
# Baltimore Neighborhood Indicators Alliance at https://bniajfi.org/vital_signs/data_downloads/
library(rgdal)
ogrInfo(dsn="data/bnia_16", 
        layer="bnia_16") # see shapefile info
csa_baltimore <- readOGR("data/bnia_16",
                         "bnia_16") # read the shapefile into R
csa_baltimore <- spTransform(csa_baltimore, 
                             CRS("+init=epsg:4326")) # transform coord system: More projections information http://trac.osgeo.org/proj/wiki/GenParms
```

Because R needs to deal with this as a dataframe, we transform the shapefile into a dataframe.

```r
# Tranform the shapefile into a dataframe, keep the CSA name information
library(ggplot2)
csa_baltimore <- fortify(csa_baltimore, 
                         region = "CSA2010") # "id" is the name of the CSA column
```

Now we get the total number of homicides per CSA

```r

# Summarize number of homicides by CSA and place that into a total column
homicides_csa <- homicides_2016 %>%
  group_by(CSA2010) %>%
  summarise(total=n()) # "total" will be the total number of shootings per CSA.
```
### Question 2
*** Which CSA had the highest number of homicides reported in 2016?

