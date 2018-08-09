# Baltimore Homicide Inequality

There is evidence that [poverty and violence go hand-in-hand](http://www.apa.org/pi/ses/resources/publications/violence.aspx) for a number of reasons. Those who are poor
may come into friction with those who are not. Or the lack of wealth is systemic, learing to a poor public
safety infrasctructure. Or those who are poor do things deemed criminal by society in order to survive.

In this project, I take data from Baltimore on homicides in 2016 and data from the Baltimore Neighborhood
Indicators Alliance (http://bniajfi.org) and see if homicides were inequitably distributed in Baltimore.

This is work that Public Health professionals can reproduce in their own locals (or even in Baltimore)
using their own data and looking at other social and health indicators.

For more information, please feel free to contact me via Twitter: @EpiRen

## Data Sources

For my doctoral dissertation, I received a list of homicide cases from [Justin Fenton](https://twitter.com/justin_fenton) at _The Baltimore Sun_. That list contained the names, locations, and some demographic (age, gender, race) information on homicides reported in Baltimore between 2005 and 2016. I added to those data the same data that I had collected from 2017. In order to validate the dataset, I randomly pulled **400 cases** and searched for news archives, obituaries, and official sources to confirm them. All 400 cases were confirmed this way. I then geocoded these data to get the latitude and longitude of the incident location through _ArcGIS_.

The information on the _Community Statistical Areas_ (CSA) came from the [Baltimore Neighborhood Indicators Alliance](http://bniajfi.org). BNIA collects and cleans data about Baltimore every year. For this project, I've used their _Vital Signs 16_ data, which you can explore too by [clicking here](https://bniajfi.org/vital_signs/). I obtained the **percent of households under the poverty level** by (CSA).

## The Main Aim of This Project

The main aim of this project is to give Public Health professionals a primer on using R programming to determine if a health indicator (homicides, in this case) is equitably distributed in a location (Baltimore) according to that location's indicator of wealth (percent of households under the poverty level, in this case).

## Step One: Are Homicides and Poverty Distributed in a Spatially Similar Way in Baltimore?

To be continued...
