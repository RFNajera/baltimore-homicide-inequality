title: Analysis of Public Health Inequalities Using R

## Poverty and Violence

There is evidence that [poverty and violence go hand-in-hand](http://www.apa.org/pi/ses/resources/publications/violence.aspx) for a number of reasons. Those who are poor
may come into friction with those who are not. Or the lack of wealth is systemic, learing to a poor public
safety infrasctructure. Or those who are poor do things deemed criminal by society in order to survive.

In this project, I take data from Baltimore on homicides in 2016 and data from the Baltimore Neighborhood
Indicators Alliance (http://bniajfi.org) and see if homicides were inequitably distributed in Baltimore.

This is work that Public Health professionals can reproduce in their own locals (or even in Baltimore)
using their own data and looking at other social and health indicators. This project is based on ["Methods for measuring inequalities in health"](https://jhu.pure.elsevier.com/en/publications/methods-for-measuring-inequalities-in-health) by Schneider et al. (The _et al_ includes my doctoral thesis advisor and mentor, [Dr. Carlos Castillo-Salgado](https://www.jhsph.edu/faculty/directory/profile/118/carlos-castillo-salgado).)

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
**How many homicides were reported in 2016?**

Now we're ready to bring in a shapefile from BNIA which contains the shape of the CSAs in Baltimore for mapping purposes.

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
**Which CSA had the highest number of homicides reported in 2016?**

Now, we're going to bring in BNIA data on population and percent of households under the poverty line. We will then join that information with the homicide counts we just created. We then save the data for later use.

```r
# Add the population and percent of households under poverty level to the information by CSA.
#These data also from BNIA at https://bniajfi.org/vital_signs/data_downloads/

bnia_demo <- read.csv("data/bnia_demographics_2016.csv") # Reads the data

csa_info <- left_join(bnia_demo,
                      homicides_csa, 
                      by = "CSA2010", 
                      type = "right") # Joins the data to the homicide per CSA table. There will be some NAs because some CSAs did not have homicides.

csa_info[is.na(csa_info)] <- 0  # Some of the CSAs did not have homicides. So we need to turn those into zeroes. 
                                # It prevent's NAs in the next step, calculating the homicide rate.

#Calculate the homicide rate per 100,000 residents
csa_info$homicide_rate <- csa_info$total / (csa_info$pop/100000)

# Save this for later use as a .csv
write.csv(csa_info, file = "data/baltimore_homicides_2016_with_counts.csv")
```

### Question 3
**Which CSA had the highest homicide rate per 100,000 residents reported in 2016?**

### Making a Choropleth Map

You've probably seen choropleth maps before. They're those maps where the shading of a geographic unit (e.g. a US State) depends on the value of some variable (e.g. Population). For this exercise, we're going to make two choropleth maps: One showing the percent of households under the poverty level by CSA in Baltimore (based off the _Vital Signs 16_ data), and one showing the homicide rate per 100,000 residents we just calculated above from the homicide data and the population per CSA from BNIA.

For this, we will join the homicide counts, rates, and percent poverty to the CSA shapefile, then make the map using [ggmap](https://blog.dominodatalab.com/geographic-visualization-with-rs-ggmaps/). Look at the code that customizes the map. The neat thing about doing this in code is that **you don't have to remember which buttons you clicked if you want to reproduce your work**. If you use a graphic user interface (GUI), or point-and-click, you might forget how you did something. Here, it's all in code, with comments and such, ready for you to plug in your data, tweak a few things in the code to fit your needs, and output something you can use.

```r
# Join CSA info (the counts of homicides, homicide rate, etc.) to CSA polygon
csa_baltimore2 <- merge(x = csa_baltimore, y = csa_info, by.x = "id", by.y = "CSA2010", all.x = TRUE)

# Choropleth of poverty

library(scales) # For the palette and pretty breaks
library(ggmap) # For the map
baltimore_roadmap <- get_map("Baltimore", 
                             zoom = 12,
                             maptype = c("toner"),
                         source = "stamen")
baltimore_poverty <- ggmap(baltimore_roadmap)
baltimore_poverty <- baltimore_poverty +
  geom_polygon(aes(x=long, 
                   y=lat, 
                   group=group, 
                   fill = poverty),
               size=.3, 
               color='black', 
               data=csa_baltimore2,
               alpha=0.8) +
  scale_fill_distiller(type="seq", 
                       palette = "PuOr",  # Gives a nice scale, but it is customizable: 
                       breaks=pretty_breaks(n=5), 
                       name="% of Households Under Poverty", 
                       na.value = "transparent") +  # There should not be any NAs, right?
  labs(x=NULL, 
       y=NULL,
       title="Percent of Households Under the Poverty Level by Community Statistical Area in Baltimore", # Remember to use a descriptive title
       subtitle=NULL,
       caption="Source: bnia.org") +
  theme(plot.title = element_text(face="bold", 
                                  family="Arial", 
                                  size=8)) +
  theme(plot.caption = element_text(face="bold", 
                                    family="Arial", 
                                    size=7, 
                                    color="gray", 
                                    margin=margin(t=10, r=80))) +
  theme(legend.position="right") +
  theme(axis.line =  element_blank(), #Eliminates the plot axes and labels
        axis.text =  element_blank(),
        axis.ticks =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# Now, let's look at the map
baltimore_poverty

# Save the map so you can use it in a document, or edit it with a graphics editing software
ggsave("images/baltimore_poverty_choropleth.png")
```
Your result should look like this:

![Choropleth Map of Poverty in Baltimore](https://raw.githubusercontent.com/RFNajera/baltimore-homicide-inequality/master/images/baltimore_poverty_choropleth.png "Poverty Choropleth")

### Other ways to map?
Yes, there are other ways to create a map in R, but this is a very "robust" way that allows you a lot of customization. Another package is [Leaflet](https://andrewbtran.github.io/NICAR/2017/maps/leaflet-r.html) , but we will leave that for another project at a later time.

For the choropleth map of homicide rate, we do the same code, just using a different value. (This is what I meant by code being better than point-and-click. All I had to do was replace the value `poverty` with `homicide_rate`.)

```r
# Choropleth of homicide rate in 2016 by CSA
# Just like the choropleth above, but with homicide rate as the filler.
baltimore_hr <- ggmap(baltimore_roadmap)
baltimore_hr <- baltimore_hr +
  geom_polygon(aes(x=long, 
                   y=lat, 
                   group=group, 
                   fill = homicide_rate), # Fill by "homicide_rate" instead of "poverty"
               size=.3, 
               color='black', 
               data=csa_baltimore2, # Same data as the choropleth for poverty
               alpha=0.8) +
  scale_fill_distiller(type="seq", 
                       palette = "PuOr", 
                       breaks=pretty_breaks(n=5), 
                       name="Homicide Rate per 100k Residents", 
                       na.value = "transparent") +
  labs(x=NULL, 
       y=NULL,
       title="2016 Homicide Rate by Community Statistical Area in Baltimore", # Remember to use a descriptive title
       subtitle=NULL,
       caption="Source: René Najera, DrPH & bnia.org") + # Remember to cite your sources
  #geom_text(aes(x=long, y=lat, label=total), data=homtxt, col="black", cex=5) + # Add the label of number of shootings. Will get warning about removing NAs
  theme(plot.title  =element_text(face="bold", family="Arial", size=8)) +
  theme(plot.caption = element_text(face="bold", family="Arial", size=7, color="gray", margin=margin(t=10, r=80))) +
  theme(legend.position="right") +
  theme(axis.line =  element_blank(),
        axis.text =  element_blank(),
        axis.ticks =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# Now, look at the homicide rate map
baltimore_hr

# Save the map so you can use it in a document, or edit it with a graphics editing software
ggsave("images/baltimore_homicide_rate_choropleth.png")
```
Your homicide rate choropleth should look like this:

![Choropleth Map of Homicide Rate in Baltimore](https://raw.githubusercontent.com/RFNajera/baltimore-homicide-inequality/master/images/baltimore_homicide_rate_choropleth.png "Homicide Rate Choropleth")

The next few lines of code will show you the maps side-by-side.

```r
# Now, arrange the two maps side-by-side (grid.arrange: https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html)
library(gridExtra)
grid.arrange(baltimore_poverty, baltimore_hr, nrow = 2)

#Save it to a file.
g <- arrangeGrob(baltimore_poverty, baltimore_hr, nrow=2) # Generates g
ggsave(file="images/baltimore_maps_side_by_side.png", g) # Saves g
dev.off()
```

### Question 4
**Does there seem to be an association between poverty and homicides?**

There does seem to be, at the very least, a spatial association between poverty and homicide rate in Baltimore in 2016. That is, the CSAs with higher homicide rates also seem to be the ones with the higher percent of households under the poverty level. But let's look at it in a scattergram to see this association a little more _mathematically_.

````r
# Now, a simple scatterplot. What does this tell you?
# (More on scatter plots using ggplot: http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization)
scatter_plot <- ggplot(csa_info, aes(x=poverty, y=homicide_rate)) +
  geom_point(size=2, shape=23, fill = "black") +
  geom_smooth(method=lm, se=FALSE, color = "blue", linetype = "dashed") + # Adds regression line
  geom_smooth(method = "loess", formula = "y ~ x", color = "red") + # Adds Loess line
  labs(title="Scatterplot of Percent Households Under Poverty vs. Homicide Rate",
       x="% of Households Under Poverty", y = "Homicide Rate per 100,000 Residents")

# Look at the line
scatter_plot

ggsave("images/scatterplot.png")
````

## Step Two:Looking at the Inequality in a Quantified Way

Now, let's look at the inequality. We will do this by ranking the CSAs in order of poverty, from the ones with the most households under poverty to the ones with the least. Those will be plotted on the X axis. Then, the cummulative number of homicides between 2005 and 2017 will be plotted on the Y axis. If all CSAs shared the same number of homicides, the line would be a 45-degree line from the X-Y intercept. (There are packages in R that will do this for you almost automatically, but this is good practice to see how it is done.)

First, let's process the data.

```r
# Rank CSAs in csa_info by "poverty" in descending order, poorest to wealthiest
csa_info_ranked <- csa_info[order(-csa_info$poverty),] 

# Get the cumulative number of homicides
csa_info_ranked$cum_hom <- cumsum(csa_info_ranked$total)

# Calculate the cumulative percent of homicides
csa_info_ranked$cum_hompct <- csa_info_ranked$cum_hom / sum(csa_info_ranked$total)

# Generate the valures if all things were equal, to plot the line of equality
csa_info_ranked$equality <- sum(csa_info_ranked$total) / 55 / 319 # There are 55 CSAs, divide by 100 to turn to percentage (decimal). Why the 319?
csa_info_ranked$equality_line <- cumsum(csa_info_ranked$equality)
```

Now, let's plot it using [ggplot2](https://ggplot2.tidyverse.org/).

```r
# Using ggplot() to make the concentration curve
csas <- csa_info_ranked$CSA2010
csa_info_ranked$CSA2010 <- factor(csa_info_ranked$CSA2010,levels = csas) # Need to make the CSAs into the proper levels, otherwise, they're plotted alphabetically
csa_info_ranked$group <- 1 # ggplot needs observations to be grouped :-/

# Making the curve
# More on customizing ggplot: http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels

con_curve <- ggplot(data=csa_info_ranked,
       aes(x = CSA2010,
           y = cum_hompct,
           group = 1)) +
  geom_line(color="red",
            size = 1) +
  geom_line(aes(y = equality_line),
            color = "black", 
            size = 0.5, 
            linetype = "dashed") +
  labs(title = "Concentration Curve of Homicides and Wealth in Baltimore", 
       subtitle = "Based on 2005-2017 Homicides and 2016 Poverty Data",
       x = "CSAs Ranked from Poorest to Wealthiest, Based on % of Households Below Poverty", 
       y = "Cumulative Percentage of Homicides",
       caption = "Data: Rene Najera & bnia.org") +
  scale_y_continuous(labels = scales::percent, 
                     expand = c(0, 0), 
                     limits = c(0, 1.01),
                     breaks = c(0.25, 0.5, 0.75, 0.9, 1)) +
  geom_hline(yintercept = 0.5, 
             linetype = "dashed", 
             color = "gray", 
             size = 0.25) + # Created a horizontal line to help visualize which CSAs had 50% of the homicide burden
  geom_hline(yintercept = 0.9, 
             linetype = "dashed", 
             color = "gray", 
             size = 0.25) + # Created a horizontal line to help visualize what perentage of homicides the wealthiest CSAs had
  geom_vline(xintercept = 16, 
             linetype = "dashed", 
             color = "gray", 
             size = 0.25) + # Created a vertical line to help visualize which CSAs had 50% of the homicide burden
  geom_vline(xintercept = 36, 
             linetype = "dashed", 
             color = "gray", 
             size = 0.25) + # Created a vertical line to help visualize what percentage of homicides the wealthiest CSAs had
  theme(axis.line =  element_line(color = "black"),
        axis.text.x = element_text(color = "blue", angle = 90, hjust = 1),
        axis.text.y = element_text(color = "red"),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# Look at your creation
con_curve

# Save the concentration curve so you can use it in a document, or edit it with a graphics editing software
ggsave("images/baltimore_inequality_curve.png")
```

So what does the curve tell you? Can you answer the questions below?

### Question 5
**Are homicides distributed equitably in Baltimore?**

### Question 6
**How many CSAs had 50% of the homicides in 2016?**

### Question 7
**What percent of the total homicides in 2016 did the wealthiest 20 CSAs have in 2016?**

## Final Thoughts

There is a process to figure out the Gini Coefficient (range 0 to 1). If homicides were equitably distributed, the Gini Coefficient would be 0. Clearly, the Gini Coefficient here is not 0. There are packages in R that can calculate it for you, but it helps you understand code and how this curve works if you do it on your own. You would do this by calculating the area between the dashed line and the red curve. Then you divide that area by 0.5, which is the area of the graph above (or below) the dashed line. You would then use the Gini Coefficient to track movement of the inequality line and understand equitability of a health condition based on an indicator over time. In our example, if the Gini Coefficient got smaller over time, then it would indicate that homicides are becoming more equitably distributed in Baltimore. (This is not necessarily a good thing. It could be that there are **more** homicides in wealthier neighborhoods.)

That is all for this project. Please feel free to use the code for your own needs, but do cite your data sources and give me a shout-out to let me know how you're using it.

Thank you for your time.
