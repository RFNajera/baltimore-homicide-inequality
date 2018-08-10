# Homicide Concentration Curve in Baltimore
# Rene F. Najera, DrPH

# Set the working directory
setwd("~/Dropbox/RProjects/baltimore-homicide-inequality") #This may need to be changed if working on another machine

# Install Packages as necessary
# Libraries as necessary

#######################################################################    PART ONE    ########################################################################
# In this first part, we will look at homicides in Baltimore in 2016 and see if poverty is in any way associated with                                         #
# the number of homicides seen per Community Statistical Area. (Read more about CSAs: http://www.prattlibrary.org/research/tools/index.aspx?cat=104&id=4909)  #
# In the second part, we'll visualize how homicides in Baltimore are not equitably distributed.                                                               #
###############################################################################################################################################################

# Read the homicides between 2005 and 2017 into a dataframe. Since we're using 2016 CSA data, we'll only use 2016 homicides.
# The data we will use was obtained from The Baltimore Sun and cleaned up by the author for work in his doctoral (DrPH) dissertation.
homicides_all <- read.csv("data/baltimore_homicides_2005_2017.csv")

#Subset only the homicides in 2016
library(tidyr)
library(dplyr)
homicides_2016 <- homicides_all %>% filter(year==2016)

# Save this subset as a .csv file if you want to use it later
write.csv(homicides_2016, file = "data/baltimore_homicides_2016.csv")

#################################################################
# Pop quiz #1: How many total homicides were reported in 2016?  #
#################################################################

# Bring in shapefile with population data obtained from the
# Baltimore Neighborhood Indicators Alliance at https://bniajfi.org/vital_signs/data_downloads/
library(rgdal)
ogrInfo(dsn="data/bnia_16", 
        layer="bnia_16") # see shapefile info
csa_baltimore <- readOGR("data/bnia_16",
                         "bnia_16") # read the shapefile into R
csa_baltimore <- spTransform(csa_baltimore, 
                             CRS("+init=epsg:4326")) # transform coord system: More projections information http://trac.osgeo.org/proj/wiki/GenParms 

# Tranform the shapefile into a dataframe, keep the CSA name information
library(ggplot2)
csa_baltimore <- fortify(csa_baltimore, 
                         region = "CSA2010") # "id" is the name of the CSA column

# Summarize number of homicides by CSA and place that into a total column
homicides_csa <- homicides_2016 %>%
  group_by(CSA2010) %>%
  summarise(total=n()) # "total" will be the total number of shootings per CSA.

#################################################################
# Pop quiz #2: Which CSA had the highest number of homicides?   #
#################################################################

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

#######################################################################
# Pop quiz #3: Which CSA had the highest rate of homicides in 2016?   #
#######################################################################

# Let's make a choropleth map of poverty first. (Learn more about choropleth maps: https://datavizcatalogue.com/methods/choropleth.html)

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

# Now, arrange the two maps side-by-side (grid.arrange: https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html)
library(gridExtra)
grid.arrange(baltimore_poverty, baltimore_hr, nrow = 2)

#Save it to a file.
g <- arrangeGrob(baltimore_poverty, baltimore_hr, nrow=2) # Generates g
ggsave(file="images/baltimore_maps_side_by_side.png", g) # Saves g
dev.off()

######################################################################################
# Pop quiz #4: Does there seem to be an association between poverty and homicides?   #
######################################################################################

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

##############################################################    PART TWO    #########################################################################
# Now, let's look at the inequality. We will do this by ranking the CSAs in order of poverty, from the ones with the most households under poverty... #
# to the ones with the least. Those will be plotted on the X axis.                                                                                    #
# Then, the cummulative number of homicides between 2005 and 2017 will be plotted on the Y axis.                                                      #
# If all CSAs shared the same number of homicides, the line would be a 45-degree line from the X-Y intercept.                                         #
# (There are packages in R that will do this for you almost automatically, but this is good practice to see how it is done.)                          #
#######################################################################################################################################################

# Rank CSAs in csa_info by "poverty" in descending order, poorest to wealthiest
csa_info_ranked <- csa_info[order(-csa_info$poverty),] 

# Get the cumulative number of homicides
csa_info_ranked$cum_hom <- cumsum(csa_info_ranked$total)

# Calculate the cumulative percent of homicides
csa_info_ranked$cum_hompct <- csa_info_ranked$cum_hom / sum(csa_info_ranked$total)

# Generate the valures if all things were equal, to plot the line of equality
csa_info_ranked$equality <- sum(csa_info_ranked$total) / 55 / 319 # There are 55 CSAs, divide by 100 to turn to percentage (decimal). Why the 319?
csa_info_ranked$equality_line <- cumsum(csa_info_ranked$equality)

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

#################################################################
# Pop quiz #5: Are homicides distributed equitably in Baltimore?#
#################################################################


#################################################################
# Pop quiz #6: How many CSAs had 50% of the homicides in 2016?  #
#################################################################


#################################################################
# Pop quiz #7: What percent of the total homicides in 2016 did  #
# the wealthiest 20 CSAs have in 2016?                          #
#################################################################

#############################################################################################
# Final Thoughts: There is a process to figure out the Gini Coefficient (range 0 to 1).     #
# If homicides were equitably distributed, the Gini Coefficient would be 0. Clearly,        #
# the Gini Coefficient here is not 0. There are packages in R that can calculate it for you,#
# but it helps you understand code and how this curve works if you do it on your own. You   #
# would do this by calculating the area between the dashed line and the red curve. Then you #
# divide that area by 0.5, which is the area of the graph above (or below) the dashed line. #
# Maybe you can do it in R when you master it?                                              #
#############################################################################################

# That's it! You are free to use this code to look at other inequalities.
# Just remember to give proper attribution to your sources, especially the data sources.