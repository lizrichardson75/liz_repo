#Code to create ScotPHO's Shiny profile platform
#In this script include packages, functions, datasets and anyting that will be used both by UI and server

############################.
##Packages ----
############################.
library(shiny)
library(shinyBS) #modals
library(shinythemes) # layouts for shiny
library(dplyr) # data manipulation
library(ggplot2) #data visualization
library(DT) # for data tables
library(leaflet) #javascript maps
library(plotly) #interactive graphs
library(shinyWidgets) # for extra widgets
library(tidyr) #for string maniupulations in ring plot
library(shinyjs)
library(shinydashboard) #for valuebox on techdoc tab
library(openxlsx)
library(orca)
library(processx)
library(webshot)
webshot::install_phantomjs()

###############################################.
## Functions ----
###############################################.  


#Function to create plot when no data available
plot_nodata <- function() {
  text_na <- list(x = 5, y = 5, text = "No data available: try treating a smaller number of individuals or a different targeting strategy" ,
                  xref = "x", yref = "y",  showarrow = FALSE)
  
plot_ly() %>%
    layout(annotations = text_na,
           #empty layout
           yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
           xaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
           font = list(family = '"Hind Siliguri", sans-serif')) %>% 
    config( displayModeBar = FALSE) # taking out plotly logo and collaborate button
}

###############################################.
## Data ----
###############################################.    
Sys.setlocale('LC_ALL', 'C')
#setwd("C:/Users/elizabethr/Desktop/Triple I/nhs-triple-i_published260419/nhs-triple-i - June19edits/")
iiidata <- data.frame(readRDS("./data/big/iii_correct_poundsign.RDS"))

#NEW June '19:
iiidata$level[iiidata$type=="Undo"|
                iiidata$intervention=="Tobacco tax +10%"] <- "National"
iiidata$level[iiidata$type=="Mitigate"|
                iiidata$intervention=="Job provision"|
                iiidata$intervention=="Benefit uptake +1%"|
                iiidata$intervention=="20 mph limits"|
                iiidata$intervention=="Council Tax increase (bands E-H)"] <- "Local"
#remove some interventions for quadrant:
remove <- c("Pedom", "GroupPA", "itup1p", "paup1k", "Employ")
interventions_lookup <- read.csv("./data/ints_lookup_poundsign.csv", stringsAsFactors = FALSE
                                 , encoding="UTF-8"
                                 )
interventions_lookup <- interventions_lookup[order(interventions_lookup$intervention),]
areas_lookup <- readRDS("./data/areas_lookup.RDS")

## area data 
area_list <- sort(areas_lookup$inputarea)
hb_names <- as.character(sort(areas_lookup$inputarea[areas_lookup$areatype=="Health board"])) 
la_names <- as.character(sort(areas_lookup$inputarea[areas_lookup$areatype=="Council area"])) 
ijb_names <- as.character(sort(areas_lookup$inputarea[areas_lookup$areatype=="IJB"])) 
deal_names <- as.character(sort(areas_lookup$inputarea[areas_lookup$areatype=="City region"])) 

#Intervention names
intervention_list <- sort(interventions_lookup$intervention)
popint_list <- as.character(sort(interventions_lookup$intervention[interventions_lookup$focus=="population" & interventions_lookup$include==1])) #select universal interventions
indivint_list <- as.character(sort(interventions_lookup$intervention[interventions_lookup$focus=="individual" & interventions_lookup$include==1])) #select individual-level interventions

###############################################.
## Palettes ----
###############################################.  

#colours
colourpicker21 <- c("darkorange", "red", "coral" , "deepskyblue3", "green2", "mediumvioletred" ,
                    "cornflowerblue", "olivedrab3", "darkred" ,"darkcyan" ,"powderblue", 
                    "maroon1", "firebrick3", "blueviolet" , "darkgrey", "dark orchid", "blue2" ,
                    "brown", "gold", "darkmagenta" ,"goldenrod4" ,"gray38", "darkgreen")             

#symbols
symbols21 <- c("circle" , "square" , "diamond" , "cross" , "x" , "triangle-up" , "triangle-down" , 
                "pentagon" , "hexagon", "star" ,"hexagram" , "star-triangle-up" , 
                "star-triangle-down" , "star-square" , "star-diamond" , "diamond-tall" ,
               "diamond-wide" , "hourglass" ,"circle" , "square" , "diamond", "cross" , "x" )

int_colours <- c(setNames(colourpicker21, unique(iiidata$intervention)))
int_symbols <- c(setNames(symbols21, unique(iiidata$intervention)))

##END
