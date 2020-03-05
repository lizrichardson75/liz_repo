#Code to create Triple I results browser

###############################################.

## Define a server for the Shiny app
function(input, output, session) {
###############################################.
## Landing page ----
###############################################.
# Creating events that take you to different tabs
# activated when pressing buttons from the landing page
observeEvent(input$jump_to_trend, {
  updateTabsetPanel(session, "intabset", selected = "trend")
})

observeEvent(input$jump_to_rank, {
  updateTabsetPanel(session, "intabset", selected = "costtable")
})

observeEvent(input$jump_to_table, {
  updateTabsetPanel(session, "intabset", selected = "table")
})

observeEvent(input$jump_to_about, {
  updateTabsetPanel(session, "intabset", selected = "about")
})

observeEvent(input$jump_to_tour, {
  updateTabsetPanel(session, "intabset", selected = "tour")
})

###############################################.        
###############################################.  
#### Time trend plot ----
###############################################.  

## Help pop-up modal dialog 
observeEvent(input$help_trend, {
  showModal(modalDialog(
    title = "How to use the 'Compare effects' page",
    fluidRow(h4(tags$b("This 'quadrant' chart compares how interventions are predicted to affect health 
                       (on the horizontal axis) and health inequalities (on the vertical axis)."))),
    fluidRow(h5(tags$b(tags$u("Choosing the data to show")))),
    fluidRow(h6("Initially you'll see a quadrant chart based on the default options.", style = "color:black; font-weight: bold;")),
    fluidRow(h6("You can change these using the dropdown menus in the grey panel to select a different area, health outcome, follow-up period, and intervention type.", style = "color:black; font-weight: bold;")),
    fluidRow(
      h6("Health outcomes:", style = "color:black; font-weight: bold;"),
      tags$ul( 
        tags$li(class= "li-custom", tags$b("Premature deaths:"),
                "deaths before the age of 75."),
        tags$li(class= "li-custom", tags$b("Years of life lost:"), " an estimate of the years a person would have lived if they had not died prematurely (based on life expectancy)."),
        tags$li(class= "li-custom", tags$b("Hospital stays:"), " continuous inpatient stays in general or psychiatric hospitals."))),
    fluidRow(
      h6("Years of follow up:", style = "color:black; font-weight: bold;"),
      tags$ul( 
        tags$li(class= "li-custom", "All interventions start in year 1."),
        tags$li(class= "li-custom", "Income-based, 20 mph limits, and tobacco taxation interventions are permanent."),
        tags$li(class= "li-custom", "All other interventions are implemented in year 1 only."),
        tags$li(class= "li-custom", "Follow up begins in year 2."),
        tags$li(class= "li-custom", "Outcomes are calculated cumulatively over the period.")
      )),
    fluidRow(
      h6("Intervention type:", style = "color:black; font-weight: bold;"),
      tags$ul( 
        tags$li(class= "li-custom", tags$b("All:"), " All interventions except 5 that have been omitted for clarity of presentation for the online chart: Job provision, Pedometer-based interventions, Group physical activity (for depression), Income Tax + 1p, and Personal Allowance +\u00A31,000."),
        tags$li(class= "li-custom", tags$b("National implementation:"), " These interventions could be implemented by UK or Scottish governments."),
        tags$li(class= "li-custom", tags$b("Local implementation:"), " These interventions could be implemented at the local level."),
        tags$li(class= "li-custom", tags$b("Undo:"),
                " These interventions 'Undo' the fundamental causes of health inequality (e.g., income inequality)."),
        tags$li(class= "li-custom", tags$b("Prevent:"), " These interventions 'Prevent' harm from wider environmental influences (e.g., unemployment)."),
        tags$li(class= "li-custom", tags$b("Mitigate:"), " These interventions 'Mitigate' against the negative impact of wider determinants on individuals' health (e.g., smoking)."),
        tags$li(class= "li-custom", "Definitions of the interventions can be found using the 'Intervention definitions' button.", 
                img(src='definitionsbutton.png', height=40, style="padding-left: 2px; vertical-align:middle;"))
      )),
    fluidRow(
      h6("And for interventions focused on individuals (e.g., smoking cessation or weight management services) you can also change the intervention targeting strategy and the number treated.", style = "color:black; font-weight: bold;"),  
      h6(tags$b("Targeting strategy:", style = "color:black; font-weight: bold;")),
      tags$ul( 
        tags$li(class= "li-custom", tags$b("Even distribution:"),
                "Treat the same proportion of each population subgroup (by age group, sex and deprivation)."),
        tags$li(class= "li-custom", tags$b("Proportionate to need:"), " The proportion of each subgroup treated is proportionate to the risk factor prevalence."),
        tags$li(class= "li-custom", tags$b("Most deprived 20% only:"), " Treat only those in Scottish Index of Multiple Deprivation (SIMD) quintile 1 (even distribution within this quintile)."),
        tags$li(class= "li-custom", tags$b("Most deprived 40% only:"), " Treat only those in SIMD quintiles 1 and 2 (even distribution within this quintile).")
      )),
    fluidRow(
      h6("Number to treat:", style = "color:black; font-weight: bold;"),
      tags$ul( 
        tags$li(class= "li-custom", tags$b("Number:"), " Select either 100, 1,000 or 100,000. ", 
                "N.B. For individual-level interventions with insufficient eligible population in the chosen area to treat this number no data will be plotted.  The eligible population is defined as the maximum number of people who could be expected to benefit from the intervention (e.g., smokers who displayed motivation to quit, or unemployed people willing and able to take up a job)."),
        tags$li(class= "li-custom", tags$b("Percentage:"),"Select a percentage of the 'eligible' population.",
                "  Hovering the mouse over a data point for an individual-level intervention will show how many interventions were delivered for this percentage.")
      )),
    br(),
    fluidRow(
      h5(tags$b(tags$u("Interpreting and navigating the chart")))),  
    fluidRow(  
      tags$ul(
        tags$li(class= "li-custom", h6("In the quadrant chart health inequalities are measured using the Relative Index of Inequality, or RII.  
                                       The RII is a summary measure of inequality which takes into account differences across the whole deprivation gradient, 
                                       not just the gap in health outcome between the most and least deprived.", style = "color:black; font-weight: bold;")),
        tags$li(class= "li-custom", h6("Interpretation is based on which 'quadrant' the intervention appears in:", style = "color:black; font-weight: bold;")))),
    fluidRow(
      img(src='quadrant_interp.png', border="1px", color="black", height=250, style="padding-right: 2px; vertical-align:middle;")
    ),
    #    br(),
    fluidRow(
      tags$ul(
        tags$li(class= "li-custom", h6("Clicking on a legend entry will hide that intervention (and the intervention will be greyed out from the legend).  Double-clicking will show only that intervention.", style = "color:black; font-weight: bold;"))
      )),
    fluidRow(
      img(src='quadrant_legend.png', height=220, style="padding-left: 2px; vertical-align:middle;")
    ),
    #    br(),
    fluidRow(
      tags$ul(
        tags$li(class= "li-custom", h6("You can also zoom in to part of the graph by clicking and dragging to select a rectangular area.", style = "color:black; font-weight: bold;"))
      )),
    fluidRow(
      img(src='clickdrag.png', height=180, style="padding-left: 2px; vertical-align:middle;")
    ),
    fluidRow(
      tags$ul(
        tags$li(class= "li-custom", h6("Hovering over a data point in the chart will reveal the underlying data.", style = "color:black; font-weight: bold;"))
      )),
    fluidRow(
      img(src='hover.png', height=100, style="padding-left: 2px; vertical-align:middle;")
    ),
    br(),
    fluidRow(h5(tags$b(tags$u("Downloading the chart and data")))),  
    fluidRow(
      tags$ul(
        tags$li(class= "li-custom", h6("You can download the chart and the chart's data using the 'Download data' button.  The file produced is an Excel spreadsheet, and includes background information about the project and the data.  The cost-effectiveness results for the scenario of interest are also included.", 
                                       style = "color:black; font-weight: bold;", img(src='downloadbutton1.png', height=60, border="1px", color="black", style="padding-right: 2px; vertical-align:middle;")))
      )),
    easyClose = TRUE, fade=FALSE
      ))
})


###############################################.
# Intervention definitions
output$defs_text_trend <- renderUI({
  lookup <- interventions_lookup
  HTML(paste(sprintf("<b><u>%s ('%s' type)</b></u> <br> %s ", 
                     lookup$intervention[lookup$include==1],
                     lookup$type2[lookup$include==1],
                     lookup$definition[lookup$include==1]
  ), collapse = "<br><br>"))
})

#####################.
# Reactive data 

# Quadrant plot data. Filtering based on user input values.
trend_data <- reactive({
  treatoptionslist <- sort(unique(iiidata$treatoptions))
  index <- match(input$area, areas_lookup$inputarea)
  equivarea <- areas_lookup[na.omit(index), ]
  eq <- as.character(equivarea$areacode)
  if (input$inttype=="all") { 
    iiidata <- iiidata[-which(iiidata$intlabel %in% remove), ]
    quadrantdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & ((stratname == input$stratname & 
                   treatoptions == treatoptionslist[as.integer(input$treatoptions)])
                | (stratname == "Population-wide")))
  } else if (input$inttype=="Undo") {
    quadrantdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & type == "Undo")
  } else if (input$inttype=="Prevent") {
    quadrantdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & (intlabel=="mph20"|intlabel=="TobTax"|intlabel=="uptake1pc"
                |(intlabel=="Employ" & stratname==input$stratname 
                  & treatoptions==treatoptionslist[as.integer(input$treatoptions)]))) 
  } else if (input$inttype=="Mitigate") {
    quadrantdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & type =="Mitigate"
             & stratname == input$stratname 
             & treatoptions == treatoptionslist[as.integer(input$treatoptions)])
  } else if (input$inttype=="National") {
    quadrantdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & level =="National")
  } else {
    quadrantdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & level =="Local"
             & (intlabel=="mph20"|intlabel=="ctxinc"|intlabel=="uptake1pc"
                |((type =="Mitigate"|intlabel=="Employ") & stratname == input$stratname 
                  & treatoptions == treatoptionslist[as.integer(input$treatoptions)])))
  }
})

#####################.
# Creating plot

#####################.
# titles 

output$title_trend <- renderText(
  sprintf("Comparing intervention effects on %s%s for %s", 
          tolower(substr(input$outcomename, 1, 1)), 
          substr(input$outcomename, 2, nchar(input$outcomename)),
          input$area)
)

gettickformat <- function(type){
  ifelse(type=="Mitigate",".2f","")
}

output$trend_plot <- renderPlotly({
  outcome<-as.character(trend_data()$outcome) 
  
  #If no data available for that period then plot message saying data is missing
  if (is.data.frame(trend_data()) && nrow(trend_data()) == 0)
  {
    plot_nodata()
  } else { 
    #If data is available then plot it
    #      trend_data()$ntreated[is.na(trend_data()$ntreated)] <- "Population"
    tooltip_trend <- ifelse(trend_data()$outcome=="Hospital stays",
                            c(paste0("Intervention: ", trend_data()$intervention,
                                     "<br>",
                                     "Number treated: ",
                                     ifelse(is.na(trend_data()$ntreated), "Population",
                                            format(trend_data()$ntreated, format = "%12.0f", scientific=FALSE, big.mark = ",", digits=0, nsmall=0)),
                                     "<br>",
                                     "Reduction in ",
                                     paste0(tolower(substr(outcome, 1, 1)),
                                            substr(outcome, 2, nchar(outcome))), ": ",
                                     sprintf("%0.0f", trend_data()$total_diff), " (",
                                     sprintf("%0.3f%%", trend_data()$total_diffpc), ")",
                                     "<br>",
                                     "Widening of inequalities: ",
                                     sprintf("%0.3f",trend_data()$rii_diff), " (",
                                     sprintf("%0.3f%%",trend_data()$rii_diffpc), ")",
                                     "<br>",
                                     "Savings on hospital stays: B#",
                                     format(trend_data()$total_diff*2512, format = "%12.0f", scientific=FALSE, big.mark = ",", digits=0, nsmall=0))),
                            c(paste0("Intervention: ", trend_data()$intervention,
                                     "<br>",
                                     "Number treated: ",                                       
                                     ifelse(is.na(trend_data()$ntreated), "Population",
                                            format(trend_data()$ntreated, format = "%12.0f", scientific=FALSE, big.mark = ",", digits=0, nsmall=0)),
                                     "<br>",
                                     "Reduction in ",
                                     paste0(tolower(substr(outcome, 1, 1)),
                                            substr(outcome, 2, nchar(outcome))), ": ",
                                     sprintf("%0.0f", trend_data()$total_diff), " (",
                                     sprintf("%0.3f%%", trend_data()$total_diffpc), ")",
                                     "<br>",
                                     "Widening of inequalities: ",
                                     sprintf("%0.3f",trend_data()$rii_diff), " (",
                                     sprintf("%0.3f%%",trend_data()$rii_diffpc), ")"
                            )))
    
    zoom <- event_data("plotly_relayout", "source")
    # if plot just rendered, event_data is NULL
    # if user double clicks for autozoom, then zoom$xaxis.autorange is TRUE
    # if user resizes page, then zoom$width is pixels of plot width
    if(is.null(zoom) || names(zoom[1]) %in% c("xaxis.autorange", "width")) {
      xmin <- min(trend_data()$total_diffpc)
      xmax <- max(trend_data()$total_diffpc)
      ymin <- min(trend_data()$rii_diffpc)
      ymax <- max(trend_data()$rii_diffpc)
      
    } else {
      xmin <- zoom$`xaxis.range[0]`
      xmax <- zoom$`xaxis.range[1]`
      ymin <- zoom$`yaxis.range[0]`
      ymax <- zoom$`yaxis.range[1]`
    }
    if ((min(trend_data()$rii_diffpc)*min(trend_data()$rii_diffpc))<(max(trend_data()$rii_diffpc)*max(trend_data()$rii_diffpc))) {
      if ((min(trend_data()$total_diffpc)*min(trend_data()$total_diffpc))>(max(trend_data()$total_diffpc)*max(trend_data()$total_diffpc))) {
        text=sprintf("Wider inequalities <br> + <br> worse health")
        annot_y=ymax/2
        annot_x=xmin/2
      } else {
        text=sprintf("Wider inequalities <br> + <br> improved health")
        annot_y=ymax/2
        annot_x=xmax/2
      }} else {
        if ((min(trend_data()$total_diffpc)*min(trend_data()$total_diffpc))>(max(trend_data()$total_diffpc)*max(trend_data()$total_diffpc))) {
          text=sprintf("Narrower inequalities <br> + <br> worse health")
          annot_y=ymin/2
          annot_x=xmin/2
        } else {
          text=sprintf("Narrower inequalities <br> + <br> improved health")
          annot_y=ymin/2
          annot_x=xmax/2
        }}
    
    trend_plot <- plot_ly(data = trend_data(), x = ~total_diffpc,
                          y = ~rii_diffpc,
                          text=tooltip_trend, hoverinfo="text", 
                          type = 'scatter', 
                          mode = 'markers',
                          symbol = ~as.character(intervention),
#                          symbols = symbols21,
                          symbols = int_symbols,
                          marker = list(size = 10),
                          color = ~as.character(intervention),
#                          colors = colourpicker21,
                          colors = int_colours,
                          height = "100%") %>%
      add_annotations(yref='y',xref="x", y=annot_y, x=annot_x, showarrow = FALSE,
                      text = text,
                      font = list(size = 12)) %>%      
      #Layout 
      layout(
        autosize = T, margin = list(b = 70), #to avoid labels getting cut out
        yaxis = list(title = "% widening of health inequalities", rangemode="tozero",  
                     size = 4, titlefont =list(size=14), 
                     tickformat=gettickformat(input$inttype), 
                     tickfont =list(size=14), fixedrange=FALSE, 
                     zeroline = T, zerolinewidth = 2), 
        xaxis = list(title = ifelse(input$outcomename=="Years of life lost",
                                    (paste0("% years of life saved")), 
                                    (sprintf("%s %s%s prevented", as.character("%"),
                                             tolower(substr(input$outcomename, 1, 1)), 
                                             substr(input$outcomename, 2, nchar(input$outcomename))))),
                     tickfont =list(size=14), tickformat=gettickformat(input$inttype), tickangle = 0, fixedrange=FALSE, zeroline = T, zerolinewidth = 2),
        shapes = list(
          if (ymin < 0 ) {
            list(type = "rect",
                 fillcolor = "green", line = list(color = "green"), opacity = 0.1,
                 x0 = 0, x1 = xmax*1.1, xref = "x",
                 y0 = ymin*1.1, y1 = 0, yref = "y")
          } else {
            list(type = "rect",
                 x0 = 0, x1 = 0, xref = "x",
                 y0 = 0, y1 = 0, yref = "y")
          }
        ),
        legend = list(font=list(size=11), bgcolor='#FFFFFF')
      ) %>%  
      config(displayModeBar = FALSE, displaylogo = F, collaborate=F, showTips=F,  doubleClick=F, editable =F) # taking out plotly logo and collaborate button
  }
})


#####################################.    
#### Excel download for quadrant chart page ----
####################################.
#Function to be able to save chart:
plotly_quadrant_plot <- function(){
  outcome<-as.character(data_quadrant_plot()$outcome)
    #If no data available for that period then plot message saying data is missing
    if (is.data.frame(data_quadrant_plot()) && nrow(data_quadrant_plot()) == 0)
    {
      plot_nodata()
    }
    else { #If data is available then plot it
      zoom <- event_data("plotly_relayout", "source")
      # if plot just rendered, event_data is NULL
      # if user double clicks for autozoom, then zoom$xaxis.autorange is TRUE
      # if user resizes page, then zoom$width is pixels of plot width
      if(is.null(zoom) || names(zoom[1]) %in% c("xaxis.autorange", "width")) {
        xmin <- min(data_quadrant_plot()$total_diffpc)
        xmax <- max(data_quadrant_plot()$total_diffpc)
        ymin <- min(data_quadrant_plot()$rii_diffpc)
        ymax <- max(data_quadrant_plot()$rii_diffpc)
      } else {
        xmin <- zoom$`xaxis.range[0]`
        xmax <- zoom$`xaxis.range[1]`
        ymin <- zoom$`yaxis.range[0]`
        ymax <- zoom$`yaxis.range[1]`
      }
      if ((min(data_quadrant_plot()$rii_diffpc)*min(data_quadrant_plot()$rii_diffpc))<(max(data_quadrant_plot()$rii_diffpc)*max(data_quadrant_plot()$rii_diffpc))) {
          if ((min(data_quadrant_plot()$total_diffpc)*min(data_quadrant_plot()$total_diffpc))>(max(data_quadrant_plot()$total_diffpc)*max(data_quadrant_plot()$total_diffpc))) {
          text=sprintf("Wider inequalities <br> + <br> worse health")
          annot_y=ymax/2
          annot_x=xmin/2
        } else {
          text=sprintf("Wider inequalities <br> + <br> improved health")
          annot_y=ymax/2
          annot_x=xmax/2
        }} else {
          if ((min(data_quadrant_plot()$total_diffpc)*min(data_quadrant_plot()$total_diffpc))>(max(data_quadrant_plot()$total_diffpc)*max(data_quadrant_plot()$total_diffpc))) {
            text=sprintf("Narrower inequalities <br> + <br> worse health")
            annot_y=ymin/2
            annot_x=xmin/2
          } else {
            text=sprintf("Narrower inequalities <br> + <br> improved health")
            annot_y=ymin/2
            annot_x=xmax/2
          }}
      trend_plot <- plot_ly(data = data_quadrant_plot(), x = ~total_diffpc,
                            y = ~rii_diffpc,
                            type = 'scatter',
                            mode = 'markers',
                            symbol = ~as.character(intervention),
                            symbols = int_symbols,
                            marker = list(size = 16),
                            color = ~as.character(intervention),
                            colors = int_colours) %>%
        add_annotations(yref='y',xref="x", y=annot_y, x=annot_x, showarrow = FALSE,
                        text = text,
                        font = list(size = 20)) %>%
        #Layout
        layout(
          autosize = T,
          yaxis = list(title = "% widening of health inequalities", rangemode="tozero",
                       size = 4, titlefont =list(size=20), fixedrange=FALSE,
                       tickfont =list(size=20), tickformat=gettickformat(input$inttype), zeroline = T, zerolinewidth = 2
                       ),


          xaxis = list(title = ifelse(input$outcomename=="Years of life lost",
                                      (paste0("% years of life saved")),
                                      (sprintf("%s %s%s prevented", as.character("%"),
                                               tolower(substr(input$outcomename, 1, 1)),
                                               substr(input$outcomename, 2, nchar(input$outcomename))))
          ),
          tickfont =list(size=20), tickangle = 0, tickformat=gettickformat(input$inttype), fixedrange=FALSE, zeroline = T, zerolinewidth = 2),
          shapes = list(
            if (ymin < 0 ) {
              list(type = "rect",
                   fillcolor = "green", line = list(color = "green"), opacity = 0.1,
                   x0 = 0, x1 = xmax*1.1, xref = "x",
                   y0 = ymin*1.1, y1 = 0, yref = "y")
            } else {
              list(type = "rect",
                   x0 = 0, x1 = 0, xref = "x",
                   y0 = 0, y1 = 0, yref = "y")
            }
          ),
          legend = list(font=list(size=16), bgcolor='#FFFFFF')
        ) %>%
        config(displayModeBar = FALSE, displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
    }
  }

title_quadrant_plot <- function(){
  sprintf("Comparing intervention effects on %s%s for %s",
          tolower(substr(input$outcomename, 1, 1)),
          substr(input$outcomename, 2, nchar(input$outcomename)),
          input$area)
}

title_cost_data <- function(){
  sprintf("Cost-effectiveness data for intervention effects on %s%s for %s",
          tolower(substr(input$outcomename, 1, 1)),
          substr(input$outcomename, 2, nchar(input$outcomename)),
          input$area)
}

# Cost table data. Filtering based on user input values.
get_costtable_data <- reactive({
  treatoptionslist <- sort(unique(iiidata$treatoptions))
  index <- match(input$area, areas_lookup$inputarea)
  equivarea <- areas_lookup[na.omit(index), ]
  eq <- as.character(equivarea$areacode)
  if (input$inttype=="all") { 
    iiidata <- iiidata[-which(iiidata$intlabel %in% remove), ]
    costdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & ((stratname == input$stratname & 
                   treatoptions == treatoptionslist[as.integer(input$treatoptions)])
                | (stratname == "Population-wide")))
  } else if (input$inttype=="Undo") {
    costdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & type == "Undo")
  } else if (input$inttype=="Prevent") {
    costdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & (intlabel=="mph20"|intlabel=="TobTax"|intlabel=="uptake1pc"
                |(intlabel=="Employ" & stratname==input$stratname 
                  & treatoptions==treatoptionslist[as.integer(input$treatoptions)]))) 
  } else if (input$inttype=="Mitigate") {
    costdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & type =="Mitigate"
             & stratname == input$stratname 
             & treatoptions == treatoptionslist[as.integer(input$treatoptions)])
  } else if (input$inttype=="National") {
    costdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & level =="National")
  } else {
    costdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & level =="Local"
             & (intlabel=="mph20"|intlabel=="ctxinc"|intlabel=="uptake1pc"
                |((type =="Mitigate"|intlabel=="Employ") & stratname == input$stratname 
                  & treatoptions == treatoptionslist[as.integer(input$treatoptions)])))
  }
  costdata$stratname <- as.character(costdata$stratname)
  costdata$rii_diffpc <- costdata$rii_diffpc*-1
  costdata$costperoutcomepc[costdata$total_diffpc< 0] <- NA
  costdata$costperriipc <- costdata$costperriipc*-1
  costdata$costperriipc[costdata$rii_diffpc< 0] <- NA
  costdata$ntreated[is.na(costdata$ntreated)] <- "Population"
  costdata <- costdata %>% arrange(type, intervention)
  table <- costdata %>% subset(select=c("intervention", "type", "outcome", "year", "interventioncost", "stratname", "ntreated",
                                        "total_diffpc","costperoutcomepc",
                                        "rii_diffpc", "costperriipc")) %>% arrange(intervention)

  
  })

# Quadrant plot data. Filtering based on user input values.
data_quadrant_plot <- reactive({
  treatoptionslist <- sort(unique(iiidata$treatoptions))
  index <- match(input$area, areas_lookup$inputarea)
  equivarea <- areas_lookup[na.omit(index), ]
  eq <- as.character(equivarea$areacode)
  if (input$inttype=="all") { 
    quadrantdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & ((stratname == input$stratname & 
                   treatoptions == treatoptionslist[as.integer(input$treatoptions)])
                | (stratname == "Population-wide")))
  } else if (input$inttype=="Undo") {
    quadrantdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & type == "Undo")
  } else if (input$inttype=="Prevent") {
    quadrantdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & (intlabel=="mph20"|intlabel=="TobTax"|intlabel=="uptake1pc"
                |(intlabel=="Employ" & stratname==input$stratname 
                  & treatoptions==treatoptionslist[as.integer(input$treatoptions)]))) 
  } else if (input$inttype=="Mitigate") {
    quadrantdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & type =="Mitigate"
             & stratname == input$stratname 
             & treatoptions == treatoptionslist[as.integer(input$treatoptions)])
  } else if (input$inttype=="National") {
    quadrantdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & level =="National")
  } else {
    quadrantdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename
             & year == input$year
             & level =="Local"
             & (intlabel=="mph20"|intlabel=="ctxinc"|intlabel=="uptake1pc"
                |((type =="Mitigate"|intlabel=="Employ") & stratname == input$stratname 
                  & treatoptions == treatoptionslist[as.integer(input$treatoptions)])))
  }
  quadrantdata <- data.frame(quadrantdata %>%
                          subset(select=c("intervention", "type", "stratname", "outcome", "ntreated", 
                                          "year", "total_diff", "total_diffpc", "rii_diff", "rii_diffpc")))
  quadrantdata <- quadrantdata %>% arrange(type, intervention)
})

intervention_list <- function(){
  lookup <- interventions_lookup[interventions_lookup$include==1, c("intervention", "definition")]
}

require(openxlsx)

#this feeds into the button in the tool used for the download
output$triple_i_effects_data.xlsx <- downloadHandler(
filename <- paste0("triple_i_effects_data.xlsx", sep = ""),
  content = function(file) {
    #create a new workbook
    wb <- createWorkbook()
    #Set styles
    general_style <- createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL)
    general_style_wrap <- createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL,  wrapText = TRUE)
    heading1_style <- createStyle(fontName = "Arial", fontSize = 16, fontColour = NULL, fgFill = NULL, halign = NULL, valign = NULL, textDecoration = "bold")
    heading2_style <- createStyle(fontName = "Arial", fontSize = 14, fontColour = "#2E77BB", textDecoration = "bold")
    heading2_shade_style <- createStyle(fontName = "Arial", fontSize = 14, fontColour = "#2E77BB", textDecoration = "bold", fgFill = "#D3D3D3")
    heading3_style <-createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL, textDecoration = "bold", fgFill = "#D3D3D3", border="TopBottom")
    heading3_style_wrap <-createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL, textDecoration = "bold", fgFill = "#D3D3D3", border="TopBottom", wrapText = TRUE)
    heading3_noshade_style <-createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL, textDecoration = "bold")
    heading4_style <-createStyle(fontName = "Arial", fontSize = 11, fontColour = NULL)
    border_style <- createStyle(border= c("top", "bottom", "left", "right") )
    integers <- createStyle(numFmt = "#,##0")
    dp2 <- createStyle(numFmt = "0.00")
    dp3 <- createStyle(numFmt = "0.000")
    
    #adding a new worksheet
    addWorksheet(wb, "Background information", gridLines = FALSE)
    #populating the worksheet
    #adding a logo
    insertImage(wb, "Background information", "www/HSlogo.jpg", width = 1.2, height = 0.7, startRow = 1, startCol = 2, units = "in", dpi = 300)
    #writing as title
    writeData(wb, "Background information", "Informing Interventions to reduce health Inequalities (Triple I) data download spreadsheet", startCol = 2, startRow = 5, xy = NULL)
df <- c("What is Triple I?",
        "NHS Health Scotland's Informing Interventions to reduce health Inequalities (Triple I) project is a modelling study that brings together the best available evidence to estimate how different interventions might affect health and health inequalities over the next 20 years.  This online tool allows users to browse, visualise, and download Triple I results for their area of interest.",
        "",
        "Why Triple I?",
        "There are many separate pieces of evidence about how specific interventions affect the health of individuals, but relatively little evidence about how these interventions might affect health and health inequalities at the population level.  Triple I is part of NHS Health Scotland's approach to addressing these population-level evidence gaps.",
        "",
        "Who is Triple I for?",
        "Triple I provides national and local decision makers with interactive tools and interpreted findings to inform discussions and decisions about different interventions.  The study will also be of use to anyone with an interest in improving population health and reducing health inequalities.",
        "",
        "How should I use the Triple I results?",
        "The information in this spreadsheet should be interpreted in the context of the intervention definitions and the technical information about the modelling.  Triple I should be considered as one source of information that complements other decision-making resources.",
        "",
        "This spreadsheet contains these tabs:",
        "",
        "",
        "",
        "",
        "",
        "Where can I find out more about Triple I?",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "How should I acknowledge this work?",
        "Please cite this work as:",
        "Pulford A, Richardson E, Agbato D, et al. Informing Interventions to reduce health Inequalities (Triple I): National overview report 2019. Edinburgh: NHS Health Scotland; 2019. http://www.healthscotland.scot/triplei")
writeData(wb, "Background information", x = df, startRow = 7, startCol = 2)
## Internal Link- Text to display
writeFormula(wb, "Background information", startRow = 20,  startCol = 2,
             x = makeHyperlinkString(sheet = "Intervention effects", row = 1, col = 2,
                            text = "Intervention effects (data from user-specified options)"))
writeFormula(wb, "Background information", startRow = 21,  startCol = 2,
             x = makeHyperlinkString(sheet = "Intervention cost-effectiveness", row = 1, col = 2,
                                     text = "Intervention cost-effectiveness (data from user-specified options)"))
writeFormula(wb, "Background information", startRow = 22,  startCol = 2,
             x = makeHyperlinkString(sheet = "Intervention definitions", row = 1, col = 2,
                                     text = "Intervention definitions"))
writeFormula(wb, "Background information", startRow = 23,  startCol = 2,
             x = makeHyperlinkString(sheet = "Option definitions", row = 1, col = 2,
                                     text = "Option definitions"))

        x <- c("http://www.healthscotland.scot/triplei",
               "http://www.healthscotland.scot/triplei",
               "http://www.healthscotland.scot/triplei",
               "http://www.healthscotland.scot/triplei",
               "http://www.healthscotland.scot/triplei",
               "http://www.healthscotland.scot/triplei",
               "http://www.healthscotland.scot/triplei",
               "mailto:nhs.triple.i@nhs.net")
        names(x) <- c("NHS Health Scotland Triple I web page",
                      "National Overview Report (2019)",
                      "Technical Report (2019)",
                      "Income-based policies briefing (2018)",
                      "Spreadsheet tools for individual interventions (2019)",
                      "Interactive results browser",
                      "Answers to Frequently Asked Questions",
                      "Contact us")
        class(x) <- "hyperlink"
        writeData(wb, sheet = "Background information", x = x, startRow = 26, startCol = 2)
        addStyle(wb, "Background information", general_style_wrap, rows=5:40, cols=2, gridExpand = TRUE, stack = TRUE)
        setColWidths(wb, "Background information", cols = 1:2, widths = c(3,150), ignoreMergedCells=TRUE)
        addStyle(wb, "Background information", heading2_style, rows=c(5,7,10,13,16,25,35), cols=c(2,2,2,2,2,2,2), gridExpand = FALSE, stack = FALSE)

#adding another worksheet, this time containing the quadrant plot and its data
    addWorksheet(wb, "Intervention effects")
    quadrant <- plotly_quadrant_plot()
    export(quadrant, file = "quadrant_plot.png")
    title <- title_quadrant_plot()
    writeData(wb, "Intervention effects", x = title, startCol = 2)
    mergeCells(wb, "Intervention effects", cols = 2:9, rows = 1)
    addStyle(wb, "Intervention effects", heading1_style, rows=1, cols=2, gridExpand = FALSE, stack = FALSE)

    writeData(wb, "Intervention effects", x = "The user-selected options:", startRow = 3, startCol = 2)
    addStyle(wb, "Intervention effects", heading2_style, rows=3, cols=2, gridExpand = FALSE, stack = FALSE)

    writeData(wb, "Intervention effects", x = "Area:", startCol = 2, startRow = 5)
    writeData(wb, "Intervention effects", x = input$area, startCol = 3, startRow = 5)

    writeData(wb, "Intervention effects", x = "Health outcome:", startCol = 2, startRow = 6)
    writeData(wb, "Intervention effects", x = input$outcomename, startCol = 3, startRow = 6)

    writeData(wb, "Intervention effects", x = "Year of follow-up:", startCol = 2, startRow = 7)
    writeData(wb, "Intervention effects", x = input$year, startCol = 3, startRow = 7)

    writeData(wb, "Intervention effects", x = "Intervention type(s):", startCol = 2, startRow = 8)
    writeData(wb, "Intervention effects", x = input$inttype, startCol = 3, startRow = 8)

    writeData(wb, "Intervention effects", x = "Options for individual-level interventions:", startCol = 2, startRow = 9)

    writeData(wb, "Intervention effects", x = "Targeting strategy:", startCol = 2, startRow = 10)
    if (input$inttype %in% c("all", "Mitigate", "Prevent", "Local")) {
      writeData(wb, "Intervention effects", x = input$stratname, startCol = 3, startRow = 10)
    } else {
      writeData(wb, "Intervention effects", x = "N/A", startCol = 3, startRow = 10)
    }
    treatoptionslist <- sort(unique(iiidata$treatoptions))
    treat <- treatoptionslist[as.integer(input$treatoptions)]

    writeData(wb, "Intervention effects", x = "Number or % of the eligible population treated:", startCol = 2, startRow = 11)
    if (input$inttype %in% c("all", "Mitigate", "Prevent", "Local")) {
      writeData(wb, "Intervention effects", x = treat, startCol = 3, startRow = 11)
    } else {
      writeData(wb, "Intervention effects", x = "N/A", startCol = 3, startRow = 11)
    }

    addStyle(wb, "Intervention effects", general_style_wrap, rows=5:11, cols=3, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Intervention effects", heading3_style, rows=5:11, cols=2, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Intervention effects", border_style, rows=5:11, cols=3, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Intervention effects", border_style, rows=5:11, cols=2, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Intervention effects", heading3_noshade_style, rows=c(9), cols=2:3, gridExpand = TRUE, stack = FALSE)

    writeData(wb, "Intervention effects", x = "The quadrant chart:", startRow = 13, startCol = 2)
    addStyle(wb, "Intervention effects", heading2_style, rows=13, cols=2, gridExpand = FALSE, stack = FALSE)

    writeData(wb, "Intervention effects",
              x = "This quadrant chart shows how interventions are predicted to affect health (on the horizontal axis) and health inequalities (on the vertical axis).",
              startRow = 14, startCol = 2)
    writeData(wb, "Intervention effects",
              x = "Interpretation is based on which quadrant the intervention appears in: interventions in the bottom right quadrant both improve health and reduce health inequalities.",
              startRow = 15, startCol = 2)
    writeData(wb, "Intervention effects",
              x = "Please note that these interventions vary markedly in the level of investment required and their population reach.",
              startRow = 16, startCol = 2)
    writeData(wb, "Intervention effects",
              x = "Health inequalities are measured using the Relative Index of Inequality, or RII.",
              startRow = 17, startCol = 2)
    writeData(wb, "Intervention effects",
              x = "Positive values for the health outcome indicate improved health, while negative values for inequalities indicate narrower inequalities. ",
              startRow = 18, startCol = 2)
    writeData(wb, "Intervention effects",
              x = "The effects shown are cumulative: they combine the effects from year 2 (the first year of follow-up) to the specified year of follow-up. ",
              startRow = 19, startCol = 2)
    
    
    addStyle(wb, "Intervention effects", general_style, rows=14:19, cols=2, gridExpand = TRUE, stack = TRUE)
    mergeCells(wb, "Intervention effects", cols = 2:9, rows = 14)
    mergeCells(wb, "Intervention effects", cols = 2:9, rows = 15)
    mergeCells(wb, "Intervention effects", cols = 2:9, rows = 16)
    mergeCells(wb, "Intervention effects", cols = 2:9, rows = 17)
    mergeCells(wb, "Intervention effects", cols = 2:9, rows = 18)
    mergeCells(wb, "Intervention effects", cols = 2:9, rows = 19)
    
    insertImage(wb, "Intervention effects", "quadrant_plot.png",
                startRow = 21, startCol = 2, width = 8, height = 6, units = "in", dpi = 1000)
    writeData(wb, "Intervention effects", x = "The data for the chart:", startRow = 52, startCol = 2)
    addStyle(wb, "Intervention effects", heading2_style, rows=52, cols=2, gridExpand = FALSE, stack = FALSE)

    writeData(wb, "Intervention effects", x = matrix(c("Intervention", "Type", "Targeting strategy",
                                                  "Health outcome", "No. treated", "Year", "No. health outcomes prevented",
                                                  "% health outcomes prevented","Widening of relative inequalities in health outcome (RII)", "% widening of relative inequalities in health outcome"),
                                                ncol=10, nrow=1), startRow = 54, startCol = 2,
              colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
              borders = c("surrounding"),
              borderColour = getOption("openxlsx.borderColour", "black"),
              borderStyle = getOption("openxlsx.borderStyle", "thin"),
              withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ",")

    addStyle(wb, "Intervention effects", heading3_style_wrap, rows=54, cols=2:11, gridExpand = FALSE, stack = TRUE)
    setColWidths(wb, "Intervention effects", cols = 2:3, widths = "auto", ignoreMergedCells=TRUE)
    setColWidths(wb, "Intervention effects", cols = c(1, 4:11), widths = c(3, 22, 19, 14, 8, 29, 29, 29, 30), ignoreMergedCells=TRUE)
    writeData(wb, "Intervention effects", data_quadrant_plot(), startRow = 55, startCol=2,
                   colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
                   borders = c("surrounding"),
                   borderColour = getOption("openxlsx.borderColour", "black"),
                   borderStyle = getOption("openxlsx.borderStyle", "thin"),
                   withFilter = FALSE, keepNA = TRUE, name = NULL, sep = ",")
    rownum4 <- nrow(data_quadrant_plot())+54
    addStyle(wb, "Intervention effects", general_style, rows=55:rownum4, cols=2:11, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Intervention effects", border_style, rows=55:rownum4, cols=2:11, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Intervention effects", integers, rows=55:rownum4, cols=6, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Intervention effects", dp3, rows=55:rownum4, cols=c(8, 9, 11), gridExpand = TRUE, stack = TRUE)
    
#adding another worksheet, this time containing the cost comparison data
    addWorksheet(wb, "Intervention cost-effectiveness")
    writeData(wb, "Intervention cost-effectiveness", x = title_cost_data(), startCol = 2)
    mergeCells(wb, "Intervention cost-effectiveness", cols = 2:9, rows = 1)
    addStyle(wb, "Intervention cost-effectiveness", heading1_style, rows=1, cols=2, gridExpand = FALSE, stack = FALSE)

    writeData(wb, "Intervention cost-effectiveness", x = "The user-selected options:", startRow = 3, startCol = 2)
    addStyle(wb, "Intervention cost-effectiveness", heading2_style, rows=3, cols=2, gridExpand = FALSE, stack = FALSE)

    writeData(wb, "Intervention cost-effectiveness", x = "Area:", startCol = 2, startRow = 5)
    writeData(wb, "Intervention cost-effectiveness", x = input$area, startCol = 3, startRow = 5)

    writeData(wb, "Intervention cost-effectiveness", x = "Health outcome:", startCol = 2, startRow = 6)
    writeData(wb, "Intervention cost-effectiveness", x = input$outcomename, startCol = 3, startRow = 6)

    writeData(wb, "Intervention cost-effectiveness", x = "Year of follow-up:", startCol = 2, startRow = 7)
    writeData(wb, "Intervention cost-effectiveness", x = input$year, startCol = 3, startRow = 7)

    writeData(wb, "Intervention cost-effectiveness", x = "Intervention type(s):", startCol = 2, startRow = 8)
    writeData(wb, "Intervention cost-effectiveness", x = input$inttype, startCol = 3, startRow = 8)

    writeData(wb, "Intervention cost-effectiveness", x = "Options for individual-level interventions:", startCol = 2, startRow = 9)

    writeData(wb, "Intervention cost-effectiveness", x = "Targeting strategy:", startCol = 2, startRow = 10)
    if (input$inttype %in% c("all", "Mitigate", "Prevent", "Local")) {
      writeData(wb, "Intervention cost-effectiveness", x = input$stratname, startCol = 3, startRow = 10)
    } else {
      writeData(wb, "Intervention cost-effectiveness", x = "N/A", startCol = 3, startRow = 10)
    }
    writeData(wb, "Intervention cost-effectiveness", x = "Number or % of the eligible population treated:", startCol = 2, startRow = 11)
    if (input$inttype %in% c("all", "Mitigate", "Prevent", "Local")) {
      writeData(wb, "Intervention cost-effectiveness", x = treat, startCol = 3, startRow = 11)
    } else {
      writeData(wb, "Intervention cost-effectiveness", x = "N/A", startCol = 3, startRow = 11)
    }
    addStyle(wb, "Intervention cost-effectiveness", general_style_wrap, rows=5:11, cols=3, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Intervention cost-effectiveness", heading3_style, rows=5:11, cols=2, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Intervention cost-effectiveness", border_style, rows=5:11, cols=3, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Intervention cost-effectiveness", border_style, rows=5:11, cols=2, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Intervention cost-effectiveness", heading3_noshade_style, rows=c(9), cols=2:3, gridExpand = TRUE, stack = FALSE)

    writeData(wb, "Intervention cost-effectiveness",
              x = "For each intervention we estimate the cost for a 1%-point improvement in the health outcome or narrowing of relative inequalities in the health outcome.",
              startRow = 14, startCol = 2)
    writeData(wb, "Intervention cost-effectiveness",
              x = "Please note that these interventions vary markedly in the level of investment required and their population reach.",
              startRow = 15, startCol = 2)
    writeData(wb, "Intervention cost-effectiveness",
              x = "Health inequalities are measured using the Relative Index of Inequality, or RII.",
              startRow = 16, startCol = 2)
    writeData(wb, "Intervention cost-effectiveness",
              x = "In this table positive values indicate improved health and narrower inequalities. ",
              startRow = 17, startCol = 2)
    writeData(wb, "Intervention cost-effectiveness",
              x = "N.B. If the intervention did not improve health or reduce inequality we did not estimate cost-effectiveness (indicated by #N/A).",
              startRow = 18, startCol = 2)
    writeData(wb, "Intervention cost-effectiveness",
              x = "The effects shown are cumulative: they combine the effects from year 2 (the first year of follow-up) to the specified year of follow-up. ",
              startRow = 19, startCol = 2)
    
    addStyle(wb, "Intervention cost-effectiveness", general_style, rows=14:19, cols=2, gridExpand = TRUE, stack = TRUE)
    mergeCells(wb, "Intervention cost-effectiveness", cols = 2:9, rows = 14)
    mergeCells(wb, "Intervention cost-effectiveness", cols = 2:9, rows = 15)
    mergeCells(wb, "Intervention cost-effectiveness", cols = 2:9, rows = 16)
    mergeCells(wb, "Intervention cost-effectiveness", cols = 2:9, rows = 17)
    mergeCells(wb, "Intervention cost-effectiveness", cols = 2:9, rows = 18)
    mergeCells(wb, "Intervention cost-effectiveness", cols = 2:9, rows = 19)
    
    setColWidths(wb, "Intervention cost-effectiveness", cols = c(1:12), widths = c(3,44,30,19,7,17,20,11,17,20,21,21), ignoreMergedCells=TRUE)
    headings <- matrix(c("Intervention", "Type", "Health outcome", "Year", "Total intervention cost (\u00A3)", "Targeting strategy",
                           "No. treated", "% health outcomes prevented", "\u00A3 per 1%-point health outcomes prevented",
                         "% narrowing of relative inequalities in health outcome", "\u00A3 per 1%-point narrowing of relative inequalities in health outcome"), ncol=11, nrow=1)
      datalength <- nrow(get_costtable_data())
    
      writeData(wb, "Intervention cost-effectiveness",
                x = paste0("Cost-effectiveness data", sep=""),
                startRow = 21, startCol = 2)
      addStyle(wb, "Intervention cost-effectiveness", heading2_style,
               rows=21, cols=2, gridExpand = FALSE, stack = FALSE)

      writeData(wb, "Intervention cost-effectiveness", x = headings,
                startRow = 23, startCol = 2,
                colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
                borders = c("surrounding"),
                borderColour = getOption("openxlsx.borderColour", "black"),
                borderStyle = getOption("openxlsx.borderStyle", "thin"),
                withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ",")
      addStyle(wb, "Intervention cost-effectiveness", heading3_style_wrap,
               rows=23, cols=2:12, gridExpand = FALSE, stack = TRUE)

      writeData(wb, "Intervention cost-effectiveness", get_costtable_data(),
                startRow = 24, startCol=2,
                colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
                borders = c("surrounding"),
                borderColour = getOption("openxlsx.borderColour", "black"),
                borderStyle = getOption("openxlsx.borderStyle", "thin"),
                withFilter = FALSE, keepNA = TRUE, name = NULL, sep = ",")
      addStyle(wb, "Intervention cost-effectiveness", general_style,
               rows=24:(24+datalength-1),
               cols=2:12, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, "Intervention cost-effectiveness", border_style,
               rows=24:(24+datalength-1),
               cols=2:12, gridExpand = TRUE, stack = TRUE)

     addStyle(wb, "Intervention cost-effectiveness", integers, rows=24:(24+datalength-1), cols=c(6,10,12), gridExpand = TRUE, stack = TRUE)
     addStyle(wb, "Intervention cost-effectiveness", dp3, rows=24:(24+datalength-1), cols=c(9,11), gridExpand = TRUE, stack = TRUE)
     
    #adding another worksheet containing the intervention definitions
    addWorksheet(wb, "Intervention definitions")
    writeData(wb, "Intervention definitions", x = "Definitions of the interventions", startRow = 1, startCol = 2)
    mergeCells(wb, "Intervention definitions", cols = 2:3, rows = 1)
    addStyle(wb, "Intervention definitions", heading1_style, rows=1, cols=2, gridExpand = FALSE, stack = TRUE)
    writeData(wb, "Intervention definitions", x = matrix(c("Intervention", "Definition"),
                                              nrow=1, ncol=2), startRow = 3, startCol = 2,
              colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
              borders = c("surrounding"),
              borderColour = getOption("openxlsx.borderColour", "black"),
              borderStyle = getOption("openxlsx.borderStyle", "thin"),
              withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ",")

    interventionlist <- intervention_list()
    writeData(wb, "Intervention definitions", interventionlist, startRow = 4, startCol=2,
              colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
              borders = c("surrounding"),
              borderColour = getOption("openxlsx.borderColour", "black"),
              borderStyle = getOption("openxlsx.borderStyle", "thin"),
              withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ",")

    setColWidths(wb, "Intervention definitions", cols=1:3, widths = c(3, 50, 108)) ## set column width for row names column
    addStyle(wb, "Intervention definitions", heading3_style, rows=3, cols=2:3, gridExpand = TRUE, stack = TRUE)
    rownum3 <- nrow(interventionlist)+3
    addStyle(wb, "Intervention definitions", heading3_style, rows=4:rownum3, cols=2, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Intervention definitions", general_style_wrap, rows=4:rownum3, cols=3, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Intervention definitions", border_style, rows=4:rownum3, cols=2, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Intervention definitions", border_style, rows=4:rownum3, cols=3, gridExpand = TRUE, stack = TRUE)

#adding another worksheet containing the option definitions
    addWorksheet(wb, "Option definitions")
    writeData(wb, "Option definitions", x = "Definitions of the options", startRow = 1, startCol = 2)
    mergeCells(wb, "Option definitions", cols = 2:3, rows = 1)
    addStyle(wb, "Option definitions", heading1_style, rows=1, cols=2, gridExpand = FALSE, stack = TRUE)
    writeData(wb, "Option definitions", x = matrix(c("Option", "Definition"),
                                              nrow=1, ncol=2), startRow = 3, startCol = 2,
              colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
              borders = c("surrounding"),
              borderColour = getOption("openxlsx.borderColour", "black"),
              borderStyle = getOption("openxlsx.borderStyle", "thin"),
              withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ",")

writeData(wb, "Option definitions", x = "Health outcomes", startRow = 5, startCol = 2)

writeData(wb, "Option definitions", x = "Premature deaths", startRow = 6, startCol = 2)
writeData(wb, "Option definitions", x = "Deaths before the age of 75.", startRow = 6, startCol = 3)

writeData(wb, "Option definitions", x = "Years of life lost", startRow = 7, startCol = 2)
writeData(wb, "Option definitions", x = "An estimate of the years a person would have lived if they had not died prematurely (based on life expectancy).", startRow = 7, startCol = 3)

writeData(wb, "Option definitions", x = "Hospital stays", startRow = 8, startCol = 2)
writeData(wb, "Option definitions", x = "Continuous inpatient stays in general or psychiatric hospitals.", startRow = 8, startCol = 3)

writeData(wb, "Option definitions", x = "Year of follow up", startRow = 10, startCol = 2)
writeData(wb, "Option definitions", x = "All interventions start in year 1, and follow up begins in year 2.  Income-based, 20 mph limits, and tobacco taxation interventions are permanent, while all other interventions are implemented in year 1 only.  Health outcomes are calculated cumulatively over the period.", startRow = 10, startCol = 3)

writeData(wb, "Option definitions", x = "Intervention type", startRow = 12, startCol = 2)

writeData(wb, "Option definitions", x = "Undo", startRow = 13, startCol = 2)
writeData(wb, "Option definitions", x = "Interventions that act to undo the fundamental causes of health inequality (e.g., income inequality).", startRow = 13, startCol = 3)

writeData(wb, "Option definitions", x = "Prevent", startRow = 14, startCol = 2)
writeData(wb, "Option definitions", x = "Interventions that act to prevent harm from wider environmental influences (e.g., unemployment).", startRow = 14, startCol = 3)

writeData(wb, "Option definitions", x = "Mitigate", startRow = 15, startCol = 2)
writeData(wb, "Option definitions", x = "Interventions that act to mitigate against the negative impact of wider determinants on individuals' health (e.g., smoking).", startRow = 15, startCol = 3)

writeData(wb, "Option definitions", x = "These additional options are available for interventions focused on individuals:", startRow = 17, startCol = 2)

writeData(wb, "Option definitions", x = "Targeting strategy", startRow = 19, startCol = 2)

writeData(wb, "Option definitions", x = "Even distribution", startRow = 20, startCol = 2)
writeData(wb, "Option definitions", x = "The same proportion of each population subgroup (by age group, sex and deprivation) is treated.", startRow = 20, startCol = 3)

writeData(wb, "Option definitions", x = "Proportionate to need", startRow = 21, startCol = 2)
writeData(wb, "Option definitions", x = "The proportion of each subgroup treated is proportionate to the risk factor prevalence.", startRow = 21, startCol = 3)

writeData(wb, "Option definitions", x = "Most deprived 20% only", startRow = 22, startCol = 2)
writeData(wb, "Option definitions", x = "Only those in Scottish Index of Multiple Deprivation (SIMD) quintile 1 are treated (even distribution within this quintile).", startRow = 22, startCol = 3)

writeData(wb, "Option definitions", x = "Most deprived 40% only", startRow = 23, startCol = 2)
writeData(wb, "Option definitions", x = "Only those in SIMD quintiles 1 and 2 are treated (even distribution within this quintile).", startRow = 23, startCol = 3)

writeData(wb, "Option definitions", x = "Number to treat", startRow = 25, startCol = 2)

writeData(wb, "Option definitions", x = "Number", startRow = 26, startCol = 2)
writeData(wb, "Option definitions", x = "Either 100, 1,000 or 100,000 interventions are delivered.  N.B. For areas and interventions with insufficient eligible population to treat this number no data will be plotted.", startRow = 26, startCol = 3)

writeData(wb, "Option definitions", x = "Percentage", startRow = 27, startCol = 2)
writeData(wb, "Option definitions", x = "A percentage of the population 'eligible' to receive the intervention is treated.", startRow = 27, startCol = 3)

setColWidths(wb, "Option definitions", cols=1:3, widths = c(3, 50, 108)) ## set column width for row names column
addStyle(wb, "Option definitions", heading2_shade_style, rows=3, cols=2:3, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Option definitions", heading2_shade_style, rows=c(5,10,12,19,25), cols=2, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Option definitions", heading3_style, rows=c(6:8, 13:15, 20:23, 26:27), cols=2, gridExpand = TRUE, stack = FALSE)
addStyle(wb, "Option definitions", heading3_noshade_style, rows=c(4, 9, 11, 16:18, 24), cols=2:3, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Option definitions", general_style_wrap, rows=4:27, cols=3, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Option definitions", border_style, rows=c(5:8, 10, 12:15, 19:23, 25:27), cols=2:3, gridExpand = TRUE, stack = TRUE)

saveWorkbook(wb, file, overwrite = TRUE)
  }
)

#####################################.
#####################################.
#### Cost Table ----
###############################################.
## Help pop-up modal dialog
observeEvent(input$help_costtable, {
  showModal(modalDialog(
    title = "How to use the 'Compare costs' page",
    fluidRow(h4(tags$b("This table gives the direct cost (or saving) required for a 1%-point reduction in the health outcome 
                       or 1%-point narrowing of inequalities in the health outcome."))),
    fluidRow(h5(tags$b(tags$u("Choosing the data to show")))),
    fluidRow(h6("You can change the default options using the dropdown menus in the grey panel to select a different area, health outcome, follow-up period, and intervention type.", 
                style = "color:black; font-weight: bold;")),
    fluidRow(
      h6("Health outcomes:", style = "color:black; font-weight: bold;"),
      tags$ul( 
        tags$li(class= "li-custom", tags$b("Premature deaths:"),
                "deaths before the age of 75."),
        tags$li(class= "li-custom", tags$b("Years of life lost:"), " an estimate of the years a person would have lived if they had not died prematurely (based on life expectancy)."),
        tags$li(class= "li-custom", tags$b("Hospital stays:"), " continuous inpatient stays in general or psychiatric hospitals."))),
    fluidRow(
      h6("Years of follow up:", style = "color:black; font-weight: bold;"),
      tags$ul( 
        tags$li(class= "li-custom", "All interventions start in year 1."),
        tags$li(class= "li-custom", "Income-based, 20 mph limits, and tobacco taxation interventions are permanent."),
        tags$li(class= "li-custom", "All other interventions are implemented in year 1 only."),
        tags$li(class= "li-custom", "Follow up begins in year 2."),
        tags$li(class= "li-custom", "Outcomes are calculated cumulatively over the period.")
      )),
    fluidRow(
      h6("Intervention type:", style = "color:black; font-weight: bold;"),
      tags$ul( 
        tags$li(class= "li-custom", tags$b("National implementation:"), " These interventions could be implemented by UK or Scottish governments."),
        tags$li(class= "li-custom", tags$b("Local implementation:"), " These interventions could be implemented at the local level."),
        tags$li(class= "li-custom", tags$b("Undo:"),
                " These interventions 'Undo' the fundamental causes of health inequality (e.g., income inequality)."),
        tags$li(class= "li-custom", tags$b("Prevent:"), " These interventions 'Prevent' harm from wider environmental influences (e.g., unemployment)."),
        tags$li(class= "li-custom", tags$b("Mitigate:"), " These interventions 'Mitigate' against the negative impact of wider determinants on individuals' health (e.g., smoking)."),
        tags$li(class= "li-custom", "Definitions of the interventions can be found using the 'Intervention definitions' button.", 
                img(src='definitionsbutton.png', height=40, style="padding-left: 2px; vertical-align:middle;"))
      )),
    fluidRow(
      h6("And for interventions focused on individuals (e.g., smoking cessation or weight management services) you can also change the intervention targeting strategy.", style = "color:black; font-weight: bold;"),  
      h6(tags$b("Targeting strategy:", style = "color:black; font-weight: bold;")),
      tags$ul( 
        tags$li(class= "li-custom", tags$b("Even distribution:"),
                "Treat the same proportion of each population subgroup (by age group, sex and deprivation)."),
        tags$li(class= "li-custom", tags$b("Proportionate to need:"), " The proportion of each subgroup treated is proportionate to the risk factor prevalence."),
        tags$li(class= "li-custom", tags$b("Most deprived 20% only:"), " Treat only those in Scottish Index of Multiple Deprivation (SIMD) quintile 1 (even distribution within this quintile)."),
        tags$li(class= "li-custom", tags$b("Most deprived 40% only:"), " Treat only those in SIMD quintiles 1 and 2 (even distribution within this quintile).")
      )),
fluidRow(h6("The number of individuals treated by these individual-level interventions has been set at 100% of the eligible population, so there is no option to select the number to treat.", 
                style = "color:black; font-weight: bold;")),
    br(),
    fluidRow(
      h5(tags$b(tags$u("Interpreting the cost-effectiveness data")))),  
    fluidRow(  
      tags$ul(
        tags$li(class= "li-custom", h6("In the cost-effectiveness table health inequalities are measured using the Relative Index of Inequality, or RII.  
                  The RII is a summary measure of inequality which takes into account differences across the whole deprivation gradient, 
                  not just the gap in health outcome between the most and least deprived.", style = "color:black; font-weight: bold;")),
        tags$li(class= "li-custom", h6("Positive values for costs, health outcomes or health inequalities represent costs incurred (as opposed to savings),
                  health outcomes prevented or health inequalities narrowed, respectively.", style = "color:black; font-weight: bold;")),
        tags$li(class= "li-custom", h6("Red figures indicate a worsening of health outcomes or health inequalities.  In these cases the cost-effectiveness has not been calculated.",
                  style = "color:black; font-weight: bold;")),
        tags$li(class= "li-custom", h6("The contents of the table can be sorted by a particular column by clicking on the grey arrowheads at the top of the column.",
                  style = "color:black; font-weight: bold;"))
      )),
img(src='tableheader.png', width="100%", style="padding-left: 2px; vertical-align:middle"),
fluidRow(
          h6("Important points to note:", style = "color:black; font-weight: bold;"),
          tags$ul( 
            tags$li(class= "li-custom",                     
                    "Annual costs/savings accumulate each year for income-based interventions and tobacco taxation.  Other interventions are costed for a single year.", style = "color:black; font-weight: bold;"),
            tags$li(class= "li-custom",                     
                    "Costs or savings arising as a result of the intervention are not included in these estimates (e.g., reduced hospital stays).", style = "color:black; font-weight: bold;"),
            tags$li(class= "li-custom", "Potential re-distributional effects from cost-saving interventions are not included (e.g., tax revenues).", style = "color:black; font-weight: bold;"),
            tags$li(class= "li-custom", "Secondary behavioural effects of the interventions are not included (e.g., change in working hours or decision to work).", style = "color:black; font-weight: bold;")
          )),
    br(),
    fluidRow(h5(tags$b(tags$u("Downloading the chart and data")))),  
    fluidRow(
      tags$ul(
        tags$li(class= "li-custom", "You can download the data in the table using the 'Download data' button.  The file produced is an Excel spreadsheet, and includes background information about the project and the data.  The quadrant chart (from the 'Compare interventions' page) for the selected options is also included.", 
                                       style = "color:black; font-weight: bold;", img(src='downloadbutton1.png', height=60, border="1px", color="black", style="padding-right: 2px; vertical-align:middle;"))
      )),
    easyClose = TRUE, fade=FALSE
      ))
})

  ###############################################.
  # Intervention definitions
  
  output$defs_text_costtable <- renderUI({
    HTML(paste(sprintf("<b><u>%s ('%s' type)</b></u> <br> %s ", 
                       interventions_lookup$intervention[interventions_lookup$include==1],
                       interventions_lookup$type2[interventions_lookup$include==1],
                       interventions_lookup$definition[interventions_lookup$include==1]
    ), collapse = "<br><br>"))
  })
  
#####################.
# Reactive data 

# Cost table data. Filtering based on user input values.
filter_costtable <- reactive({
  treatoptionslist <- sort(unique(iiidata$treatoptions))
  treatoptionslist <- sort(unique(iiidata$treatoptions))
  index <- match(input$area4, areas_lookup$inputarea)
  equivarea <- areas_lookup[na.omit(index), ]
  eq <- as.character(equivarea$areacode)
  if (input$inttype4=="all") { 
    costdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename4
             & year == input$year4
             & ((stratname == input$stratname4 & treatoptions == treatoptionslist[10])| (stratname == "Population-wide")))
  } else if (input$inttype4=="Undo") {
    costdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename4
             & year == input$year4
             & type == "Undo")
  } else if (input$inttype4=="Prevent"){
    costdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename4
             & year == input$year4
             & (intlabel=="mph20"|intlabel=="TobTax"|intlabel=="uptake1pc"
                |(intlabel=="Employ" & stratname==input$stratname4 & treatoptions == treatoptionslist[10]))) 
  } else if (input$inttype4=="Mitigate"){
    costdata <- iiidata %>%
      subset(area == eq
             & outcome == input$outcomename4
             & year == input$year4
             & type == "Mitigate"
             & stratname == input$stratname4 
             & treatoptions == treatoptionslist[10])
  } else if (input$inttype4=="National") {
    costdata <- iiidata %>%
    subset(area == eq
           & outcome == input$outcomename4
           & year == input$year4
           & level =="National")
  } else {
    costdata <- iiidata %>%
    subset(area == eq
           & outcome == input$outcomename4
           & year == input$year4
           & level =="Local"
           & (intlabel=="mph20"|intlabel=="ctxinc"|intlabel=="uptake1pc"
              |((type =="Mitigate"|intlabel=="Employ") & stratname == input$stratname4 
                & treatoptions == treatoptionslist[10])))
  }

  costdata$stratname <- as.character(costdata$stratname)
  costdata$rii_diffpc <- costdata$rii_diffpc*-1
  costdata$costperriipc <- costdata$costperriipc*-1
  costdata$costperoutcomepc <- costdata$interventioncost/costdata$total_diffpc
  costdata$costperoutcomepc[costdata$total_diffpc< 0] <- NA
  costdata$costperriipc[costdata$rii_diffpc< 0] <- NA
  costdata$ntreated[is.na(costdata$ntreated)] <- "Population"
  costdata <- costdata %>% arrange(type, intervention)
  table <- costdata %>% subset(select=c("type", "intervention", "interventioncost", "stratname", "ntreated", 
                                        "total_diffpc","costperoutcomepc",
                                        "rii_diffpc", "costperriipc"))
})
 
get_data_by_type <- function(type) {
  data <- filter_costtable()[(filter_costtable()$type==type),]
  }
  
############################.
#Title of cost table
output$costtable_title <- renderText( 
  sprintf("Cost-effectiveness data for intervention effects on %s%s for %s",
          tolower(substr(input$outcomename4, 1, 1)),
          substr(input$outcomename4, 2, nchar(input$outcomename4)),
          input$area4)
)

container2 = htmltools::withTags(table(class="compact stripe",
  thead(
    tr(
      th(colspan = 5),
      th(colspan = 2, 'Health outcome', style="background-color:#ecf0f2"),
      th(colspan = 2, 'Relative inequality in health outcome (RII)', style="background-color:#ecf0f2")
    ),
    tr(
      th('Type', style="background-color:#ecf0f2"),
      th('Intervention', style="background-color:#ecf0f2"),
      th('Total intervention cost (£)', style="background-color:#ecf0f2; text-align:left"),
      th('Targeting strategy', style="background-color:#ecf0f2"),
      th('Number treated', style="background-color:#ecf0f2"),
      th('% fewer health outcomes', style="background-color:#ecf0f2; text-align:left"),
      th('£ per 1%-point fewer health outcomes', style="background-color:#ecf0f2; text-align:left"),
      th('% narrowing', style="background-color:#ecf0f2; text-align:left"),
      th('£ per 1%-point narrowing', style="background-color:#ecf0f2; text-align:left")
    )
)
))

output$costtable_filtered <- DT::renderDataTable({
  DT::datatable(filter_costtable(),
    rownames = FALSE, options = list(dom = 't', pageLength = 25), class = 'table-condensed table-bordered table-striped', container = container2) %>%
    formatRound(columns=c("total_diffpc"), digits=3) %>%
    formatRound(columns=c("rii_diffpc"), digits=3) %>%
    formatCurrency(columns=c("ntreated"), currency = "", digits=0, interval = 3, mark = ",") %>%
    formatCurrency(columns=c("costperoutcomepc", "costperriipc", "interventioncost"), currency = "\u00A3", digits=0, interval = 3, mark = ",") %>%
    formatStyle(columns=c("total_diffpc", "rii_diffpc"), color = styleInterval(c(0), c('red', 'normal')), fontWeight = styleInterval(c(0), c('bold', 'normal'))) 
})

#####################################.    
#### Cost table excel download ----
####################################.
title_quadrant_plot4 <- function(){
     sprintf("Comparing intervention effects on %s%s for %s", 
             tolower(substr(input$outcomename4, 1, 1)), 
             substr(input$outcomename4, 2, nchar(input$outcomename4)),
             input$area4)
   }
   
title_cost_data4 <- function(){
     sprintf("Cost-effectiveness data for intervention effects on %s%s for %s",
             tolower(substr(input$outcomename4, 1, 1)),
             substr(input$outcomename4, 2, nchar(input$outcomename4)),
             input$area4)
   }

# Get data for the spreadsheet download. Filtering based on user input values.
get_data <- reactive({
   treatoptionslist <- sort(unique(iiidata$treatoptions))
   index <- match(input$area4, areas_lookup$inputarea)
   equivarea <- areas_lookup[na.omit(index), ]
   eq <- as.character(equivarea$areacode)
   if (input$inttype4=="all") { 
     costdata <- iiidata %>%
       subset(area == eq
              & outcome == input$outcomename4
              & year == input$year4
              & ((stratname == input$stratname4 & 
                    treatoptions == treatoptionslist[10])
                 | (stratname == "Population-wide")))
   } else if (input$inttype4=="Undo") {
     costdata <- iiidata %>%
       subset(area == eq
              & outcome == input$outcomename4
              & year == input$year4
              & type == "Undo")
   } else if (input$inttype4=="Prevent") {
     costdata <- iiidata %>%
       subset(area == eq
              & outcome == input$outcomename4
              & year == input$year4
              & (intlabel=="mph20"|intlabel=="TobTax"|intlabel=="uptake1pc"
                 |(intlabel=="Employ" & stratname==input$stratname4 
                   & treatoptions==treatoptionslist[10]))) 
   } else if (input$inttype4=="Mitigate") {
     costdata <- iiidata %>%
       subset(area == eq
              & outcome == input$outcomename4
              & year == input$year4
              & type =="Mitigate"
              & stratname == input$stratname4 
              & treatoptions == treatoptionslist[10])
   } else if (input$inttype4=="National") {
     costdata <- iiidata %>%
       subset(area == eq
              & outcome == input$outcomename4
              & year == input$year4
              & level =="National")
   } else {
     costdata <- iiidata %>%
       subset(area == eq
              & outcome == input$outcomename4
              & year == input$year4
              & level =="Local"
              & (intlabel=="mph20"|intlabel=="ctxinc"|intlabel=="uptake1pc"
                 |((type =="Mitigate"|intlabel=="Employ") & stratname == input$stratname4 
                   & treatoptions == treatoptionslist[10])))
   }
   table <- costdata %>% subset(select=c("intervention", "type", "outcome", "year", "interventioncost", "stratname", "ntreated",
                                         "total_diffpc","costperoutcomepc",
                                         "rii_diffpc", "costperriipc", "total_diff", "rii_diff" ))
 })

get_costtable_data4 <- function(){
  costdata <- data.frame(get_data())
  costdata$stratname <- as.character(costdata$stratname)
  #change % change in RII to be a measure of narrowing:
  costdata$rii_diffpc <- costdata$rii_diffpc*-1
  costdata$costperoutcomepc[costdata$total_diffpc< 0] <- NA
  costdata$costperriipc <- costdata$costperriipc*-1
  costdata$costperriipc[costdata$rii_diffpc< 0] <- NA
  costdata$ntreated[is.na(costdata$ntreated)] <- "Population"
  costdata <- costdata %>% 
    subset(select=c("intervention", "type", "outcome", "year", "interventioncost", "stratname", "ntreated",
                    "total_diffpc","costperoutcomepc",
                    "rii_diffpc", "costperriipc")) %>% 
    arrange(type, intervention)
}

data_quadrant_plot4 <- function(){
  quadrant_data <- data.frame(get_data() %>% 
                        subset(select=c("intervention", "type", "stratname", "outcome", 
                                         "ntreated", "year", "total_diff", "total_diffpc", 
                                         "rii_diff", "rii_diffpc"))) %>% 
                        arrange(type, intervention)
 }
 
#Downloading data and plot
#Function to be able to save chart:
plotly_quadrant_plot4 <- function(){
   outcome<-as.character(data_quadrant_plot4()$outcome) 
   #If no data available for that period then plot message saying data is missing
   if (is.data.frame(data_quadrant_plot4()) && nrow(data_quadrant_plot4()) == 0)
   {
     plot_nodata()
   }
   else { #If data is available then plot it
     zoom <- event_data("plotly_relayout", "source")
     # if plot just rendered, event_data is NULL
     # if user double clicks for autozoom, then zoom$xaxis.autorange is TRUE
     # if user resizes page, then zoom$width is pixels of plot width
     if(is.null(zoom) || names(zoom[1]) %in% c("xaxis.autorange", "width")) {
       xmin <- min(data_quadrant_plot4()$total_diffpc)
       xmax <- max(data_quadrant_plot4()$total_diffpc)
       ymin <- min(data_quadrant_plot4()$rii_diffpc)
       ymax <- max(data_quadrant_plot4()$rii_diffpc)
     } else {
       xmin <- zoom$`xaxis.range[0]`
       xmax <- zoom$`xaxis.range[1]`
       ymin <- zoom$`yaxis.range[0]`
       ymax <- zoom$`yaxis.range[1]`
     }
     if ((min(data_quadrant_plot4()$rii_diffpc)*min(data_quadrant_plot4()$rii_diffpc))<(max(data_quadrant_plot4()$rii_diffpc)*max(data_quadrant_plot4()$rii_diffpc))) {
       if ((min(data_quadrant_plot4()$total_diffpc)*min(data_quadrant_plot4()$total_diffpc))>(max(data_quadrant_plot4()$total_diffpc)*max(data_quadrant_plot4()$total_diffpc))) {
         text=sprintf("Wider inequalities <br> + <br> worse health")
         annot_y=ymax/2
         annot_x=xmin/2
       } else {
         text=sprintf("Wider inequalities <br> + <br> improved health")
         annot_y=ymax/2
         annot_x=xmax/2
       }} else {
         if ((min(data_quadrant_plot4()$total_diffpc)*min(data_quadrant_plot4()$total_diffpc))>(max(data_quadrant_plot4()$total_diffpc)*max(data_quadrant_plot4()$total_diffpc))) {
           text=sprintf("Narrower inequalities <br> + <br> worse health")
           annot_y=ymin/2
           annot_x=xmin/2
         } else {
           text=sprintf("Narrower inequalities <br> + <br> improved health")
           annot_y=ymin/2
           annot_x=xmax/2
         }}
     trend_plot <- plot_ly(data = data_quadrant_plot4(), x = ~total_diffpc,
                           #                            y = ~sii_diffpc,
                           y = ~rii_diffpc,
                           type = 'scatter', 
                           mode = 'markers',
                           symbol = ~as.character(intervention),
                           symbols = int_symbols,
                           marker = list(size = 16),
                           color = ~as.character(intervention),
                           colors = int_colours) %>%
       add_annotations(yref='y',xref="x", y=annot_y, x=annot_x, showarrow = FALSE,
                       text = text,
                       font = list(size = 20)) %>%      
       #Layout 
       layout(
         autosize = T, 
         yaxis = list(title = "% widening of health inequalities", rangemode="tozero",  
                      size = 4, titlefont =list(size=20), fixedrange=FALSE, 
                      tickfont =list(size=20), tickformat ="", zeroline = T, zerolinewidth = 2
         ), 
         
         
         xaxis = list(title = ifelse(input$outcomename4=="Years of life lost",
                                     (paste0("% years of life saved")), 
                                     (sprintf("%s %s%s prevented", as.character("%"),
                                              tolower(substr(input$outcomename4, 1, 1)), 
                                              substr(input$outcomename4, 2, nchar(input$outcomename4))))    
         ),
         #tickformat does not work here to remove the unwanted micro formatting on smaller numbers
         tickfont =list(size=20), tickangle = 0, tickformat ="", fixedrange=FALSE, zeroline = T, zerolinewidth = 2),
         shapes = list(
           if (ymin < 0 ) {
             list(type = "rect",
                  fillcolor = "green", line = list(color = "green"), opacity = 0.1,
                  x0 = 0, x1 = xmax*1.1, xref = "x",
                  y0 = ymin*1.1, y1 = 0, yref = "y")
           } else {
             list(type = "rect",
                  x0 = 0, x1 = 0, xref = "x",
                  y0 = 0, y1 = 0, yref = "y")
           }
         ),
         legend = list(font=list(size=16), bgcolor='#FFFFFF')
       ) %>%  
       config(displayModeBar = FALSE, displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
   }
 }

require(openxlsx)

   #this feeds into the button in the tool used for the download
   output$triple_i_cost_data.xlsx <- downloadHandler(
     filename <- paste0("triple_i_cost_data.xlsx", sep = ""), 
     content = function(file) {
       
       #create a new workbook
       wb <- createWorkbook()
       
       #Set styles
       general_style <- createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL) 
       general_style_wrap <- createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL,  wrapText = TRUE) 
       heading1_style <- createStyle(fontName = "Arial", fontSize = 16, fontColour = NULL, fgFill = NULL, halign = NULL, valign = NULL, textDecoration = "bold")
       heading2_style <- createStyle(fontName = "Arial", fontSize = 14, fontColour = "#2E77BB", textDecoration = "bold")
       heading2_shade_style <- createStyle(fontName = "Arial", fontSize = 14, fontColour = "#2E77BB", textDecoration = "bold", fgFill = "#D3D3D3")
       heading3_style <-createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL, textDecoration = "bold", fgFill = "#D3D3D3", border="TopBottom")
       heading3_style_wrap <-createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL, textDecoration = "bold", fgFill = "#D3D3D3", border="TopBottom", wrapText = TRUE)
       heading3_noshade_style <-createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL, textDecoration = "bold")
       heading4_style <-createStyle(fontName = "Arial", fontSize = 11, fontColour = NULL)
       border_style <- createStyle(border= c("top", "bottom", "left", "right") )
       integers <- createStyle(numFmt = "#,##0")
       dp2 <- createStyle(numFmt = "0.00")
       dp3 <- createStyle(numFmt = "0.000")
       #adding a new worksheet 
       addWorksheet(wb, "Background information", gridLines = FALSE)
       #populating the worksheet
       #adding a logo
       insertImage(wb, "Background information", "www/HSlogo.jpg", width = 1.2, height = 0.7, startRow = 1, startCol = 2, units = "in", dpi = 300)
       #writing as title    
       writeData(wb, "Background information", "Informing Interventions to reduce health Inequalities (Triple I) data download spreadsheet", startCol = 2, startRow = 5, xy = NULL)
       df <- c("What is Triple I?",
               "NHS Health Scotland's Informing Interventions to reduce health Inequalities (Triple I) project is a modelling study that brings together the best available evidence to estimate how different interventions might affect health and health inequalities over the next 20 years.  This online tool allows users to browse, visualise, and download Triple I results for their area of interest.",
               "",
               "Why Triple I?",
               "There are many separate pieces of evidence about how specific interventions affect the health of individuals, but relatively little evidence about how these interventions might affect health and health inequalities at the population level.  Triple I is part of NHS Health Scotland's approach to addressing these population-level evidence gaps.",
               "",
               "Who is Triple I for?",
               "Triple I provides national and local decision makers with interactive tools and interpreted findings to inform discussions and decisions about different interventions.  The study will also be of use to anyone with an interest in improving population health and reducing health inequalities.",
               "",
               "How should I use the Triple I results?",
               "The information in this spreadsheet should be interpreted in the context of the intervention definitions and the technical information about the modelling.  Triple I should be considered as one source of information that complements other decision-making resources.",
               "",
               "This spreadsheet contains these tabs:",
               "",
               "",
               "",
               "",
               "",
               "Where can I find out more about Triple I?",
               "",
               "",
               "",
               "",
               "",
               "",
               "",
               "",
               "",
               "How should I acknowledge this work?",
               "Please cite this work as:",
               "Pulford A, Richardson E, Agbato D, et al. Informing Interventions to reduce health Inequalities (Triple I): National overview report 2019. Edinburgh: NHS Health Scotland; 2019. http://www.healthscotland.scot/triplei")
       writeData(wb, "Background information", x = df, startRow = 7, startCol = 2)
       ## Internal Link- Text to display
       writeFormula(wb, "Background information", startRow = 20,  startCol = 2,
                    x = makeHyperlinkString(sheet = "Intervention cost-effectiveness", row = 1, col = 2,
                                            text = "Intervention cost-effectiveness (data from user-specified options)"))
       writeFormula(wb, "Background information", startRow = 21,  startCol = 2,
                    x = makeHyperlinkString(sheet = "Intervention effects", row = 1, col = 2,
                                            text = "Intervention effects (data from user-specified options)"))
       writeFormula(wb, "Background information", startRow = 22,  startCol = 2,
                    x = makeHyperlinkString(sheet = "Intervention definitions", row = 1, col = 2,
                                            text = "Intervention definitions"))
       writeFormula(wb, "Background information", startRow = 23,  startCol = 2,
                    x = makeHyperlinkString(sheet = "Option definitions", row = 1, col = 2,
                                            text = "Option definitions"))
       
       x <- c("http://www.healthscotland.scot/triplei", 
              "http://www.healthscotland.scot/triplei",
              "http://www.healthscotland.scot/triplei",
              "http://www.healthscotland.scot/triplei",
              "http://www.healthscotland.scot/triplei",
              "http://www.healthscotland.scot/triplei",
              "http://www.healthscotland.scot/triplei",
              "mailto:nhs.triple.i@nhs.net")
       names(x) <- c("NHS Health Scotland Triple I web page",
                     "National Report (2019)",
                     "Technical Report (2019)",
                     "Income-based policies briefing (2018)",
                     "Spreadsheet tools for individual interventions (2019)",
                     "Interactive results browser",
                     "Answers to Frequently Asked Questions",
                     "Contact us")
       class(x) <- "hyperlink"
       writeData(wb, sheet = "Background information", x = x, startRow = 26, startCol = 2)        
       addStyle(wb, "Background information", general_style_wrap, rows=5:40, cols=2, gridExpand = TRUE, stack = TRUE)
       setColWidths(wb, "Background information", cols = 1:2, widths = c(3,150), ignoreMergedCells=TRUE)
       addStyle(wb, "Background information", heading2_style, rows=c(5,7,10,13,16,25,35), cols=c(2,2,2,2,2,2,2), gridExpand = FALSE, stack = FALSE)
       
       #adding another worksheet, this time containing the cost comparison data
       addWorksheet(wb, "Intervention cost-effectiveness")
       writeData(wb, "Intervention cost-effectiveness", x = title_cost_data4(), startCol = 2)
       mergeCells(wb, "Intervention cost-effectiveness", cols = 2:9, rows = 1)
       addStyle(wb, "Intervention cost-effectiveness", heading1_style, rows=1, cols=2, gridExpand = FALSE, stack = FALSE)

       writeData(wb, "Intervention cost-effectiveness", x = "The user-selected options:", startRow = 3, startCol = 2)
       addStyle(wb, "Intervention cost-effectiveness", heading2_style, rows=3, cols=2, gridExpand = FALSE, stack = FALSE)

       writeData(wb, "Intervention cost-effectiveness", x = "Area:", startCol = 2, startRow = 5)
       writeData(wb, "Intervention cost-effectiveness", x = input$area4, startCol = 3, startRow = 5)

       writeData(wb, "Intervention cost-effectiveness", x = "Health outcome:", startCol = 2, startRow = 6)
       writeData(wb, "Intervention cost-effectiveness", x = input$outcomename4, startCol = 3, startRow = 6)

       writeData(wb, "Intervention cost-effectiveness", x = "Year of follow-up:", startCol = 2, startRow = 7)
       writeData(wb, "Intervention cost-effectiveness", x = input$year4, startCol = 3, startRow = 7)

       writeData(wb, "Intervention cost-effectiveness", x = "Intervention type:", startCol = 2, startRow = 8)
       writeData(wb, "Intervention cost-effectiveness", x = input$inttype4, startCol = 3, startRow = 8)
       
       writeData(wb, "Intervention cost-effectiveness", x = "Options for individual-level interventions:", startCol = 2, startRow = 9)

       writeData(wb, "Intervention cost-effectiveness", x = "Targeting strategy:", startCol = 2, startRow = 10)
       if (input$inttype %in% c("all", "Mitigate", "Prevent", "Local")) {
         writeData(wb, "Intervention cost-effectiveness", x = input$stratname4, startCol = 3, startRow = 10)
       } else {
         writeData(wb, "Intervention cost-effectiveness", x = "N/A", startCol = 3, startRow = 10)
       }
       writeData(wb, "Intervention cost-effectiveness", x = "Number or % of eligible population treated:", startCol = 2, startRow = 11)
       if (input$inttype %in% c("all", "Mitigate", "Prevent", "Local")) {
         writeData(wb, "Intervention cost-effectiveness", x = "Maximum eligible population (100%)", startCol = 3, startRow = 11)
       } else {
         writeData(wb, "Intervention cost-effectiveness", x = "N/A", startCol = 3, startRow = 11)
       }
       addStyle(wb, "Intervention cost-effectiveness", general_style_wrap, rows=5:11, cols=3, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Intervention cost-effectiveness", heading3_style, rows=5:11, cols=2, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Intervention cost-effectiveness", border_style, rows=5:11, cols=3, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Intervention cost-effectiveness", border_style, rows=5:11, cols=2, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Intervention cost-effectiveness", heading3_noshade_style, rows=c(9), cols=2:3, gridExpand = TRUE, stack = FALSE)

       writeData(wb, "Intervention cost-effectiveness",
                 x = "For each intervention we estimate the cost for a 1%-point improvement in the health outcome or narrowing of relative inequalities in the health outcome.",
                 startRow = 14, startCol = 2)
       writeData(wb, "Intervention cost-effectiveness",
                 x = "Please note that these interventions vary markedly in the level of investment required and their population reach.",
                 startRow = 15, startCol = 2)
       writeData(wb, "Intervention cost-effectiveness",
                 x = "Health inequalities are measured using the Relative Index of Inequality, or RII.",
                 startRow = 16, startCol = 2)
       writeData(wb, "Intervention cost-effectiveness",
                 x = "In this table positive values indicate improved health and narrower inequalities. ",
                 startRow = 17, startCol = 2)
       writeData(wb, "Intervention cost-effectiveness",
                 x = "N.B. If the intervention did not improve health or reduce inequality we did not estimate cost-effectiveness (indicated by #N/A).",
                 startRow = 18, startCol = 2)
       writeData(wb, "Intervention cost-effectiveness",
                 x = "The effects shown are cumulative: they combine the effects from year 2 (the first year of follow-up) to the specified year of follow-up. ",
                 startRow = 19, startCol = 2)
       
       addStyle(wb, "Intervention cost-effectiveness", general_style, rows=14:19, cols=2, gridExpand = TRUE, stack = TRUE)
       mergeCells(wb, "Intervention cost-effectiveness", cols = 2:9, rows = 14)
       mergeCells(wb, "Intervention cost-effectiveness", cols = 2:9, rows = 15)
       mergeCells(wb, "Intervention cost-effectiveness", cols = 2:9, rows = 16)
       mergeCells(wb, "Intervention cost-effectiveness", cols = 2:9, rows = 17)
       mergeCells(wb, "Intervention cost-effectiveness", cols = 2:9, rows = 18)
       mergeCells(wb, "Intervention cost-effectiveness", cols = 2:9, rows = 19)
       
       setColWidths(wb, "Intervention cost-effectiveness", cols = c(1:12), widths = c(3,44,30,19,7,17,20,11,17,20,21,21), ignoreMergedCells=TRUE)
       headings <- matrix(c("Intervention", "Type", "Health outcome", "Year", "Total intervention cost (\u00A3)", "Targeting strategy",
                            "No. treated", "% health outcomes prevented", "\u00A3 per 1%-point health outcomes prevented",
                            "% narrowing of relative inequalities in health outcome", "\u00A3 per 1%-point narrowing of relative inequalities in health outcome"), ncol=11, nrow=1)
         datalength <- nrow(get_costtable_data4())

         writeData(wb, "Intervention cost-effectiveness",
                   x = paste0("Cost-effectiveness data", sep=""),
                   startRow = 21, startCol = 2)
         addStyle(wb, "Intervention cost-effectiveness", heading2_style,
                  rows=21, cols=2, gridExpand = FALSE, stack = FALSE)

         writeData(wb, "Intervention cost-effectiveness", x = headings,
                   startRow = 23, startCol = 2,
                   colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
                   borders = c("surrounding"),
                   borderColour = getOption("openxlsx.borderColour", "black"),
                   borderStyle = getOption("openxlsx.borderStyle", "thin"),
                   withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ",")
         addStyle(wb, "Intervention cost-effectiveness", heading3_style_wrap,
                  rows=23, cols=2:12, gridExpand = FALSE, stack = TRUE)

         writeData(wb, "Intervention cost-effectiveness", get_costtable_data4(),
                   startRow = 24, startCol=2,
                   colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
                   borders = c("surrounding"),
                   borderColour = getOption("openxlsx.borderColour", "black"),
                   borderStyle = getOption("openxlsx.borderStyle", "thin"),
                   withFilter = FALSE, keepNA = TRUE, name = NULL, sep = ",")
         addStyle(wb, "Intervention cost-effectiveness", general_style,
                  rows=24:(24+datalength-1),
                  cols=2:12, gridExpand = TRUE, stack = TRUE)
         addStyle(wb, "Intervention cost-effectiveness", border_style,
                  rows=24:(24+datalength-1),
                  cols=2:12, gridExpand = TRUE, stack = TRUE)

         addStyle(wb, "Intervention cost-effectiveness", integers, rows=24:(24+datalength-1), cols=c(6,10,12), gridExpand = TRUE, stack = TRUE)
         addStyle(wb, "Intervention cost-effectiveness", dp3, rows=24:(24+datalength-1), cols=c(9,11), gridExpand = TRUE, stack = TRUE)
         
       # #adding another worksheet, this time containing the quadrant plot and its data
       addWorksheet(wb, "Intervention effects")
       quadrant <- plotly_quadrant_plot4()
       export(quadrant, file = "quadrant_plot4.png")
       title <- title_quadrant_plot4()
       writeData(wb, "Intervention effects", x = title, startCol = 2)
       mergeCells(wb, "Intervention effects", cols = 2:9, rows = 1)
       addStyle(wb, "Intervention effects", heading1_style, rows=1, cols=2, gridExpand = FALSE, stack = FALSE)

       writeData(wb, "Intervention effects", x = "The user-selected options:", startRow = 3, startCol = 2)
       addStyle(wb, "Intervention effects", heading2_style, rows=3, cols=2, gridExpand = FALSE, stack = FALSE)

       writeData(wb, "Intervention effects", x = "Area:", startCol = 2, startRow = 5)
       writeData(wb, "Intervention effects", x = input$area4, startCol = 3, startRow = 5)

       writeData(wb, "Intervention effects", x = "Health outcome:", startCol = 2, startRow = 6)
       writeData(wb, "Intervention effects", x = input$outcomename4, startCol = 3, startRow = 6)

       writeData(wb, "Intervention effects", x = "Year of follow-up:", startCol = 2, startRow = 7)
       writeData(wb, "Intervention effects", x = input$year4, startCol = 3, startRow = 7)

       writeData(wb, "Intervention effects", x = "Intervention type(s):", startCol = 2, startRow = 8)
       writeData(wb, "Intervention effects", x = input$inttype4, startCol = 3, startRow = 8)

       writeData(wb, "Intervention effects", x = "Options for individual-level interventions:", startCol = 2, startRow = 9)

       writeData(wb, "Intervention effects", x = "Targeting strategy:", startCol = 2, startRow = 10)
       if (input$inttype4 %in% c("all", "Mitigate", "Prevent", "Local")) {
         writeData(wb, "Intervention effects", x = input$stratname4, startCol = 3, startRow = 10)
       } else {
         writeData(wb, "Intervention effects", x = "N/A", startCol = 3, startRow = 10)
       }
       treatoptionslist <- sort(unique(iiidata$treatoptions))
       treat <- treatoptionslist[as.integer(input$treatoptions)]

       writeData(wb, "Intervention effects", x = "Number or % of eligible population treated:", startCol = 2, startRow = 11)
       if (input$inttype4 %in% c("all", "Mitigate", "Prevent", "Local")) {
         writeData(wb, "Intervention effects", x = "Maximum eligible population (100%)", startCol = 3, startRow = 11)
       } else {
         writeData(wb, "Intervention effects", x = "N/A", startCol = 3, startRow = 11)
       }

       addStyle(wb, "Intervention effects", general_style_wrap, rows=5:11, cols=3, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Intervention effects", heading3_style, rows=5:11, cols=2, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Intervention effects", border_style, rows=5:11, cols=3, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Intervention effects", border_style, rows=5:11, cols=2, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Intervention effects", heading3_noshade_style, rows=c(9), cols=2:3, gridExpand = TRUE, stack = FALSE)

       writeData(wb, "Intervention effects", x = "The quadrant chart:", startRow = 13, startCol = 2)
       addStyle(wb, "Intervention effects", heading2_style, rows=13, cols=2, gridExpand = FALSE, stack = FALSE)

       writeData(wb, "Intervention effects",
                 x = "This quadrant chart shows how interventions are predicted to affect health (on the horizontal axis) and health inequalities (on the vertical axis).",
                 startRow = 14, startCol = 2)
       writeData(wb, "Intervention effects",
                 x = "Interpretation is based on which quadrant the intervention appears in: interventions in the bottom right quadrant both improve health and reduce health inequalities.",
                 startRow = 15, startCol = 2)
       writeData(wb, "Intervention effects",
                 x = "Please note that these interventions vary markedly in the level of investment required and their population reach.",
                 startRow = 16, startCol = 2)
       writeData(wb, "Intervention effects",
                 x = "Health inequalities are measured using the Relative Index of Inequality, or RII.",
                 startRow = 17, startCol = 2)
       writeData(wb, "Intervention effects",
                 x = "Positive values for the health outcome indicate improved health, while negative values for inequalities indicate narrower inequalities. ",
                 startRow = 18, startCol = 2)
       writeData(wb, "Intervention effects",
                 x = "The effects shown are cumulative: they combine the effects from year 2 (the first year of follow-up) to the specified year of follow-up. ",
                 startRow = 19, startCol = 2)
       
       addStyle(wb, "Intervention effects", general_style, rows=14:19, cols=2, gridExpand = TRUE, stack = TRUE)
       mergeCells(wb, "Intervention effects", cols = 2:9, rows = 14)
       mergeCells(wb, "Intervention effects", cols = 2:9, rows = 15)
       mergeCells(wb, "Intervention effects", cols = 2:9, rows = 16)
       mergeCells(wb, "Intervention effects", cols = 2:9, rows = 17)
       mergeCells(wb, "Intervention effects", cols = 2:9, rows = 18)
       mergeCells(wb, "Intervention effects", cols = 2:9, rows = 19)
       
       insertImage(wb, "Intervention effects", "quadrant_plot4.png",
                   startRow = 21, startCol = 2, width = 8, height = 6, units = "in", dpi = 1000)
       writeData(wb, "Intervention effects", x = "The data for the chart:", startRow = 52, startCol = 2)
       addStyle(wb, "Intervention effects", heading2_style, rows=52, cols=2, gridExpand = FALSE, stack = FALSE)

       writeData(wb, "Intervention effects", x = matrix(c("Intervention", "Type", "Targeting strategy",
                                                          "Health outcome", "No. treated", "Year", "No. health outcomes prevented",
                                                          "% health outcomes prevented","Widening of relative inequalities in health outcome (RII)", "% widening of relative inequalities in health outcome"),
                                                        ncol=10, nrow=1), startRow = 54, startCol = 2,
                 colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
                 borders = c("surrounding"),
                 borderColour = getOption("openxlsx.borderColour", "black"),
                 borderStyle = getOption("openxlsx.borderStyle", "thin"),
                 withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ",")

       addStyle(wb, "Intervention effects", heading3_style_wrap, rows=54, cols=2:11, gridExpand = FALSE, stack = TRUE)
       setColWidths(wb, "Intervention effects", cols = 2:3, widths = "auto", ignoreMergedCells=TRUE)
       setColWidths(wb, "Intervention effects", cols = c(1, 4:11), widths = c(3, 22, 19, 14, 8, 29, 29, 29, 30), ignoreMergedCells=TRUE)
       writeData(wb, "Intervention effects", data_quadrant_plot4(), startRow = 55, startCol=2,
                 colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
                 borders = c("surrounding"),
                 borderColour = getOption("openxlsx.borderColour", "black"),
                 borderStyle = getOption("openxlsx.borderStyle", "thin"),
                 withFilter = FALSE, keepNA = TRUE, name = NULL, sep = ",")
       rownum4 <- nrow(data_quadrant_plot4())+54
       addStyle(wb, "Intervention effects", general_style, rows=55:rownum4, cols=2:11, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Intervention effects", border_style, rows=55:rownum4, cols=2:11, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Intervention effects", integers, rows=55:rownum4, cols=6, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Intervention effects", dp3, rows=55:rownum4, cols=c(8,9,11), gridExpand = TRUE, stack = TRUE)
       
       #adding another worksheet containing the intervention definitions
       addWorksheet(wb, "Intervention definitions")
       writeData(wb, "Intervention definitions", x = "Definitions of the interventions", startRow = 1, startCol = 2)
       mergeCells(wb, "Intervention definitions", cols = 2:3, rows = 1)
       addStyle(wb, "Intervention definitions", heading1_style, rows=1, cols=2, gridExpand = FALSE, stack = TRUE)
       writeData(wb, "Intervention definitions", x = matrix(c("Intervention", "Definition"), 
                                                            nrow=1, ncol=2), startRow = 3, startCol = 2, 
                 colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
                 borders = c("surrounding"),
                 borderColour = getOption("openxlsx.borderColour", "black"),
                 borderStyle = getOption("openxlsx.borderStyle", "thin"),
                 withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ",")
       
       #    writeData(wb, "Intervention definitions", intervention_list(), startRow = 3, startCol = 2)      
       interventionlist <- intervention_list()
       writeData(wb, "Intervention definitions", interventionlist, startRow = 4, startCol=2,
                 colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
                 borders = c("surrounding"),
                 borderColour = getOption("openxlsx.borderColour", "black"),
                 borderStyle = getOption("openxlsx.borderStyle", "thin"),
                 withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ",")
       
       setColWidths(wb, "Intervention definitions", cols=1:3, widths = c(3, 50, 108)) ## set column width for row names column
       addStyle(wb, "Intervention definitions", heading3_style, rows=3, cols=2:3, gridExpand = TRUE, stack = TRUE)
       rownum3 <- nrow(interventionlist)+3
       addStyle(wb, "Intervention definitions", heading3_style, rows=4:rownum3, cols=2, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Intervention definitions", general_style_wrap, rows=4:rownum3, cols=3, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Intervention definitions", border_style, rows=4:rownum3, cols=2, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Intervention definitions", border_style, rows=4:rownum3, cols=3, gridExpand = TRUE, stack = TRUE)  
       
       #adding another worksheet containing the option definitions
       addWorksheet(wb, "Option definitions")
       writeData(wb, "Option definitions", x = "Definitions of the options", startRow = 1, startCol = 2)
       mergeCells(wb, "Option definitions", cols = 2:3, rows = 1)
       addStyle(wb, "Option definitions", heading1_style, rows=1, cols=2, gridExpand = FALSE, stack = TRUE)
       writeData(wb, "Option definitions", x = matrix(c("Option", "Definition"), 
                                                      nrow=1, ncol=2), startRow = 3, startCol = 2, 
                 colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
                 borders = c("surrounding"),
                 borderColour = getOption("openxlsx.borderColour", "black"),
                 borderStyle = getOption("openxlsx.borderStyle", "thin"),
                 withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ",")
       
       writeData(wb, "Option definitions", x = "Health outcomes", startRow = 5, startCol = 2)
       
       writeData(wb, "Option definitions", x = "Premature deaths", startRow = 6, startCol = 2)
       writeData(wb, "Option definitions", x = "Deaths before the age of 75.", startRow = 6, startCol = 3)
       
       writeData(wb, "Option definitions", x = "Years of life lost", startRow = 7, startCol = 2)
       writeData(wb, "Option definitions", x = "An estimate of the years a person would have lived if they had not died prematurely (based on life expectancy).", startRow = 7, startCol = 3)
       
       writeData(wb, "Option definitions", x = "Hospital stays", startRow = 8, startCol = 2)
       writeData(wb, "Option definitions", x = "Continuous inpatient stays in general or psychiatric hospitals.", startRow = 8, startCol = 3)
       
       writeData(wb, "Option definitions", x = "Year of follow up", startRow = 10, startCol = 2)
       writeData(wb, "Option definitions", x = "All interventions start in year 1, and follow up begins in year 2.  Income-based, 20 mph limits, and tobacco taxation interventions are permanent, while all other interventions are implemented in year 1 only.  Health outcomes are calculated cumulatively over the period.", startRow = 10, startCol = 3)
       
       writeData(wb, "Option definitions", x = "Intervention type", startRow = 12, startCol = 2)
       
       writeData(wb, "Option definitions", x = "Undo", startRow = 13, startCol = 2)
       writeData(wb, "Option definitions", x = "Interventions that act to undo the fundamental causes of health inequality (e.g., income inequality).", startRow = 13, startCol = 3)
       
       writeData(wb, "Option definitions", x = "Prevent", startRow = 14, startCol = 2)
       writeData(wb, "Option definitions", x = "Interventions that act to prevent harm from wider environmental influences (e.g., unemployment).", startRow = 14, startCol = 3)
       
       writeData(wb, "Option definitions", x = "Mitigate", startRow = 15, startCol = 2)
       writeData(wb, "Option definitions", x = "Interventions that act to mitigate against the negative impact of wider determinants on individuals' health (e.g., smoking).", startRow = 15, startCol = 3)
       
       writeData(wb, "Option definitions", x = "These additional options are available for interventions focused on individuals:", startRow = 17, startCol = 2)
       
       writeData(wb, "Option definitions", x = "Targeting strategy", startRow = 19, startCol = 2)
       
       writeData(wb, "Option definitions", x = "Even distribution", startRow = 20, startCol = 2)
       writeData(wb, "Option definitions", x = "The same proportion of each population subgroup (by age group, sex and deprivation) is treated.", startRow = 20, startCol = 3)
       
       writeData(wb, "Option definitions", x = "Proportionate to need", startRow = 21, startCol = 2)
       writeData(wb, "Option definitions", x = "The proportion of each subgroup treated is proportionate to the risk factor prevalence.", startRow = 21, startCol = 3)
       
       writeData(wb, "Option definitions", x = "Most deprived 20% only", startRow = 22, startCol = 2)
       writeData(wb, "Option definitions", x = "Only those in Scottish Index of Multiple Deprivation (SIMD) quintile 1 are treated (even distribution within this quintile).", startRow = 22, startCol = 3)
       
       writeData(wb, "Option definitions", x = "Most deprived 40% only", startRow = 23, startCol = 2)
       writeData(wb, "Option definitions", x = "Only those in SIMD quintiles 1 and 2 are treated (even distribution within this quintile).", startRow = 23, startCol = 3)
       
       writeData(wb, "Option definitions", x = "Number to treat", startRow = 25, startCol = 2)
       
       writeData(wb, "Option definitions", x = "Number", startRow = 26, startCol = 2)
       writeData(wb, "Option definitions", x = "Either 100, 1,000 or 100,000 interventions are delivered.  N.B. For areas and interventions with insufficient eligible population to treat this number no data will be plotted.", startRow = 26, startCol = 3)
       
       writeData(wb, "Option definitions", x = "Percentage", startRow = 27, startCol = 2)
       writeData(wb, "Option definitions", x = "A percentage of the population 'eligible' to receive the intervention is treated.", startRow = 27, startCol = 3)
       
       setColWidths(wb, "Option definitions", cols=1:3, widths = c(3, 50, 108)) ## set column width for row names column
       addStyle(wb, "Option definitions", heading2_shade_style, rows=3, cols=2:3, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Option definitions", heading2_shade_style, rows=c(5,10,12,19,25), cols=2, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Option definitions", heading3_style, rows=c(6:8, 13:15, 20:23, 26:27), cols=2, gridExpand = TRUE, stack = FALSE)
       addStyle(wb, "Option definitions", heading3_noshade_style, rows=c(4, 9, 11, 16:18, 24), cols=2:3, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Option definitions", general_style_wrap, rows=4:27, cols=3, gridExpand = TRUE, stack = TRUE)
       addStyle(wb, "Option definitions", border_style, rows=c(5:8, 10, 12:15, 19:23, 25:27), cols=2:3, gridExpand = TRUE, stack = TRUE)  
       
       saveWorkbook(wb, file, overwrite = TRUE)
     }
   )
   
#####################################.    
#### Table ----
####################################.
## Help pop-up modal dialog 
   observeEvent(input$help_table, {
     showModal(modalDialog(
       title = "How to use the 'Data' page",
       fluidRow(h4(tags$b("Triple I results for the options selected by the user can be displayed and downloaded on this page."))),
       fluidRow(h5(tags$b(tags$u("Choosing the data")))),
       fluidRow(h6("You can change the default options using the dropdown menus to select the area(s), health outcome(s), intervention(s) and follow-up period(s) of interest.  Multiple interventions and health outcomes can be selected, as well as one or all of the areas and follow-up periods.", 
                   style = "color:black; font-weight: bold;")),
       fluidRow(
         h6("Health outcomes:", style = "color:black; font-weight: bold;"),
         tags$ul( 
           tags$li(class= "li-custom", tags$b("Premature deaths:"),
                   "deaths before the age of 75."),
           tags$li(class= "li-custom", tags$b("Years of life lost:"), " an estimate of the years a person would have lived if they had not died prematurely (based on life expectancy)."),
           tags$li(class= "li-custom", tags$b("Hospital stays:"), " continuous inpatient stays in general or psychiatric hospitals."))),
       fluidRow(
         h6("Years of follow up:", style = "color:black; font-weight: bold;"),
         tags$ul( 
           tags$li(class= "li-custom", "All interventions start in year 1."),
           tags$li(class= "li-custom", "Income-based, 20 mph limits, and tobacco taxation interventions are permanent."),
           tags$li(class= "li-custom", "All other interventions are implemented in year 1 only."),
           tags$li(class= "li-custom", "Follow up begins in year 2."),
           tags$li(class= "li-custom", "Outcomes are calculated cumulatively over the period.")
         )),
       fluidRow(
         h6("Interventions:", style = "color:black; font-weight: bold;"),
         tags$ul( 
           tags$li(class= "li-custom", "Definitions of the interventions can be found using the 'Intervention definitions' button.", 
                   img(src='definitionsbutton.png', height=40, style="padding-left: 2px; vertical-align:middle;"))
         )),
       fluidRow(
         h6("And for interventions focused on individuals (e.g., smoking cessation or weight management services) you can also change the intervention targeting strategy and number treated.  Multiple options can also be selected.", style = "color:black; font-weight: bold;"),  
         h6(tags$b("Targeting strategy:", style = "color:black; font-weight: bold;"), "Individual-level interventions can be implemented in different ways:"),
         tags$ul( 
           tags$li(class= "li-custom", tags$b("Even distribution:"),
                   "Treat the same proportion of each population subgroup (by age group, sex and deprivation)."),
           tags$li(class= "li-custom", tags$b("Proportionate to need:"), " The proportion of each subgroup treated is proportionate to the risk factor prevalence."),
           tags$li(class= "li-custom", tags$b("Most deprived 20% only:"), " Treat only those in Scottish Index of Multiple Deprivation (SIMD) quintile 1 (even distribution within this quintile)."),
           tags$li(class= "li-custom", tags$b("Most deprived 40% only:"), " Treat only those in SIMD quintiles 1 and 2 (even distribution within this quintile).")
         )),
       fluidRow(
         h6("Number to treat:", style = "color:black; font-weight: bold;"),
         tags$ul( 
           tags$li(class= "li-custom", tags$b("Number:"), " Select 100, 1,000 or 100,000. ", 
                   "N.B. For individual-level interventions with insufficient 'eligible' population in the chosen area to treat this number no data will be available.  The eligible population is defined as the maximum number of people who could be expected to benefit from the intervention (e.g., smokers who displayed motivation to quit, or unemployed people willing and able to take up a job)."),
           tags$li(class= "li-custom", tags$b("Percentage:"),"Select a percentage of the 'eligible' population.")
         )),
       br(),
       fluidRow(
         h5(tags$b(tags$u("Using and interpreting the data")))),  
       fluidRow(  
         tags$ul(
           tags$li(class= "li-custom", h6("The contents of the table can be sorted by a particular column by clicking on the grey arrowheads at the top of the column.",
                                          style = "color:black; font-weight: bold;")),
           tags$li(class= "li-custom", h6("More columns are available in the data download.  These are intervention type, total intervention cost, cost per 1% reduction in health outcome, and cost per 1% narrowing of inequalities.",
                                          style = "color:black; font-weight: bold;")),
           tags$li(class= "li-custom", h6("Health inequalities are measured using the Relative Index of Inequality, or RII.  
                                          The RII is a summary measure of inequality which takes into account differences across the whole deprivation gradient, 
                                          not just the gap in health outcome between the most and least deprived.", style = "color:black; font-weight: bold;")),
           tags$li(class= "li-custom", h6("Positive values for costs, health outcomes or health inequalities represent costs incurred (as opposed to savings),
                                          health outcomes prevented or health inequalities narrowed, respectively.", style = "color:black; font-weight: bold;"))
           )),
       br(),
       fluidRow(h5(tags$b(tags$u("Downloading the data")))),  
       fluidRow(
         tags$ul(
           tags$li(class= "li-custom", "You can download the data in the table using the 'Download selected data' button.  
                   The file produced is an Excel spreadsheet, and includes background information about the project and the data.  
                   It is possible to select all the results from the browser options (approximately 800,000 records), but this results in a very large file that may take more than 10 minutes to download.", 
                   style = "color:black; font-weight: bold;", img(src='downloadbutton2.png', height=100, border="1px", color="black", style="padding-right: 2px; vertical-align:middle;"))
         )),
       easyClose = TRUE, fade=FALSE
       ))
   })
###############################################.
# Intervention definitions
  output$defs_data <- renderUI({
    HTML(paste(sprintf("<b><u>%s</b></u> <br> %s ", 
                       interventions_lookup$intervention[interventions_lookup$include==1],
                       interventions_lookup$definition[interventions_lookup$include==1]
    ), collapse = "<br><br>"))
  })
  
    
filter_table <- reactive ({
  treatoptionslist <- sort(unique(iiidata$treatoptions))
  iiidata$area <- as.character(iiidata$area) 
  iiidata$areaname <- as.character(iiidata$areaname) 
#AREAS:
    #if 'all available areas' or no area selected (currently default is Scotland though):
    if (input$all_areas == TRUE|is.null(input$area3)) {
      filtered_data <- iiidata 
#if an area has been selected (NB no multiples allowed)
    } else  {
    # match selected area to its equivalent area in the data (e.g., Borders LA if Borders HB is selected)
        index <- match(input$area3, areas_lookup$inputarea)
        equivarea <- areas_lookup[na.omit(index), ]
        eq <- as.character(equivarea$areacode)
        filtered_data <- iiidata %>% filter(area == eq)
        filtered_data$areaname[filtered_data$area==eq] <- input$area3 
        } 

#INTERVENTIONS    
    #if 'all available interventions' or no options selected:
    if (input$all_ints == TRUE|is.null(input$interventions3)){
      filtered_data2 <- filtered_data 
      #if intervention(s) selected 
      } else {
          filtered_data2 <- filtered_data %>% filter(intervention %in% input$interventions3)  
          } 
#OUTCOMES    
    #if 'all available outcomes' or no options selected:
    if (input$all_outs == TRUE|is.null(input$outcomename3)) {
      filtered_data3 <- filtered_data2 
      #if outcome(s) selected 
    } else {
        filtered_data3 <- filtered_data2 %>% filter(outcome %in% input$outcomename3)  
        } 
    

#YEARS    
    #if 'all years' or no options selected:
    if (input$all_years == TRUE|is.null(input$year3)) {
      filtered_data4 <- filtered_data3 
      #if year(s) selected 
    } else {
      filtered_data4 <- filtered_data3 %>% filter(year %in% input$year3)  
      } 
    
#STRATEGIES    
    #if 'all strategies' or no strategies selected:
    if (input$all_strats == TRUE|is.null(input$stratname3)) {
      filtered_data5 <- filtered_data4 
      #if strategy(/ies) selected 
    } else {
      strats <- c(input$stratname3, "Population-wide")  
      filtered_data5 <- filtered_data4 %>% filter(stratname %in% strats)  
      } 
    
#TREATED NUMBER OPTIONS    
    #if 'all available numbers' or no options selected:
    if (input$all_nums == TRUE|is.null(input$treatoptions3)) {
      filtered_data6 <- filtered_data5 
      #if treated number options(s) selected 
    } else {
      o <- as.integer(input$treatoptions3)
      options <- c(o, "1")
      filtered_data6 <- filtered_data5 %>% filter(treatoptions %in% treatoptionslist[as.integer(options)])  
    } 
  filtered_data6$ntreated[is.na(filtered_data6$ntreated)] <- "Population"
  
  table <- filtered_data6 %>% subset(select=c("areaname", "intervention", "outcome","year", 
                                             "stratname","treatoptions", "ntreated", 
                                             "total_diff", "total_diffpc",
                                             "rii_diff","rii_diffpc"))
  })   

#display table based on selection made by user 
  container = htmltools::withTags(table(class="compact stripe", style="text-align:left", 
    thead(
      tr(
        th(colspan = 7),
        th(colspan = 2, 'Health outcome', style="background-color:#ecf0f2"),
        th(colspan = 2, 'Relative inequality in health outcome (RII)', style="background-color:#ecf0f2")
      ),
  tr(
    th('Area', style="background-color:#ecf0f2"),
    th('Intervention', style="background-color:#ecf0f2"),
    th('Outcome', style="background-color:#ecf0f2"),
    th('Follow up (year)', style="background-color:#ecf0f2; text-align:left"),
    th('Targeting strategy', style="background-color:#ecf0f2"),
    th('Number, percent or population treated?', style="background-color:#ecf0f2"),
    th('Number treated', style="background-color:#ecf0f2"),
    th('N fewer outcomes', style="background-color:#ecf0f2; text-align:left"),
    th('% fewer health outcomes', style="background-color:#ecf0f2; text-align:left"),
    th('Widening of inequality  (RII)', style="background-color:#ecf0f2; text-align:left"),
    th('% widening of inequality', style="background-color:#ecf0f2; text-align:left")
  ))
))

output$table_filtered <- DT::renderDataTable({
    DT::datatable(filter_table(),
                  rownames = FALSE, options = list(dom = 'tp', pageLength = 25), 
                  class = 'table-condensed table-bordered table-striped', container = container) %>%
                  formatRound(columns=c("ntreated"), digits=0) %>%
                  formatRound(columns=c("total_diff"), digits=0) %>%
                  formatRound(columns=c("rii_diff"), digits=4) %>%
                  formatRound(columns=c("total_diffpc"), digits=3) %>%
                  formatRound(columns=c("rii_diffpc"), digits=3)
    })

  #Clearing all user inputs to default
  observeEvent(input$clear, {
    updateCheckboxInput(session, "all_areas", label = NULL, value = FALSE)
    updateCheckboxInput(session, "all_ints", label = NULL, value = FALSE)
    updateCheckboxInput(session, "all_outs", label = NULL, value = FALSE)
    updateCheckboxInput(session, "all_years", label = NULL, value = FALSE)
    updateCheckboxInput(session, "all_strats", label = NULL, value = FALSE)
    updateCheckboxInput(session, "all_nums", label = NULL, value = FALSE)
    updateSelectizeInput(session, "area3", choices = list( 'Scotland',
                                                           `Health boards`= c(hb_names),
                                                           `Council areas`= c(la_names),
                                                           `City regions`= c(deal_names),
                                                           `Integrated Joint Boards`= c(ijb_names)), 
                         label = NULL, selected = NULL, options = NULL)
    updateSelectizeInput(session, "interventions3", choices = interventions_lookup$intervention[interventions_lookup$include==1],
                         label = NULL, selected = NULL, options = NULL)
    updateSelectizeInput(session, "outcomename3", choices = c("Premature deaths", "Years of life lost", "Hospital stays"),
                         label = NULL, selected = NULL, options = NULL)
    updateSelectizeInput(session, "stratname3", choices = c("Even distribution",
                                                            "Proportionate to need", 
                                                            "Most deprived 20% only",
                                                            "Most deprived 40% only"),
                         label = NULL, selected = NULL, options = NULL)
    updateSelectizeInput(session, "treatoptions3", choices = 
                           list(`Absolute number:` 
                                = c("number: 10","number: 100","number: 1,000",
                                    "number: 10,000","number: 100,000"),
                                `% of eligible population:` = 
                                  c("percent: 1", "percent: 5", "percent: 10", 
                                    "percent: 25", "percent: 50", "percent: 100")),  
                         label = NULL, selected = NULL, options = NULL)
  })

#####################################.    
#### Excel download of data extract table ----
####################################.
# Now make data and definitions for the excel download

  data_table <- reactive ({
    treatoptionslist <- sort(unique(iiidata$treatoptions))
    iiidata$area <- as.character(iiidata$area) 
    iiidata$areaname <- as.character(iiidata$areaname) 
    #AREAS:
    #if 'all available areas' or no area selected (currently default is Scotland though):
    if (input$all_areas == TRUE|is.null(input$area3)) {
      filtered_data <- iiidata 
      #if an area has been selected (NB no multiples allowed)
    } else  {
      # match selected area to its equivalent area in the data (e.g., Borders LA if Borders HB is selected)
      index <- match(input$area3, areas_lookup$inputarea)
      equivarea <- areas_lookup[na.omit(index), ]
      eq <- as.character(equivarea$areacode)
      filtered_data <- iiidata %>% filter(area == eq)
      filtered_data$areaname[filtered_data$area==eq] <- input$area3 
    } 
    #INTERVENTIONS    
    #if 'all available interventions' or no options selected:
    if (input$all_ints == TRUE|is.null(input$interventions3)){
      filtered_data2 <- filtered_data 
      #if intervention(s) selected 
    } else {
      filtered_data2 <- filtered_data %>% filter(intervention %in% input$interventions3)  
    } 
    #OUTCOMES    
    #if 'all available outcomes' or no options selected:
    if (input$all_outs == TRUE|is.null(input$outcomename3)) {
      filtered_data3 <- filtered_data2 
      #if outcome(s) selected 
    } else {
      filtered_data3 <- filtered_data2 %>% filter(outcome %in% input$outcomename3)  
    } 
    #YEARS    
    #if 'all years' or no options selected:
    if (input$all_years == TRUE|is.null(input$year3)) {
      filtered_data4 <- filtered_data3 
      #if year(s) selected 
    } else {
      filtered_data4 <- filtered_data3 %>% filter(year %in% input$year3)  
    } 
    #STRATEGIES    
    #if 'all strategies' or no strategies selected:
    if (input$all_strats == TRUE|is.null(input$stratname3)) {
      filtered_data5 <- filtered_data4 
      #if strategy(/ies) selected 
    } else {
      strats <- c(input$stratname3, "Population-wide")  
      filtered_data5 <- filtered_data4 %>% filter(stratname %in% strats)  
    } 
    #TREATED NUMBER OPTIONS    
    #if 'all available numbers' or no options selected:
    if (input$all_nums == TRUE|is.null(input$treatoptions3)) {
      filtered_data6 <- filtered_data5 
      #if treated number options(s) selected 
    } else {
      o <- as.integer(input$treatoptions3)
      options <- c(o, "1")
#      filtered_data6$ntreated[is.na(filtered_data6$ntreated)] <- "Population"
      filtered_data6 <- filtered_data5 %>% filter(treatoptions %in% treatoptionslist[as.integer(options)])  
    } 
    
    filtered_data6$total_diffpc <- round(filtered_data6$total_diffpc, digits=3)
    filtered_data6$rii_diffpc <- round(filtered_data6$rii_diffpc, digits=3)
    filtered_data6$total_diff <- round(filtered_data6$total_diff, digits=2)
    filtered_data6$costperriipc <- filtered_data6$costperriipc*-1
    filtered_data6$costperriipc[filtered_data6$rii_diff> 0] <- NA
    filtered_data6$costperoutcomepc[filtered_data6$total_diff< 0] <- NA
    filtered_data6$ntreated[is.na(filtered_data6$ntreated)] <- "Population"
    table <- filtered_data6 %>% subset(select=c("areaname", "intervention", "type", "outcome","year", 
                                                "stratname","treatoptions", "ntreated", "interventioncost", 
                                                "total_diff", "total_diffpc",
                                                "rii_diff","rii_diffpc",
                                                "costperoutcomepc", "costperriipc"))
  })   

  intervention_list_data <- function(){
    lookup <- interventions_lookup[interventions_lookup$include==1, c("intervention", "definition")]
  }

  area_lookup_tab <- function(){
    lookup <- areas_lookup[order(areas_lookup$inputarea, areas_lookup$areaname), c("inputarea", "areaname")]
  }

  require(openxlsx)
  
  #this feeds into the button in the tool used for the download
  output$triple_i_data_extract.xlsx <- downloadHandler(
    filename = "triple_i_data_extract.xlsx",
    content = function(file) {
      #create a new workbook
      wb <- createWorkbook()
      #Set styles
      general_style <- createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL) 
      general_style_wrap <- createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL,  wrapText = TRUE) 
      heading1_style <- createStyle(fontName = "Arial", fontSize = 16, fontColour = NULL, fgFill = NULL, halign = NULL, valign = NULL, textDecoration = "bold")
      heading2_style <- createStyle(fontName = "Arial", fontSize = 14, fontColour = "#2E77BB", textDecoration = "bold")
      heading2_shade_style <- createStyle(fontName = "Arial", fontSize = 14, fontColour = "#2E77BB", textDecoration = "bold", fgFill = "#D3D3D3")
      heading3_style <-createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL, textDecoration = "bold", fgFill = "#D3D3D3", border="TopBottom")
      heading3_style_wrap <-createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL, textDecoration = "bold", fgFill = "#D3D3D3", border="TopBottom", wrapText = TRUE)
      heading3_noshade_style <-createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL, textDecoration = "bold")
      heading4_style <-createStyle(fontName = "Arial", fontSize = 11, fontColour = NULL)
      border_style <- createStyle(border= c("top", "bottom", "left", "right") )
      integers <- createStyle(numFmt = "#,##0")
      dp2 <- createStyle(numFmt = "0.00")
      dp3 <- createStyle(numFmt = "0.000")
      
      #adding a new worksheet 
      addWorksheet(wb, "Background information", gridLines = FALSE)
      #populating the worksheet
      #adding a logo
      insertImage(wb, "Background information", "www/HSlogo.jpg", width = 1.2, height = 0.7, startRow = 1, startCol = 2, units = "in", dpi = 300)
      #writing as title    
      writeData(wb, "Background information", "Informing Interventions to reduce health Inequalities (Triple I) data download spreadsheet", startCol = 2, startRow = 5, xy = NULL)
      df <- c("What is Triple I?",
              "NHS Health Scotland's Informing Interventions to reduce health Inequalities (Triple I) project is a modelling study that brings together the best available evidence to estimate how different interventions might affect health and health inequalities over the next 20 years.  This online tool allows users to browse, visualise, and download Triple I results for their area of interest.",
              "",
              "Why Triple I?",
              "There are many separate pieces of evidence about how specific interventions affect the health of individuals, but relatively little evidence about how these interventions might affect health and health inequalities at the population level.  Triple I is part of NHS Health Scotland's approach to addressing these population-level evidence gaps.",
              "",
              "Who is Triple I for?",
              "Triple I provides national and local decision makers with interactive tools and interpreted findings to inform discussions and decisions about different interventions.  The study will also be of use to anyone with an interest in improving population health and reducing health inequalities.",
              "",
              "How should I use the Triple I results?",
              "The information in this spreadsheet should be interpreted in the context of the intervention definitions and the technical information about the modelling.  Triple I should be considered as one source of information that complements other decision-making resources.",
              "",
              "This spreadsheet contains these tabs:",
              "",
              "",
              "",
              "",
              "",
              "",
              "Where can I find out more about Triple I?",
              "",
              "",
              "",
              "",
              "",
              "",
              "",
              "",
              "How should I acknowledge this work?",
              "Please cite this work as:",
              "Pulford A, Richardson E, Agbato D, et al. Informing Interventions to reduce health Inequalities (Triple I): National overview report 2019. Edinburgh: NHS Health Scotland; 2019. http://www.healthscotland.scot/triplei",
              ""
              )
      writeData(wb, "Background information", x = df, startRow = 7, startCol = 2)
      ## Internal Link- Text to display
      writeFormula(wb, "Background information", startRow = 20,  startCol = 2,
                   x = makeHyperlinkString(sheet = "Selected options", row = 1, col = 2,
                                           text = "Selected options (the options selected by the user)"))
      writeFormula(wb, "Background information", startRow = 21,  startCol = 2,
                   x = makeHyperlinkString(sheet = "Data extract", row = 1, col = 2,
                                           text = "Data extract (based on user-specified options)"))
      writeFormula(wb, "Background information", startRow = 22,  startCol = 2,
                   x = makeHyperlinkString(sheet = "Intervention definitions", row = 1, col = 2,
                                           text = "Intervention definitions"))
      writeFormula(wb, "Background information", startRow = 23,  startCol = 2,
                   x = makeHyperlinkString(sheet = "Option definitions", row = 1, col = 2,
                                           text = "Option definitions"))
      writeFormula(wb, "Background information", startRow = 24,  startCol = 2,
                   x = makeHyperlinkString(sheet = "Area lookup", row = 1, col = 2,
                                           text = "Area lookup (to match areas if 'all areas' have been downloaded)"))
      
      x <- c("http://www.healthscotland.scot/triplei", 
             "http://www.healthscotland.scot/triplei",
             "http://www.healthscotland.scot/triplei",
             "http://www.healthscotland.scot/triplei",
             "http://www.healthscotland.scot/triplei",
             "http://www.healthscotland.scot/triplei",
             "http://www.healthscotland.scot/triplei",
             "mailto:nhs.triple.i@nhs.net")
      names(x) <- c("NHS Health Scotland Triple I web page",
                    "National Report (2019)",
                    "Technical Report (2019)",
                    "Income-based policies briefing (2018)",
                    "Spreadsheet tools for individual interventions (2019)",
                    "Interactive results browser",
                    "Answers to Frequently Asked Questions",
                    "Contact us")
      class(x) <- "hyperlink"
      writeData(wb, sheet = "Background information", x = x, startRow = 26, startCol = 2)        
      addStyle(wb, "Background information", general_style_wrap, rows=5:50, cols=2, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, "Background information", heading2_style, rows=c(5,7,10,13,16,26,35,39), cols=c(2,2,2,2,2,2,2,2), gridExpand = FALSE, stack = FALSE)

     setColWidths(wb, "Background information", cols = 1:2, widths = c(3,150), ignoreMergedCells=TRUE)
      
      #adding another worksheet, this time containing the user-selected options
      addWorksheet(wb, "Selected options")
      writeData(wb, "Selected options", x = "User-selected options", startCol = 2)
      mergeCells(wb, "Selected options", cols = 2:9, rows = 1)
      addStyle(wb, "Selected options", heading1_style, rows=1, cols=2, gridExpand = FALSE, stack = FALSE)

      writeData(wb, "Selected options", x = "The user-selected options:", startRow = 3, startCol = 2)
      addStyle(wb, "Selected options", heading2_style, rows=3, cols=2, gridExpand = FALSE, stack = FALSE)

      writeData(wb, "Selected options", x = "Area:", startCol = 2, startRow = 5)
      if (input$all_areas == TRUE|is.null(input$area3)) {
        writeData(wb, "Selected options", x = "All areas", startCol = 2, startRow = 7)
      } else {  
        writeData(wb, "Selected options", x = input$area3, startCol = 2, startRow = 7)
      }

      writeData(wb, "Selected options", x = "Intervention(s):", startCol = 3, startRow = 5)
      if (input$all_ints == TRUE|is.null(input$interventions3)) {
        writeData(wb, "Selected options", x = "All interventions", startCol = 3, startRow = 7)
      } else {  
        writeData(wb, "Selected options", x = input$interventions3, startCol = 3, startRow = 7)
      }
      
      writeData(wb, "Selected options", x = "Health outcome(s):", startCol = 4, startRow = 5)
      if (input$all_outs == TRUE|is.null(input$outcomename3)) {
        writeData(wb, "Selected options", x = "All outcomes", startCol = 4, startRow = 7)
      } else {  
        writeData(wb, "Selected options", x = input$outcomename3, startCol = 4, startRow = 7)
      }
      
      writeData(wb, "Selected options", x = "Year of follow-up:", startCol = 5, startRow = 5)
      if (input$all_years == TRUE|is.null(input$year3)) {
        writeData(wb, "Selected options", x = "All years", startCol = 5, startRow = 7)
      } else {  
        writeData(wb, "Selected options", x = input$year3, startCol = 5, startRow = 7)
      }

      writeData(wb, "Selected options", x = "Options for individual-level interventions:", startCol = 6, startRow = 4)

      writeData(wb, "Selected options", x = "Targeting strategy(ies):", startCol = 6, startRow = 5)
      if (input$all_strats == TRUE|is.null(input$stratname3)) {
        writeData(wb, "Selected options", x = "All strategies", startCol = 6, startRow = 7)
      } else {  
        writeData(wb, "Selected options", x = input$stratname3, startCol = 6, startRow = 7)
      }
      treatoptionslist <- sort(unique(iiidata$treatoptions))
      treat <- treatoptionslist[as.integer(input$treatoptions3)]
      
      writeData(wb, "Selected options", x = "Number or % treated option:", startCol = 7, startRow = 5)
      if (input$all_nums == TRUE|is.null(input$treatoptions3)) {
        writeData(wb, "Selected options", x = "All options", startCol = 7, startRow = 7)
      } else {  
        writeData(wb, "Selected options", x = treat, startCol = 7, startRow = 7)
      }
      
      mergeCells(wb, "Selected options", cols = 6:7, rows = 4)
      addStyle(wb, "Selected options", heading3_style, rows=4:5, cols=2:7, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, "Selected options", border_style, rows=4:5, cols=2:7, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, "Selected options", general_style_wrap, rows=7:57, cols=2:7, gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, "Selected options", cols = 2:10, widths = "auto", ignoreMergedCells=TRUE)

      #adding another worksheet, this time containing the data extract
      addWorksheet(wb, "Data extract")
      writeData(wb, "Data extract", x = "Data extract for the user's selected options", startCol = 2)
      mergeCells(wb, "Data extract", cols = 2:9, rows = 1)
      addStyle(wb, "Data extract", heading1_style, rows=1, cols=2, gridExpand = FALSE, stack = FALSE)

      notes <- c("Below are the Triple I results corresponding to the user's selected options (see previous tab).",
      "The interventions should be compared with caution as they vary markedly in the level of investment required and their population reach.",
      "Health inequalities are measured using the Relative Index of Inequality, or RII.",
      "Positive values for the health outcome indicate improved health.",
      "Positive values for inequalities indicate wider inequalities.",
      "Cost-effectiveness has been estimated for a 1%-point reduction in the health outcome or reduction in health inequalities in the health outcome.",
      "Cost-effectiveness estimates are not given if the intervention scenario did not improve health or reduce inequality.",
      "Negative costs indicate savings or revenue.",
      "When 'all areas' has been selected we have reduced the file size by providing results for coincident areas (same boundaries) only once.",
      "If you selected 'all areas' please use the list on the 'Area lookup' tab to find out which areas are represented by another.")
      writeData(wb, "Data extract", x = notes, startRow = 3, startCol = 2)
      addStyle(wb, "Data extract", general_style_wrap, rows=3:12, cols=2, gridExpand = TRUE, stack = TRUE)
      mergeCells(wb, "Data extract", cols = 2:8, rows = 3)
      mergeCells(wb, "Data extract", cols = 2:8, rows = 4)
      mergeCells(wb, "Data extract", cols = 2:8, rows = 5)
      mergeCells(wb, "Data extract", cols = 2:8, rows = 6)
      mergeCells(wb, "Data extract", cols = 2:8, rows = 7)
      mergeCells(wb, "Data extract", cols = 2:8, rows = 8)
      mergeCells(wb, "Data extract", cols = 2:8, rows = 9)
      mergeCells(wb, "Data extract", cols = 2:8, rows = 10)
      mergeCells(wb, "Data extract", cols = 2:8, rows = 11)
      mergeCells(wb, "Data extract", cols = 2:8, rows = 12)
      
      
headings <- matrix(c("Area", "Intervention", "Intervention type", "Health outcome", "Year", "Targeting strategy", "No. or % treated",
                     "No. treated", "Total intervention cost (\u00A3)", "No. health outcomes prevented" , "% health outcomes prevented", 
                     "Widening of relative inequalities in health outcome (RII)", "% widening of relative inequalities in health outcome", 
                     "\u00A3 per 1%-point health outcomes prevented","\u00A3 per 1%-point narrowing of relative inequalities in health outcome"), ncol=15, nrow=1)
  writeData(wb, "Data extract", x = headings,
              startRow = 14, startCol = 2,
              colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
              borders = c("surrounding"),
              borderColour = getOption("openxlsx.borderColour", "black"),
              borderStyle = getOption("openxlsx.borderStyle", "thin"),
              withFilter = TRUE, keepNA = FALSE, name = NULL, sep = ",")
  addStyle(wb, "Data extract", heading3_style_wrap, rows=14, cols=2:16, gridExpand = FALSE, stack = TRUE)

  writeData(wb, "Data extract", data_table(), startRow = 15, startCol=2,
              colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
              withFilter = FALSE, keepNA = TRUE, name = NULL, sep = ",")
  datalength <- nrow(data_table())
  addStyle(wb, "Data extract", general_style, rows=15:(datalength+14), cols=2:16, gridExpand = TRUE, stack = FALSE)
  addStyle(wb, "Data extract", border_style, rows=15:(datalength+14), cols=2:16, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Data extract", integers, rows=15:(datalength+14), cols=c(10,15,16), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Data extract", dp3, rows=15:(datalength+14), cols=c(11,12,14), gridExpand = TRUE, stack = TRUE)
  
  setColWidths(wb, "Data extract", cols = 2:16, widths = "auto", ignoreMergedCells=TRUE)
  setColWidths(wb, "Data extract", cols = c(3,10:16), widths = c(40,14,16,19,18,19,24,23), ignoreMergedCells=TRUE)
  
  #adding another worksheet containing the intervention definitions
  addWorksheet(wb, "Intervention definitions")
  writeData(wb, "Intervention definitions", x = "Definitions of the interventions", startRow = 1, startCol = 2)
  mergeCells(wb, "Intervention definitions", cols = 2:3, rows = 1)
  addStyle(wb, "Intervention definitions", heading1_style, rows=1, cols=2, gridExpand = FALSE, stack = TRUE)
  writeData(wb, "Intervention definitions", x = matrix(c("Intervention", "Definition"), 
                                                       nrow=1, ncol=2), startRow = 3, startCol = 2, 
            colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
            borders = c("surrounding"),
            borderColour = getOption("openxlsx.borderColour", "black"),
            borderStyle = getOption("openxlsx.borderStyle", "thin"),
            withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ",")
  
  interventionlist <- intervention_list()
  writeData(wb, "Intervention definitions", interventionlist, startRow = 4, startCol=2,
            colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
            borders = c("surrounding"),
            borderColour = getOption("openxlsx.borderColour", "black"),
            borderStyle = getOption("openxlsx.borderStyle", "thin"),
            withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ",")
  
  setColWidths(wb, "Intervention definitions", cols=1:3, widths = c(3, 50, 108)) ## set column width for row names column
  addStyle(wb, "Intervention definitions", heading3_style, rows=3, cols=2:3, gridExpand = TRUE, stack = TRUE)
  rownum3 <- nrow(interventionlist)+3
  addStyle(wb, "Intervention definitions", heading3_style, rows=4:rownum3, cols=2, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Intervention definitions", general_style_wrap, rows=4:rownum3, cols=3, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Intervention definitions", border_style, rows=4:rownum3, cols=2, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Intervention definitions", border_style, rows=4:rownum3, cols=3, gridExpand = TRUE, stack = TRUE)  
  
  #adding another worksheet containing the option definitions
  addWorksheet(wb, "Option definitions")
  writeData(wb, "Option definitions", x = "Definitions of the options", startRow = 1, startCol = 2)
  mergeCells(wb, "Option definitions", cols = 2:3, rows = 1)
  addStyle(wb, "Option definitions", heading1_style, rows=1, cols=2, gridExpand = FALSE, stack = TRUE)
  writeData(wb, "Option definitions", x = matrix(c("Option", "Definition"), 
                                                 nrow=1, ncol=2), startRow = 3, startCol = 2, 
            colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
            borders = c("surrounding"),
            borderColour = getOption("openxlsx.borderColour", "black"),
            borderStyle = getOption("openxlsx.borderStyle", "thin"),
            withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ",")
  
  writeData(wb, "Option definitions", x = "Health outcomes", startRow = 5, startCol = 2)
  
  writeData(wb, "Option definitions", x = "Premature deaths", startRow = 6, startCol = 2)
  writeData(wb, "Option definitions", x = "Deaths before the age of 75.", startRow = 6, startCol = 3)
  
  writeData(wb, "Option definitions", x = "Years of life lost", startRow = 7, startCol = 2)
  writeData(wb, "Option definitions", x = "An estimate of the years a person would have lived if they had not died prematurely (based on life expectancy).", startRow = 7, startCol = 3)
  
  writeData(wb, "Option definitions", x = "Hospital stays", startRow = 8, startCol = 2)
  writeData(wb, "Option definitions", x = "Continuous inpatient stays in general or psychiatric hospitals.", startRow = 8, startCol = 3)
  
  writeData(wb, "Option definitions", x = "Year of follow up", startRow = 10, startCol = 2)
  writeData(wb, "Option definitions", x = "All interventions start in year 1, and follow up begins in year 2.  Income-based, 20 mph limits, and tobacco taxation interventions are permanent, while all other interventions are implemented in year 1 only.  Health outcomes are calculated cumulatively over the period.", startRow = 10, startCol = 3)
  
  writeData(wb, "Option definitions", x = "Intervention type", startRow = 12, startCol = 2)
  
  writeData(wb, "Option definitions", x = "Undo", startRow = 13, startCol = 2)
  writeData(wb, "Option definitions", x = "Interventions that act to undo the fundamental causes of health inequality (e.g., income inequality).", startRow = 13, startCol = 3)
  
  writeData(wb, "Option definitions", x = "Prevent", startRow = 14, startCol = 2)
  writeData(wb, "Option definitions", x = "Interventions that act to prevent harm from wider environmental influences (e.g., unemployment).", startRow = 14, startCol = 3)
  
  writeData(wb, "Option definitions", x = "Mitigate", startRow = 15, startCol = 2)
  writeData(wb, "Option definitions", x = "Interventions that act to mitigate against the negative impact of wider determinants on individuals' health (e.g., smoking).", startRow = 15, startCol = 3)
  
  writeData(wb, "Option definitions", x = "These additional options are available for interventions focused on individuals:", startRow = 17, startCol = 2)
  
  writeData(wb, "Option definitions", x = "Targeting strategy", startRow = 19, startCol = 2)
  
  writeData(wb, "Option definitions", x = "Even distribution", startRow = 20, startCol = 2)
  writeData(wb, "Option definitions", x = "The same proportion of each population subgroup (by age group, sex and deprivation) is treated.", startRow = 20, startCol = 3)
  
  writeData(wb, "Option definitions", x = "Proportionate to need", startRow = 21, startCol = 2)
  writeData(wb, "Option definitions", x = "The proportion of each subgroup treated is proportionate to the risk factor prevalence.", startRow = 21, startCol = 3)
  
  writeData(wb, "Option definitions", x = "Most deprived 20% only", startRow = 22, startCol = 2)
  writeData(wb, "Option definitions", x = "Only those in Scottish Index of Multiple Deprivation (SIMD) quintile 1 are treated (even distribution within this quintile).", startRow = 22, startCol = 3)
  
  writeData(wb, "Option definitions", x = "Most deprived 40% only", startRow = 23, startCol = 2)
  writeData(wb, "Option definitions", x = "Only those in SIMD quintiles 1 and 2 are treated (even distribution within this quintile).", startRow = 23, startCol = 3)
  
  writeData(wb, "Option definitions", x = "Number to treat", startRow = 25, startCol = 2)
  
  writeData(wb, "Option definitions", x = "Number", startRow = 26, startCol = 2)
  writeData(wb, "Option definitions", x = "Either 100, 1,000 or 100,000 interventions are delivered.  N.B. For areas and interventions with insufficient eligible population to treat this number no data will be plotted.", startRow = 26, startCol = 3)
  
  writeData(wb, "Option definitions", x = "Percentage", startRow = 27, startCol = 2)
  writeData(wb, "Option definitions", x = "A percentage of the population 'eligible' to receive the intervention is treated.", startRow = 27, startCol = 3)
  
  setColWidths(wb, "Option definitions", cols=1:3, widths = c(3, 50, 108)) ## set column width for row names column
  addStyle(wb, "Option definitions", heading2_shade_style, rows=3, cols=2:3, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Option definitions", heading2_shade_style, rows=c(5,10,12,19,25), cols=2, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Option definitions", heading3_style, rows=c(6:8, 13:15, 20:23, 26:27), cols=2, gridExpand = TRUE, stack = FALSE)
  addStyle(wb, "Option definitions", heading3_noshade_style, rows=c(4, 9, 11, 16:18, 24), cols=2:3, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Option definitions", general_style_wrap, rows=4:27, cols=3, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Option definitions", border_style, rows=c(5:8, 10, 12:15, 19:23, 25:27), cols=2:3, gridExpand = TRUE, stack = TRUE)  
  
#adding another worksheet containing the area lookup
  addWorksheet(wb, "Area lookup")
  writeData(wb, "Area lookup", x = "Match the area names here", startRow = 1, startCol = 2)
  mergeCells(wb, "Area lookup", cols = 2:3, rows = 1)
  addStyle(wb, "Area lookup", heading1_style, rows=1, cols=2, gridExpand = FALSE, stack = TRUE)

  writeData(wb, "Area lookup", x = "This lookup table shows which areas are represented by other coincident areas in the data extract.  This is only relevant for data extracts for all the areas in Scotland (due to file size constraints).", startRow = 3, startCol = 2)
  mergeCells(wb, "Area lookup", cols = 2:11, rows = 3)
  addStyle(wb, "Area lookup", general_style_wrap, rows=3, cols=2, gridExpand = TRUE, stack = FALSE)
  
  writeData(wb, "Area lookup", x = matrix(c("Area required", "Matching area used in this spreadsheet"), 
                                                 nrow=1, ncol=2), startRow = 5, startCol = 2, 
            colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
            borders = c("surrounding"),
            borderColour = getOption("openxlsx.borderColour", "black"),
            borderStyle = getOption("openxlsx.borderStyle", "thin"),
            withFilter = TRUE, keepNA = FALSE, name = NULL, sep = ",")
  addStyle(wb, "Area lookup", heading3_style_wrap, rows=5, cols=2:3, gridExpand = FALSE, stack = TRUE)
  writeData(wb, "Area lookup", area_lookup_tab(), startRow = 6, startCol=2,
            colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
            withFilter = FALSE, keepNA = TRUE, name = NULL, sep = ",")
  datalength <- nrow(area_lookup_tab())
  addStyle(wb, "Area lookup", general_style, rows=6:(datalength+5), cols=2:3, gridExpand = TRUE, stack = FALSE)
  addStyle(wb, "Area lookup", border_style, rows=6:(datalength+5), cols=2:3, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, "Area lookup", cols = 2:3, widths = "auto", ignoreMergedCells=TRUE)
  
      #saving the workbook  
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )  
}   #server closing bracket

#########################  END ----








