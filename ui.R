#Code to create the Triple I results browser
#This script includes the user-interface definition of the app.

###############################################.
## Header ---- 
###############################################.
tagList( #needed for shinyjs
  useShinyjs(),  # Include shinyjs
  navbarPage(id = "intabset", #needed for landing page
           title = div(tags$a(img(src="hslogowhite.png", height=50, width=60), href= "http://www.healthscotland.scot/"),
                         style = "position: relative; top: -15px; color:#ffffff"), # Navigation bar
           windowTitle = "Triple I", #title for browser tab
           theme = shinytheme("darkly"), #Theme of the app 
           collapsible = TRUE, #tab panels collapse into menu in small screens
           header =         
             tags$head( #CSS styles
               tags$link(rel="shortcut icon", href="HSlogo.jpg"), #Icon for browser tab
               #Including Google analytics
   #            includeScript("google-analytics.html"),
               includeHTML(("google-analytics.html")),
               #Style sidebars/well panels
               tags$style(
                          "hr {margin-top: 10px; margin-bottom: 10px}",
                          ".well {background-color: #ffffff ; border: 0px solid #336699;
                          padding: 5px; box-shadow: none;  }",
                          #Background colour of header navBar
#                          ".navbar-brand {background-color: #437281}",
                          ".navbar {font-size: 14px; border: 0; color: white; background-color: #437281;}", #font size and border
                          ".navbar-default .navbar-nav li a:hover {color:white; background-color:#275868;}",
                          ".navbar-default .navbar-nav li a:focus {color:white; background-color:#275868;}",
                          ".navbar-default .navbar-nav .active a {color:white; background-color:#103e4e;}",
                          ".modal-header {color:#fff; }",
                          ".modal-body {color:#333; font-size:14px; background-color: white;}",
                          #Text size and line height. Padding needed for footer
                          "body { color: #333; font-size: 14px; 
                           background-color: #fff; line-height: 1; padding-bottom:10px}",
                          #Padding and margins of filters and labels
                          ".form-group {margin: 3px}",
                          ".li-custom {margin-bottom: 10px; text-align: left;}", #bullet point list items
                          ".shiny-options-group { margin-top: 3px; }",
                          ".selectize-control { margin-bottom: 3px}",
                          ".selectize-input {font-size: 12px; line-height: 10px;}", 
                          ".selectize-input {padding: 3px 3px; min-height: 10px; }",
                          ".selectize-dropdown {line-height: 10px; }",
                          ".control-label { margin-bottom: 1px}",
                          "a {color: #007fa9}",
                          "a:hover {color: #007fa9}",
                          "a:focus {color: #007fa9}",
                          #Style for download buttons 
                          ".down {background-color:#5490A3; color: white; background-image:none; 
                          font-size: 14px; padding: 5px 10px; margin-top: 5px; margin-left: 3px}",
                 #center image - for normal icons
                 "img.center {object-fit: scale-down; position:absolute; width:100%; height:100%; margin-left:auto; margin-right: auto; display: block; padding:20px;}",
               #to avoid red text error messages in the whole app, take out for testing
#               ".shiny-output-error { visibility: hidden; }",
#               ".shiny-output-error:before { visibility: hidden; }",
               #External links underlined an open a new tab
               ".externallink{text-decoration: underline;} ",
               ".definitionbox {width:100%; height:100%; text-align:left ;background-color:white;
               border: 2px solid #2FA4E7; padding: 10px; margin-top: 0px; margin-bottom: 5px; float: left; transition: 0.5s ease; position: relative; object-fit: scale-down;}",
               #tooltip styling
               ".tooltip-inner {text-align: left; 
                       min-width: 250px; 
                       placement: bottom; 
                       -webkit-border-radius: 0px;
                       -moz-border-radius: 0px;
                       border-radius: 0px;
                       margin-bottom: 6px;
                       font-size: 12px; }"
               ),
               HTML("<base target='_blank'>")
               ),

###############################################.
## Landing page ----
###############################################.
tabPanel(
  title="Home", icon = icon("home"), 
  style="margin-top:-10px; color:#333; ",
  windowTitle = "Triple I: Home page", #title for browser tab
  mainPanel(
    style="margin-left:10%; margin-right:10%; margin-bottom:10%",
       fluidRow(h3("Welcome to the Triple I interactive results browser", style="color:black; font-weight: bold;")),
       fluidRow(h5("Triple I (Informing Interventions to reduce health Inequalities) is a project that allows you to compare how different interventions might affect health and health inequalities at national and local levels in Scotland.", style="margin-top:0px; color: #333; ")),
    fluidRow(h4("First-time visitors", style="margin-top:0px; color: #333; ")),
    h5(tags$ul( 
      tags$li(div(actionLink("jump_to_tour", "Take a tour"), class= "li-custom",
                  " - to learn how to use this results browser.")),
      tags$li(div(actionLink("jump_to_about", "About"), class= "li-custom", 
                  " - to find out more about the Triple I project."))
    )), #Bullet point list bracket
    br(),
    fluidRow(h4("Get started by comparing interventions for a selected area", style="margin-top:0px; color: #333; ")),
       h5(tags$ul( 
         tags$li(div(actionLink("jump_to_trend", "Compare effects"), class= "li-custom",
                         " - to compare effects of the interventions on health and inequalities.")),
         tags$li(div(actionLink("jump_to_rank", "Compare costs"), class= "li-custom", 
                        " - to compare cost-effectiveness of the interventions."))
       )), #Bullet point list bracket
    br(),
    
       fluidRow(h4("Access the data behind this results browser", style="margin-top:0px; color: #333; ")),
       h5(tags$ul( 
         tags$li(div(actionLink("jump_to_table", "Data"), class= "li-custom",
                     " - to view and download the data."))
       )) #Bullet point list bracket
       
) #main Panel bracket
  ),# tab panel bracket

###############################################.
## Quadrant chart ----
###############################################.
tabPanel("Compare effects", icon = icon("th-large"), value = "trend", 
         style="margin-top:-10px; color:#333; ",
         windowTitle = "Triple I: Health & inequalities effects", #title for browser tab
         sidebarPanel(width=3, style="margin-top:-10px; background-color: #dad9df;",
          p(selectizeInput("area", label = 'Area of interest', choices = list( 'Scotland',
                                                          `Health boards`= c(hb_names),
                                                          `Council areas`= c(la_names),
                                                          `City regions`= c(deal_names),
                                                          `Integrated Joint Boards`= c(ijb_names)),
                        selected = NULL, multiple=FALSE)),

          p(selectizeInput("outcomename", "Health outcome",
                                            choices = list( "Premature deaths","Years of life lost", 
                                                 "Hospital stays"), selected = NULL, multiple=FALSE)),
          p(selectizeInput("year", "Year of follow up",
                           choices = list( `(N.B. Intervention is year 1)`= c(2:10, 15, 20)), 
                           selected = NULL, multiple=FALSE)),
          p(selectizeInput("inttype", "Include these intervention types",
                           choices = list("All types*"="all",
                                          `Implementation level:`= 
                                            c("National implementation"="National", 
                                              "Local implementation"="Local"),
                                          `Type of action: (see 'Further info and help' for definitions)`= 
                                            c("'Undo' types"="Undo", 
                                              "'Prevent' types"="Prevent",
                                              "'Mitigate' types"="Mitigate")), 
                           selected = "all", multiple=FALSE)),
          p(conditionalPanel(condition = ("input.inttype=='all'"),
                             tags$b(HTML("<small/><font color=DimGrey>(*5 omitted for clearer presentation)</font color=DimGrey></small/>")))),
          p(conditionalPanel(condition = ("input.inttype!='Undo' & input.inttype!='National'"),
                             tags$b(HTML("For individual-level** interventions:")))),
          p(conditionalPanel(condition = ("input.inttype!='Undo' & input.inttype!='National'"),
                             (selectInput("stratname", 'Targeting strategy', 
                                          c("Even distribution",
                                            "Proportionate to need", 
                                            "Most deprived 20% only",
                                            "Most deprived 40% only"), selected = "Proportionate to need")))),
          p(conditionalPanel(condition = ("input.inttype!='Undo' & input.inttype!='National'"),
                             (selectizeInput("treatoptions", label = 'Number to treat',  
                                             choices = list(`Absolute number:` 
                                                            = c("Number: 100" = "2",
                                                                "Number: 1,000" = "3",
                                                                "Number: 10,000" = "4"),
                                                            `% of eligible population:` = 
                                                              c("Percent: 1" = "5", 
                                                                "Percent: 5" = "6", 
                                                                "Percent: 10" = "7", 
                                                                "Percent: 25" = "8", 
                                                                "Percent: 50" = "9", 
                                                                "Percent: 100" = "10")), 
                                             selected = "7", multiple=FALSE)))),
          p(conditionalPanel(condition = ("input.inttype!='Undo' & input.inttype!='National'"),
                             tags$b(HTML("<small/><font color=DimGrey>(**all 'Mitigate' interventions plus Job provision)</font color=DimGrey></small/>")))),
          
          actionButton("help_trend",label="Further info and help", icon= icon('question-circle'), class ="down", width = "200px"),
          actionButton("defs_trend",label="Intervention definitions", icon= icon("info-circle"), class ="down", width = "200px"),
          downloadButton('triple_i_effects_data.xlsx', class ="down", width = "200px", label='Download data (XLSX file)')

         ),
         mainPanel(width = 9, style="margin-top:0px; margin-bottom:10%", #Main panel
                   h6(HTML("Select options from the grey panel. Click an intervention's legend label to hide/unhide it, and click and drag on the graph to zoom in."), style="margin-top:0px; margin-bottom:0px; text-align: left; "),
                   hr(),
                   h4(textOutput("title_trend"), style="margin-top:-5px; margin-bottom:0px; text-align: left; font-weight: bold;"),
                   plotlyOutput("trend_plot"),
                   bsModal("mod_defs_trend", "Intervention definitions", "defs_trend", htmlOutput('defs_text_trend'), role="dialog")
         )
), #Tab panel bracket

###############################################.
## Cost table ---- 
###############################################.
tabPanel("Compare costs", icon = icon("gbp"), value = "costtable",
         style="margin-top:-10px; color:#333; ",
         windowTitle = "Triple I: Relative costs for health effect", #title for browser tab
         sidebarPanel(width=3, style="margin-top:-10px; background-color: #dad9df;",
                      p(selectizeInput("area4", 
                                       label = 'Area of interest', 
                                       choices = list( 'Scotland',
                                                       `Health boards`= c(hb_names),
                                                       `Council areas`= c(la_names),
                                                       `City regions`= c(deal_names),
                                                       `Integrated Joint Boards`= c(ijb_names)),
                                       selected = NULL, multiple=FALSE)),
                      p(selectizeInput("outcomename4", "Health outcome",
                                       choices = c("Premature deaths",
                                                   "Years of life lost", 
                                                   "Hospital stays"), selected = NULL, multiple=FALSE)),
                      p(selectizeInput("year4", "Year of follow up",
                                       choices = list( `(N.B. Intervention is year 1)`= c(2:10, 15, 20)), 
                                       selected = NULL, multiple=FALSE)),
                      p(selectizeInput("inttype4", "Intervention types",
                                       choices = list("All types"="all",
                                                  `Implementation level:`= 
                                                    c("National implementation"="National", 
                                                      "Local implementation"="Local"),
                                                  `Type of action: (see 'Further info and help' for definitions)`= 
                                                    c("'Undo' types"="Undo", 
                                                   "'Prevent' types"="Prevent",
                                                   "'Mitigate' types"="Mitigate")), 
                                       selected = "Mitigate", multiple=FALSE)),

                      p(conditionalPanel(condition = ("input.inttype4!='Undo' & input.inttype4!='National'"),
                                         tags$b(HTML("For individual-level* interventions:")))),
                      p(conditionalPanel(condition = ("input.inttype4!='Undo' & input.inttype4!='National'"),
                      (selectInput("stratname4", 'Targeting strategy', 
                                c("Even distribution",
                                  "Proportionate to need", 
                                  "Most deprived 20% only",
                                  "Most deprived 40% only"), selected = "Proportionate to need")))),
                      p(conditionalPanel(condition = ("input.inttype4!='Undo' & input.inttype4!='National'"),
                                         tags$b(HTML("<small/><font color=DimGrey>(*all 'Mitigate' interventions plus Job provision)</font color=DimGrey></small/>")))),
                      actionButton("help_costtable",label="Further info and help", icon= icon('question-circle'), class ="down", width = "200px"),
                      actionButton("defs_costtable",label="Intervention definitions", icon= icon("info-circle"), class ="down", width = "200px"),
                      downloadButton('triple_i_cost_data.xlsx', label='Download data (XLSX file)', class ="down", width = "200px")
         ),
         mainPanel(width = 9, style="margin-top:0px; margin-bottom:10%", #Main panel
                   bsModal("mod_defs_costtable", "Intervention definitions", "defs_costtable", htmlOutput('defs_text_costtable'), role="dialog"),
                   h6(HTML("Select the options required from the grey panel.  If required, sort the table contents using the grey arrowheads."), style="margin-top:0px; margin-bottom:0px; text-align: left; "),
                   h6(HTML("For individual-level interventions maximum effects (resulting from treating 100% of the eligible population) are presented."), style="margin-top:0px; margin-bottom:0px; text-align: left; "),
                   h6(HTML("Positive values = health outcomes prevented, health inequalities narrowed, or costs incurred (negative costs = savings)."), style="margin-top:0px; margin-bottom:0px; text-align: left; "),
                   h6(HTML("<font color='red'><b>Bold red</b></font> figures represent worsening of health outcomes or health inequalities: cost-effectiveness column is left blank in these cases.") , style="margin-top:0px; margin-bottom:0px; text-align: left; "),
                   hr(),
                   h4(textOutput("costtable_title"), style="margin-top:0px; margin-bottom:0px; text-align: left;  font-weight: bold;"),
                   p(),
                   fluidRow(
                     column(12, div(DT::dataTableOutput("costtable_filtered"),
                     style = "font-size: 80%; width: 98%; margin-bottom : 5%; lineHeight:10%"))
                   ))
), #Tab panel bracket

###############################################.
## Data ----
###############################################.
tabPanel("Data", icon = icon("table"), value = "table",
         style="margin-top:-10px; color:#333; ",
         windowTitle = "Triple I: Select and download data", #title for browser tab
         mainPanel(
           width = 12, style="margin-left:1%; margin-right:1%; margin-bottom:-10%,
           margin-top:-10px; color:#333; ",
           #Row 1 for intro
           fluidRow(
                    h4("Filter and download selected Triple I results", style = "font-weight: bold; color: black;")
                    ),
           #Row 2 for selections
           fluidRow(
             column(3,
                    p(),
                    selectizeInput("area3", label = 'Area of interest',
                                width = "229px", 
                                choices = list( "Scotland",
                                   `Health boards`= c(hb_names),
                                   `Council areas`= c(la_names),
                                   `City regions`= c(deal_names),
                                   `Integrated Joint Boards`= c(ijb_names)), 
                                selected = NULL, multiple=FALSE, options = NULL),
                    awesomeCheckbox("all_areas",label = "All available areas", value = FALSE),

                    selectizeInput("interventions3", label = 'Intervention(s)',
                                width = "229px", 
                                choices = interventions_lookup$intervention[interventions_lookup$include==1], 
                                selected = NULL, multiple=TRUE, options = NULL),
                    awesomeCheckbox("all_ints",label = "All available interventions", value = FALSE)
             ),
             column(3,
                    p(),
                    selectizeInput("outcomename3", label = 'Health outcome(s)',
                                   width = "229px", 
                                   choices = c("Premature deaths",
                                               "Years of life lost", 
                                               "Hospital stays"), 
                                   selected = NULL, multiple=TRUE),
                    awesomeCheckbox("all_outs",label = "All available outcomes", value = FALSE),

                    p(selectizeInput("year3", "Year of follow up",
                                     choices = list(`(N.B. Intervention is year 1)`= c(2:10, 15, 20)), 
                                     selected = NULL, multiple=FALSE)),
                    awesomeCheckbox("all_years",label = "All available years", value = FALSE)
                    ),
                    column(3,style='background-color: #dad9df;',
                           p(tags$b("For individual-level interventions:")),
                           p(selectizeInput("stratname3", 'Targeting strategy', 
                                     width = "229px", 
                                     choices = c("Even distribution",
                                    "Proportionate to need", 
                                    "Most deprived 20% only",
                                    "Most deprived 40% only"),
                                    selected = NULL, multiple=TRUE)),
                           awesomeCheckbox("all_strats",label = "All strategies", value = FALSE),
                           
                           p(selectizeInput("treatoptions3", 'Number to treat' , 
                                     width = "229px", 
                                     choices = list(`Absolute number:` 
                                                    = c("Number: 100" = "2",
                                                        "Number: 1,000" = "3",
                                                        "Number: 10,000" = "4"),
                                                    `% of eligible population:` = 
                                                      c("Percent: 1" = "5", 
                                                        "Percent: 5" = "6", 
                                                        "Percent: 10" = "7", 
                                                        "Percent: 25" = "8", 
                                                        "Percent: 50" = "9", 
                                                        "Percent: 100" = "10")), 
                                     selected = NULL, multiple=TRUE)),
                           awesomeCheckbox("all_nums",label = "All available numbers", value = FALSE)
                           ),
                  column(3,
                    actionButton("help_table",label="Further information and help", class = "down", width = "253px", icon= icon('question-circle'), class ="down"),
                    br(),
                    actionButton("defs_data",label="Intervention definitions", class = "down", width = "253px", icon= icon("info-circle"), class ="down"),
                    br(),
                    actionButton("clear", label = "Clear all filters", class = "down", width = "253px", icon ("eraser")),
                    downloadButton('triple_i_data_extract.xlsx', HTML('Download selected data<br/>(XLSX, file size from <1MB (one area)<br/>to 80MB (all areas))'), class ="down" , width = "253px"),
                    bsModal("mod_defs_table", "Intervention definitions", "defs_data", htmlOutput('defs_data'), role="dialog")
                    )
           ),
fluidRow(br()),

           #Row 3- Table
           fluidRow(
             column(12, div(DT::dataTableOutput("table_filtered"), 
                            style = "font-size: 80%; width: 98%; margin-bottom : 5%; lineHeight:10%"))
           ))

 ), #Tab panel bracket


###############################################.             
## About----    
tabPanel("About", icon = icon("info-circle"), value = "about",
#         sidebarPanel(width=1),
         windowTitle = "Triple I: About the project", #title for browser tab
         mainPanel(width=8,
                   style="margin-left:10%; margin-right:10%; margin-bottom:10%",
                   h4("What is Triple I?", style = "color:black; font-weight: bold;"),
                   p("NHS Health Scotland's ", (tags$a(href="http://www.healthscotland.scot/triplei",
                     "Informing Interventions to reduce health Inequalities ", class="externallink")), 
                     "(Triple I) project is a modelling study that brings together the best available evidence 
                     to estimate how different interventions might affect health and health inequalities over 
                     the next 20 years.  This online tool allows users to browse, visualise, and download 
                     Triple I results for their area of interest."),
                   h4("Why Triple I?", style = "color:black; font-weight: bold;"),
                   p("There are many separate pieces of evidence about how specific interventions affect the 
                     health of individuals, but relatively little evidence about how these interventions might 
                     affect health and health inequalities at the population level.  Triple I is part of NHS 
                     Health Scotland's approach to addressing these population-level evidence gaps."),
                   h4("Who is Triple I for?", style = "color:black; font-weight: bold;"),
                   p("Triple I provides national and local decision makers with interactive tools and 
                     interpreted findings to inform discussions and decisions about different interventions.  
                     The study will also be of use to anyone with an interest in improving population health 
                     and reducing health inequalities."),
                   h4("How should I use the Triple I results?", style = "color:black; font-weight: bold;"),
                   p("Triple I should be considered as one source of information that complements other 
                     decision-making resources. The information should be interpreted in the context of the ", 
                     (tags$a(href="http://www.healthscotland.scot/triplei",
                     "technical information", class="externallink")), 
                     " about the project."),
                   h4("Where can I find out more about Triple I?", style = "color:black; font-weight: bold;"),
                   p("All the reports and spreadsheet tools are available from our ", (tags$a(href="http://www.healthscotland.scot/triplei",
                     "Triple I", class="externallink")),  " web page."),
                   tags$ul( 
                     #Link to user guide
                     tags$li(class= "li-custom", 
                             "National overview report (April 2019): high-level summary of results for Scotland."),
                     tags$li(class= "li-custom", 
                             "Frequently Asked Questions (April 2019): answers to a number of questions about the project."),
                     tags$li(class= "li-custom", 
                             "Spreadsheet tools (April 2019): download and interact with the spreadsheet for each intervention to produce bespoke results."),
                     tags$li(class= "li-custom", 
                             "Income-based policies report (October 2018): The first output from the project, summarising the income-based policies."),
                     tags$li(class= "li-custom", 
                             "Technical report (April 2019): technical information about the Triple I modelling.")
                     ), #Bullet point list bracket
                     p("Contact us: If you have any further questions or feedback relating to the data or the tool, please contact us at ",
                              tags$b(tags$a(href="mailto:nhs.triple.i@nhs.net", "nhs.triple.i@nhs.net", class="externallink")), 
                             " and we will be happy to help.")
         ) #main panel bracket
),#Tab panel
###############################################.
## Take a tour----    
###############################################.      
tabPanel("Take a tour", icon = icon("map-signs"), value = "tour",
windowTitle = "Triple I: How to use the results browser", #title for browser tab
mainPanel(
style="margin-left:10%; margin-right:10%; margin-bottom:10%",
h4("How to get the most out of this results browser", style = "color:black; font-weight: bold;" ),
h5("This interactive tool provides access to results from our Informing Interventions to reduce health Inequalities (Triple I) project 
    for Scotland, NHS boards, council areas, Integrated Joint Boards (IJBs) and city regions."),
tags$h4(br(), "Step 1. Getting around the results browser", style = "color:black; font-weight: bold;"),
tags$h5("Select the data visualisation or information page you want:"),
fluidRow(
  column(1, HTML("<i class='fa fa-th-large'></i>"), style = "text-align: right; padding:12px; "),
  column(11, tags$h5(tags$b("  Compare effects:"), " Compare intervention effects on health and health inequalities in your area."))
  ),
fluidRow(
  column(1, HTML("<i class='fa fa-gbp'></i>"), style = "text-align: right; padding:12px; "),
  column(11, tags$h5(tags$b("  Compare costs:"), " Compare the cost-effectiveness of the interventions."))
  ),
fluidRow(
  column(1, HTML("<i class='fa fa-table'></i>"), style = "text-align: right; padding:12px; "),
  column(11, tags$h5(tags$b("  Data:"), " View and download selected data in spreadsheet format."))
  ),
fluidRow(             
  column(1, HTML("<i class='fa fa-info-circle'></i>"), style = "text-align: right; padding:12px; "),
  column(11, tags$h5(tags$b("  About:"), " Find out more about the project."))
),
tags$h5("You can get to the right page using the links on the home page or the relevant tab in the navigation bar."),
img(src='navbar.png', height=50, style="padding-right: 2px; vertical-align:middle"), 
tags$h5("Click on the house symbol ",
        HTML("<i class='fa fa-home'></i>"), 
       " to return to the Home Page at any point."),
tags$h5("Click on the Health Scotland logo to go to the main Health Scotland website."),

tags$h4(br(), "Step 2. Interacting with the results browser", style = "color:black; font-weight: bold;"),
tags$h5("On the charts and data pages you can customise the data shown."),
tags$h5("Make selections using dropdown menus, view the results corresponding to these selections, and download the data if required."),
tags$h5("The 'Help' button on the relevant page will provide more information and guidance."),
img(src='help.png', height=50, style="padding-right: 2px; vertical-align:middle")
) #mainPanel bracket
) #Tab panel bracket
###############################################.             
##############Footer----    
###############################################.
#Copyright warning
# tags$footer(
#   column(4, "B) NHS Health Scotland 2019"), 
#             column(3, tags$a(href="mailto:ScotPHO@nhs.net", tags$b("Contact us!"), 
#                              class="externallink", style = "color: white; text-decoration: none;")), 
#             column(4, tags$a(href="http://www.healthscotland.scot/privacy-policy", tags$b("Privacy and cookies"), 
#                              class="externallink", style = "color: white; text-decoration: none")), 
#             column(1, actionLink("twitter_share", label = "Share", icon = icon("twitter"),
#                                  style= "color:white;", onclick = sprintf("window.open('%s')", 
#                                                                           "https://twitter.com/intent/tweet?text=Check%out%Health%Scotland's%interactive%Triple%I%results&url=https://scotland.shinyapps.io/Triple_I/"))), 
#             style = "
#             position:fixed;
#             text-align:left;
#             left: 0;
#             bottom:0;
#             width:100%;
#             z-index:1000;  
#             height:5px; /* Height of the footer */
#             color: white;
#             padding: 0px;
#             font-weight: bold;
#             background-color: #437281"
# ) 
################################################.

) #bracket navbar
) #bracket tagList
###END