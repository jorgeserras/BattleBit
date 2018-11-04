######## Amazon

# Using an User belonging to group Admin:

 #library(aws.s3)
 #library(aws.signature)
 
# 
# acessKeys <- read.csv("accessKeys.csv")
# 
 s3BucketName <- "dbnodinput"
# 
# Sys.setenv("AWS_ACCESS_KEY_ID" = toString (acessKeys$Access.key.ID),
#            "AWS_SECRET_ACCESS_KEY" = toString (acessKeys$Secret.access.key),
#            "AWS_DEFAULT_REGION" = "eu-west-2")

########
 
 
 ############################################################################# CHECK FOR R UPDATES, THIS WAS TESTED WITH 3.5.1
 
 #if(!require(installr)) { install.packages("installr"); require(installr)}
 #check.for.updates.R()
 ##install.R()
 
 
 ######### AUTOMATICALLY INSTALL AND LOAD ALL NECESSARY PACKAGES
 # if(!require('pacman'))install.packages('pacman')
 # pacman::p_load(shiny, rJava, here, digest, shinyjs,dplyr,DiagrammeR,shinythemes, shinycustomloader, bsplus, reshape2, ggplot2, gsubfn, jmotif, qdapTools, mclust) 


library(dplyr)
library(DiagrammeR)

library(shiny)
library(shinythemes)
library(shinycustomloader)
library(bsplus)

require(reshape2)
require(ggplot2)
library(gsubfn)

#library(DT)
library(shinyjs)

library(rJava)
.jinit()


which.nonnum <- function(x) { ########### Find non-numeric values in a dataframe row
  badNum <- is.na(suppressWarnings(as.numeric(as.character(x))))
  which(badNum & !is.na(x))
}

sourceDir <- utils::getSrcDirectory(which.nonnum)
print(sourceDir)

setwd(sourceDir)

# Other scripts
library(here)
set_here(path = ".", verbose = TRUE)


print(here::here())

source(here::here('ui.R'))

source(here::here('amazon.R'))
source(here::here('processing.R'))
source(here::here('front.R'))
source(here::here('formatting.R'))
source(here::here('scrolling_nav.R'))
source(here::here('GMM_analysis.R'))

source(here::here('statistical', 'score_analysis.R'))

.jaddClassPath('DBNOD.jar')



# mytab  <- function (tabname) {
#   #Setup the content 
#   
#   #first content is a plot of something 
#   content1= fluidPage(
#     h2("Plot1")
#     # fluidRow(
#     #   plotOutput("plot1",height=600,width=800)
#     # )
#   )
#   #A plot of something else
#   content2= 	fluidRow(
#     h3("Plot2")
#     #plotOutput("plot2",height=900,width=1400)
#   )
#   
#   content3= 	fluidRow(
#     h3("A Table")
#     #dataTableOutput("tbl1")
#   )
#   
#   
#   #insert the content into the nav_content_section and scrollable_navmenu part
#   tabPanel(tabname,icon = icon("bar-chart"),
#            scrollable_navmenu(c("Plot1","Plot2","A table"),toptarget="thistop"), #build the menu to navigate scrolling page, use toptarget if using multiple navbars in an app. We definte toptarget as a unique page target for this section
#            nav_content_section("Plot1",class="intro-section",toptarget="thistop",
#                                content=content1
#            ),
#            nav_content_section("Plot2",class="intro-section",toptarget="thistop",
#                                content= content2	
#            ),
#            nav_content_section("A table",class="intro-section",toptarget="thistop",
#                                content= content3
#            )
#   )
# } 
# 
# 
# 
# 
# shinyUI(    
#   fluidPage(
#     nav_include_css(),
#     nav_include_in_UI(), 
#     h1("Demo of Scrolling Nav using Bootstrap and CSS"),
#     div(class="jumbotron",
#         p("Create your content as separate UI elements and add them to the scrollable_navmenu(). Each section menu name must match each nav_conent_section() name.")
#     ),
#     tabsetPanel("T",
#                 mytab("section1")
#     )
#   )
# )

# 
# #loading-content {
# position: absolute;
# background: #000000;
#   opacity: 0.9;
# z-index: 100;
# left: 0;
# right: 0;
# height: 100%;
# text-align: center;
# color: #FFFFFF;
#   }


# Define UI for app that draws a histogram ----




data_example <- read.csv("ECG_TRAINTEST_APPROPRIATE.csv")

# The user interface (ui) object controls the layout and appearance of your app. 
ui <- fluidPage(#theme = shinytheme("spacelab"),
              
  
  # tags$head(tags$script("
  #       window.onload = function() {
  #           $('#mynavlist a:contains(\"Data Check\")').parent().addClass('disabled');
  #           $('#mynavlist a:contains(\"Dry Run\")').parent().addClass('disabled');
  #           $('#mynavlist a:contains(\"Output\")').parent().addClass('disabled');
  #       };
  # 
  #       Shiny.addCustomMessageHandler('activeNavs', function(nav_label) {
  #           $('#mynavlist a:contains(\"' + nav_label + '\")').parent().removeClass('disabled');
  #       });
  #  ")),
  
  # nav_include_css(),
  # nav_include_in_UI(), 
  # h1("Demo of Scrolling Nav using Bootstrap and CSS"),
  # div(class="jumbotron",
  #     p("Create your content as separate UI elements and add them to the scrollable_navmenu(). Each section menu name must match each nav_conent_section() name.")
  # ),
  #  tabsetPanel("T",
  #              mytab("section1")
  #  ),
  
                #includeHTML("www/header.html"),
                
                useShinyjs(),
                tags$head(
                  tags$style(HTML("
                      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    "))
                ),
                
                use_bs_tooltip(),
                use_bs_popover(),
                withMathJax(),
                
                includeCSS("www/animate_loading.css"),
                
                # # Loading message
                # hidden(
                #   div(class = "container", id = "container_loading",
                #     div(class = "row center",
                #       div(
                #        class = "loading",
                #         id = "loading-content",
                #         h2("Loading..."),
                #        span(), span(), span(), span(), span(), span(), span(), span(), span(), span(), span(), span(), span(), span(), span(), span(), span(), span(), span(), span()
                #        )))
                # ),
                hidden(
                  div(id = "loading-content", class = "loading-content",
                    withLoader(type = "html", loader = "dnaspin",
                      div(
                          h2( "Processing request, please stand by", class = "animated infinite pulse")
                      )
                    )
                  )
                ),
                
                navbarPage(title="DBNOD", #shiny::HTML("<a style=color:grey;  href=\"\"><font face=\"helvetica\">DBNOD</font></a>")
                           # YOU CAN ADD AN IMAGE/GIF TO THE NAVBAR
                  #title=div(img(src="analytics.gif", width="50"), "My Title in the Navbar"),
                  
                  ###### JS CODE FOR THE NAVBAR:
                  # header = tags$script("var a = $('#nbpID a[data-value=\"Test\"]');
                  #            a.attr('data-toggle', 'noclass');
                  #                      a.click(function() {
                  #                      alert('link clicked');
                  #                      });"),
                  
                  
                  
                  
                  position = "fixed-top",
                  collapsible = TRUE,
                  tabPanel("Start", shiny::includeHTML("www/index.html"), icon = icon("home")),
                  
                  navbarMenu("Data Formatting", icon = icon("gears"),
                             tabPanel("Horizontal/Panel", shiny::includeHTML("www/header.html"), # frontp(),
                                      titlePanel("Horizontal/Panel Data Formatting"),
                                      
                                      
                                      # Sidebar layout with input and output definitions ----
                                      sidebarLayout(
                                        
                                        # Sidebar panel for inputs ----
                                        sidebarPanel(position = "right",
                                                     
                                                     # Input: Select a file ----
                                                     fileInput("file_format", "Choose CSV File",
                                                               multiple = FALSE,
                                                               accept = c("text/csv",
                                                                          "text/comma-separated-values,text/plain",
                                                                          ".csv")),
                                                     
                                                     # Horizontal line ----
                                                     tags$hr(),
                                                     
                                                     ######## HIDE THIS SOMEHOW:
                                                     
                                                     
                                                     # Input: Checkbox if file has header ----
                                                     checkboxInput("header", "Header", TRUE),
                                                     
                                                     # Input: Select separator ----
                                                     radioButtons("sep", "Separator",
                                                                  choices = c(Comma = ",",
                                                                              Semicolon = ";",
                                                                              Tab = "\t"),
                                                                  selected = ","),
                                                     
                                                     # Input: Select quotes ----
                                                     radioButtons("quote", "Quote",
                                                                  choices = c(None = "",
                                                                              "Double Quote" = '"',
                                                                              "Single Quote" = "'"),
                                                                  selected = '"'),
                                                     tags$hr(),
                                                     
                                                     
                                                     # Type: Select separator ----
                                                     radioButtons("format", "Format",
                                                                  choices = c(Horizontal = "H",
                                                                              Panel = "P"),
                                                                  selected = "H"),
                                                     
                                                     ###########
                                                     
                                                     # Horizontal line ----
                                                     tags$hr(),
                                                     
                                                     p("Check out the online version",
                                                       a("here", 
                                                         href = "https://jorgeserras.shinyapps.io/outlierdetection/")),
                                                     
                                                     actionButton("format_button", "Format")
                                                     
                                        ),
                                        
                                        # Main panel for displaying outputs ----
                                        mainPanel(
                                          column(12, align="center",
                                                 img(src = "Word_Art.png", height = 250, width = 200)
                                          ),
                                          
                                          # Output: Data file ----

                                          dataTableOutput('before_data'),
                                          
                                          div(id = "after_div",
                                            dataTableOutput("after_data")
                                          ),
                                          
                                          textOutput("error"), tags$head(tags$style("#error{color: red;
                                                                                            font-size: 20px;
                                                                                            font-style: bold;
                                                                                            }")),
                                          #hidden(
                                            div(id = "download_button",
                                                #h2("The Download Button will be functional once formatting is complete with no errors."),
                                                textOutput("button_text"),
                                                disabled(
                                                  downloadButton("downloadData", label = "Download")
                                                )
                                            )
                                          #)
                                          
                                          #tableOutput("after_data")
                                        
                                          
                                        )
                                      )

                               )
                             
                             
                             
                                #tabPanel("Panel")
                             
                             ), 
                  
                  tabPanel("Score Analysis",  icon = icon("bar-chart-o"),
                           titlePanel("Outlierness Histogram - Tukey's Method"),
                           
                           
                           # Sidebar layout with input and output definitions ----
                           sidebarLayout(
                             
                             # Sidebar panel for inputs ----
                             sidebarPanel(position = "right",
                                          
                                          # Input: Select a file ----
                                          fileInput("file1", "Choose CSV File",
                                                    multiple = FALSE,
                                                    accept = c("text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv")),
                                          
                                          # Horizontal line ----
                                          tags$hr(),
                                          
                                          # Input: Checkbox if file has header ----
                                          checkboxInput("header", "Header", TRUE),
                                          
                                          # Input: Select separator ----
                                          radioButtons("sep", "Separator",
                                                       choices = c(Comma = ",",
                                                                   Semicolon = ";",
                                                                   Tab = "\t"),
                                                       selected = ","),
                                          
                                          # Input: Select quotes ----
                                          radioButtons("quote", "Quote",
                                                       choices = c(None = "",
                                                                   "Double Quote" = '"',
                                                                   "Single Quote" = "'"),
                                                       selected = '"'),
                                          
                                          # Horizontal line ----
                                          tags$hr(),
                                          
                                          # Input: Select number of rows to display ----
                                          radioButtons("disp", "Display",
                                                       choices = c(Head = "head",
                                                                   All = "all"),
                                                       selected = "head"),
                                          
                                          
                                          p("Check out the online version",
                                            a("here", 
                                              href = "https://jorgeserras.shinyapps.io/outlierdetection/")),
                                          
                                          
                                          # Input: Slider for the number of bins ----
                                          sliderInput(inputId = "bins",
                                                      label = "Number of bins:",
                                                      min = 1,
                                                      max = 50,
                                                      value = 30),
                                          
                                          sliderInput("slider_color", label = h3("Subject Range"), min = 0, 
                                                      max = 100, value = c(40, 60)),
                                          
                                          p("Higher the number of bins, higher the resolution of the histogram."),
                                          p("This does not affect the outcome of the outlier detection.")
                                          
                             ),
                             
                             # Main panel for displaying outputs ----
                             mainPanel(
                               column(12, align="center",
                                      img(src = "Word_Art.png", height = 250, width = 200)
                               ),
                               
                               # Output: Data file ----
                               tableOutput("contents"),
                               
                               # Output: Histogram ----
                               plotOutput(outputId = "distPlot"),
                               
                               plotOutput(outputId = "matplot"),
                               
                               textOutput("java_test")
                               
                             )
                           )),
                  
                  
                  tabPanel("Outlier Detection",  icon = icon("gears"),
                           titlePanel("Outlierness Detection"),
                           
                          
                           
                           
                           # Sidebar layout with input and output definitions ----
                           sidebarLayout(
                             
                             
                             # Sidebar panel for inputs ----
                             sidebarPanel(position = "right",
                               style="position:fixed; ", tags$head(tags$style(
                                 type = 'text/css',
                                 'form.well { max-height: 560px; overflow-y: auto; border-radius: 35px;}'
                               )),
                               
                               div(id="first_div",
                                 h3("Input Data", style = "display:inline; color: #75AADB;font-size:125%;"),
                                 
                                 div(id = "download_button_example",
                                    tags$hr(),
                                    downloadButton("downloadData_example", label = "Continuous example data"),
                                    tags$hr()
                                 ),
                                 
                                 tags$head(
                                   tags$style(type="text/css", "
                                              .check_box_1 { width:250px}
                                              ")
                                   ),
                                 
                                 # Input: Select a file ----
                                 fileInput("file_panel1", "Choose CSV File", width = '250px',
                                           multiple = FALSE,
                                           accept = c("text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv")) %>%
                                   shinyInput_label_embed(
                                     icon("info") %>%
                                       bs_embed_tooltip(title = "The input data must be in Horizontal format. Please check \"Data Formatting\" tab in the navigation bar or use example data.")
                                   ),
                                 
                                 tags$div(class = "check_box_1", checkboxInput("checkbox", label = "Is Data discrete?", value = FALSE) %>%
                                   shinyInput_label_embed(
                                     icon("info") %>%
                                       bs_embed_tooltip(title = "If not checked, SAX processing will be employed next.")
                                   )),
                                 
                                 actionButton(style="display:inline-block;width:100%;text-align: center;",inputId="first_button", "Continue"),
                                 
                                 textOutput("error1"), tags$head(tags$style("#error1{color: red;font-size: 15px;font-style: bold;}"))
                                 
                                ),
                               
                               hidden(
                               div(id="second_div",
                                   h3("Pre-processing: SAX"),
                                   
                                   tags$hr(),
                                   
                                   #################### NOT WORKING YET ##############################################################
                                   
                                   ######## THE LIMITS MUST BE ADAPTED TO THE DATA AND DEFAULT VALUE IS MAX WHICH DOES NOT PREFORM PAA
                                   
                                   sliderInput(inputId ="PAA_slider",label = "PAA reduction:", min = 4, 
                                               max = 39, value = 39, step = 1) %>%
                                     shinyInput_label_embed(
                                       icon("info") %>%
                                         bs_embed_tooltip(title = "Dimensionality reduction. Choose the new length for each time series. Picking the maximum value does not preform PAA.")
                                     ),
                                   
                                   ###################################################################################################
                                   
                                   
                                   numericInput("alphabet_size", "Alphabet Size:", 5, min = 2, max = 20) %>%
                                     shinyInput_label_embed(
                                       icon("info") %>%
                                         bs_embed_tooltip(title = "Size of the alphabet for every variable in the MTS dataset.")
                                     ),
                                   
                                   
                                   actionButton(style="display:inline-block;width:100%;text-align: center;", inputId="discretize_button", "Discretize"),
                                   
                                   disabled(div(id= "second_button_div",actionButton(style="display:inline-block;width:100%;text-align: center;", inputId="second_button", "Continue")))
                               )),
                               
                               
                               hidden(
                                 div(id="third_div",
                                     h3("DBN Modeling", style = "display:inline; text-align:center; font-family: 'Lobster';color: #75AADB;font-size:125%;"),
                                     
                                     #tags$hr(),
                                     tags$head(
                                       tags$style(type="text/css", "
                                            .inline .form-control { width: 50%; height: 30px; padding: 10px 10px; font-size: 10px;}
                                            .inline, .check_box { font-size: 14px; width:200px}
                                            .control-label { font-size: 14px;}
                                                  ")
                                       ),
                                     
                                     tags$div(class = "check_box", checkboxInput("checkbox_stationary", label = "Stationary", value = TRUE) %>%
                                       shinyInput_label_embed(
                                         icon("info") %>%
                                           bs_embed_tooltip(title = "Will the trained DBN be stationary (same transition network for every time slice) or not?")
                                       )),
                                     
                                     tags$div(class = "inline",numericInput("markov_lag", "Markov Lag:", 1, min = 1, max = 5, width = "200px") %>%
                                       shinyInput_label_embed(
                                         icon("info") %>%
                                           bs_embed_tooltip(title = "How much lag is allowed in the DBN. A lag of n allows a node to possess parents at its previous n slices.")
                                       )),
                                     
                                     tags$div(class = "inline",numericInput("previous_parents", "Preceding parents:", 1, min = 1, max = 10, width = "200px") %>%
                                       shinyInput_label_embed(
                                         icon("info") %>%
                                           bs_embed_tooltip(title = "Determines the number of parents from preceding slices allowed for each node.")
                                       )),
                                     
                                     #p("MAYBE ADD TYPE OF SCORE"),
                                     
                                     actionButton(style="display:inline-block;width:200px;text-align: center; margin-right: 2px; margin-left: 2px; font-size: 14px;", inputId="train_button", "Train Model"),
                                     
                                     disabled(div(id= "third_button_div",actionButton(style="display:inline-block;width:200px;text-align: center;", inputId="third_button", "Continue")))
                                 )),
                               
                               hidden(
                                 div(id="fourth_div",
                                     
                                     tags$hr(style="margin-top: 10px; margin-bottom: 10px;"),
                                     
                                     div(id="detection_div",h3("Outlier Detection", style = "display:inline; text-align:center; font-family: 'Lobster';color: #ad1d28;font-size:125%;")), #actionButton(style="display:inline-block; font-size:70%;", inputId="train_again_button", "Modeling")),
                                     
                                     #tags$hr(),
                                     
                                     #h4("Transition Outliers"),
                                     
                                     # Input: Slider for the number of bins ----
                                     # sliderInput(inputId = "bins_trans",
                                     #             label = "Number of bins:",
                                     #             min = 1,
                                     #             max = 50,
                                     #             value = 30),
                                     
                                     #sliderInput("slider_color_trans", label = "Subject Range", min = 0, 
                                     #           max = 100, value = c(40, 60)),
                                     
                                     sliderInput(inputId ="Threshold_slider",label = "Transition threshold", min = -50, 
                                                  max = 0, value = 0, step = 0.01),
                                     
                                     
                                     #tags$hr(),
                                     
                                     #h4("Subject Outliers"),
                                     
                                     # Input: Slider for the number of bins ----
                                     # sliderInput(inputId = "bins_subje",
                                     #             label = "Number of bins:",
                                     #             min = 1,
                                     #             max = 50,
                                     #             value = 30),
                                     
                                     # sliderInput("slider_color_subje", label = "Subject Range", min = 0, 
                                     #             max = 100, value = c(40, 60)),
                                     
                                     sliderInput(inputId ="Threshold_slider_sub", label = "Subject threshold", min = -50, 
                                                 max = 0, value = 0, step = 0.01)
                                     
                                     #div(id= "fourth_button_div",actionButton(style="display:inline-block;width:200px;text-align: center;", inputId="fourth_button", "Detect Outliers"))
                                     
                                     
                                 ))
                               
                               
                               # # Partial example
                               # checkboxInput("smooth", "Smooth"),
                               # conditionalPanel(
                               #   condition = "input.smooth == true",
                               #   selectInput("smoothMethod", "Method",
                               #               list("lm", "glm", "gam", "loess", "rlm"))
                               # ),
                               # 
                               # 
                               # 
                               # 
                               # conditionalPanel("$('li.active a').first().html()==='Panel 2'",
                               #                  actionButton("bar","Bar")
                               # ),
                               # conditionalPanel(condition="input.conditionedPanels==2",
                               #                  helpText("Content Panel 2"),
                               #                  h3("LOL")
                               # )
                               
                               
                             #tags style expects a css file
                             #which is what make_css creates
                             #the first element of the list is the html class to modify
                             #the second is the border-width
                             #the third is the value of the width
                             #tags$style(make_css(list('.well', 'border-width', '10px'))),
                             
                             # singleton(
                             #   tags$head(tags$script("
                             #      window.onload = function() { # Do not disable the first tab
                             #          $('#mynavlist a:contains(\"SECOND\")').parent().addClass('disabled');
                             # 
                             #      };
                             # 
                             #      Shiny.addCustomMessageHandler('activeNavs', function(nav_label) {
                             #          $('#mynavlist a:contains(\"' + nav_label + '\")').parent().removeClass('disabled');
                             #      });
                             #   "))
                             # ),
                             
                             # navlistPanel( selected = "FIRST", id='mynavlist',
                             #   well = FALSE,
                             #   fluid = FALSE, # FIXED
                             #   widths = c(12, 12), "Menu",
                             #   tabPanel("FIRST",
                             #            actionButton('first_done', 'Done')
                             #   ),
                             #   tabPanel("Second Tab",
                             #            actionButton('second_done', 'Done'),
                             #            h3("TELOGO")
                             #            
                             #   )
                             # )
                                
                             
                             # 
                             # navlistPanel( selected = "FIRST", id='mynavlist',
                             #               well = FALSE,
                             #               fluid = FALSE, # FIXED
                             #               widths = c(12, 12), "Menu",
                             #               tabPanel("FIRST",
                             #                        actionButton(inputId="first_button", "Continue")
                             #               ),
                             #               tabPanel("Second Tab",
                             #                        actionButton(inputId="second_button", "Continue")
                             #                        
                             #               )
                             #               
                             # )
                             # 
                             
                             
                             
                             
                             
                                          
                             ),
                             
                             # Main panel for displaying outputs ----
                             mainPanel(
                               
                               uiOutput('mytabs')
                               
                               
                               # tabsetPanel(
                               #   id = "conditionedPanels",
                               #   tabPanel("Panel 1", h3("LOL1")), 
                               #   tabPanel("Panel 2", h3("LOL2"))
                               # ),
                               
                               
                               # column(12, align="center",
                               #        img(src = "Word_Art.png", height = 250, width = 200)
                               # ),
                               # # Input: Select a file ----
                               # fileInput("file1", "Choose CSV File",
                               #           multiple = FALSE,
                               #           accept = c("text/csv",
                               #                      "text/comma-separated-values,text/plain",
                               #                      ".csv")),
                               # 
                               # # Horizontal line ----
                               # tags$hr(),
                               # 
                               # # Input: Checkbox if file has header ----
                               # checkboxInput("header", "Header", TRUE),
                               # 
                               # # Input: Select separator ----
                               # radioButtons("sep", "Separator",
                               #              choices = c(Comma = ",",
                               #                          Semicolon = ";",
                               #                          Tab = "\t"),
                               #              selected = ","),
                               # 
                               # # Input: Select quotes ----
                               # radioButtons("quote", "Quote",
                               #              choices = c(None = "",
                               #                          "Double Quote" = '"',
                               #                          "Single Quote" = "'"),
                               #              selected = '"'),
                               # 
                               # # Horizontal line ----
                               # tags$hr(),
                               # 
                               # # Input: Select number of rows to display ----
                               # radioButtons("disp", "Display",
                               #              choices = c(Head = "head",
                               #                          All = "all"),
                               #              selected = "head"),
                               # 
                               # 
                               # p("Check out the online version",
                               #   a("here", 
                               #     href = "https://jorgeserras.shinyapps.io/outlierdetection/")),
                               # 
                               # 
                               # # Input: Slider for the number of bins ----
                               # sliderInput(inputId = "bins",
                               #             label = "Number of bins:",
                               #             min = 1,
                               #             max = 50,
                               #             value = 30),
                               # # Input: Slider for the number of bins ----
                               # sliderInput("slider_color", label = h3("Subject Range"), min = 0, 
                               #             max = 100, value = c(40, 60)),
                               # 
                               # p("Higher the number of bins, higher the resolution of the histogram."),
                               # p("This does not affect the outcome of the outlier detection."),
                               # 
                               # # Output: Data file ----
                               # tableOutput("contents"),
                               # 
                               # # Output: Histogram ----
                               # plotOutput(outputId = "distPlot"),
                               # 
                               # plotOutput(outputId = "matplot"),
                               # 
                               # textOutput("java_test")
                               
                             )
                           )
                  ),

                  theme = shinytheme("spacelab")
                  
                  )
  
            
                    
  #htmlTemplate("www/index.html"),
  #includeHTML("www/index.html"),
   
      
)



#The server function contains the instructions that your computer needs to build your app. 

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
    ############## BUTTONS OF NAVLIST (IN OUTLIER DETECTION
    # observe(input$first_button,{
    #     
  
    myTabs <<- list()
    
      output$mytabs <- renderUI({
        
        output$downloadData_example <- downloadHandler(
          # This function returns a string which tells the client
          # browser what name to use when saving the file.
          filename = function() {
            paste("non_discrete_example_data", ".csv", sep="")
          },
          content = function(file) {
            write.csv(data_example, file, row.names = FALSE)
          }
        )

          if (input$first_button == 0){
            return()
          }else{

            
            if (input$discretize_button == 0){
              ############# TEM DE VERIFICAR SE O FICHEIRO FOI INSERIDO
              # when reading semicolon separated files,
              # having a comma separator causes `read.csv` to error

              updateTabsetPanel(session, inputId="tabsetpanel_id", selected = 'processing_tab')
              tryCatch(
                {
                  
                  ### VERIFY IF IT IS HORIZONTAL FORMAT and continuos if not discrete
                  discrete_flag <- FALSE
                  
                  req(input$file_panel1)
                  data_input_OD <<- read.csv(input$file_panel1$datapath)
                  
                  #print(data.frame(data_input_OD$subject_id))
                  if(length(apply(data.frame(data_input_OD$subject_id),2,which.nonnum))!=0){ # Check if there is no symbols in the data entries
                    #print("NON NUMERIC SUBJECT ID")
                    stop("The input file has atleast one non-numeric subject_id, resolve the issues before trying again.")
                  }
                  
                  if(anyDuplicated(data.frame(data_input_OD$subject_id))==0){ # No duplicated subject_ids
                  }else{
                    #print("DUPLICATE IDS")
                    stop("The dataset can not possess duplicated subject_ids.")
                  }
                  
                  #print("BEFORE")
                  check_data <- parseToPanel(data_input_OD) ####### Check if in Horizontal
                  
                  check_data <- parseToHorizontal(check_data) #### Check other issues
                  
                  check_data <- data.frame(check_data)

                  miss <- colSums(is.na(check_data)) ### Check for missing values
                  #print(typeof(miss))
                  miss <<- data.frame(miss)
                  #print(miss)
                  if(length(miss[miss>0])!=0){
                    stop("The input file has missing values, please resolve before uploading.")
                  }
                  
                  
                  if(input$checkbox == TRUE){ # input is discrete

                  }else{ # input is continuous
                    
                    if(length(apply(check_data,2,which.nonnum))!=0){ # Check if there is no symbols in the data entries
                      #print("Must choose discrete")
                      discrete_flag <- TRUE
                      stop("The input file has atleast one non-numeric entry, check the discrete data checkbox or resolve the issues before trying again.")
                    }
                  }
                  #print("middle")
                  
                  # Save in the AWS server:
                  filepath <<- saveData(data_input_OD, s3BucketName) # Local filepath
                  
                  tabs1 <<- lapply(1, first_panel)
                  
                  myTabs<<-c(myTabs,lapply(1, first_panel))
                  
                  output$first_panel_table <- renderDataTable(
                    #return(head(data_input))
                    data_input_OD,
                    options = list(
                      pageLength = 10,
                      scrollX = TRUE ))
                  
                  shinyjs::hide(id = "first_div")
                  
                  if(input$checkbox == TRUE){ # Data already discrete

                    shinyjs::show(id = "third_div")
                    
                  }else{
                    shinyjs::show(id = "second_div")
                  }

  
                },
                error = function(e) {
                  # return a safeError if a parsing error occurs
                  if(discrete_flag==FALSE){
                    output$error1 <- renderText({
                      "Insert the .csv file in Horizontal format"
                    })
                    # return a safeError if a parsing error occurs
                    stop(safeError("The input file is not in the correct format and/or presents missing values. Please check the instructions or inspect the example files available for download."))
                    
                  }else{
                    output$error1 <- renderText({
                      paste("Atleast one non-numeric entry.")
                    })
                    # return a safeError if a parsing error occurs
                    stop(safeError("The input file has atleast one non-numeric entry, check the discrete data checkbox or resolve the issues before trying again."))
                    
                    
                  }
                }
              )
            }
          }
          
        
        
        
          if (input$discretize_button == 0){
            #print("discretize_button = 0")
            output$processing_description_string = renderText({
              shinyjs::hide(id = "download_processing_div")
              paste0("Since the input dataset is already discretized and in the desired format, the pre-processing phase is skipped.")
            })
            
            tabs2 <<- lapply(2, second_panel)
          }else{
            #print("discretize_button = 1")
            if (input$second_button == 0){
              
              shinyjs::show(id = "loading-content", anim = TRUE, animType = "fade")
              output$processing_description_string = renderText({
                paste0("Each variable time series from the input dataset is discretized using a SAX algorithm with an alphabet size of ", input$alphabet_size, " symbols. Input data undergoes normalization prior to discretization. The diagram below displays the mean of the normalized time series of every variable. The resulting discretized dataset is present in the table below and can be downloaded.")
              })
              

              updateTabsetPanel(session, inputId="tabsetpanel_id", selected = 'processing_tab')

              ########## SHOULD HAVE ANOTHER BUTTON TO REPEAT DISCRETIZATION PROCEDURE WITHOUT PRESSING CONTINUE
              
              ########## SHOULD CHECK FOR ERRORS, TRY CATCH
              
              list[discrete_data_set,subject_normalized, timestamp, n_variables ] <- discretize(data_input_OD, input$alphabet_size, 0) # , paa_size
              
              filepath <<- saveData(discrete_data_set, s3BucketName)
              
              output$mean_std_plot <- renderPlot({ 
                mean_std_plot(subject_normalized, timestamp, n_variables, input$alphabet_size, output)
                
              })
              
              output$second_panel_table <- renderDataTable(
                #return(head(data_input))
                discrete_data_set,
                options = list(
                  pageLength = 10,
                  scrollX = TRUE ))
              
              output$download_data_discrete <- downloadHandler(
                # This function returns a string which tells the client
                # browser what name to use when saving the file.
                filename = function() {
                  paste("output_discrete_data", ".csv", sep="")
                },
                content = function(file) {
                  write.csv(discrete_data_set, file, row.names = FALSE)
                }
              )
              
              tabs2 <<- lapply(2, second_panel)
              
              myTabs<<-c(tabs1,lapply(2, second_panel))
              shinyjs::hide(id = "discretize_button")
              shinyjs::enable(id = "second_button_div")
              
              
              shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade") 
            }
          }
        
        
        if (input$second_button == 0){
        }else{
          if (input$third_button == 0){
            updateTabsetPanel(session, inputId="tabsetpanel_id", selected = 'processing_tab')
            shinyjs::hide(id = "second_div")
            
            shinyjs::show(id = "third_div")
            
          }
        }
        
        
        if (input$train_button == 0){
        }else{
          
          shinyjs::show(id = "loading-content", anim = TRUE, animType = "fade") 
          
          #if (input$third_button == 0){
          
            observeEvent(input$train_button, {
              updateTabsetPanel(session, inputId="tabsetpanel_id", selected = 'modeling_tab')
            })
            
            if(exists("aux_text")){
              rm(aux_text)
            }
            
            
            jDialog <- J('com/github/tDBN/cli/Model_training_outlier_detection')
            #.jconstructors(jDialog)
            
            #j_filepath <- new(J("java.lang.String"), "ECG_TRAINTEST_APPROPRIATE_DISCRETE.csv")
            j_filepath <- new(J("java.lang.String"), filepath)
            
            j_score <- new(J("java.lang.String"), "ll")
            
            # aux <- J("java.lang.Integer")
            # j_lag <- new(aux, as.integer(1))
            # aux <- J("java.lang.Integer")
            # j_previous <- new(aux, as.integer(1))
            
            # aux <- J("java.lang.String")
            # j_boolean <- new(aux, "TRUE")
            # 
            # aux <- J("java.lang.Boolean")
            # j_stationary <- new(aux, j_boolean)
            
            
            # if (file.exists("dbn_structure_dot")) file.remove("dbn_structure_dot")
            # if (file.exists("dbn_structure_output")) file.remove("dbn_structure_output")
            # if (file.exists("scores_transition_output.csv")) file.remove("scores_transition_output.csv")
            # if (file.exists("subject_scores.csv")) file.remove("subject_scores.csv")
            
            #if (file.exists(aux_dot_name)) file.remove(aux_dot_name)
            
            isolate(java_object <- new(jDialog, j_filepath, as.integer(input$markov_lag),as.integer(input$previous_parents),input$checkbox_stationary,j_score))
            
            #aux_dot_name <<- paste0(paste(as.integer(Sys.time()), sep = "_" ),"")
            
            #file.rename("dbn_structure_dot",aux_dot_name)
            
            ############ Take care of the files
            transition_scores<<-read.csv("scores_transition_output.csv")
            subject_scores<<-read.csv("subject_scores_output.csv")
            n_subjects <<- nrow(subject_scores)
            
            transition_scores_path<-saveData(transition_scores, s3BucketName)
            subject_scores_path<-saveData(subject_scores, s3BucketName)
            

            if(input$checkbox_stationary){

              output$structure_diagram_grviz <- renderGrViz({
                #shinyjs::show(id = "diagram_div")
                #diagram_path <- paste(getwd(), "/dbn_structure_dot", sep = "")
                grViz("dbn_structure_dot")
 
              })
              
              ############################################################################################# TEXT FOR STATIONARY
              output$modeling_description_string = renderText({
                paste0("The next diagram represents the transition network of the stationary t-DBN model of order ", as.integer(input$markov_lag), ". A node Xi[t] illustrates a variable Xi at time frame t. Each attribute can posses at most ", as.integer(input$previous_parents), " parent(s) from preceding nodes. Nodes highlighted in blue depict attributes from the last time frame of the transition and are conditioned by previous nodes according to the structure of the transition network. Highlighted attributes are thus scored by capturing windows of each multivariate time series and observing their conditional probabilities with respect to the the network. A Log-Likelihood function is used. A multivariate time series (subject) is scored by computing the average of all its transition scores. Transition and Subject scores can be downloaded as well as the structure of the DBN in .dot format along with every parameter.")
              })

            
            }else{ ####### DONT SHOW THE DIAGRAM, MENTION THAT
              
              ############################################################################################# TEXT FOR NON-STATIONARY
              output$modeling_description_string = renderText({
                paste0("Non-stationary models are not displayed, however, their structure can be downloaded. The trained model represents the transition networks of the non-stationary t-DBN model of order ", as.integer(input$markov_lag), ". A node Xi[t] illustrates a variable Xi at time frame t. Each attribute can posses at most ", as.integer(input$previous_parents), " parent(s) from preceding nodes. Nodes highlighted in blue depict attributes from the last time frame of the transition and are conditioned by previous nodes according to the structure of the transition network. Highlighted attributes are thus scored by capturing windows of each multivariate time series and observing their conditional probabilities with respect to the the network. A Log-Likelihood function is used. A multivariate time series (subject) is scored by computing the average of all its transition scores. Transition and Subject scores can be downloaded as well as the structure of the DBN in .dot format along with every parameter.")
              })
              
              
              output$structure_diagram_grviz <- renderGrViz({

                grViz(" ", width = 250)
                
              })
              
              
            }
              
              

            
            ############
            
            
            ############# DOWNLOAD BUTTONS:
            output$download_transition_scores <- downloadHandler(
              # This function returns a string which tells the client
              # browser what name to use when saving the file.
              filename = function() {
                paste("transition_scores_output", ".csv", sep="")
              },
              content = function(file) {
                write.csv(transition_scores, file, row.names = FALSE)
              }
            )
            output$download_subject_scores <- downloadHandler(
              # This function returns a string which tells the client
              # browser what name to use when saving the file.
              filename = function() {
                paste("subject_scores_output", ".csv", sep="")
              },
              content = function(file) {
                write.csv(subject_scores, file, row.names = FALSE)
              }
            )
            
            output$download_structure <- downloadHandler(
              filename <- function() {
                paste("dbn_structure_output", "txt", sep=".")
              },
              
              content <- function(file) {
                file.copy("dbn_structure_output", file)
              },
              contentType = "application/txt"
            )
            
            tabs3 <<- lapply(3, third_panel)
            
            myTabs<<-c(tabs1,tabs2,lapply(3, third_panel))

          #}
            
            shinyjs::enable(id = "third_button")
            shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade") 
        }
        
        if (input$third_button == 0){
        }else{
          ########### ADVANCE TO SCORE HISTOGRAMS AND THRESHOLDS
            
            #print("normal")
            shinyjs::show(id = "loading-content", anim = TRUE, animType = "fade") 
            
            ################## BUTTON FOR TUKEY SCORE_ANALYSIS
            observeEvent(input$Tukey_button, {
              
              #print("Tukey pressed")
              output$transition_hist_plot <- renderPlot({
                # if (input$fourth_button == 0){
                # }else{
                # }
                
                #print("normal before")
                # observeEvent(input$Tukey_button, {
                #   print("Tukey pressed")
                #   
                #   aux <- transition_scores
                #   aux$subject_id <- NULL
                #   
                #   aux <- as.vector(t(aux))
                #   score_array <- aux ## array with all scores in serie
                #   
                #   bins <- seq(min(score_array), max(score_array), length.out = input$bins_trans + 1)
                #   
                #   Tukey_threshold <<- Threshold(score_array)
                #   
                #   Threshold_plot <<- Tukey_threshold
                # 
                #   if(input$Threshold_slider){
                #     Threshold_plot <<- input$Threshold_slider
                #     #print(as.numeric(Threshold_plot))
                #   }else{
                #     #print("IN")
                #     updateSliderInput(session, "Threshold_slider", value = as.numeric(Tukey_threshold),
                #                       min = as.integer(min(score_array), digits = 2), max = 0)
                #   }
                #   
                #   if(length(which(score_array < Threshold_plot))==0){ # No Outliers Detected for this threshold
                #     
                #     no_outliers <<- TRUE ########## FLAG
                #     
                #     hist(score_array, 
                #          main="Histogram for Transition Outlier Scores", 
                #          xlab="Scores", 
                #          border="black", 
                #          col = "springgreen3",
                #          las=1, 
                #          breaks=bins) # breaks are the number of breakpoints which determine the bins
                #     legend("topright", c("Normal"), col=c("springgreen3"), lwd=10)
                #     aux <- paste("Threshold: ", round(Threshold_plot, digits = 2))
                #     mtext(aux, side=3)
                #     abline(v=Threshold_plot,col="red", lwd=2) # Computed Threshold
                #     rm(aux)
                #     
                #   }else{
                #     ##### FOR THE COLORS:
                #     red_array <- array("red3",dim = c(1,min(which(bins > Threshold_plot))-2)) # -2 because of 0 and yellow
                #     green_array <- array("springgreen3",dim = c(1,length(bins) - min(which(bins > Threshold_plot))))
                #     
                #     colours <- cbind(red_array,"orange",green_array)
                #     
                #     hist(score_array, 
                #          main="Histogram for Transition Outlier Scores using Tukey's score-analysis", 
                #          xlab="Scores", 
                #          border="black", 
                #          col = colours,
                #          las=1, 
                #          breaks=bins) # breaks are the number of breakpoints which determine the bins
                #     
                #     legend("topright", c("Normal", "Outlier"), col=c("springgreen3", "red3"), lwd=10)
                #     aux <- paste("Tukey's Threshold: ", round(Threshold_plot, digits = 2))
                #     mtext(aux, side=3)
                #     abline(v=Threshold_plot,col="red", lwd=2) # Computed Threshold
                #     rm(aux)
                #   }
                #   
                #   updateSliderInput(session, "bins_trans", value = 30, min = 1, max = 50)
                #   
                #   updateSliderInput(session, "slider_color_trans", value = c(0, nrow(transition_scores)%/%10), # Initial value is 10%
                #                     min = 0, max = nrow(transition_scores))
                #   
                # })
                
                #print("normal after")
                
                if(Tukey_threshold == 0){ # first time
                  updateSliderInput(session, "bins_trans", value = 30, min = 1, max = 50) # estava bins_trans
                  
                  updateSliderInput(session, "slider_color_trans", value = c(0, nrow(transition_scores)%/%10), # Initial value is 10%
                                    min = 0, max = nrow(transition_scores))
                }
                
                shinyjs::disable(id = "download_transition")
                aux <- transition_scores
                
                
                aux$subject_id <- NULL
                
                aux <- as.vector(t(aux))
                score_array <- aux ## array with all scores in serie
                
                bins <- seq(min(score_array), max(score_array), length.out = input$bins_trans + 1)
                
                Tukey_threshold <<- Threshold(score_array)
                
                
                ############################# OUTLIERS DOWNLOAD BUTTON
                aux_outlier <- transition_scores
                subject_id <- aux_outlier$subject_id
                aux_outlier$subject_id <- NULL

                # aux_outlier$outlier=factor(c(0,1))
                # aux_outlier[aux_outlier$score<=Tukey_threshold,]$outlier <- 1 # Anomalies
                # aux_outlier[aux_outlier$score>Tukey_threshold,]$outlier <- 0 # Normal
                
                aux_outlier[aux_outlier<=Tukey_threshold] <- 1 # Anomalies
                aux_outlier[aux_outlier>Tukey_threshold & aux_outlier!=1] <- 0 # Normal
                
                aux_outlier <- cbind(subject_id, aux_outlier)
                
                shinyjs::enable(id = "download_transition")
                output$download_transition <- downloadHandler(
                  # This function returns a string which tells the client
                  # browser what name to use when saving the file.
                  filename = function() {
                    paste("transition_outliers_Tukey_output", ".csv", sep="")
                  },
                  content = function(file) {
                    write.csv(aux_outlier, file, row.names = FALSE)
                  }
                )
                ###############################
                
                
                #print(as.numeric(Tukey_threshold))
                
                # print("WTF1")
                # if(exists("aux_text")){
                #   print("WTF2")
                #   print(aux_text)
                #   # Not the first time, so its probably the user changing parameters
                # }else{
                #   # First time, we only want Tuckey_Threshold to compute once in the beginning
                #   print("WTF3")
                #   aux_text <<- 1
                #   output$Tuckey_threshold_text <- renderText({
                #     paste("Tukey Threshold: ",round(Tukey_threshold, digits = 2))
                #   })
                # }

                #Threshold_plot <<- Tukey_threshold
                
                # if(input$Threshold_slider){
                #   Threshold_plot <- input$Threshold_slider
                # }
                  
                #if(input$Threshold_slider){
                  #Threshold_plot <<- input$Threshold_slider
                  #print(as.numeric(Threshold_plot))
                #}else{
                  #print("IN")
                  updateSliderInput(session, "Threshold_slider", value = as.numeric(Tukey_threshold),
                                    min = as.integer(min(score_array), digits = 2), max = 0)
                #}
                
                if(length(which(score_array < Tukey_threshold))==0){ # No Outliers Detected for this threshold
                  
                  no_outliers <<- TRUE ########## FLAG
                  
                  hist(score_array, 
                       main="Histogram for Transition Outlier Scores using Tukey's score-analysis", 
                       xlab="Scores", 
                       border="black", 
                       col = "springgreen3",
                       las=1, 
                       breaks=bins) # breaks are the number of breakpoints which determine the bins
                  legend("topright", c("Normal"), col=c("springgreen3"), lwd=10)
                  aux <- paste("Tukey's threshold: ", round(Tukey_threshold, digits = 2))
                  mtext(aux, side=3)
                  abline(v=Tukey_threshold,col="red", lwd=2) # Computed Threshold
                  rm(aux)
                  shinyjs::show(id = "sliders_trans")
                  
                }else{
                  ##### FOR THE COLORS:
                  red_array <- array("red3",dim = c(1,min(which(bins > Tukey_threshold))-2)) # -2 because of 0 and yellow
                  green_array <- array("springgreen3",dim = c(1,length(bins) - min(which(bins > Tukey_threshold))))
                  
                  colours <- cbind(red_array,"orange",green_array)
                  
                  hist(score_array, 
                       main="Histogram for Transition Outlier Scores using Tukey's score-analysis", 
                       xlab="Scores", 
                       border="black", 
                       col = colours,
                       las=1, 
                       breaks=bins) # breaks are the number of breakpoints which determine the bins
                  
                  legend("topright", c("Normal", "Outlier"), col=c("springgreen3", "red3"), lwd=10)
                  aux <- paste("Tukey's threshold: ", round(Tukey_threshold, digits = 2))
                  mtext(aux, side=3)
                  abline(v=Tukey_threshold,col="red", lwd=2) # Computed Threshold
                  rm(aux)
                  shinyjs::show(id = "sliders_trans")
                }
                
                # updateSliderInput(session, "bins_trans", value = 30, min = 1, max = 50)
                # 
                # updateSliderInput(session, "slider_color_trans", value = c(0, nrow(transition_scores)%/%10), # Initial value is 10%
                #                   min = 0, max = nrow(transition_scores))
            
            })
          

          # For the color matrix that shows the outliers:
          # Reactive to Subject Slider
          output$transition_mat_plot <- renderPlot({
            # if (input$fourth_button == 0){
            # 
            # }else{
            # }
            
            #if(input$Threshold_slider){}
              
            x <- as.matrix(transition_scores)
            x <- matrix(x, ncol = ncol(transition_scores), dimnames = NULL)
            x <- t(x)
            
            x1=melt(x)
            names(x1)=c("x","y","color")
            
            x1 <- x1[!x1$x==1,]
            
            
            x1$color=factor(x1$color<Tukey_threshold)
            levels(x1$color)=c("Normal","Outlier")
            
            shinyjs::show(id = "sliders_trans")

            aux <- x1[x1$y>input$slider_color_trans[1] & x1$y<input$slider_color_trans[2],]
            
            ggplot(data = aux, aes(x=x-1.5, y=y, fill=color)) + ggtitle("Transition Outlierness by subject") +
              theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold")) + labs(x = "Transition", y = "Subject_id") +
              geom_tile() + scale_fill_manual(values=c("Outlier"="red", "Normal"="springgreen3"))
            
          })
        })
            
            
            
            ################################ GMM SCORE_ANALYSIS
            observeEvent(input$GMM_button, {
              
              #print("GMM pressed")
              output$transition_hist_plot <- renderPlot({
                
                if(GMM_threshold == 0){ # first time
                  updateSliderInput(session, "bins_trans", value = 30, min = 1, max = 50) # estava bins_trans
                  
                  updateSliderInput(session, "slider_color_trans", value = c(0, nrow(transition_scores)%/%10), # Initial value is 10%
                                    min = 0, max = nrow(transition_scores))
                }
                
                shinyjs::disable(id = "download_transition")
                # if (input$fourth_button == 0){
                # }else{
                # }
                aux <- transition_scores
                aux$subject_id <- NULL
                aux <- as.vector(t(aux))
                score_array <- aux ## array with all scores in serie
                bins <- seq(min(score_array), max(score_array), length.out = input$bins_trans + 1)
                
                #print(head(aux))
                #print(head(score_array))
                
                GMM_threshold <<- GMM_score_analysis(score_array)
                
                ############################# OUTLIERS DOWNLOAD BUTTON
                aux_outlier <- transition_scores
                subject_id <- aux_outlier$subject_id
                aux_outlier$subject_id <- NULL
                
                aux_outlier[aux_outlier<=GMM_threshold] <- 1 # Anomalies
                aux_outlier[aux_outlier>GMM_threshold & aux_outlier!=1] <- 0 # Normal
                
                aux_outlier <- cbind(subject_id, aux_outlier)
                
                shinyjs::enable(id = "download_transition")
                output$download_transition <- downloadHandler(
                  # This function returns a string which tells the client
                  # browser what name to use when saving the file.
                  filename = function() {
                    paste("transition_outliers_GMM_output", ".csv", sep="")
                  },
                  content = function(file) {
                    write.csv(aux_outlier, file, row.names = FALSE)
                  }
                )
                ###############################
                
                
                #Threshold_plot <<- GMM_threshold
                #print(Threshold_plot)

                #if(input$Threshold_slider){
                  #print("ARE U HERE?")
                  #GMM_threshold <<- input$Threshold_slider
                #}else{
                  updateSliderInput(session, "Threshold_slider", value = as.numeric(GMM_threshold),
                                    min = as.integer(min(score_array), digits = 2), max = 0)
                #}
                if(length(which(score_array < GMM_threshold))==0){ # No Outliers Detected for this threshold
                  
                  no_outliers <<- TRUE ########## FLAG
                  hist(score_array, 
                       main="Histogram for Transition Outlier Scores using GMM's score-analysis", 
                       xlab="Scores", 
                       border="black", 
                       col = "springgreen3",
                       las=1, 
                       breaks=bins) # breaks are the number of breakpoints which determine the bins
                  legend("topright", c("Normal"), col=c("springgreen3"), lwd=10)
                  aux <- paste("GMM's threshold: ", round(GMM_threshold, digits = 2))
                  mtext(aux, side=3)
                  abline(v=GMM_threshold,col="red", lwd=2) # Computed Threshold
                  #print(GMM_threshold)
                  rm(aux)
                  shinyjs::show(id = "sliders_trans")
                  #rm(GMM_threshold)
                }else{
                  ##### FOR THE COLORS:
                  red_array <- array("red3",dim = c(1,min(which(bins > GMM_threshold))-2)) # -2 because of 0 and yellow
                  green_array <- array("springgreen3",dim = c(1,length(bins) - min(which(bins > GMM_threshold))))
                  
                  colours <- cbind(red_array,"orange",green_array)
                  
                  hist(score_array, 
                       main="Histogram for Transition Outlier Scores using GMM's score-analysis", 
                       xlab="Scores", 
                       border="black", 
                       col = colours,
                       las=1, 
                       breaks=bins) # breaks are the number of breakpoints which determine the bins
                  
                  legend("topright", c("Normal", "Outlier"), col=c("springgreen3", "red3"), lwd=10)
                  aux <- paste("GMM's threshold: ", round(GMM_threshold, digits = 2))
                  mtext(aux, side=3)
                  abline(v=GMM_threshold,col="red", lwd=2) # Computed Threshold
                  rm(aux)
                  shinyjs::show(id = "sliders_trans")
                  #rm(GMM_threshold)
                }
                
                # updateSliderInput(session, "bins_trans", value = 30, min = 1, max = 50)
                # 
                # updateSliderInput(session, "slider_color_trans", value = c(0, nrow(transition_scores)%/%10), # Initial value is 10%
                #                   min = 0, max = nrow(transition_scores))
                
              })
              
              # For the color matrix that shows the outliers:
              # Reactive to Subject Slider
              output$transition_mat_plot <- renderPlot({
                # if (input$fourth_button == 0){
                #   
                # }else{
                # }
                
                #if(input$Threshold_slider){}
                
                x <- as.matrix(transition_scores)
                x <- matrix(x, ncol = ncol(transition_scores), dimnames = NULL)
                x <- t(x)
                
                x1=melt(x)
                names(x1)=c("x","y","color")
                x1 <- x1[!x1$x==1,]
                x1$color=factor(x1$color<GMM_threshold)
                levels(x1$color)=c("Normal","Outlier")
                
                shinyjs::show(id = "sliders_trans")
                
                aux <- x1[x1$y>input$slider_color_trans[1] & x1$y<input$slider_color_trans[2],]
                
                ggplot(data = aux, aes(x=x-1.5, y=y, fill=color)) + ggtitle("Transition Outlierness by subject") +
                  theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold")) + labs(x = "Transition", y = "Subject_id") +
                  geom_tile() + scale_fill_manual(values=c("Outlier"="red", "Normal"="springgreen3"))
                
              })
            })
            

            
            
            
            
            

            # if(input$Threshold_slider){
            #   print("ARE U HERE?")
            #   Threshold_plot <<- input$Threshold_slider
            # 
            #   output$transition_hist_plot <- renderPlot({
            #     if (input$fourth_button == 0){
            #     }else{
            #     }
            #     aux <- transition_scores
            #     aux$subject_id <- NULL
            #     aux <- as.vector(t(aux))
            #     score_array <- aux ## array with all scores in serie
            #     bins <- seq(min(score_array), max(score_array), length.out = input$bins_trans + 1)
            # 
            #     Threshold_plot <<- GMM_score_analysis(score_array)
            #     updateSliderInput(session, "Threshold_slider", value = as.numeric(Threshold_plot),
            #                       min = as.integer(min(score_array), digits = 2), max = 0)
            #     #}
            #     if(length(which(score_array < Threshold_plot))==0){ # No Outliers Detected for this threshold
            #       
            #       no_outliers <<- TRUE ########## FLAG
            #       hist(score_array, 
            #            main="Histogram for Transition Outlier Scores", 
            #            xlab="Scores", 
            #            border="black", 
            #            col = "springgreen3",
            #            las=1, 
            #            breaks=bins) # breaks are the number of breakpoints which determine the bins
            #       legend("topright", c("Normal"), col=c("springgreen3"), lwd=10)
            #       aux <- paste("Threshold: ", round(Threshold_plot, digits = 2))
            #       mtext(aux, side=3)
            #       abline(v=Threshold_plot,col="red", lwd=2) # Computed Threshold
            #       print(Threshold_plot)
            #       rm(aux)
            #       #rm(GMM_threshold)
            #     }else{
            #       ##### FOR THE COLORS:
            #       red_array <- array("red3",dim = c(1,min(which(bins > Threshold_plot))-2)) # -2 because of 0 and yellow
            #       green_array <- array("springgreen3",dim = c(1,length(bins) - min(which(bins > Threshold_plot))))
            #       
            #       colours <- cbind(red_array,"orange",green_array)
            #       
            #       hist(score_array, 
            #            main="Histogram for Transition Outlier Scores using GMM's score-analysis", 
            #            xlab="Scores", 
            #            border="black", 
            #            col = colours,
            #            las=1, 
            #            breaks=bins) # breaks are the number of breakpoints which determine the bins
            #       
            #       legend("topright", c("Normal", "Outlier"), col=c("springgreen3", "red3"), lwd=10)
            #       aux <- paste("GMM's threshold: ", round(Threshold_plot, digits = 2))
            #       mtext(aux, side=3)
            #       abline(v=Threshold_plot,col="red", lwd=2) # Computed Threshold
            #       rm(aux)
            #     }
            #     
            #     updateSliderInput(session, "bins_trans", value = 30, min = 1, max = 50)
            #     updateSliderInput(session, "slider_color_trans", value = c(0, nrow(transition_scores)%/%10), # Initial value is 10%
            #                       min = 0, max = nrow(transition_scores))
            #     
            #   })
            #   
            #   # For the color matrix that shows the outliers:
            #   # Reactive to Subject Slider
            #   output$transition_mat_plot <- renderPlot({
            #     if (input$fourth_button == 0){
            #       
            #     }else{
            #     }
            #     
            #     if(input$Threshold_slider){}
            #     
            #     x <- as.matrix(transition_scores)
            #     x <- matrix(x, ncol = ncol(transition_scores), dimnames = NULL)
            #     x <- t(x)
            #     
            #     x1=melt(x)
            #     names(x1)=c("x","y","color")
            #     x1 <- x1[!x1$x==1,]
            #     x1$color=factor(x1$color<Threshold_plot)
            #     levels(x1$color)=c("Normal","Outlier")
            #     
            #     aux <- x1[x1$y>input$slider_color_trans[1] & x1$y<input$slider_color_trans[2],]
            #     
            #     ggplot(data = aux, aes(x=x-1.5, y=y, fill=color)) + labs(title="Transition Outlierness by subject", x = "Transition", y = "Subject_id") + 
            #       geom_tile() + scale_fill_manual(values=c("Outlier"="red", "Normal"="springgreen3"))
            #     
            #   })
            #   
            # }
            
            
            #### This is only done in the begining
            Tukey_threshold<<-0
            GMM_threshold<<-0
            
            
          #if(exists("Threshold_plot")){ # First time, dont run this
            observeEvent(input$Threshold_slider,{
              
              #print("CUSTOM")
              Threshold_plot <<- input$Threshold_slider
              #print(Threshold_plot)
              #print(as.numeric(Tukey_threshold))
              #print(as.numeric(GMM_threshold))
              if(Threshold_plot == round(GMM_threshold, digits = 2) || Threshold_plot == round(Tukey_threshold, digits = 2)){ # The user pressed a strategy button
                
              }else{
                
                if(Threshold_plot == 0){ # first time
                  updateSliderInput(session, "bins_trans", value = 30, min = 1, max = 50) # estava bins_trans
                  
                  updateSliderInput(session, "slider_color_trans", value = c(0, nrow(transition_scores)%/%10), # Initial value is 10%
                                    min = 0, max = nrow(transition_scores))
                }
                
                shinyjs::disable(id = "download_transition")
                ############################# OUTLIERS DOWNLOAD BUTTON
                aux_outlier <- transition_scores
                subject_id <- aux_outlier$subject_id
                aux_outlier$subject_id <- NULL
                
                aux_outlier[aux_outlier<=Threshold_plot] <- 1 # Anomalies
                aux_outlier[aux_outlier>Threshold_plot & aux_outlier!=1] <- 0 # Normal
                
                aux_outlier <- cbind(subject_id, aux_outlier)
                
                shinyjs::enable(id = "download_transition")
                output$download_transition <- downloadHandler(
                  # This function returns a string which tells the client
                  # browser what name to use when saving the file.
                  filename = function() {
                    paste("transition_outliers_Manual_output", ".csv", sep="")
                  },
                  content = function(file) {
                    write.csv(aux_outlier, file, row.names = FALSE)
                  }
                )
                ###############################


                output$transition_hist_plot <- renderPlot({
                  # if (input$fourth_button == 0){
                  # }else{
                  # }
                  aux <- transition_scores
                  
                  aux$subject_id <- NULL
                  aux <- as.vector(t(aux))
                  score_array <- aux ## array with all scores in serie
                  bins <- seq(min(score_array), max(score_array), length.out = input$bins_trans + 1)


                  if(length(which(score_array < Threshold_plot))==0){ # No Outliers Detected for this threshold

                    no_outliers <<- TRUE ########## FLAG
                    hist(score_array,
                         main="Histogram for Transition Outlier Scores",
                         xlab="Scores",
                         border="black",
                         col = "springgreen3",
                         las=1,
                         breaks=bins) # breaks are the number of breakpoints which determine the bins
                    legend("topright", c("Normal"), col=c("springgreen3"), lwd=10)
                    aux <- paste("Threshold: ", round(Threshold_plot, digits = 2))
                    mtext(aux, side=3)
                    abline(v=Threshold_plot,col="red", lwd=2) # Computed Threshold
                    #print(Threshold_plot)
                    rm(aux)
                    shinyjs::show(id = "sliders_trans")
                    #rm(GMM_threshold)
                  }else{
                    ##### FOR THE COLORS:
                    red_array <- array("red3",dim = c(1,min(which(bins > Threshold_plot))-2)) # -2 because of 0 and yellow
                    green_array <- array("springgreen3",dim = c(1,length(bins) - min(which(bins > Threshold_plot))))

                    colours <- cbind(red_array,"orange",green_array)

                    hist(score_array,
                         main="Histogram for Transition Outlier Scores",
                         xlab="Scores",
                         border="black",
                         col = colours,
                         las=1,
                         breaks=bins) # breaks are the number of breakpoints which determine the bins

                    legend("topright", c("Normal", "Outlier"), col=c("springgreen3", "red3"), lwd=10)
                    aux <- paste("Threshold: ", round(Threshold_plot, digits = 2))
                    mtext(aux, side=3)
                    abline(v=Threshold_plot,col="red", lwd=2) # Computed Threshold
                    rm(aux)
                    shinyjs::show(id = "sliders_trans")
                  }

                  # updateSliderInput(session, "bins_trans", value = 30, min = 1, max = 50)
                  # updateSliderInput(session, "slider_color_trans", value = c(0, nrow(transition_scores)%/%10), # Initial value is 10%
                  #                   min = 0, max = nrow(transition_scores))

                })

                # For the color matrix that shows the outliers:
                # Reactive to Subject Slider
                output$transition_mat_plot <- renderPlot({
                  # if (input$fourth_button == 0){
                  # 
                  # }else{
                  # }

                  if(input$Threshold_slider){}

                  x <- as.matrix(transition_scores)
                  x <- matrix(x, ncol = ncol(transition_scores), dimnames = NULL)
                  x <- t(x)

                  x1=melt(x)
                  names(x1)=c("x","y","color")
                  x1 <- x1[!x1$x==1,]
                  x1$color=factor(x1$color<Threshold_plot)
                  levels(x1$color)=c("Normal","Outlier")
                  
                  shinyjs::show(id = "sliders_trans")

                  aux <- x1[x1$y>input$slider_color_trans[1] & x1$y<input$slider_color_trans[2],]

                  ggplot(data = aux, aes(x=x-1.5, y=y, fill=color)) + ggtitle("Transition Outlierness by subject") +
                    theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold")) + labs(x = "Transition", y = "Subject_id") +
                    geom_tile() + scale_fill_manual(values=c("Outlier"="red", "Normal"="springgreen3"))

                })
              }
            })
          #}
          
            
          Threshold_plot<<-0 # This only done once in the beginning after the oberve_event
            
            
          observeEvent(input$Tukey_button_sub, {
          
            output$subject_hist_plot <- renderPlot({
               # if (input$fourth_button == 0){ #&& n_subjects > 1){ ### IF THERE IS ONLY ONE SUBJECT, THERE IS NO LOGIC IN SHOWING
               # }else{
               # }
              
              if(Tukey_threshold_subje == 0){ # first time
                updateSliderInput(session, "bins_subje", value = 30, min = 1, max = 50) # estava bins_trans
                
                updateSliderInput(session, "slider_color_subje", value = c(0, nrow(subject_scores)%/%10), # Initial value is 10%
                                  min = 0, max = nrow(subject_scores))
              }
              
              shinyjs::disable(id = "download_subjects")
              aux <- subject_scores
              
              aux$subject_id <- NULL
              
              aux <- as.vector(t(aux))
              score_array <- aux ## array with all scores in serie
              
              bins <- seq(min(score_array), max(score_array), length.out = input$bins_subje + 1)
              
              Tukey_threshold_subje <<- Threshold(score_array)
              
              ############################# OUTLIERS DOWNLOAD BUTTON
              aux_subj <- subject_scores
              aux_subj$outlier <- NULL
              
              aux_subj$outlier=factor(c(0,1))
              aux_subj[aux_subj$score<=Tukey_threshold_subje,]$outlier <- 1 # Anomalies
              aux_subj[aux_subj$score>Tukey_threshold_subje & aux_subj$score!=1,]$outlier <- 0 # Normal
              
              shinyjs::enable(id = "download_subjects")
              output$download_subjects <- downloadHandler(
                # This function returns a string which tells the client
                # browser what name to use when saving the file.
                filename = function() {
                  paste("subject_outliers_Tukey_output", ".csv", sep="")
                },
                content = function(file) {
                  write.csv(aux_subj, file, row.names = FALSE)
                }
              )
              ###############################
              
              
              updateSliderInput(session, "Threshold_slider_sub", value = as.numeric(Tukey_threshold_subje),
                                min = as.integer(min(score_array), digits = 2), max = 0)
              
              
              if(length(which(score_array < Tukey_threshold_subje))==0){ # No Outliers Detected for this threshold
                
                no_outliers <<- TRUE ########## FLAG
                shinyjs::show(id = "sliders_subje")
                hist(score_array, 
                     main="Histogram for Subject Outlier Scores using Tukey's strategy", 
                     xlab="Scores", 
                     border="black", 
                     col = "springgreen3",
                     las=1, 
                     breaks=bins) # breaks are the number of breakpoints which determine the bins
                legend("topright", c("Normal"), col=c("springgreen3"), lwd=10)
                aux <- paste("Tukey's threshold: ", round(Tukey_threshold_subje, digits = 2))
                mtext(aux, side=3)
                abline(v=Tukey_threshold_subje,col="red", lwd=2) # Computed Threshold
                rm(aux)
              
                
              }else{
                ##### FOR THE COLORS:
                red_array <- array("red3",dim = c(1,min(which(bins > Tukey_threshold_subje))-2)) # -2 because of 0 and yellow
                green_array <- array("springgreen3",dim = c(1,length(bins) - min(which(bins > Tukey_threshold_subje))))
                
                colours <- cbind(red_array,"orange",green_array)
                shinyjs::show(id = "sliders_subje")
                
                hist(score_array, 
                     main="Histogram for Subject Outlier Scores using Tukey's strategy", 
                     xlab="Scores", 
                     border="black", 
                     col = colours,
                     las=1, 
                     breaks=bins) # breaks are the number of breakpoints which determine the bins
                
                legend("topright", c("Normal", "Outlier"), col=c("springgreen3", "red3"), lwd=10)
                aux <- paste("Tukey's threshold: ", round(Tukey_threshold_subje, digits = 2))
                mtext(aux, side=3)
                abline(v=Tukey_threshold_subje,col="red", lwd=2) # Computed Threshold
                rm(aux)
                
                
              }
              
              # updateSliderInput(session, "bins_subje", value = 30, min = 1, max = 50) # estava bins_trans
              # 
              # updateSliderInput(session, "slider_color_subje", value = c(0, nrow(subject_scores)%/%10), # Initial value is 10%
              #                   min = 0, max = nrow(subject_scores))
              
              
            })
            
            # For the color matrix that shows the outliers:
            # Reactive to Subject Slider
            output$subject_mat_plot <- renderPlot({
              # if (input$fourth_button == 0){ #&& n_subjects > 1){ ### IF THERE IS ONLY ONE SUBJECT, THERE IS NO LOGIC IN SHOWING
              #   
              # }else{
              # }
              
              x <- as.matrix(subject_scores)
              x <- matrix(x, ncol = ncol(subject_scores), dimnames = NULL)
              x <- t(x)
              
              x1=melt(x)
              names(x1)=c("x","y","color")
              
              x1 <- x1[!x1$x==1,]
              
              
              x1$color=factor(x1$color<Tukey_threshold_subje)
              levels(x1$color)=c("Normal","Outlier")
              
              shinyjs::show(id = "sliders_subje")
              
              aux <- x1[x1$y>input$slider_color_subje[1] & x1$y<input$slider_color_subje[2],]
              
              ggplot(data = aux, aes(x=x-1.5, y=y, fill=color)) + ggtitle("Subject Outlierness by subject") +
                theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold"), axis.text.x = element_blank()) + labs(x = "Transition", y = "Subject_id") + 
                geom_tile() + scale_fill_manual(values=c("Outlier"="red", "Normal"="springgreen3"))

              
            })
          
          
          })
          
          
          
          
          
          observeEvent(input$GMM_button_sub, {
            
            output$subject_hist_plot <- renderPlot({
              # if (input$fourth_button == 0 && n_subjects > 1){ ### IF THERE IS ONLY ONE SUBJECT, THERE IS NO LOGIC IN SHOWING
              # }else{
              # }
              
              if(GMM_threshold_sub == 0){ # first time
                updateSliderInput(session, "bins_subje", value = 30, min = 1, max = 50) # estava bins_trans
                
                updateSliderInput(session, "slider_color_subje", value = c(0, nrow(subject_scores)%/%10), # Initial value is 10%
                                  min = 0, max = nrow(subject_scores))
              }
              
              
              shinyjs::disable(id = "download_subjects")
              aux <- subject_scores
              aux$subject_id <- NULL
              aux <- as.vector(t(aux))
              score_array <- aux ## array with all scores in serie
              bins <- seq(min(score_array), max(score_array), length.out = input$bins_subje + 1)
              
              #print(head(aux))
              #print(head(score_array))
              
              GMM_threshold_sub <<- GMM_score_analysis(score_array)
              
              
              ############################# OUTLIERS DOWNLOAD BUTTON
              aux_subj <- subject_scores
              aux_subj$outlier <- NULL
              
              aux_subj$outlier=factor(c(0,1))
              aux_subj[aux_subj$score<=GMM_threshold_sub,]$outlier <- 1 # Anomalies
              aux_subj[aux_subj$score>GMM_threshold_sub & aux_subj$score!=1,]$outlier <- 0 # Normal
              
              shinyjs::enable(id = "download_subjects")
              output$download_subjects <- downloadHandler(
                # This function returns a string which tells the client
                # browser what name to use when saving the file.
                filename = function() {
                  paste("subject_outliers_GMM_output", ".csv", sep="")
                },
                content = function(file) {
                  write.csv(aux_subj, file, row.names = FALSE)
                }
              )
              ###############################
              
              
              updateSliderInput(session, "Threshold_slider_sub", value = as.numeric(GMM_threshold_sub),
                                min = as.integer(min(score_array), digits = 2), max = 0)
              
              
              if(length(which(score_array < GMM_threshold_sub))==0){ # No Outliers Detected for this threshold
                
                no_outliers <<- TRUE ########## FLAG
                shinyjs::show(id = "sliders_subje")
                hist(score_array, 
                     main="Histogram for Subject Outlier Scores using GMM's strategy", 
                     xlab="Scores", 
                     border="black", 
                     col = "springgreen3",
                     las=1, 
                     breaks=bins) # breaks are the number of breakpoints which determine the bins
                legend("topright", c("Normal"), col=c("springgreen3"), lwd=10)
                aux <- paste("GMM threshold: ", round(GMM_threshold_sub, digits = 2))
                mtext(aux, side=3)
                abline(v=GMM_threshold_sub,col="red", lwd=2) # Computed Threshold
                rm(aux)
                
                
              }else{
                ##### FOR THE COLORS:
                red_array <- array("red3",dim = c(1,min(which(bins > GMM_threshold_sub))-2)) # -2 because of 0 and yellow
                green_array <- array("springgreen3",dim = c(1,length(bins) - min(which(bins > GMM_threshold_sub))))
                
                colours <- cbind(red_array,"orange",green_array)
                shinyjs::show(id = "sliders_subje")
                hist(score_array, 
                     main="Histogram for Subject Outlier Scores using GMM's strategy", 
                     xlab="Scores", 
                     border="black", 
                     col = colours,
                     las=1, 
                     breaks=bins) # breaks are the number of breakpoints which determine the bins
                
                legend("topright", c("Normal", "Outlier"), col=c("springgreen3", "red3"), lwd=10)
                aux <- paste("GMM threshold: ", round(GMM_threshold_sub, digits = 2))
                mtext(aux, side=3)
                abline(v=GMM_threshold_sub,col="red", lwd=2) # Computed Threshold
                rm(aux)
              }
              
              # updateSliderInput(session, "bins_subje", value = 30, min = 1, max = 50)
              # 
              # updateSliderInput(session, "slider_color_subje", value = c(0, nrow(subject_scores)%/%10), # Initial value is 10%
              #                   min = 0, max = nrow(subject_scores))
              
              
            })
            
            # For the color matrix that shows the outliers:
            # Reactive to Subject Slider
            output$subject_mat_plot <- renderPlot({
              # if (input$fourth_button == 0 && n_subjects > 1){ ### IF THERE IS ONLY ONE SUBJECT, THERE IS NO LOGIC IN SHOWING
              #   
              # }else{
              # }
              
              x <- as.matrix(subject_scores)
              x <- matrix(x, ncol = ncol(subject_scores), dimnames = NULL)
              x <- t(x)
              
              x1=melt(x)
              names(x1)=c("x","y","color")
              
              x1 <- x1[!x1$x==1,]
              
              shinyjs::show(id = "sliders_subje")
              x1$color=factor(x1$color<GMM_threshold_sub)
              levels(x1$color)=c("Normal","Outlier")
              
              
              aux <- x1[x1$y>input$slider_color_subje[1] & x1$y<input$slider_color_subje[2],]
              
              ggplot(data = aux, aes(x=x-1.5, y=y, fill=color)) + ggtitle("Subject Outlierness by subject") +
                theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold"), axis.text.x = element_blank()) + labs(x = "Transition", y = "Subject_id") +
                geom_tile() + scale_fill_manual(values=c("Outlier"="red", "Normal"="springgreen3"))
              
            })
            

          })
          
          
          
          
          #### This is only done in the begining
          Tukey_threshold_subje<<-0
          GMM_threshold_sub<<-0
          
          
          #if(exists("Threshold_plot_sub")){ # First time, dont run this
            observeEvent(input$Threshold_slider_sub,{
              #print("CUSTOM_sub")
              Threshold_plot_sub <<- input$Threshold_slider_sub
              if(Threshold_plot_sub == round(GMM_threshold_sub, digits = 2) || Threshold_plot_sub == round(Tukey_threshold_subje, digits = 2)){ # The user pressed a strategy button
                
              }else{
                
                
                if(Threshold_plot_sub == 0){ # first time
                  updateSliderInput(session, "bins_subje", value = 30, min = 1, max = 50) # estava bins_trans
                  
                  updateSliderInput(session, "slider_color_subje", value = c(0, nrow(subject_scores)%/%10), # Initial value is 10%
                                    min = 0, max = nrow(subject_scores))
                }
                
                output$subject_hist_plot <- renderPlot({
                  shinyjs::disable(id = "download_subjects")
                  aux <- subject_scores
                  aux$subject_id <- NULL
                  aux <- as.vector(t(aux))
                  score_array <- aux ## array with all scores in serie
                  bins <- seq(min(score_array), max(score_array), length.out = input$bins_subje + 1)
                  
                  ############################# OUTLIERS DOWNLOAD BUTTON
                  aux_subj <- subject_scores
                  aux_subj$outlier <- NULL
                  
                  aux_subj$outlier=factor(c(0,1))
                  aux_subj[aux_subj$score<=Threshold_plot_sub,]$outlier <- 1 # Anomalies
                  aux_subj[aux_subj$score>Threshold_plot_sub & aux_subj$score!=1,]$outlier <- 0 # Normal
                  
                  shinyjs::enable(id = "download_subjects")
                  output$download_subjects <- downloadHandler(
                    # This function returns a string which tells the client
                    # browser what name to use when saving the file.
                    filename = function() {
                      paste("subject_outliers_Manual_output", ".csv", sep="")
                    },
                    content = function(file) {
                      write.csv(aux_subj, file, row.names = FALSE)
                    }
                  )
                  ###############################
                  
                  
                  updateSliderInput(session, "Threshold_slider_sub", value = as.numeric(Threshold_plot_sub),
                                    min = as.integer(min(score_array), digits = 2), max = 0)
                  
                  
                  if(length(which(score_array < Threshold_plot_sub))==0){ # No Outliers Detected for this threshold
                    
                    no_outliers <<- TRUE ########## FLAG
                    shinyjs::show(id = "sliders_subje")
                    hist(score_array, 
                         main="Histogram for Subject Outlier Scores using GMM's strategy", 
                         xlab="Scores", 
                         border="black", 
                         col = "springgreen3",
                         las=1, 
                         breaks=bins) # breaks are the number of breakpoints which determine the bins
                    legend("topright", c("Normal"), col=c("springgreen3"), lwd=10)
                    aux <- paste("GMM threshold: ", round(Threshold_plot_sub, digits = 2))
                    mtext(aux, side=3)
                    abline(v=Threshold_plot_sub,col="red", lwd=2) # Computed Threshold
                    rm(aux)
                    
                  }else{
                    ##### FOR THE COLORS:
                    red_array <- array("red3",dim = c(1,min(which(bins > Threshold_plot_sub))-2)) # -2 because of 0 and yellow
                    green_array <- array("springgreen3",dim = c(1,length(bins) - min(which(bins > Threshold_plot_sub))))
                    
                    colours <- cbind(red_array,"orange",green_array)
                    shinyjs::show(id = "sliders_subje")
                    hist(score_array, 
                         main="Histogram for Subject Outlier Scores using GMM's strategy", 
                         xlab="Scores", 
                         border="black", 
                         col = colours,
                         las=1, 
                         breaks=bins) # breaks are the number of breakpoints which determine the bins
                    
                    legend("topright", c("Normal", "Outlier"), col=c("springgreen3", "red3"), lwd=10)
                    aux <- paste("GMM threshold: ", round(Threshold_plot_sub, digits = 2))
                    mtext(aux, side=3)
                    abline(v=Threshold_plot_sub,col="red", lwd=2) # Computed Threshold
                    rm(aux)
                    
                  }
                  
                  # updateSliderInput(session, "bins_subje", value = 30, min = 1, max = 50)
                  # 
                  # updateSliderInput(session, "slider_color_subje", value = c(0, nrow(subject_scores)%/%10), # Initial value is 10%
                  #                   min = 0, max = nrow(subject_scores))
                  
                  
                })
                
                # For the color matrix that shows the outliers:
                # Reactive to Subject Slider
                output$subject_mat_plot <- renderPlot({
                  # if (input$fourth_button == 0 && n_subjects > 1){ ### IF THERE IS ONLY ONE SUBJECT, THERE IS NO LOGIC IN SHOWING
                  #   
                  # }else{
                  # }
                  
                  if(input$Threshold_slider_sub){}
                  
                  x <- as.matrix(subject_scores)
                  x <- matrix(x, ncol = ncol(subject_scores), dimnames = NULL)
                  x <- t(x)
                  
                  x1=melt(x)
                  names(x1)=c("x","y","color")
                  
                  x1 <- x1[!x1$x==1,]
                  
                  shinyjs::show(id = "sliders_subje")
                  x1$color=factor(x1$color<Threshold_plot_sub)
                  levels(x1$color)=c("Normal","Outlier")
                  
                  
                  aux <- x1[x1$y>input$slider_color_subje[1] & x1$y<input$slider_color_subje[2],]
                  
                  ggplot(data = aux, aes(x=x-1.5, y=y, fill=color)) + ggtitle("Subject Outlierness by subject") +
                    theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold"), axis.text.x = element_blank()) + labs(x = "Transition", y = "Subject_id") +
                    geom_tile() + scale_fill_manual(values=c("Outlier"="red", "Normal"="springgreen3"))
                  
                })
                

                
              }
            })
          #}
          
          
          Threshold_plot_sub<<-0 # This only done once in the beginning after the oberve_event
          
          
          
          
          # aux_trans <- transition_scores
          # aux_trans$outlier <- NULL
          # 
          # ### TEM QUE ESTAR AQUI UM FOR OU ASSIM, PARA VER TODAS AS COLUNAS t_i E SUBSTITUIR POR TRUE OR FALSE
          # 
          # #aux_trans$outlier=factor(aux_trans$score<Tukey_threshold)
          # 
          # output$download_transition <- downloadHandler(
          #   # This function returns a string which tells the client
          #   # browser what name to use when saving the file.
          #   filename = function() {
          #     paste("transition_scores_output", ".csv", sep="")
          #   },
          #   content = function(file) {
          #     write.csv(aux_trans, file, row.names = FALSE)
          #   }
          # )
          

          
            # aux_subj <- subject_scores
            # aux_subj$outlier <- NULL
            # #names(aux_subj)=c(names(aux_subj),"outlier")
            # 
            # aux_subj$outlier=factor(aux_subj$score<Tukey_threshold_subje)
            # 
            # print("DOWNLOAD")
            # 
            # print(Tukey_threshold_subje)
            # 
            # print(head(aux_subj))
            # 
            # output$download_subjects <- downloadHandler(
            #   # This function returns a string which tells the client
            #   # browser what name to use when saving the file.
            #   filename = function() {
            #     paste("subject_scores_output", ".csv", sep="")
            #   },
            #   content = function(file) {
            #     write.csv(aux_subj, file, row.names = FALSE)
            #   }
            # )

          
          myTabs<<-c(tabs1,tabs2,tabs3,lapply(4, fourth_panel), lapply(5, fifth_panel))
          
          shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade") 
          shinyjs::hide(id = "third_button")
          shinyjs::show(id = "fourth_div")
          #shinyjs::show(id = "fourth_button")
          
        }

        
        # observeEvent(input$train_again_button,{
        #   shinyjs::toggle(id="third_div")
        # })
          #print(length(myTabs))

          #nTabs = input$nTabs
          #myTabs = lapply(paste('Tab', 1: 2), tabPanel)
        
        #tabsetpanel_id
          #myTabs <<- c(list("tabsetpanel_id"),myTabs) # Add the id of the tabsetpanel
        
            
          do.call(tabsetPanel, c(myTabs, id = "tabsetpanel_id"))
          
          
        })
    # })
  
    # observe({
    #   if (input$first_done > 0) {
    #     session$sendCustomMessage('activeNavs', 'Second Tab')
    #   }
    # })
  
    ##############
  
  
    ###################################### JAVA TEST
    # output$java_test <- renderText({
    #   java_object <- .jnew('Banana1/Banana', 10)
    #   
    #   print("LUL")
    #   .jcall(java_object,returnSig = "S", "printme") # S for String (return type)
    #   print(.jcall(java_object,returnSig = "S", "test_other_classfile"))
    #   #"You have selected this"
    # })
      
      
      
  
     # output$java_test <- renderText({ 
     #   .jcall("LearnFromFile","V","main",.jarray(c("-i", "artificial_data_v2_9000_1000_10.csv" ,"-p" ,"1", "-m", "1" ,"-s" ,"ll"  ,"-cDBN"  ,"-pm" ,"-o", "res_cDBN"), "java/lang/String")) 
     #   #"You have selected this"
     # })
  
    
    # Global Variables:
    data_input <- data.frame()
    data_output <- data.frame()
  
    output$contents <- renderTable({
    
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      req(input$file1)
      
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          data_input <<- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
          
          # Save in the AWS server:
          saveData(data_input, s3BucketName)
          ##################### THIS SHOULD BE MORE ORGANIZED, THIS INPUT DATA IS FOR OUTLIER DETECTION TAB NOT FORMATTING TAB
          
          updateSliderInput(session, "slider_color", value = c(0, nrow(data_input)%/%10), # Initial value is 10%
                            min = 0, max = nrow(data_input))
        },
        error = function(e) {
          output$error <- renderText({
            if(is.data.frame(data_output) && nrow(data_output)==0){ # if data_output is empty
              
              shinyjs::disable(id = "downloadData")
              output$button_text <- renderText({ # NO ERROR MESSAGE FOR NOW
                "The Download Button will be functional once formatting is complete with no errors."
              })
              output$button_text1 <- renderText({ # NO ERROR MESSAGE FOR NOW
                "The Download Button will be functional once formatting is complete with no errors."
              })
              output$button_text2 <- renderText({ # NO ERROR MESSAGE FOR NOW
                "The Download Button will be functional once formatting is complete with no errors."
              })
              output$button_text3 <- renderText({ # NO ERROR MESSAGE FOR NOW
                "The Download Button will be functional once formatting is complete with no errors."
              })
              output$button_text4 <- renderText({ # NO ERROR MESSAGE FOR NOW
                "The Download Button will be functional once formatting is complete with no errors."
              })
              shinyjs::hide(id = "after_div")
              if(isolate(input$format)=="H"){
                "ERROR: The input file is not on panel format or presents other issues. Try converting to Panel format or check for discrepancies."
              }else{
                "ERROR: The input file is not on horizontal format or presents other issues. Try converting to Horizontal format or check for discrepancies."
              }
              #output$after_data <- renderDataTable()
            }else{
              " "
            }
          })
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      
      if(input$disp == "head") {
        return(head(data_input))
      }
      else {
        return(data_input)
      }
    
    })
  
    
    ######################## FOR FORMAT BUTTON REACTION:
    
    observeEvent(input$format_button,{
      req(input$file_format)
      # If the file was not inserted dont show/try any plot
      if(is.null(input$file_format)){
        paste("Please insert a .csv file with the correct format")
        return(NULL)
      }

      tryCatch(
        {
          data_input <<- read.csv(input$file_format$datapath,
                                  header = input$header,
                                  sep = input$sep,
                                  quote = input$quote)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      after_string <- paste("Data After Formatting (", input$format, "):", sep = "")
      
      ####### FOR THE BEFORE/AFTER TABLES

      output$before_data <- renderDataTable(
        #return(head(data_input))
        data_input,
        options = list(
          pageLength = 5,
          scrollX = TRUE ))
      
      
      tryCatch(
        {
          data_output <<- data.frame()
          output$error <- renderText({ # NO ERROR MESSAGE FOR NOW
            " "
          })
          

          data_output <<- format_data(data_input,input$format)
          
          shinyjs::enable(id = "downloadData")
          output$button_text <- renderText({ # NO ERROR MESSAGE FOR NOW
            "The Download Button is available."
          })
          shinyjs::show(id = "after_div")
          output$after_data <- renderDataTable(
            #return(head(data_input))
            data_output,
            options = list(
              pageLength = 5,
              scrollX = TRUE ))
          
          ################# ESTE BUTAO DA SEMPRE ? E SE A DATA DEU ERRO ?  MOSTRA SEMPRE?
          output$downloadData <- downloadHandler(
            # This function returns a string which tells the client
            # browser what name to use when saving the file.
            filename = function() {
              paste("output_data", ".csv", sep="")
            },
            content = function(file) {
              write.csv(data_output, file, row.names = FALSE)
            }
          )
          

          ###############

        },
        error = function(e) {
          ######## SEND TO OUTPUT ERROR MESSAGE
          # if(exists(output$after_data)){
          #   shinyjs::hide(output$after_data)
          # }
          output$error <- renderText({
            if(is.data.frame(data_output) && nrow(data_output)==0){ # if data_output is empty
              
              shinyjs::disable(id = "downloadData")
              output$button_text <- renderText({ # NO ERROR MESSAGE FOR NOW
                "The Download Button will be functional once formatting is complete with no errors."
              })
              output$button_text1 <- renderText({ # NO ERROR MESSAGE FOR NOW
                "The Download Button will be functional once formatting is complete with no errors."
              })
              output$button_text2 <- renderText({ # NO ERROR MESSAGE FOR NOW
                "The Download Button will be functional once formatting is complete with no errors."
              })
              output$button_text3 <- renderText({ # NO ERROR MESSAGE FOR NOW
                "The Download Button will be functional once formatting is complete with no errors."
              })
              output$button_text4 <- renderText({ # NO ERROR MESSAGE FOR NOW
                "The Download Button will be functional once formatting is complete with no errors."
              })
              shinyjs::hide(id = "after_div")
              if(isolate(input$format)=="H"){
                "ERROR: The input file is not on panel format or presents other issues. Try converting to Panel format or check for discrepancies."
              }else{
                "ERROR: The input file is not on horizontal format or presents other issues. Try converting to Horizontal format or check for discrepancies."
              }
              #output$after_data <- renderDataTable()
            }else{
              " "
            }
          })

        }
      )
      

      # output$after_data <- renderTable({
      #   data_output <<- format_data(data_input,input$format)
      #   return(head(data_output))
      # }, caption = after_string,
      # caption.placement = getOption("xtable.caption.placement", "top"),
      # caption.width = getOption("xtable.caption.width", NULL))

      
    })
    
  ###################################################
  
  # Histogram of the Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    # If the file was not inserted dont show/try any plot
    if(is.null(input$file1)){
      paste("Please insert a .csv file with the correct format")
      return(NULL)
    }
    
    aux <- data_input
    aux$subject_id <- NULL
    
    aux <- as.vector(t(aux))
    score_array <- aux ## array with all scores in serie
    
    
    bins <- seq(min(score_array), max(score_array), length.out = input$bins + 1)
    
    
    # hist(x, breaks = bins, col = "#75AADB", border = "white",
    #      xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram of waiting times")
    
    Tukey_threshold <<- Threshold(score_array)
    
    ##### FOR THE COLORS:
    min(which(bins > Tukey_threshold))
    
    if(length(which(score_array < Tukey_threshold))==0){ # No Outliers Detected for this threshold
      
      hist(score_array, 
           main="Histogram for Transition Outlier Scores", 
           xlab="Scores", 
           border="black", 
           col = "springgreen3",
           las=1, 
           breaks=bins) # breaks are the number of breakpoints which determine the bins
      legend("topright", c("Normal"), col=c("springgreen3"), lwd=10)
      aux <- paste("Threshold: ", round(Tukey_threshold, digits = 2))
      mtext(aux, side=3)
      abline(v=Tukey_threshold,col="red", lwd=2) # Computed Threshold
      rm(aux)
    }else{
      red_array <- array("red3",dim = c(1,min(which(bins > Tukey_threshold))-2)) # -2 because of 0 and yellow
      green_array <- array("springgreen3",dim = c(1,length(bins) - min(which(bins > Tukey_threshold))))
      
      colours <- cbind(red_array,"orange",green_array)
      
      hist(score_array, 
           main="Histogram for Outlier Scores", 
           xlab="Scores", 
           border="black", 
           col = colours,
           las=1, 
           breaks=bins) # breaks are the number of breakpoints which determine the bins
      
      legend("topright", c("Normal", "Outlier"), col=c("springgreen3", "red3"), lwd=10)
      aux <- paste("Threshold: ", round(Tukey_threshold, digits = 2))
      mtext(aux, side=3)
      abline(v=Tukey_threshold,col="red", lwd=2) # Computed Threshold
      rm(aux)
    }
    
    
    
  })
    
    # For the color matrix that shows the outliers:
    # Reactive to Subject Slider
    output$matplot <- renderPlot({
      
      # If the file was not inserted dont show/try any plot
      if(is.null(input$file1)){
        paste("Please insert a .csv file with the correct format")
        return(NULL)
      }
      
      
      x <- as.matrix(data_input)
      x <- matrix(x, ncol = ncol(data_input), dimnames = NULL)
      x <- t(x)
      
      x1=melt(x)
      names(x1)=c("x","y","color")
      
      x1 <- x1[!x1$x==1,]
      
      x1$color=factor(x1$color<Tukey_threshold)
      levels(x1$color)=c("Normal","Outlier")
      
      aux <- x1[x1$y>input$slider_color[1] & x1$y<input$slider_color[2],]
      
      ggplot(data = aux, aes(x=x-1.5, y=y, fill=color)) + labs(title="Transition Outlierness by subject", x = "Transition", y = "Subject_id") + 
        geom_tile() + scale_fill_manual(values=c("Outlier"="red", "Normal"="springgreen3"))
      
      
    })
    
    
    
    # onStop(function() { # CODE DONE WHEN THE SESSION IS ENDED
    #   #if (file.exists(aux_dot_name)) file.remove(aux_dot_name)
    # 
    #   print("telogo")
    #   # rm(aux_text)
    #   # rm(Tukey_threshold)
    #   # rm(Tukey_threshold_subje)
    #   # rm(Scores)
    # })
    
    
    
    session$onSessionEnded(function() {
      print("telogo")
      #stopApp()
    })
    
    
    
}

shinyApp(ui = ui, server = server,
         onStart = function() { # WHEN SESSION BEGINS
           cat("Doing application setup\n")
           Tukey_threshold_subje <<- 0.0 # Initializing
           Tukey_threshold <<- 0.0
           onStop(function() {
             cat("Doing application cleanup\n")
           })
         })


