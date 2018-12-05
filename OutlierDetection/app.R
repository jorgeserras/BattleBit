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

data_discrete_example <- read.csv("artificial_C_20_1000.csv")

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
                                 h3("Input Data", style = "display:inline; text-align:center; font-family: 'Lobster';color: #75AADB;font-size:125%;"),
                                 
                                 div(id = "download_button_example", style="text-align: center;",
                                    tags$br(),
                                    downloadButton("downloadData_example", label = "Continuous example data"),
                                    tags$br(),
                                    downloadButton("downloadData_discrete_example", label = "Discrete example data"),
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
                                     
                                     
                                     sliderInput(inputId ="Threshold_slider",label = "Transition threshold", min = -50, 
                                                  max = 0, value = 0, step = 0.01),
                                     
                                     sliderInput(inputId ="Threshold_slider_sub", label = "Subject threshold", min = -50, 
                                                 max = 0, value = 0, step = 0.01)

                                     
                                 ))
                               
                                          
                             ),
                             
                             # Main panel for displaying outputs ----
                             mainPanel(
                               
                               uiOutput('mytabs')
                               
                               
                             )
                           )
                  ),

                  theme = shinytheme("spacelab")
                  
                  )
  
)



#The server function contains the instructions that your computer needs to build your app. 

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  
    myTabs <<- list()
    
      output$mytabs <- renderUI({
        
        output$downloadData_example <- downloadHandler(
          # This function returns a string which tells the client
          # browser what name to use when saving the file.
          filename = function() {
            paste("continuous_example_data", ".csv", sep="")
          },
          content = function(file) {
            write.csv(data_example, file, row.names = FALSE)
          }
        )
        
        output$downloadData_discrete_example <- downloadHandler(
          # This function returns a string which tells the client
          # browser what name to use when saving the file.
          filename = function() {
            paste("discrete_example_data", ".csv", sep="")
          },
          content = function(file) {
            write.csv(data_discrete_example, file, row.names = FALSE)
          }
        )
        
          if (input$first_button == 0){
            return()
          }else{
            isolate(
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
                  #start_time <- Sys.time()
                  check_data <- parseToPanel(data_input_OD) ####### Check if in Horizontal
                  #end_time <- Sys.time()
                  #print("ParseToPanel:")
                  #print(end_time - start_time)
                  
                  #start_time <- Sys.time()
                  #check_data <- parseToHorizontal(check_data) #### Check other issues
                  #end_time <- Sys.time()
                  #print("ParseToHorizontal:")
                  #print(end_time - start_time)
                  #print(head(check_data[1]))
                  check_data <- data.frame(check_data[1], stringsAsFactors=FALSE)
                  
                  #check_data <- ldply (check_data[1], data.frame)
                  
                  #print(head(check_data))
                  #print("wtf")
                  #print(check_data$timestamp)
                  #print(typeof(check_data$timestamp))
                  number_slices <<- max(check_data$timestamp)
                  
                  #print(number_slices)
                  check_data <- data_input_OD
                  
                  miss <- colSums(is.na(check_data)) ### Check for missing values
                  #print(typeof(miss))
                  miss <- data.frame(miss)
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
                    
                    ### Update the value of the PAA slider
                    updateSliderInput(session, "PAA_slider", value = number_slices, min = 2, max = number_slices) # estava bins_trans
                    
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
            )
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
                paste0("Each variable time series from the input dataset is discretized using a SAX algorithm with an alphabet size of ", input$alphabet_size, " symbols. Input data undergoes normalization prior to discretization. The diagram below displays the mean and standard deviation of the normalized time series of every variable. The resulting discretized dataset is present in the table below and can be downloaded. ATTENTION: If the PAA reduction value is not set on max, time series are reduced and will impact every posterior procedures. The plot continues to display the full length data, however, the table shows the reduced data which can be downloaded. ")
              })
              
              updateTabsetPanel(session, inputId="tabsetpanel_id", selected = 'processing_tab')
              
              list[discrete_data_set,subject_normalized, timestamp, n_variables ] <- discretize(data_input_OD, input$alphabet_size, input$PAA_slider) # , paa_size
              
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

            j_filepath <- new(J("java.lang.String"), filepath)
            
            j_score <- new(J("java.lang.String"), "ll")

            isolate(java_object <- new(jDialog, j_filepath, as.integer(input$markov_lag),as.integer(input$previous_parents),input$checkbox_stationary,j_score))
            

            ############ Take care of the files
            transition_scores<<-read.csv("scores_transition_output.csv")
            subject_scores<<-read.csv("subject_scores_output.csv")
            n_subjects <<- nrow(subject_scores)
            
            transition_scores_path<-saveData(transition_scores, s3BucketName)
            subject_scores_path<-saveData(subject_scores, s3BucketName)
            

            if(input$checkbox_stationary){

              output$structure_diagram_grviz <- renderGrViz({
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
          #### First time in the score-analysis phase nothing is selected
          Score_analysis_transition_mode <<- "D" 
          Score_analysis_subject_mode <<- "D"
        }else{
          ########### ADVANCE TO SCORE HISTOGRAMS AND THRESHOLDS
            print("MODEL TRAINED")
            Tukey_threshold <<- 0
            GMM_threshold <<- 0
            Threshold_plot <<- 0 # This only done once in the beginning after the oberve_event

            # Flag to know what is the strategy currently selected for transitions
            #Score_analysis_transition_mode <<- "D" # Default, after training
            #### MODES:
            ## Default: D
            ## Transition Tukey: TT
            ## Transition GMM : TG
            ## Transition Manual: TM
            
            # Flag to know what is the strategy currently selected for subjects
            #Score_analysis_subject_mode <<- "D" # Default, after training
            #### MODES:
            ## Default: D
            ## Subject Tukey: ST
            ## Subject GMM : SG
            ## Subject Manual: SM
            
            
            shinyjs::show(id = "loading-content", anim = TRUE, animType = "fade") 
            
            print(Score_analysis_transition_mode)
            if(Score_analysis_transition_mode != "D"){ # Not the first model trained, recompute the selected modes
              if(Score_analysis_transition_mode=="TT") click("Tukey_button") # Simulate the clicking
              if(Score_analysis_transition_mode=="TG") click("GMM_button") # Simulate the clicking
              flag_transition_train <<- TRUE
            }
            print(Score_analysis_subject_mode)
            if(Score_analysis_subject_mode != "D"){ # Not the first model trained, recompute the selected modes
              if(Score_analysis_subject_mode=="ST") click("Tukey_button_sub") # Simulate the clicking
              if(Score_analysis_subject_mode=="SG") click("GMM_button_sub") # Simulate the clicking
              flag_subject_train <<- TRUE
            }
            
            ################## BUTTON FOR TUKEY TRANSITION SCORE_ANALYSIS
            observeEvent(input$Tukey_button, {
            
                print("Tukey pressed")
                shinyjs::disable(id = "download_transition")
                
                aux <- transition_scores
                aux$subject_id <- NULL
                aux <- as.vector(t(aux))
                score_array <- aux ## array with all scores in serie
                
                if(Tukey_threshold == 0){ # first time after training
                  updateSliderInput(session, "bins_trans", value = 30, min = 1, max = 50) # estava bins_trans
                  
                  updateSliderInput(session, "slider_color_trans", value = c(0, nrow(transition_scores)%/%10), # Initial value is 10%
                                    min = 0, max = nrow(transition_scores))
                  print("Performing Tukey")
                  
                  Tukey_threshold <<- Threshold(score_array)
                }
                
                
                ############################# OUTLIERS DOWNLOAD BUTTON
                aux_outlier <- transition_scores
                subject_id <- aux_outlier$subject_id
                aux_outlier$subject_id <- NULL
                
                if(length(aux_outlier[aux_outlier<=Tukey_threshold])==0){ # Only normal
                  aux_outlier[aux_outlier>Tukey_threshold & aux_outlier!=1] <- 0 # Normal
                }else{
                  aux_outlier[aux_outlier<=Tukey_threshold] <- 1 # Anomalies
                  if(length(aux_outlier[aux_outlier>Tukey_threshold & aux_outlier!=1])!=0) # There are normals
                    aux_outlier[aux_outlier>Tukey_threshold & aux_outlier!=1] <- 0 # Normal
                }
                
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
                
                Score_analysis_transition_mode <<- "TT" # Transition Tukey mode
                
                Tukey_threshold <<- unname(Tukey_threshold) #### It was a named numeric
                
                if(round(Tukey_threshold, digits = 2) < as.integer(min(score_array), digits = 2)) # Tukey can give values outside the domain
                {
                  updateSliderInput(session, "Threshold_slider", value = round(Tukey_threshold, digits = 2),
                                    min = round(Tukey_threshold, digits = 2), max = round(as.double(max(score_array)), digits = 2)-0.01)
                }else{
                  updateSliderInput(session, "Threshold_slider", value = round(Tukey_threshold, digits = 2),
                                    min = as.integer(min(score_array), digits = 2), max = round(as.double(max(score_array)), digits = 2)-0.01)
                }
                # updateSliderInput(session, "Threshold_slider", value = as.numeric(Tukey_threshold),
                #                     min = as.integer(min(score_array), digits = 2), max = round(as.double(max(score_array)), digits = 2)-0.01)
                

                output$transition_hist_plot <- renderPlot({
                  bins <- seq(min(score_array), max(score_array), length.out = input$bins_trans + 1)
                  histogram_plot(Score_analysis_mode = Score_analysis_transition_mode,score_array=score_array,Threshold = Tukey_threshold, bins=bins)
                  legend("topright", c("Normal", "Outlier"), col=c("springgreen3", "red3"), lwd=10)
                  aux <- paste("Tukey's threshold: ", round(Tukey_threshold, digits = 2))
                  mtext(aux, side=3)
                  abline(v=Tukey_threshold,col="red", lwd=2) # Computed Threshold
                  rm(aux)
                })
                
              # For the color matrix that shows the outliers:
              # Reactive to Subject Slider
              output$transition_mat_plot <- renderPlot({
                  
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
            
            ################## GMM TRANSITION SCORE_ANALYSIS
            observeEvent(input$GMM_button, {
              
              #print("GMM pressed")
              #output$transition_hist_plot <- renderPlot({
                
                if(GMM_threshold == 0){ # first time
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
                
                
                if(GMM_threshold == 0){ # Dont recompute if the model is the same
                  shinyjs::show(id = "loading-content", anim = TRUE, animType = "fade")
                  GMM_threshold <<- GMM_score_analysis(score_array)
                  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
                }
                ############################# OUTLIERS DOWNLOAD BUTTON
                aux_outlier <- transition_scores
                subject_id <- aux_outlier$subject_id
                aux_outlier$subject_id <- NULL
                
                if(length(aux_outlier[aux_outlier<=GMM_threshold])==0){ # Only normal
                  aux_outlier[aux_outlier>GMM_threshold & aux_outlier!=1] <- 0 # Normal
                }else{
                  aux_outlier[aux_outlier<=GMM_threshold] <- 1 # Anomalies
                  if(length(aux_outlier[aux_outlier>GMM_threshold & aux_outlier!=1])!=0) # There are normals
                    aux_outlier[aux_outlier>GMM_threshold & aux_outlier!=1] <- 0 # Normal
                }
                
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
                
                updateSliderInput(session, "Threshold_slider", value = as.numeric(GMM_threshold),
                                  min = as.integer(min(score_array), digits = 2), max = round(as.double(max(score_array)), digits = 2)-0.01)
                
                Score_analysis_transition_mode <<- "TG" # Transition GMM mode
                
                output$transition_hist_plot <- renderPlot({
                  bins <- seq(min(score_array), max(score_array), length.out = input$bins_trans + 1)
                  histogram_plot(Score_analysis_mode = Score_analysis_transition_mode,score_array=score_array,Threshold = GMM_threshold, bins=bins)
                  legend("topright", c("Normal", "Outlier"), col=c("springgreen3", "red3"), lwd=10)
                  aux <- paste("GMM's threshold: ", round(GMM_threshold, digits = 2))
                  mtext(aux, side=3)
                  abline(v=GMM_threshold,col="red", lwd=2) # Computed Threshold
                  rm(aux)
                })
                
              # For the color matrix that shows the outliers:
              # Reactive to Subject Slider
              output$transition_mat_plot <- renderPlot({
                
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
            
            No_react <<- FALSE
            ################# MANUA TRANSITIONL SCORE_ANALYSIS
            observeEvent(input$Threshold_slider,{
              if(No_react){ #### So that the slider does not react to itself
                No_react <<- FALSE
                return()
              }
              
              if(flag_transition_train && Score_analysis_transition_mode!="TM"){ # There was another training
                print("Another training")
                flag_transition_train <<- FALSE
                return()
              }
              
              Threshold_plot <<- input$Threshold_slider
              
              if((Threshold_plot==0 && GMM_threshold==0 && Tukey_threshold==0)){ #### First time
                aux <- transition_scores
                aux$subject_id <- NULL
                No_react <<- TRUE
                
                #print("bro...")
                updateSliderInput(session, "Threshold_slider", value = as.integer(min(aux), digits = 2),
                                  min = as.integer(min(aux), digits = 2), max = round(as.double(max(aux)), digits = 2)-0.01)
                
              }
              
              print("CUSTOM")

              if((Threshold_plot == round(GMM_threshold, digits = 2) || Threshold_plot == round(Tukey_threshold, digits = 2))){ # The user pressed a strategy button
                 #print("Strategy") ### A strategy button was pressed
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
                
                aux <- as.vector(t(aux_outlier))
                score_array <- aux ## array with all scores in serie

                if(length(aux_outlier[aux_outlier<=Threshold_plot])==0){ # Only normal
                  print("only normal")
                  aux_outlier[aux_outlier>Threshold_plot & aux_outlier!=1] <- 0 # Normal
                }else{
                  aux_outlier[aux_outlier<=Threshold_plot] <- 1 # Anomalies
                  if(length(aux_outlier[aux_outlier>Threshold_plot & aux_outlier!=1])!=0) # There are normals
                    aux_outlier[aux_outlier>Threshold_plot & aux_outlier!=1] <- 0 # Normal
                }

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

          
                Score_analysis_transition_mode <<- "TM" # Transition Manual mode
                
                output$transition_hist_plot <- renderPlot({
                  bins <- seq(min(score_array), max(score_array), length.out = input$bins_trans + 1)
                  histogram_plot(Score_analysis_mode = Score_analysis_transition_mode,score_array=score_array,Threshold = Threshold_plot, bins=bins)
                  legend("topright", c("Normal", "Outlier"), col=c("springgreen3", "red3"), lwd=10)
                  aux <- paste("Threshold: ", round(Threshold_plot, digits = 2))
                  mtext(aux, side=3)
                  abline(v=Threshold_plot,col="red", lwd=2) # Computed Threshold
                  rm(aux)
                })
                

                # For the color matrix that shows the outliers:
                # Reactive to Subject Slider
                output$transition_mat_plot <- renderPlot({

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
                
                aux <- transition_scores
                aux$subject_id <- NULL
                updateSliderInput(session, "Threshold_slider", value = Threshold_plot,
                                  min = as.integer(min(aux), digits = 2), max = round(as.double(max(aux)), digits = 2)-0.01)
                
              }
            })
            
            #### This is only done in the begining
            Tukey_threshold_subje <<- 0
            GMM_threshold_sub <<- 0
            Threshold_plot_sub <<- 0 # This only done once in the beginning after the oberve_event
            
            ################## BUTTON FOR TUKEY SUBJECT SCORE_ANALYSIS
            observeEvent(input$Tukey_button_sub, {
          
              #output$subject_hist_plot <- renderPlot({
              print("Tukey Subject")
              
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
              
              #bins <- seq(min(score_array), max(score_array), length.out = input$bins_subje + 1)
              
              if(Tukey_threshold_subje==0)
                Tukey_threshold_subje <<- Threshold(score_array)
              
              ############################# OUTLIERS DOWNLOAD BUTTON
              aux_subj <- subject_scores
              aux_subj$outlier <- NULL
   
              aux_subj$outlier=factor(c(0,1))
              if(nrow(aux_subj[aux_subj$score<=Tukey_threshold_subje,])==0){ # No anomalies
                aux_subj[aux_subj$score>Tukey_threshold_subje & aux_subj$score!=1,]$outlier <- 0 # Only Normal
              }else{
                aux_subj[aux_subj$score<=Tukey_threshold_subje,]$outlier <- 1 # Anomalies
                if(nrow(aux_subj[aux_subj$score>Tukey_threshold_subje & aux_subj$score!=1,])!=0){ #Are there normals?
                  aux_subj[aux_subj$score>Tukey_threshold_subje & aux_subj$score!=1,]$outlier <- 0 # Normal
                }
              }
              
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
              
              Score_analysis_subject_mode <<- "ST"
              
              Tukey_threshold_subje <<- unname(Tukey_threshold_subje) #### It was a named numeric
              
              #print("update slider Tukey")
              #print(round(Tukey_threshold_subje, digits = 2))
              if(round(Tukey_threshold_subje, digits = 2) < as.integer(min(score_array), digits = 2)) # Tukey can sometimes give a threshold outside the domain
              {
                updateSliderInput(session, "Threshold_slider_sub", value = round(Tukey_threshold_subje, digits = 2),
                    min = round(Tukey_threshold_subje, digits = 2), max = round(as.double(max(score_array)), digits = 2)-0.01)
              }else{
                 updateSliderInput(session, "Threshold_slider_sub", value = round(Tukey_threshold_subje, digits = 2),
                    min = as.integer(min(score_array), digits = 2), max = round(as.double(max(score_array)), digits = 2)-0.01)
              }

              output$subject_hist_plot <- renderPlot({
                  bins <- seq(min(score_array), max(score_array), length.out = input$bins_subje + 1)
                  histogram_plot(Score_analysis_mode = Score_analysis_subject_mode,score_array=score_array,Threshold = Tukey_threshold_subje, bins=bins)
                  legend("topright", c("Normal", "Outlier"), col=c("springgreen3", "red3"), lwd=10)
                  aux <- paste("Tukey's threshold: ", round(Tukey_threshold_subje, digits = 2))
                  mtext(aux, side=3)
                  abline(v=Tukey_threshold_subje,col="red", lwd=2) # Computed Threshold
                  rm(aux)
              })
              
            
            # For the color matrix that shows the outliers:
            # Reactive to Subject Slider
            output$subject_mat_plot <- renderPlot({

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
          
            ################## BUTTON FOR GMM SUBJECT SCORE_ANALYSIS
            observeEvent(input$GMM_button_sub, {
            
              
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
              #bins <- seq(min(score_array), max(score_array), length.out = input$bins_subje + 1)

              if(GMM_threshold_sub == 0){ # Dont recompute if the model is the same
                shinyjs::show(id = "loading-content", anim = TRUE, animType = "fade")
                GMM_threshold_sub <<- GMM_score_analysis(score_array)
                shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
              }
              
              ############################# OUTLIERS DOWNLOAD BUTTON
              aux_subj <- subject_scores
              aux_subj$outlier <- NULL
              
              aux_subj$outlier=factor(c(0,1))
              if(nrow(aux_subj[aux_subj$score<=GMM_threshold_sub,])==0){ # No anomalies
                aux_subj[aux_subj$score>GMM_threshold_sub & aux_subj$score!=1,]$outlier <- 0 # Only Normal
              }else{
                aux_subj[aux_subj$score<=GMM_threshold_sub,]$outlier <- 1 # Anomalies
                if(nrow(aux_subj[aux_subj$score>GMM_threshold_sub & aux_subj$score!=1,])!=0){ #Are there normals?
                  aux_subj[aux_subj$score>GMM_threshold_sub & aux_subj$score!=1,]$outlier <- 0 # Normal
                }
              }
              
              
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

              Score_analysis_subject_mode <<- "SG"
              
              updateSliderInput(session, "Threshold_slider_sub", value = as.numeric(GMM_threshold_sub),
                                min = as.integer(min(score_array), digits = 2), max = round(as.double(max(score_array)), digits = 2)-0.01)
              
              output$subject_hist_plot <- renderPlot({
                bins <- seq(min(score_array), max(score_array), length.out = input$bins_subje + 1)
                histogram_plot(Score_analysis_mode = Score_analysis_subject_mode,score_array=score_array,Threshold = GMM_threshold_sub, bins=bins)
                legend("topright", c("Normal", "Outlier"), col=c("springgreen3", "red3"), lwd=10)
                aux <- paste("GMM's threshold: ", round(GMM_threshold_sub, digits = 2))
                mtext(aux, side=3)
                abline(v=GMM_threshold_sub,col="red", lwd=2) # Computed Threshold
                rm(aux)
              })
            
            # For the color matrix that shows the outliers:
            # Reactive to Subject Slider
            output$subject_mat_plot <- renderPlot({
              
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
            
            No_react_sub <<- FALSE
            ################## BUTTON FOR MANUAL SUBJECT SCORE_ANALYSIS
            observeEvent(input$Threshold_slider_sub,{
              if(No_react_sub){ # So that the slider does not react to itself
                No_react_sub <<- FALSE
                return()
              }
              

              if(flag_subject_train && Score_analysis_subject_mode!="SM"){ # There was another training
                print("Another training")
                flag_subject_train <<- FALSE
                return()
              }
              
              Threshold_plot_sub <<- input$Threshold_slider_sub
              
              if((Threshold_plot_sub==0 && GMM_threshold_sub==0 && Tukey_threshold_subje==0)){ #### First time
                aux <- subject_scores
                aux$subject_id <- NULL
                No_react_sub <<- TRUE
                
                #print("bro...")
                updateSliderInput(session, "Threshold_slider_sub", value = as.integer(min(aux), digits = 2),
                                  min = as.integer(min(aux), digits = 2), max = round(as.double(max(aux)), digits = 2)-0.01)
              }
              
              #print(Threshold_plot_sub)
              #print(round(as.double(Tukey_threshold_subje), digits = 2))
              if(Threshold_plot_sub == round(GMM_threshold_sub, digits = 2) || Threshold_plot_sub == round(as.double(Tukey_threshold_subje), digits = 2)){ # The user pressed a strategy button
                ### Dont react
                #print("Strategy")
              }else{
                #print("Manual")
                if(Threshold_plot_sub == 0){ # first time
                  updateSliderInput(session, "bins_subje", value = 30, min = 1, max = 50) # estava bins_trans
                  
                  updateSliderInput(session, "slider_color_subje", value = c(0, nrow(subject_scores)%/%10), # Initial value is 10%
                                    min = 0, max = nrow(subject_scores))
                }
                
                #output$subject_hist_plot <- renderPlot({
                  #print("Render Manual")
                  shinyjs::disable(id = "download_subjects")
                  aux <- subject_scores
                  aux$subject_id <- NULL
                  aux <- as.vector(t(aux))
                  score_array <- aux ## array with all scores in serie
                  #bins <- seq(min(score_array), max(score_array), length.out = input$bins_subje + 1)
                  
                  ############################# OUTLIERS DOWNLOAD BUTTON
                  aux_subj <- subject_scores
                  aux_subj$outlier <- NULL
                  
                  aux_subj$outlier=factor(c(0,1))
                  if(nrow(aux_subj[aux_subj$score<=Threshold_plot_sub,])==0){ # No anomalies
                    aux_subj[aux_subj$score>Threshold_plot_sub & aux_subj$score!=1,]$outlier <- 0 # Only Normal
                  }else{
                    aux_subj[aux_subj$score<=Threshold_plot_sub,]$outlier <- 1 # Anomalies
                    if(nrow(aux_subj[aux_subj$score>Threshold_plot_sub & aux_subj$score!=1,])!=0){ #Are there normals?
                      aux_subj[aux_subj$score>Threshold_plot_sub & aux_subj$score!=1,]$outlier <- 0 # Normal
                    }
                  }
                  
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
                  
                  
                  # updateSliderInput(session, "Threshold_slider_sub", value = as.numeric(Threshold_plot_sub),
                  #                   min = as.integer(min(score_array), digits = 2), max = 0)
                  
                  Score_analysis_subject_mode <<- "SM"
                  
                  updateSliderInput(session, "Threshold_slider_sub", value = as.numeric(Threshold_plot_sub),
                                    min = as.integer(min(score_array), digits = 2), max = round(as.double(max(score_array)), digits = 2)-0.01)
                  
                  output$subject_hist_plot <- renderPlot({
                    bins <- seq(min(score_array), max(score_array), length.out = input$bins_subje + 1)
                    histogram_plot(Score_analysis_mode = Score_analysis_subject_mode,score_array=score_array,Threshold = Threshold_plot_sub, bins=bins)
                    legend("topright", c("Normal", "Outlier"), col=c("springgreen3", "red3"), lwd=10)
                    aux <- paste("Threshold: ", round(Threshold_plot_sub, digits = 2))
                    mtext(aux, side=3)
                    abline(v=Threshold_plot_sub,col="red", lwd=2) # Computed Threshold
                    rm(aux)
                  })
                  
                #})
                
                # For the color matrix that shows the outliers:
                # Reactive to Subject Slider
                output$subject_mat_plot <- renderPlot({

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
                aux <- subject_scores
                aux$subject_id <- NULL
                
                updateSliderInput(session, "Threshold_slider_sub", value = Threshold_plot_sub,
                                  min = as.integer(min(aux), digits = 2), max = round(as.double(max(aux)), digits = 2)-0.01)
              }
            })
          
            
            myTabs<<-c(tabs1,tabs2,tabs3,lapply(4, fourth_panel), lapply(5, fifth_panel))
            
            shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade") 
            shinyjs::hide(id = "third_button")
            shinyjs::show(id = "fourth_div")
            #shinyjs::show(id = "fourth_button")
          
        }

            
          do.call(tabsetPanel, c(myTabs, id = "tabsetpanel_id"))
          
          
        })

  
    ##############
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
          
          #################
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
    #   # rm(aux_text)
    #   # rm(Tukey_threshold)
    #   # rm(Tukey_threshold_subje)
    #   # rm(Scores)
    # })
    
    
    
    session$onSessionEnded(function() {
      print("Stopped application")
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


