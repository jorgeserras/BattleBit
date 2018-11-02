

library(shiny)
library(DiagrammeR)



first_panel <- function(n){
  tabPanel(value = "input_data_tab","Input Data",
           
           tags$br(),
           
           
           tags$div(
             HTML( "
                   <style>
                   
                   .input_data_title, .download_title_processing{
                   text-align: center;
                   width:100%
                   }
                   
                   </style>
                   
                   <h2 class=\"input_data_title\">Input Data</h2>
                   
                   ")
             ),
           
           tags$hr(),
           
           paste("Input data can be observed in the table below. When advancing through each phase of the system, previous tabs can be selected and inspected, even when these are not interactive anymore."),
           
           tags$br(),
           
           tags$br(),
           
           dataTableOutput("first_panel_table"),
           
           tags$br()
           
           
  )
  
}


second_panel <- function(n){
  
  
  tabPanel(value = "processing_tab","Pre-Processing",
           tags$br(),
           
           
           tags$div(
             HTML( "
                   <style>
                   
                   .processing_title, #download_processed{
                   text-align: center;
                   width:100%
                   }
                   
                   </style>
                   
                   <h2 class=\"processing_title\">Pre-processing</h2>
                   
                   ")
             ),
           
           tags$hr(),
           
      div(id = "processing_description", textOutput("processing_description_string")),
      
      tags$br(),
      
      # PLOT OF MEAN AND STD NORMALIZED 
      
      plotOutput(outputId = "mean_std_plot"),
    
      
      # COOL TO PLOT A SPECIFIC NORMALIZED SUBJECT SELECTED BY THE USER 
      
      dataTableOutput("second_panel_table"),
      
      tags$hr(),
      
      div(id="download_processing_div",
        tags$div(
          HTML( "<h2 class=\"download_title_processing\">Download Processed Data</h2>")
        ),
        
        div(id = "download_button_discrete",
            #h2("The Download Button will be functional once formatting is complete with no errors."),
            textOutput("button_text1"),
    
            tags$div(id = "download_processed" ,downloadButton("download_data_discrete", label = "Download processed dataset"))
            
        )
      ),
      
      tags$br()
    
  )
  
}


third_panel <- function(n){
  tabPanel(value = "modeling_tab","DBN Modeling and Scoring",
           
           tags$br(),
           
           
           tags$div(
             HTML( "
                   <style>
                   .btn-toolbar {
                   margin: auto;
                   display: flex;
                   flex-direction: row;
                   justify-content: center;
                   }
                   
                   .dbn_modeling_title, .download_title{
                   text-align: center;
                   width:100%
                   }
                   
                  </style>
                  
                  <h2 class=\"dbn_modeling_title\">DBN Modeling and Scoring</h2>
                  
                  ")
             ),
           
           tags$hr(),
           
           div(id = "model_description", textOutput("modeling_description_string")),
           
           tags$br(),
           
           div(id = "diagram_div",grVizOutput('structure_diagram_grviz')),
           
           
           tags$hr(),
           
           tags$div(
             HTML( "<h2 class=\"download_title\">Downloads</h2>")
           ),
           
           div(id = "download_transition_scores_div", style = "display: flex; align-items: center; justify-content: center;",
               
             div(id = "download_button_transition_scores",
                 #h2("The Download Button will be functional once formatting is complete with no errors."),
                 textOutput("button_text2"),
                 
                 downloadButton("download_transition_scores", label = "Download Transition Scores")
                 
             ),
             
             div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
             
             div(id = "download_button_subject_scores",
                 #h2("The Download Button will be functional once formatting is complete with no errors."),
                 textOutput("button_text3"),
                 
                 downloadButton("download_subject_scores", label = "Download Subject Scores")
                 
             ),
             
             div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
             
             div(id = "download_button_structure",
                 #h2("The Download Button will be functional once formatting is complete with no errors."),
                 textOutput("button_text4"),
                 
                 downloadButton("download_structure", label = "Download DBN Structure")
                 
             )
           )
        
  )
  
}

fourth_panel <- function(n){
  tabPanel(value = "transition_tab","Transition Outliers",
           
           tags$br(),
           
           
           tags$div(
             HTML( "
                   <style>
                   .btn-toolbar {
                   margin: auto;
                   display: flex;
                   flex-direction: row;
                   justify-content: center;
                   }
                   
                   .score_analysis_title, .download_title {
                   text-align: center;
                   width:100%
                   }
                   
                   #download_button_transition_outliers { 
                   text-align: center;
                   width:100%
                  }
                  
                  </style>
                  
                  
                  <h2 class=\"score_analysis_title\">Score-Analysis Strategy</h2>
                  
                  <div class=\"btn-toolbar\">
                  <div class=\"btn-group\">
                  <button id=\"Tukey_button\" type=\"button\" class=\"btn btn-info btn-default action-button shiny-bound-input\">Tukey</button>
                  <button id=\"GMM_button\" type=\"button\" class=\"btn btn-info btn-default action-button shiny-bound-input\">GMM</button>
                  </div>
                  </div>
                  
                  ")
             ),
           
           tags$hr(),
           
           textOutput("Tuckey_threshold_text"),
           
           # Output: Histogram ----
           plotOutput(outputId = "transition_hist_plot"),
           
           hidden(
           div(id = "sliders_trans", style = "display: flex; align-items: center; justify-content: center;",
               div( sliderInput(inputId = "bins_trans",
                                label = "Number of bins:",
                                min = 1,
                                max = 50,
                                value = 30)),
               
               div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
               
               div( sliderInput("slider_color_trans", label = "Subject range:", min = 0,
                                max = 100, value = c(40, 60)))
           )),
           
           plotOutput(outputId = "transition_mat_plot"),
           
           tags$hr(),
           
           tags$div(
             HTML( "<h2 class=\"download_title\">Download Transition Results</h2>")
           ),
           
           div(id = "download_button_transition_outliers",
               disabled(
                 downloadButton("download_transition", label = "Download Transition Outliers")
               )
           ),
           
           tags$br()

  )
  
}

fifth_panel <- function(n){
  tabPanel(value = "subject_tab","Subject Outliers",
           
           #paste("Subject Outlier Detection"),
           tags$br(),


           tags$div(
            HTML( "
                  <style>
                    .btn-toolbar {
                      margin: auto;
                      display: flex;
                      flex-direction: row;
                      justify-content: center;
                    }

                    .score_analysis_title, .download_title {
                          text-align: center;
                          width:100%
                    }

                    #download_button_subject_outliers { 
                        text-align: center;
                        width:100%
                    }



                  </style>


                  <h2 class=\"score_analysis_title\">Score-Analysis Strategy</h2>

                  <div class=\"btn-toolbar\">
                    <div class=\"btn-group\">
                      <button id=\"Tukey_button_sub\" type=\"button\" class=\"btn btn-info btn-default action-button shiny-bound-input\">Tukey</button>
                      <button id=\"GMM_button_sub\" type=\"button\" class=\"btn btn-info btn-default action-button shiny-bound-input\">GMM</button>
                    </div>
                  </div>
                  
                  ")
           ),
           
           tags$hr(),
           
           #actionButton("Tukey_button_sub", "Tukey"),
           #actionButton("GMM_button_sub", "GMM"),
           
           # Output: Histogram ----
           plotOutput(outputId = "subject_hist_plot"),
           
           shinyjs::hidden(
           div(id = "sliders_subje", style = "display: flex; align-items: center; justify-content: center;",
               div( sliderInput(inputId = "bins_subje",
                         label = "Number of bins:",
                         min = 1,
                         max = 50,
                         value = 30)),
              
               div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
               
               div( sliderInput("slider_color_subje", label = "Subject range:", min = 0,
                         max = 100, value = c(40, 60)))
           ))
           ,
           
           
           
           plotOutput(outputId = "subject_mat_plot"),
           
           
           tags$hr(),
           
           tags$div(
             HTML( "<h2 class=\"download_title\">Download Subject Results</h2>")
           ),
           
           div(id = "download_button_subject_outliers",
               disabled(
                 downloadButton("download_subjects", label = "Download Subject Outliers")
               )
           ),
           
           tags$br()
           
  )
  
}




