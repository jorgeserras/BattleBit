
frontp = function() div(class = "frontp",
                        div(class = "front-banner",
                            div(class = "imgcon"),
                            div(class = "hcon", br(), br(), h1("The New Zealand"), h1("Tourism Dashboard"))
                        ),
                        #tags$p(tags$span(class = "warning", "This app is still in active development and has not been officially launched.")),
                        tags$p(class = "intro", "The New Zealand Tourism Dashboard is a one-stop shop for all information about tourism. It brings together a range of tourism datasets produced by MBIE and Statistics New Zealand into one easy-to-use tool. Information is presented using dynamic graphs and data tables."),
                        div(class = "intro-divider"),
                        tags$p("Main subject-area groupings of tourism data are shown on the toolbar above. To navigate around this site, left-click one of the subject areas and then select one of the related categories in the drop down list."),
                        tags$p("Graphs can be downloaded as png and tables can be downloaded to a csv file. Information on how to use the graphs and tables, along with an example, is available in the",
                               tags$a("Help", title = "Help Tab", href = "#", id = "HelpTabLink"), "tab.",
                               tags$script("$('#HelpTabLink').click(function(){$('a[data-value=\"Help\"]')[0].click();});")
                        ),
                        div(class = "box-con",
                            tags$a(target = "_blank",
                                   href = "http://mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/tourism-data-sources",
                                   div(class = "float box box-more",
                                       tags$p(class = "intro", "Find out more"),
                                       tags$p("Click here for more information on the data sources used and their differences.")
                                   )),
                            tags$a(target = "_blank",
                                   href = "http://www.mbie.govt.nz/info-services/business/business-growth-agenda/regions",
                                   div(class = "float box box-rear",
                                       tags$p(
                                         tags$img(class = "rear-preview", src = "REAR-cover-2015.jpg"),
                                         "The", span(class = "bold", "Regional Economic Activity Report"),
                                         "produced by MBIE presents regional information and trends that supplements the information in this dashboard. Click here to find the online tool and mobile app."
                                       )
                                   )
                            )
                        ),
                        div(class = "box box-timeout",
                            tags$p(tags$span(class = "bold", "PLEASE NOTE:"),
                                   "This app may time-out if left idle too long, which will cause the screen to grey-out.",
                                   "To use the app again, refresh the page. This will reset all previously-selected input options.")
                        )
)