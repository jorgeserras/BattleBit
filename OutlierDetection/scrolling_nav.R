
#Scrollable nav methods for Shiny

#custom developed by Z Albertyn


#use this to set up a bootstrap-enabled scrolling page 

#required in UI.R

nav_include_in_UI<-function(){
	tagList(
	tags$script(src="jquery.easing.min.js"),
	tags$script(src="scrolling-nav.js")
	)
}

#include CSS in UI.R
nav_include_css<-function(){
	singleton(tags$head(tags$link(href='scrolling-nav.css',rel='stylesheet',type='text/css')))
}


#Create the top navbar with 4 titles and the target ID 
scrollable_navmenu<-function(tlist,toptarget=NULL){
if (is.null(toptarget)) {
	toptarget="ptop"
}
menulist=	lapply(tlist,function(x){
			link=gsub("\\s+|\\?","",x,perl=T);
			tags$li(tags$a(class="page-scroll btn",type="button",href=sprintf("#%s",link),tags$li(h5(x)))) 
			} 
			)
spanlist= lapply(tlist,function(x){
			tags$span(class="icon-bar")
			} 
			)
tagList(
	 div(id=sprintf("%s",toptarget),"data-spy"="scroll","data-target"=".navbar-fixed-top"),
	 tags$nav(class="navbar navbar-default",role="navigation",
	   tags$div(class="container",
		 tags$div(class="navbar-header page-scroll",
		    tags$button(class="navbar-toggle","data-toggle"="collapse", "data-target"=".navbar-ex1-collapse",
				  spanlist
		   )
	    )
	    ),
	  div(class="collapse navbar-collapse navbar-ex1-collapse",	
			tags$ul(class="nav navbar-nav",
			menulist
		)
	)
  )
  )

}



#This is method to assign content
#inputId, label, icon = NULL, header = "", content = "")
nav_content_section<-function(inputID,toptarget=NULL,content="",class="") {
if (is.null(toptarget)) {
	toptarget="ptop"
}
	link=gsub("\\s+|\\?","",inputID,perl=T);
	tags$section(id=link, class=class,
	#tags$a(class="navbar-brand page-scroll",href=sprintf("#%s",toptarget),"Top"),
	#hr(),
		tags$div(class="container",
				content
		),
		tags$a(class="navbar-brand page-scroll",href=sprintf("#%s",toptarget),"Top"),
		hr()
	)
}


example_multilevel_tab<-function(tabname){
#Setup the content 
	  content1= fluidPage(
		  h2("Gene Frequency"),
		  wellPanel(
			fluidRow(
				uiOutput("freqchartopts"),
				checkboxInput("freqtype","Percentage",value=FALSE)
			)
			),
			fluidRow(
			plotOutput("genefreqbar",height=600,width=800)
			)
		)
	   content2= 	fluidRow(
			h3("Variant Count Distribution"),
			plotOutput("vbox",height=900,width=1400)
	    )
		
	#insert the content into the nav_content_section and scrollable_navmenu part
     tabPanel(tabname,icon = icon("bar-chart"),
		scrollable_navmenu(c("Gene Frequency","Variant Count Distribution")), #build the menu to navigate scrolling page, use toptarget if using multiple navbars in an app
	 #individual section elements
	 nav_content_section("Gene Frequency",class="intro-section",
		content=content1
	  ),
	  #second nav section, the ids should be unique for each 
   	 nav_content_section("Variant Count Distribution",class="intro-section",
	 content= content2	
	)
	)		
}
