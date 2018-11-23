
library(here)
library(shiny)
library(gsubfn)

source(here::here('formatting.R'))


library(jmotif)
# Check out: https://github.com/jMotif/jmotif-R#20-piecewise-aggregate-approximation-ie-paa
# This was also helpfull: https://stackoverflow.com/questions/46430559/sax-time-series-representation-in-r-using-the-jmotif-package
library(plyr)

library(dplyr)

library(qdapTools)

library(data.table)
library(Hmisc)


#library(ggfortify)
library(ggplot2)
library(gridExtra)

############################## SOMETIMES WE DONT WANT TO DO PAA SINCE WE HAVE VERY FEW POINTS
########### CHECK OTHER COMMENTED STUFF SO THAT PAA WORKS

#alphabet_size <- 4
#paa_size <- 1000



discretize <- function(MTS_data, alphabet_size, paa_size){
  
  # Original data is in Horizontal Format:
  #MTS_data <- read.csv(paste(base_filename, "_APPROPRIATE.csv", sep=""))
  #print("SAX!")
  
  
  list[panel_data, n_variables] <- parseToPanel(MTS_data)
  
  c(panel_data, n_variables)
  
  # Transform into a DataFrame
  p_data <- data.frame(panel_data)
  
  # Separate Subjects
  DATA_BY_SUBJECT<-split(p_data, p_data$subject_id)
  
  final_data_discrete <- data.frame()
  subject_normalized <- list()
  subject_normalized_paa <- list()
  
  current_subject <- 0
  
  for(i in DATA_BY_SUBJECT){
    subject <- as.data.frame(i)
    
    current_subject <- current_subject + 1
    
    # Store to regroup later
    id <- subject$subject_id
    timestamp <- subject$timestamp
    
    subject$subject_id <- NULL
    subject$timestamp <- NULL
    
    subject <- transpose(subject)
    
    subject <- as.list(as.data.frame(t(subject))) # Convert each row of the dataframe to a list (each list is a timeseries sequence)

    # Normalize the data (mean=0 and std=1)
    #subject_normalized <- llply(subject, function(x){znorm(x, threshold = 0.01)})
    subject_normalized <- append(subject_normalized,list(llply(subject, function(x){znorm(x, threshold = 0)}))) # 0.01

    #print(typeof(subject_normalized))
    
    

    # Reduce the dimensionality using PAA
    #subject_normalized_paa <- subject_normalized
    subject_normalized_paa <- append(subject_normalized_paa,list(llply(subject_normalized[current_subject][[1]], function(x){paa(x, paa_size)})))
    #subject_normalized_paa <- llply(subject, function(x){paa(x, paa_size)}) ########################## PAA
    #print("After PAA")
    #subject_normalized <- subject_normalized_paa
    #print(typeof(subject_normalized_paa))
    
    
    
    # Use the reduced data and convert it to symbolic using a certain alphabet size
    subject <- llply(subject_normalized_paa[current_subject][[1]], function(x){series_to_string(x, alphabet_size)}) 
    
    # Turn each character into an element, columns represent position on string
    subject <- llply(subject, function(x){strsplit (x, "")}) 
    
    subject_dataframe = ldply(transpose(unlist(subject, recursive = FALSE )))
    
    
    #subject_regroup <- data.frame(cbind(id, timestamp, subject_dataframe))
    #names(subject_regroup)[1] <- "subject_id"
    
    #print(nrow(subject_dataframe))
    #subject_regroup <- data.frame(cbind(id[1:nrow(subject_dataframe)], timestamp[1:nrow(subject_dataframe)], subject_dataframe))
    #subject_regroup <- data.frame(cbind(rep(id,nrow(subject_dataframe)), timestamp[1:nrow(subject_dataframe)], subject_dataframe))
    
    subject_regroup <- data.frame(cbind(id[1:nrow(subject_dataframe)], timestamp[1:nrow(subject_dataframe)], subject_dataframe))
    names(subject_regroup)[1] <- "subject_id"
    names(subject_regroup)[2] <- "timestamp"
    
    #print(head(subject_regroup))
    # Correct format for Java Algorithm:
    subject_data_horizontal <- parseToHorizontal(subject_regroup)
    
    final_data_discrete <- rbind.data.frame(final_data_discrete, subject_data_horizontal)
    #print(head(final_data_discrete))
  }
  
  # Write CSV
  #write.csv(final_data_discrete, file = paste(base_filename, "_APPROPRIATE_DISCRETE.csv", sep=""), row.names = FALSE)
  out <<- list(final_data_discrete, subject_normalized, timestamp, n_variables)
  
  return(out) # subject_normalized, timestamp are needed for the plot
  
  
}



mean_std_plot <- function(subject_normalized, timestamp, n_variables, alphabet_size, output){
  
  ###### AUXILIARY COMPUTATIONS TO TRANSFORM THE NORMALIZED DATA TO PANEL DATA
  normalized_MTS_data <- data.frame()
  
  for(i in 1:length(subject_normalized)){
    subject_id <- array(i, dim = length(timestamp)) # timestamp was declared earlier when handeling each subject
    normalized_MTS_data <- rbind(normalized_MTS_data,data.frame(cbind(subject_id, timestamp, data.frame(subject_normalized[[i]]))))
  }
  
  
  ############## THIS DEPENDS ON THE NUMBER OF VARIABLES AND THE NUMBER OF ROWS OF THE DATAFRAMES:
  ################
  normalized_for_plot <- normalized_MTS_data
  normalized_for_plot$subject_id <- NULL
  
  normalized_for_plot_V <- list()
  
  aux <- data.frame()
  aux3 <- rep(0, 0)
  for(i in 1:n_variables){
    v_i <- paste("V",i, sep = "") # name of the variable
    aux2 <- data.frame(normalized_for_plot$timestamp, normalized_for_plot %>% select_(v_i))# normalized_for_plot$V1
    colnames(aux2) <- c("timestamp", "value")
    
    aux <- rbind(aux, aux2)
    
    aux3 <- c(aux3, rep(v_i,nrow(aux2)))
  }
  normalized_for_plot <- aux
  normalized_for_plot$variable <- aux3
  
  # normalized_for_plot1 <- data.frame(normalized_for_plot$timestamp, normalized_for_plot$V1)
  # colnames(normalized_for_plot1) <- c("timestamp", "value")
  # 
  # normalized_for_plot2 <- data.frame(normalized_for_plot$timestamp, normalized_for_plot$V2)
  # colnames(normalized_for_plot2) <- c("timestamp", "value")
  
  #normalized_for_plot <- rbind(normalized_for_plot1,normalized_for_plot2)
  
  #$variable=c(rep("V1",nrow(normalized_for_plot1)),rep("V2",nrow(normalized_for_plot1))) # DIVIDE THE ROWS INTO EQUAL SIZED REGIONS ACCORDING TO NUMBER OF VARIABLES
  
  #output$mean_std_plot <- renderPlot({
  
    return(
        plot_combined <- ggplot(normalized_for_plot, aes(timestamp,value,group=variable,col=variable)) + 
          stat_summary(fun.y = 'mean', geom = 'line', size = 1.3) +
          stat_summary(fun.data = 'mean_sdl', geom = 'ribbon', alpha = 0.2) +
          geom_hline(yintercept = alphabet_to_cuts(alphabet_size)[2:alphabet_size], color = 'magenta', linetype="dashed") +
          ggtitle("Mean and standard deviation of each timestamp for every normalized variable") +
          theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold")) +
          xlab("Timestamp") + ylab("Normalized value")
    )
  
  #})
  
}




