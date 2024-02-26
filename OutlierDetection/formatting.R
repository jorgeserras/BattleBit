


######## CODE FOR FORMATTING MORTALITY .csv

##### subject_id must be written manually

# library("cellWise")
# 
# data("mortality")
# 
# n_variables <- ncol(mortality)
# aux <- mortality[1,]
# 
# current_names <- sprintf("X%s__%s", seq(1:n_variables), 1)
# auxnames <- c(current_names)
# for (i in 2:nrow(mortality)) {
#   
#   aux <- cbind(aux, mortality[i,])
#   
#   current_names <- sprintf("X%s__%s", seq(1:n_variables), i)
#   
#   auxnames <- c(auxnames, current_names)
# }
# 
# rownames(aux) <- c("1")
# 
# colnames(aux) <- auxnames
# 
# write.csv(aux, "formated_mortality.csv")


# scores <- read.csv("scores_mortality_10_transition_output.csv")
# 
# out <- which(scores < -88.55)
# 
# out_years <- out + 1816 ############# 1816 ESTA BEM ???????



parseToPanel <- function(data){
  # Parsing the input data
  col <- colnames(data)
  numVariables <- 0
  variables <- c()
  time <- c()
  for(i in 2:length(col)){
    aux <- strsplit(col[i], "__")
    time = append(time, strtoi(aux[[1]][[2]]))
    variables = append(variables,aux[[1]][[1]])
  }
  
  time = unique(time)
  variables = unique(variables)
  subject_id <- data[,1]
  subject_id_panel <- c()
  
  for(i in 1:dim(data)[1]){
    x = rep(subject_id[i], length(time))
    subject_id_panel <- append(subject_id_panel, x)
  }  
  
  time_panel <- rep(time, dim(data)[1])
  
  panel_data = cbind(subject_id_panel, time_panel)
  for (variable in variables) {
    
    str <- c()
    for(t in time){
      str <- append(str,paste(variable,t, sep = "__"))
    }
    panel_data = cbind(panel_data, as.vector(t(data[,str])))
    
  }
  colnames(panel_data) =  c(col[1], "timestamp", variables)
  
  return(list(panel_data, length(variables))) ############ NEED TO KNOW NUMBER OF VARIABLES
}


parseToHorizontal <- function(data){
  
  data <- as.data.frame(data)
  data_horizontal <- c()
  col <- colnames(data)
  head <- c()
  head <- append(head, col[1])
  col <- col[-c(1,2)]
  time <- unlist(unique(data["timestamp"]))
  for(t in time){
    for(v in col){
      str <- paste(v, t, sep="__")
      head <- append(head, str)
    }
  }
  
  
  subjects <- unlist(unique(data["subject_id"]))
  for(subject in subjects){
    data_aux <- c()
    data_aux <- append(data_aux, subject)
    aux <- subset(data, subject_id == subject)
    aux <- as.vector(t(aux[,-c(1,2)]))
    data_aux <- append(data_aux, aux)
    data_horizontal <- cbind(data_horizontal, data_aux)
  }
  data_horizontal <- as.matrix(t(data_horizontal))
  colnames(data_horizontal) <- c(head)
  rownames(data_horizontal) <- c()
  # data_horizontal[,-c(1)] <- format(data_horizontal[,-c(1)], nsmall = 1)
  # data_horizontal[,1] <- format(data_horizontal[,1], nsmall = 0)
  return(data_horizontal)
  
}




format_data <- function(MTS_data,format){

  if(format=="H"){
    tryCatch(
      {
        aux_data <- parseToHorizontal(MTS_data)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(paste('The input file is not on panel format or presents other issues. Try Horizontal format and check for discrepancies.')))
      }
    )
  }else{
    tryCatch(
      {
        aux_data <- parseToPanel(MTS_data)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(paste('The input file is not on horizontal format or presents other issues. Try Panel format and check for discrepancies.')))
      }
    )
  }
  
  data_output <- data.frame(aux_data)
  
  # panel_data <- parseToPanel(MTS_data)
  # 
  # # Transform into a DataFrame
  # p_data <- data.frame(panel_data)
  # 
  # # Separate Subjects
  # DATA_BY_SUBJECT<-split(p_data, p_data$subject_id)
  # 
  # # Correct format for Java Algorithm:
  # MTS_data2 <- parseToHorizontal(panel_data)
  # 
  # 
  # # Write CSV of all subjects
  # write.csv(MTS_data2, file = paste(base_filename, "_APPROPRIATE_fixed_ready.csv", sep=""), row.names = FALSE)
  # 
  # ######### EXAMPLE FOR ONE SUBJECT
  # # Write CSV of one subject
  # subject_1 <- as.data.frame(DATA_BY_SUBJECT[1]$`0`)
  # subject_1$timestamp <- NULL
  # 
  # # Write CSV
  # write.csv(subject_1, file = paste(base_filename, "_APPROPRIATE_subject1.csv", sep=""), row.names = FALSE)
  # 

  return(data_output)
  
}









