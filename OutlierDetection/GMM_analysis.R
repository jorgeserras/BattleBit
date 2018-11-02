

library(mclust)


GMM_score_analysis <- function(Scores_data){

  # Scores <- Scores_data$score #case of subject scores
  # 
  # if(is.null(Scores)){
  #   # Transition Scores case
  #   aux <- Scores_data
  #   aux$subject_id <- NULL
  #   m = as.matrix(aux)
  #   rm(aux)
  #   Scores = c()
  #   for (i in seq(1:nrow(m))){
  #     Scores = c(Scores, as.numeric(m[i,]))
  #   }
  # }
  
  #print("BEFORE GMM")
  tryCatch({
    mod4 <- densityMclust(Scores_data) #was Scores before
  },
  error = function(e) {
    
    #print("Trying again") ### Initialization failed, so just try again
    mod4 <- densityMclust(Scores_data) #was Scores before
    # return a safeError if a parsing error occurs
    #stop(safeError(e))
  })
  #print("AFTER GMM")
  
  #mod4$classification #### Which Model (class) does each value belong too 
  
  #plot(mod4, what = "BIC")
  
  #plot(mod4, what = "density", data = Scores, breaks = 15)
  
  
  mod_scores <- Mclust(Scores_data, G=2) ### Force 2 Gaussians (2 classes, abnormal and normal)  # Was Scores before
  
  #### To determine the Treshold find the last value of class 1 and first of class 2 and make a mean of those values
  #### This is the threshold between Abnormality and normality
  
  results <- data.frame()
  
  results <- rbind(results,mod_scores$data)
  results <- cbind(results,mod_scores$classification)
  
  colnames(results) <- cbind("score","class") # Class 1 = Outlier , Class 2 = Normal
  
  outliers <- results[results$class == "1", ]
  outliers <- sort(outliers$score, decreasing = TRUE) # Sort the scores of the outliers
  
  first_outlier <- outliers[1] # First position is the outlier with higher score
  
  
  normals<- results[results$class == "2", ]
  normals <- sort(normals$score, decreasing = FALSE) # Sort the scores of the normals
  
  last_normal <- normals[1] # First position is the normal with lower score
  
  Threshold <- (first_outlier+last_normal)/2 #### Threshold is the mean of both values (its an option we could consider more complex stuff)
  
  #summary(mod_scores)
  #Threshold
  #plot(mod_scores, what = c("classification")) #### Select 2
  
  #plot(mod4, what = "density", data = Scores, breaks = 15, col = c("blue"))
  #title(main = "Optimal GMM for the obtained scores") # according to BIC for EM clustering
  #abline(v = Threshold, col = "red")
  #text(Threshold-1,0.5, paste("Threshold: ",round(Threshold,2)), col = 2, adj = c(-.1, -.1))
  
  
  print(Threshold)
  
  return(Threshold) # GMM threshold
  
}