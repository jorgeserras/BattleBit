
#library(aws.s3)
library(digest)

# Upload data to AWS bucket

saveData <- function(data, s3BucketName) {
  # Create a temporary file to hold the data
  
  #data <- t(data)
  
  # Create a unique file name:
  file_name <- paste0(
    paste(
      as.integer(Sys.time()),
      digest(data, algo = "md5"),
      sep = "_"
    ),
    ".csv"
  )
  
  # Create a unique file name
  #file_name <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  
  
  file_path <- file.path(tempdir(), file_name)
  write.csv(data ,file_path, row.names = FALSE, quote = TRUE)
  
  # Upload the file to S3
  #put_object(file = file_path, object = file_name, bucket = s3BucketName)
  
  return(file_path) # return the Local filepath
}


# Download a specific file from AWS

loadData <- function(data) {
  # data should be a string of the name of the file to remove
  object <- get_object(data, s3BucketName)
  object_data <- readBin(object, "character")
  data <- read.csv(text = object_data, stringsAsFactors = FALSE)
  data  
}



# Download all data from AWS bucket

# loadallData <- function() {
#   # Get a list of all files
#   file_names <- get_bucket_df(s3BucketName)[["Key"]]
#   # Read all files into a list
#   data <- lapply(file_names, function(x) {
#     object <- get_object(x, s3BucketName)
#     object_data <- readBin(object, "character")
#     read.csv(text = object_data, stringsAsFactors = FALSE)
#   })
#   # Concatenate all data together into one data.frame
#   data <- do.call(rbind, data)
#   data  
# }


deleteData <- function(data){
  # data should be a string of the name of the file to remove
  delete_object(data, bucket = s3BucketName)
  
}


