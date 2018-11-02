

Threshold <- function(x, na.rm = TRUE) 
{
  ## Find 25% and 75% Quantiles using inbuild function
  quant <- quantile(x, probs=c(.25, .75), na.rm = na.rm) # First and Third quartile
  
  ## Find Interquantile range and multiply it by 1.5 
  ## to derive factor for range calculation
  H <- 1.5 * IQR(x, na.rm = na.rm)
  
  #y <- x
  
  ## fill the outlier elements with NA
  # y[x < (quant[1] - H)] <- NA
  # y[x > (quant[2] + H)] <- NA
  
  ### ONLY WANT THE LOWER BOUND
  
  
  y <- (quant[1] - H)
}


# filename <- "scores_artificial_data_v2_9000_1000_10"
# 
# # Original data is in Horizontal Format:
# data <- read.csv(paste(filename, ".csv", sep=""))
# 
# x <- as.matrix(data)
# 
# x <- matrix(x, ncol = ncol(data), dimnames = NULL)
# 
# x <- t(x)
# 
# require(reshape2)
# require(ggplot2)
# x1=melt(x)
# names(x1)=c("x","y","color")
# 
# x1 <- x1[!x1$x==1,]
# 
# x1$color=factor(x1$color>-5.5)
# levels(x1$color)=c("Outlier","Normal")
# 
# # cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# # scale_fill_manual(values=cbPalette)
# # qplot(x, y, fill=color, data=x1,geom='tile', xlab = "Transition", ylab = "Subject_id") +  scale_fill_manual(values=c("red", "springgreen3"))
# 
# 
# ggplot(data = x1, aes(x=x-1.5, y=y, fill=color)) + labs(title="Transition Outlierness by subject", x = "Transition", y = "Subject_id") + 
#   geom_tile()  + scale_fill_manual(values=c("red", "springgreen3"))
