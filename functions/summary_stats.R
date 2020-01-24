
# necessary packages
library(dplyr)
library(plyr)

# this is the function creation- don't touch :)
summary_continuous <- function(x) {
        mean = mean(x,na.rm=T)
        median = median(x,na.rm=T)
        sd = sd(x,na.rm=T)
        length = length(x[!is.na(x)])
        error = qt(.975,df=(length-1)*sd/sqrt(length))
        upper95 = mean + error
        lower95 = mean - error
        range_min = min(x,na.rm=T)
        range_max = max(x,na.rm=T)
        range_total = range_max - range_min
  return(as.data.frame(list(mean=mean,median=median,sd=sd,upper95=upper95,lower95=lower95,range_min=range_min,
              range_max=range_max,range_total=range_total)))
}

summary_cont <- function(x) {
  x %>%
    lapply(summary_continuous) %>%
    lapply(as.data.frame) %>%
    Map(cbind,var=names(x),.) %>%
    bind_r %>%
    group_by(var)
}

summary_categorical <- function(x) {
  x %>%
    lapply(table) %>%
    lapply(as.data.frame)%>%
    Map(cbind,var=names(x),.) %>%
    bind_rows() %>%
    dplyr::group_by(var) %>%
    dplyr::mutate(percent= Freq/sum(Freq))
}

# Function to loop through data and get stats

summary_all <- function(x) {
  variable <- NULL
  for(i in 1:ncol(x)){
    if(is.numeric(x[,i])){
      variable[[i]] <- summary_continuous(x[,i])
    } else {
      variable[[i]] <- summary_categorical(x[i])
    }
  }
  names(variable) <- colnames(x)
  return(variable)
}

#test <- summary_all(iris)
#erer::write.list(test,"test.csv")
