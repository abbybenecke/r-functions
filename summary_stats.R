# load necessary packages
library(dplyr)
library(plyr)

summary_continuous <- function(x) {
        mean = mean(x)
        median = median(x)
        sd = sd(x)
        error = qt(.975,df=length(x,)-1*sd/sqrt(length(x)))
        upper95 = mean + error
        lower95 = mean - error
        range_min = min(x)
        range_max = max(x)
        range_total = range_max - range_min
  return(as.data.frame(list(mean=mean,median=median,sd=sd,upper95=upper95,
              range_max=range_max,range_min=range_min,range_total=range_total)
}

#summary_continuous <- function(x) {
#    x %>%
#        lapply(
#        summarize(mean=mean(x),median=median(x),sd=sd(x),
#                  error=qt(.975,df=length(x,)-1*sd/sqrt(length(x))),
#                  upper95=mean+error,lower95=mean-error,
#                  range_min=min(x),range_max=max(x),
#                  range_total=range_max-range_min)
#}

summary_categorical <- function(x) {
    x %>%
        lapply(table) %>%
        lapply(as.data.frame) %>%
        Map(cbind,variable=names(x),.) %>%
        rbind_all %>%
        group_by(variable) %>%
        mutate(percent=Freq/sum(Freq))
}
