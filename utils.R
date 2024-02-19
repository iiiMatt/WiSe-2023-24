describe_metric <- function(x){
  data.frame(Min = min(x, na.rm=T), erstesQuartil =quantile(x, 0.25, na.rm=T) , 
             Median = median(x, na.rm=T), Mean =mean(x, na.rm=T), 
             SD = sd(x, na.rm=T), drittesQuartil =quantile(x, 0.75, na.rm=T),
             Max = max(x, na.rm=T),
             NAs = sum(is.na(x))
             )
}
