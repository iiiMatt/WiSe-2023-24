#2a - vi)
#Additional functions suitable for description and visualization
Mosaikplot <- function(var1, var2){
  mosaicplot(table(var1, var2 ), main = "Mosaic Plot", shade = TRUE, xlab = deparse(substitute(var1)) , ylab = deparse(substitute(var2)))
}
Mosaikplot(dataset$Survived, dataset$Age)

Boxplots_with_Trendline <- function(var1, var2) {
  plot(var1, var2, main = "Boxplot with Trendline",
       xlab = deparse(substitute(var1)), ylab = deparse(substitute(var2)))
  abline(lm(var2 ~ var1), col = "red")
}
