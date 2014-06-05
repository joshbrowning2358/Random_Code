#Input: fit should be a gam object as returned by mgcv:::gam()
#Output: a list where each element is a ggplot2 object.  Calling out[[3]], for example, will produce the plot for the 3rd coefficient.

library(ggplot2)
library(mgcv)
library(plyr)

plot_gam = function(fit)
{
  if( sessionInfo()$otherPkgs$mgcv$Version!="1.7-22" )
    warning("Different version of mgcv than when code was written.  Plot code may need updating")
  trace(mgcv:::plot.gam, at=list(c(26,3,4,3)), 
        # Make sure the at call corresponds to where mgcv:::plot.gam loops over the variables to plot.
        # as.list( as.list( as.list(as.list(body(mgcv:::plot.gam))[[26]])[[3]] )[[4]] )
        quote({
               # browser()
               plotData[[i]] <<- pd[[i]]
          })
        )
  plotData <<- list()
  mgcv:::plot.gam(fitSpeed, seWithMean=TRUE, pages=1)
  #Restructure plotting data to dataframe with needed columns:
  plotDataDf = lapply( plotData, function(df){
    data.frame(df$x, df$fit, df$se) } )
  #Save plot for each independent variable, total=ncol of model mat-1.
  out = lapply( 1:(ncol(fitSpeed$model)-1), function(i){
    ggplot( plotDataDf[[i]], aes(x=df.x) ) + geom_line(aes(y=df.fit)) +
      geom_ribbon(aes(ymax=df.fit+2*df.se, ymin=df.fit-2*df.se), alpha=.2) +
      labs(x=plotData[[i]]$xlab, y="GAM Fit") } )
  return(out)
}
