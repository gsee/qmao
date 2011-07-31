addVLine <-
function(dtlist, on=1, col='blue') #add vertical line to a chart
 {
   plot(addTA(xts( rep(TRUE, NROW(dtlist)), dtlist), on=on, col=col))
 }

