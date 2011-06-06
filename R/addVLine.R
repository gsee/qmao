addVLine <-
function(dtlist) #add vertical line to a chart
 {
   plot(addTA(xts( rep(TRUE, NROW(dtlist)), dtlist), on=1, col="blue"))
 }

