my_fancy <- function (model, main = "",extra1=NA, sub, type1=NA, branch.lty1 = NA, ...) 
{  #copied from Rattle fanctyRpartPlot, to allow to pass the extra parameters and type 
  #I do not claim any ownership, all credit goes to them
  
  if (missing(sub)) 
    sub <- paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), 
                 Sys.info()["user"])
  num.classes <- length(attr(model, "ylevels"))
  numpals <- 6
  palsize <- 5
  pals <- c(RColorBrewer::brewer.pal(9, "Greens")[1:5], RColorBrewer::brewer.pal(9, 
                                                                                 "Blues")[1:5], RColorBrewer::brewer.pal(9, "Oranges")[1:5], 
            RColorBrewer::brewer.pal(9, "Purples")[1:5], RColorBrewer::brewer.pal(9, 
                                                                                  "Reds")[1:5], RColorBrewer::brewer.pal(9, "Greys")[1:5])
  if (model$method == "class") {
    yval2per <- -(1:num.classes) - 1
    per <- apply(model$frame$yval2[, yval2per], 1, function(x) x[1 + 
                                                                   x[1]])
  }
  else {
    per <- model$frame$yval/max(model$frame$yval)
  }
  per <- as.numeric(per)
  if (model$method == "class") 
    col.index <- ((palsize * (model$frame$yval - 1) + trunc(pmin(1 + 
                                                                   (per * palsize), palsize)))%%(numpals * palsize))
  else col.index <- round(per * (palsize - 1)) + 1
  col.index <- abs(col.index)
  if (is.na(extra1)) {
  if (model$method == "class" ) 
    extra1 <- 104
  else extra1 <- 101
  }
  type1 = ifelse(is.na(type1),2,type1)
  branch.lty1 <- ifelse(is.na(branch.lty1),3,branch.lty1)
  rpart.plot::prp(model, type = type1, extra = extra1, box.col = pals[col.index], 
                  nn = TRUE, varlen = 0, faclen = 0, shadow.col = 0, 
                  fallen.leaves = TRUE, branch.lty = branch.lty1, ...)
  title(main = main, sub = sub)
}