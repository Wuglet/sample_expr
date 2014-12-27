
# Plot Inbuffers and Outbuffers

PlotBuffers <- function(data=prod.data, subject = "1", sess = "NoPert", plotbuff=TRUE, plotident=TRUE){
  d <- prod.data[which((prod.data$subj== subject) & prod.data$session==sess),]
  d <- droplevels(d)
  meanF1 <- mean(d[,'in.f1.50'], na.rm=T)
  meanF2 <- mean(d[,'in.f2.50'], na.rm=T)
  sdF1 <- sd(d[,'in.f1.50'], na.rm=T)
  sdF2 <- sd(d[,'in.f2.50'], na.rm=T)
  
  d <- d[which(d[,'in.f1.50'] < meanF1 + 2*sdF1 & d[,'in.f1.50'] > meanF1 - 2*sdF1),]
  d <- d[which(d[,'in.f2.50'] < meanF2 + 2*sdF2 & d[,'in.f2.50'] > meanF2 - 2*sdF2),]
  
  f1Baseline = mean(d[,'in.f1.50'][which(d$block==1)], na.rm=T)
  f2Baseline = mean(d[,'in.f2.50'][which(d$block==1)], na.rm=T)
  
  block.size= max(d$trial)/max(d$block)
  #create plots with baselines
  if(plotbuff==TRUE){
    if(sess=="NoPert"){
      plot(c(1,max(d$trial)), c(0, 3500), type='n', xlab="trial", ylab="frequency", main="Unaltered Feedback")
    }else{
      plot(c(1,max(d$trial)), c(0, 3500), type='n', xlab="trial", ylab="frequency", main="Altered Feedback")
    }
    points(d$trial, d[,'in.f1.50'], col='red')
    points(d$trial, d[,'in.f2.50'], col = 'red', pch=3)
    abline(h=f1Baseline)
    abline(lm(d[,'in.f1.50']~trial, data=d), lty="dashed")
    points(d$trial, d[,'out.f1.50'], col='blue')
    points(d$trial, d[,'out.f2.50'], col='blue', pch=3)
    abline(h=f2Baseline)
    abline(lm(d[,'in.f2.50']~trial, data=d), lty="dashed")
    abline(v=block.size, lty="dotted")
    abline(v=block.size*2, lty="dotted")
    abline(v=block.size*3, lty="dotted")
    
    if(max(d$block) > 3 ){
      abline(v=block.size*4, lty="dotted")
      abline(v=block.size*5, lty="dotted")
    }
  }
  
  # add identification data
  if(plotident==TRUE){
    par(new=T)
    d.ident <- ident.summarized.data[which(ident.summarized.data$subj==subject & ident.summarized.data$session==sess),]
    d.ident <- droplevels(d.ident)
    plot(d.ident$proportion, xlab=NA, ylab=NA, axes=F, ylim=c(0,1), cex=3)
    axis(side=4)
    lines(d.ident$proportion, xlab=NA, ylab=NA, ylim=c(0,1), cex=3)
  }

  # measure difference between inbuffer and baseline
  #return(cbind( with(d, tapply(in.f1.50, block, mean)) -f1Baseline, with(d, tapply(in.f2.50, block, mean))-f2Baseline))
}