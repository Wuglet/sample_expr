# Plot Identification Boundary

PlotIdent <- function(subj, block = 1){
  par(mfrow=c(2,2), oma=c(0,0,3,0))
  d <- ident.data[which(ident.data$subj==subj & ident.data$block==block),]
  steps <- sort(unique(d$stimulus[which(d$session=="NoPert")]))
  interpolated_steps <- approx(steps, n=200)

  # plot original and interpolated responses
  plot(with(d[which(d$session=="NoPert"),], tapply(response, stimulus, mean)), ylab="response", xaxt='n', main = "No Pert")
  points(approx(with(d[which(d$session=="NoPert"),], tapply(response, stimulus, mean)), n=200), pch="*", col =2)
  axis(side=1, at=c(1:length(with(d[which(d$session=="NoPert"),], tapply(response, stimulus, mean)))),labels=steps)

  # insert main plot title
  mtext(paste(paste("Participant", subj), paste("Block", block)), outer=TRUE, cex=1.5)
  
  # create data frame of interpolated responses 
  new_d = data.frame(approx(with(d[which(d$session=="NoPert"),], tapply(response, stimulus, mean)), n=200))
  new_d$stimulus <- interpolated_steps$y
  
  # fitting 4-parameter log-logistic model
  # Coefficients: b = slope, c = lower limit, d = upper limit, e = half-way between upper limit and lower limit
  model4 <- drm(y~stimulus, data=new_d, fct=LL.4())
  
  plot(predict(model4),xaxt='n', ylim=c(0,1), main = round(model4$coefficients[4]))
  axis(side=1, at=c(1:200),labels=round(new_d$stimulus))
  abline(0.5, 0, lty='dashed')
  

  ## yes pert 
  steps <- sort(unique(d$stimulus[which(d$session=="YesPert")]))
  interpolated_steps <- approx(steps, n=200)
  
  # plot original and interpolated responses
  plot(with(d[which(d$session=="YesPert"),], tapply(response, stimulus, mean)), ylab="response", xaxt='n', main = "Yes Pert")
  points(approx(with(d[which(d$session=="YesPert"),], tapply(response, stimulus, mean)), n=200), pch="*", col =2)
  axis(side=1, at=c(1:length(with(d[which(d$session=="YesPert"),], tapply(response, stimulus, mean)))),labels=steps)
  
  # create data frame of interpolated responses 
  new_d = data.frame(approx(with(d[which(d$session=="YesPert"),], tapply(response, stimulus, mean)), n=200))
  new_d$stimulus <- interpolated_steps$y
  
  # fitting 4-parameter log-logistic model
  # Coefficients: b = slope, c = lower limit, d = upper limit, e = half-way between upper limit and lower limit
  model4 <- drm(y~stimulus, data=new_d, fct=LL.4())
  
  plot(predict(model4),xaxt='n', ylim=c(0,1), main = round(model4$coefficients[4]))
  axis(side=1, at=c(1:200),labels=round(new_d$stimulus))
  abline(0.5, 0, lty='dashed')
}

  