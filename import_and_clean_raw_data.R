# Copyright: Will Schuerman. Monday, October 27th, 2014.

# File Description: Import production and identification data from compensation for coarticulation/altered auditory feedback experiment.

# Packages 

require(plyr)

# Read in production data 
text.files.production <- dir("Production_data/")

prod.data = data.frame(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,'x', 'x')
names(prod.data) <- c('in.f1.25', 'in.f2.25', 'out.f1.25', 'out.f2.25', 
                      'in.f1.50', 'in.f2.50', 'out.f1.50', 'out.f2.50',
                      'in.f1.75', 'in.f2.75', 'out.f1.75', 'out.f2.75', 
                      'block', 'trial', 'trialb', 'subj', 'session')
for(text.file in text.files.production){
  tmp <- read.csv(file=paste("Production_data/",
                             text.file, sep=""),
                  header = FALSE)
  tmp$trial = c(1:nrow(tmp))
  tmp$trialb = rep(c(1:26), max(tmp$V13)+1)
  tmp$subj <- substr(x = text.file,
                     start =1,
                     stop = regexpr(pattern = "_",
                                    text.file)-1)
  if(length(grep("YesPert", text.file))>0){
    tmp$session <- "YesPert"
  }else{
    tmp$session <- "NoPert"
  }
  names(tmp) <-c('in.f1.25', 'in.f2.25', 'out.f1.25', 'out.f2.25', 
                 'in.f1.50', 'in.f2.50', 'out.f1.50', 'out.f2.50',
                 'in.f1.75', 'in.f2.75', 'out.f1.75', 'out.f2.75', 
                 'block', 'trial', 'trialb', 'subj', 'session')
  prod.data <- rbind(prod.data, tmp)
}
prod.data = prod.data[-1,]

# Increase raw block count to align with experiment procedure
prod.data$block = prod.data$block+1

# get rid of NaN trials
prod.data <- prod.data[complete.cases(prod.data),]

# get rid of 'S' in subject
prod.data$subj <- substr(prod.data$subj, start=2, stop=3)
prod.data$subj <- factor(prod.data$subj)

# Convert to factors 
prod.data$subj <- factor(prod.data$subj)
prod.data$session <- factor(prod.data$session)

# restrict data to measurements taken at vowel midpoint.
prod.data <- prod.data[,c("subj", "session", "block","trialb","trial","in.f1.50","out.f1.50","in.f2.50","out.f2.50")]

# remove outliers, by scaling the the inbuffers and outbuffers, and discarding anything greater than 3 standard deviations from average
prod.data <- ddply(prod.data,
                   c("subj", "session"),
                   transform,
                   z.in.f2.50 = scale(in.f2.50))
prod.data <- ddply(prod.data,
                   c("subj", "session"),
                   transform,
                   z.in.f1.50 = scale(in.f1.50))
total.cases <- nrow(prod.data)
prod.data <- prod.data[which(prod.data$z.in.f1.50 < 3 &
                               prod.data$z.in.f1.50 > -3),]
f1.outliers <- total.cases - nrow(prod.data)
prod.data <- prod.data[which(prod.data$z.in.f2.50 < 3 &
                               prod.data$z.in.f2.50 > -3),]
f2.outliers <- total.cases-f1.outliers-nrow(prod.data)

# calculate dispersion between first and second resonant frequencies
prod.data$in.dispersion <- prod.data$in.f2.50 - prod.data$in.f1.50
prod.data$out.dispersion <- prod.data$out.f2.50 - prod.data$out.f1.50
prod.data <- ddply(prod.data,
                   c("subj", "session"),
                   transform,
                   z.in.dispersion = scale(in.dispersion))

# calculate baselines as average F1 and F2 in the first block.
prod.data$baseline.f1.50 <- 0
prod.data$baseline.f2.50 <- 0
prod.data$baseline.dispersion <- 0
for(session in levels(prod.data$session)){
  for(subj in levels(prod.data$subj)){
    prod.data$baseline.f1.50[which(prod.data$session==session & prod.data$subj==subj)] <- mean(prod.data[which(prod.data$session==session & prod.data$subj==subj),'in.f1.50'][which(prod.data$block==1)], na.rm=T)
    prod.data$baseline.f2.50[which(prod.data$session==session & prod.data$subj==subj)] <- mean(prod.data[which(prod.data$session==session & prod.data$subj==subj),'in.f2.50'][which(prod.data$block==1)], na.rm=T)
    prod.data$baseline.dispersion[which(prod.data$session==session & prod.data$subj==subj)] <- mean(prod.data[which(prod.data$session==session & prod.data$subj==subj),'in.dispersion'][which(prod.data$block==1)], na.rm=T)
  }
}

# Read in identification data
text.files.identification <- dir("Identification_data/")

ident.data = data.frame(1,1,1,1,1,'NoPert')
names(ident.data) <- c("subj", "trial", "stimulus" , "response", "block", "session")
for(text_file in text.files.identification){
  tmp <- read.table(file=paste("Identification_data/",
                               text_file,
                               sep=""),
                    header = T)
  if(length(grep("YESPERT", text_file))>0){
    tmp$session <- "YesPert"
  }else{
    tmp$session <- "NoPert"
  }
  ident.data <- rbind(ident.data, tmp)
}
ident.data = ident.data[-1,]

# move block down by one, so that block 0 is the pre-test
ident.data$block <- ident.data$block-1

# Convert to factors 
ident.data$subj <- factor(ident.data$subj)
ident.data$trial <- factor(ident.data$trial)
ident.data$session <- factor(ident.data$session)
ident.data$block <- factor(ident.data$block)

# fix times when participants definitely hit the wrong button 
ident.data$response[which(ident.data$response==2 & ident.data$stimulus == 0)] <- 1
ident.data$response[which(ident.data$response==1 & ident.data$stimulus == 100)] <- 2

# fix times when participants hit the number 3
ident.data$response[which(ident.data$response==3)] <- 2

# subtract 1 from the responses to make it 0 and 1
ident.data$response <- ident.data$response -1

# keep a record of old stimulus values
ident.data$old.stimulus <- ident.data$stimulus

# change all "100" stimuli to 2 more than participant's maximum
max_stimuli <- with(ident.data[which(ident.data$stimulus!=100),],
                    tapply(stimulus, subj, max))
max_stimuli <- data.frame(max_stimuli)
max_stimuli$subj <- row.names(max_stimuli)

for(subj in unique(ident.data$subj)){
  ident.data$stimulus[which(ident.data$stimulus==100 & ident.data$subj==subj)] <- max_stimuli$max_stimuli[which(max_stimuli$subj==subj)] + 2
}

# change all "0" stimuli to 2 less than participant's minimum
min_stimuli <- with(ident.data[which(ident.data$stimulus!=0),],
                    tapply(stimulus, subj, min))
min_stimuli <- data.frame(min_stimuli)
min_stimuli$subj <- row.names(min_stimuli)

for(subj in unique(ident.data$subj)){
    ident.data$stimulus[which(ident.data$stimulus==0 & ident.data$subj==subj)] <- min_stimuli$min_stimuli[which(min_stimuli$subj==subj)] - 2
}

# summarise ident.data for use with plotting function
ident.summarized.data <- ddply(ident.data, c("subj", "block", "session"), summarise, mean(response))

# rename variable.
names(ident.summarized.data) <- c("subj", "block", "session", "proportion")