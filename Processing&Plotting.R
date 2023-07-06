
# Qualtrics ------

readQualtricsData <- function(exp=2, part='task') {
  
  if (exp == 1) {
    if (part == 'intake') { file <- 'fast+forward+lab+intake_April+17,+2023_21.00.csv' }
    if (part == 'task')   { file <- 'fast+forward+lab+task_April+17,+2023_21.01.csv' }
  }
  if (exp == 2) {
    if (part == 'intake') { file <- 'new+dataset+fast+forward+lab+intake_June+19,+2023_00.48.csv' }
    if (part == 'task')   { file <- 'new+dataset+fast+forward+lab+task_July+6,+2023_01.09.csv' }
  }
  
  filepath <- sprintf('data//%s',file)
  
  if (part == 'intake') {
    columnnames <- c('IntakeDate', 'EndDate', 'Status', 'IPAddress',	'Progress_intake', 'Duration (in seconds)', 'Finished_intake', 'RecordedDate', 'ResponseId', 'RecipientLastName', 'RecipientFirstName', 'RecipientEmail', 'ExternalReference', 'LocationLatitude', 'LocationLongitude', 'DistributionChannel', 'UserLanguage', 'consent', 'age', 'sex', 'height_cm', 'handedness', 'id', 'group_intake')
    use_cols <- c('IntakeDate', 'Progress_intake', 'Finished_intake', 'consent', 'age', 'sex', 'height_cm', 'handedness', 'id', 'group_intake')
  }
  if (part == 'task') {
    columnnames <- c('TaskDate', 'EndDate', 'Status', 'IPAddress',	'Progress_task', 'Duration (in seconds)', 'Finished_task', 'RecordedDate', 'ResponseId', 'RecipientLastName', 'RecipientFirstName', 'RecipientEmail', 'ExternalReference', 'LocationLatitude', 'LocationLongitude', 'DistributionChannel', 'UserLanguage', 'needs_vision_correction', 'has_vision_correction', 'strategy_verbalization', 'matoviation one', 'motivation two', 'motivation three', 'motivation four', 'motivation five', 'motivation six', 'motivation seven', 'id', 'group_task', 'condition')
    use_cols <- c('TaskDate','Progress_task', 'Finished_task', 'needs_vision_correction', 'has_vision_correction', 'strategy_verbalization', 'id', 'group_task', 'condition')
  }
  
  df <- read.csv(filepath)
  df <- df[c(3:dim(df)[1]),]
  names(df) <- columnnames
  
  write.csv(df, 'data/temp.csv', row.names = FALSE)
  df <- read.csv('data/temp.csv', stringsAsFactors = FALSE)
  
  
  return(df[,use_cols])
  
  
}

processQualtrics <- function(exp=2) {
  
  # read the data:
  df_intake <- readQualtricsData(exp=exp, part='intake')
  df_task <- readQualtricsData(exp=exp, part='task')
  
  # remove empty IDs:
  df_intake <- df_intake[which(df_intake$id != ""),]
  df_task <- df_task[which(df_task$id != ""),]

  # remove id's with the word 'test' or 'demo' in it
  df_intake <- df_intake[which(!grepl('test', df_intake$id, fixed=TRUE)),]
  df_intake <- df_intake[which(!grepl('demo', df_intake$id, fixed=TRUE)),]
  df_task <- df_task[which(!grepl('test', df_task$id, fixed=TRUE)),]
  df_task <- df_task[which(!grepl('demo', df_task$id, fixed=TRUE)),]

  # remove people who did not provide consent, or finished all parts of the experiment:
  df_intake <- df_intake[which(df_intake$consent == 'I agree to participate in the study'),]
  df_intake <- df_intake[which(df_intake$Finished_intake == 'True'),]
  df_task <- df_task[which(df_task$Finished_task == 'True'),]

  # remove people who did not have normal or corrected-to-normal vision:
  df_task[which(df_task$needs_vision_correction == 'No' | df_task$has_vision_correction == 'Yes'),]

  # remove columns we will not use:
  df_task <- df_task[,which(!colnames(df_task) %in% c('strategy_verbalization'))]
  df_intake <- df_intake[,which(!colnames(df_intake) %in% c('group_intake'))]

  # combine the two data frames:
  dfnice <- merge(df_intake, df_task, by=c('id'))
  
  names(dfnice)[which(names(dfnice) == 'condition')] <- 'condition_q'
  
  return(dfnice)
  
}

# Pavlovia sources -----

getPavloviaParticipants <- function(exp=2) {
  # we need to look in specific folders for every experiment: 
  if (exp == 1) {folders <- c('itc3tab_ffw')}
  if (exp == 2) {folders <- c('itc4tab_expl', 'itc5tab_rot','itc6tab_dt')}
  
  all_paths <- c()
  
  for (fold in folders) {
    # from each folder we need csv files with the string "_implicit_time_" in their name:
    all_paths <- c(all_paths, Sys.glob(sprintf('data/%s/*_implicit_time_*.csv',fold)))
  }
  
  remove_ids <- c()
  
  id <- c()
  timestamp <- c()
  path <- c()
  folder <- c()
  condition <- c()
  condition_label <- c()
  rotation <- c()
  
  for (one_path in all_paths) {
    
    added <- FALSE
    
    # extract participant id (and pavlovia timestamp):
    id_time   <- getIDtimestamp(basename(one_path), task='implicit_time')
    
    # check if the file makes sense (just counting lines for now):
    pav_check <- checkPavloviaFile(path=one_path)

    if ( pav_check['check'] ) {
      adapt_check <- checkAdapted(path=one_path)
      if (adapt_check['check']) {
        path <- c(path, one_path)
        added <- TRUE
      } else{
        # or else?
      }
    } else {
      if (pav_check['rotated']) {
        remove_ids <- c(remove_ids, id_time['participant'])
      }
      # if there are too few lines in the file, we skip it:
      next()
    }
    
    # print(added)
    
    # add participant ID and file timestamp to lists:
    if (added) {
      id              <- c( id,              id_time['participant'] )
      timestamp       <- c( timestamp,       id_time['timestamp']   )
      folder          <- c( folder,          substr(dirname(one_path),6,nchar(dirname(one_path))))
      condition       <- c( condition,       adapt_check['condition'])
      condition_label <- c( condition_label, adapt_check['condition_label'])
      rotation        <- c( rotation,        adapt_check['rotation'])
    }
    
  }
  
  # make a new data frame with the collected info
  df <- data.frame(id, timestamp, path, folder, condition, condition_label, rotation)
  
  # remove participants who did rotated trials in incomplete files:
  df <- df[which(!df$id %in% remove_ids),]
  
  # see if there are duplicate participants:
  df <- df[order(df$timestamp),] # sort alphabetically by timestamp
  if (length(which(duplicated(df$id)))) {
    first_id <- which(!duplicated(df$id))
    df <- df[first_id,]
  }
  
  return(df)
  
}

getIDtimestamp <- function(filename, task) {
  
  pattern <- sprintf('_%s_', task)
  
  pos <- gregexpr(pattern=pattern, filename)[[1]][1]
  pp <- substr(filename, 1, pos-1)
  ts <- substr(filename, pos+nchar(pattern), nchar(filename)-4)
  
  return(c('participant'=pp, 'timestamp'=ts))
  
}

checkPavloviaFile <- function(path) {
  
  # _ffw  -> 272
  # _expl -> 264
  # _rot  -> 320
  
  directory <- dirname(path)
  min_lines <- as.numeric(c('data/itc5tab_rot'=320,
                            'data/itc4tab_expl'=264,
                            'data/itc3tab_ffw'=272,
                            'data/itc6tab_dt'=272)[directory])
  
  con <- file(path, 'r')
  rotated <- FALSE
  nlines <- -1 # header line adds another one to the count...
  while(TRUE) {
    line_n <- readLines(con,n=1)
    if (nlines == -1) {
      rot_col <- which( strsplit(line_n,',')[[1]] == "rotation_deg" )
    }
    if (length(line_n)==0) {
      break()
    }
    if (nlines > -1 & length(rot_col)) {
      line_data <- strsplit(line_n,',')[[1]]
      if (length(line_data) >= rot_col) {
        if (line_data[rot_col] != "0") {
          rotated <- TRUE
        }
      } else {
        # this should NOT ever happen:
        print(line_n)
      }
    }
    
    nlines <- nlines+1
  }
  close(con)
  
  check <- nlines >= min_lines
  
  return(c('check'=check, 'nlines'=nlines, 'rot_col'=rot_col, 'rotated'=rotated))
  
}

checkAdapted <- function(path) {
  
  df <- read.csv(path)
  
  # print(str(df))
  condition       <- df$condition[1]
  condition_label <- df$condition_label[1]
  
  df <- df[which(df$trialtype == 1),]
  rot_start <- which(df$rotation_deg != 0)[1] # always 37!
  # lets take the last 16 trials:
  df <- df[c(rot_start-1+c(85:100)),]
  
  X <- c()
  Y <- c()
  for (trial in c(dim(df)[1])) {
    target <- c(df$target[trial])
    theta <- -1*(target/180)*pi
    R <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),nrow=2)
    #step <- convertCellToNumVector(df$step[trial])
    relx <- convertCellToNumVector(df$mousex_rel[trial])
    rely <- convertCellToNumVector(df$mousey_rel[trial])
    d <- sqrt(relx^2 + rely^2)
    idx <- which(d > 0.2)[1]
    xy <- c(relx[idx],rely[idx])
    npos <- R %*% xy
    X <- c(X, npos[1])
    Y <- c(Y, npos[2])
  }
  
  reach_deviations <- (atan2(Y,X)/pi)*180
  
  rotation <- df$rotation_deg[1]
  # CHANGE THIS TO 1.5 TO FIND 10 DEGREES OF LEARNING IN 15 GROUP, KEEP IT AT 2 TO GET 50% LEARNING RATE
  criterion <- abs(rotation/2)
  
  if (rotation > 0) {reach_deviations <- reach_deviations * -1}
  
  return(c('check'=median(reach_deviations, na.rm=TRUE) > criterion, 'rotation'=rotation, 'condition'=condition, 'condition_label'=condition_label) ) 
  
}

Cleandata <- function(filename) {
  
  if (filename == "condition 0 trialtype 0_cuttoff0.3.csv" | filename == "condition 0 trialtype 1_cuttoff0.3.csv") {
    data <- read.csv(filename, header = TRUE)
    aligned<-data[data$rot =="baseline",2:ncol(data)]
    rot15<-data[data$rot =="rotation",2:ncol(data)]
    washout<-data[data$rot =="washout",2:ncol(data)]
    
    aligned[aligned > 40] <- NA
    aligned[aligned < -40] <- NA
    
    rot15[rot15 < -45]<- NA
    rot15[rot15 > 15]<- NA
    
    washout[washout < -30]<- NA
    washout[washout >  75]<- NA
    
    alldata15<- rbind(aligned, rot15, washout)
    return(alldata15)
    
    } else if (filename == "condition 1 trialtype 0_cuttoff0.3.csv" | filename == "condition 1 trialtype 1_cuttoff0.3.csv") {
      data <- read.csv(filename, header = TRUE)
      aligned<-data[data$rot =="baseline",2:ncol(data)]
      rot30<-data[data$rot =="rotation",2:ncol(data)]
      washout<-data[data$rot =="washout",2:ncol(data)]

      aligned[aligned > 40] <- NA
      aligned[aligned < -40] <- NA

      rot30[rot30 < -60]<- NA
      rot30[rot30 > 15]<- NA

      washout[washout < -30]<- NA
      washout[washout >  75]<- NA

      alldata30<- rbind(aligned, rot30, washout)
      return(alldata30)

  } else if (filename == "condition 2 trialtype 0_cuttoff0.3.csv" | filename == "condition 2 trialtype 1_cuttoff0.3.csv") {
    data <- read.csv(filename, header = TRUE)
    aligned<-data[data$rot =="baseline",2:ncol(data)]
    rot45<-data[data$rot =="rotation",2:ncol(data)]
    washout<-data[data$rot =="washout",2:ncol(data)]
    
    aligned[aligned > 40] <- NA
    aligned[aligned < -40] <- NA
    
    rot45[rot45 < -75]<- NA
    rot45[rot45 > 15]<- NA
    
    washout[washout < -30]<- NA
    washout[washout >  75]<- NA
    
    alldata45<- rbind(aligned, rot45, washout)
    return(alldata45)
    
    } else if (filename == "condition 3 trialtype 0_cuttoff0.3.csv" | filename == "condition 3 trialtype 1_cuttoff0.3.csv") {
      data <- read.csv(filename, header = TRUE)
      aligned<-data[data$rot =="baseline",2:ncol(data)]
      rot60<-data[data$rot =="rotation",2:ncol(data)]
      washout<-data[data$rot =="washout",2:ncol(data)]

      aligned[aligned > 40] <- NA
      aligned[aligned < -40] <- NA

      rot60[rot60 < -90]<- NA
      rot60[rot60 > 15]<- NA

      washout[washout < -30]<- NA
      washout[washout >  75]<- NA

      alldata60<- rbind(aligned, rot60, washout)
      return(alldata60)
    
  } else if (filename == "condition 7 trialtype 0_cuttoff0.3.csv" | filename == "condition 7 trialtype 1_cuttoff0.3.csv") {
    data <- read.csv(filename, header = TRUE)
    aligned<-data[data$rot =="baseline",2:ncol(data)]
    rot45<-data[data$rot =="rotation",2:ncol(data)]
    washout<-data[data$rot =="washout",2:ncol(data)]
    
    aligned[aligned > 40] <- NA
    aligned[aligned < -40] <- NA
    
    rot45[rot45 < -75]<- NA
    rot45[rot45 > 15]<- NA
    
    washout[washout < -30]<- NA
    washout[washout >  75]<- NA
    
    alldatadelayafterFB<- rbind(aligned, rot45, washout)
    return(alldatadelayafterFB)
    
  } else if (filename == "condition 8 trialtype 0_cuttoff0.3.csv" | filename == "condition 8 trialtype 1_cuttoff0.3.csv") {
    data <- read.csv(filename, header = TRUE)
    aligned<-data[data$rot =="baseline",2:ncol(data)]
    rot45<-data[data$rot =="rotation",2:ncol(data)]
    washout<-data[data$rot =="washout",2:ncol(data)]
    
    aligned[aligned > 40] <- NA
    aligned[aligned < -40] <- NA
    
    rot45[rot45 < -75]<- NA
    rot45[rot45 > 15]<- NA
    
    washout[washout < -30]<- NA
    washout[washout >  75]<- NA
    
    alldatadelaybeforeFB<- rbind(aligned, rot45, washout)
    return(alldatadelaybeforeFB)
    
  } else if (filename == "condition 1 trialtype 0_cuttoff0.3.csv" | filename == "condition 1 trialtype 1_cuttoff0.3.csv") {
    data <- read.csv(filename, header = TRUE)
    aligned<-data[data$rot =="baseline",2:ncol(data)]
    rot45<-data[data$rot =="rotation",2:ncol(data)]
    
    aligned[aligned > 40] <- NA
    aligned[aligned < -40] <- NA
    
    rot45[rot45 < -75]<- NA
    rot45[rot45 > 15]<- NA
    
    alldatacontinuous<- rbind(aligned, rot45)
    return(alldatacontinuous)
    
  } else if (filename == "condition 3 trialtype 0_cuttoff0.3.csv" | filename == "condition 3 trialtype 1_cuttoff0.3.csv") {
    data <- read.csv(filename, header = TRUE)
    aligned<-data[data$rot =="baseline",2:ncol(data)]
    rot45<-data[data$rot =="rotation",2:ncol(data)]
    
    aligned[aligned > 40] <- NA
    aligned[aligned < -40] <- NA
    
    rot45[rot45 < -75]<- NA
    rot45[rot45 > 15]<- NA
    
    alldataterminal<- rbind(aligned, rot45)
    return(alldataterminal)
    
  } else if (filename == "condition 4 trialtype 0_cuttoff0.3.csv" | filename == "condition 4 trialtype 1_cuttoff0.3.csv") {
    data <- read.csv(filename, header = TRUE)
    aligned<-data[data$rot =="baseline",2:ncol(data)]
    rot45<-data[data$rot =="rotation",2:ncol(data)]
    
    aligned[aligned > 40] <- NA
    aligned[aligned < -40] <- NA
    
    rot45[rot45 < -75]<- NA
    rot45[rot45 > 15]<- NA
    
    alldatacursorjump<- rbind(aligned, rot45)
    return(alldatacursorjump)
    
  }
}

loaddata<- function() {
  
  schedule<- c(rep(0, times = 20), rep(15, times = 100), rep(0, times = 24))
  fifteengroup_reaches<<- cbind(schedule,Cleandata("condition 0 trialtype 1_cuttoff0.3.csv"))
  fifteengroup_nocursors<<- cbind(schedule,Cleandata("condition 0 trialtype 0_cuttoff0.3.csv"))

  schedule<- c(rep(0, times = 20), rep(30, times = 100), rep(0, times = 24))
  thirtygroup_reaches<<- cbind(schedule,Cleandata("condition 1 trialtype 1_cuttoff0.3.csv"))
  thirtygroup_nocursors<<- cbind(schedule,Cleandata("condition 1 trialtype 0_cuttoff0.3.csv"))

  schedule<- c(rep(0, times = 20), rep(45, times = 100), rep(0, times = 24))
  fourtyfivegroup_reaches<<- cbind(schedule,Cleandata("condition 2 trialtype 1_cuttoff0.3.csv"))
  fourtyfivegroup_nocursors<<- cbind(schedule,Cleandata("condition 2 trialtype 0_cuttoff0.3.csv"))

  schedule<- c(rep(0, times = 20), rep(60, times = 100), rep(0, times = 24))
  sixtygroup_reaches<<- cbind(schedule,Cleandata("condition 3 trialtype 1_cuttoff0.3.csv"))
  sixtygroup_nocursors<<- cbind(schedule,Cleandata("condition 3 trialtype 0_cuttoff0.3.csv"))

  schedule<- c(rep(0, times = 20), rep(45, times = 100))
  delayafterFB_reaches<<- cbind(schedule,Cleandata("condition 7 trialtype 1_cuttoff0.3.csv"))
  delayafterFB_nocursors<<- cbind(schedule,Cleandata("condition 7 trialtype 0_cuttoff0.3.csv"))
 
  schedule<- c(rep(0, times = 20), rep(45, times = 100))
  delaybeforeFB_reaches<<- cbind(schedule,Cleandata("condition 8 trialtype 1_cuttoff0.3.csv"))
  delaybeforeFB_nocursors<<- cbind(schedule,Cleandata("condition 8 trialtype 0_cuttoff0.3.csv"))
}

Baselinedata<- function(data) {
  
  for (i in 1:ncol(data)){
    average<- mean(data[1:20,i], na.rm = TRUE)
    data[,i]<- data[,i]-average
  }
  return(data) 
  
}


getdataforexponentialfit <- function(num) {
  
  setwd('/Users/Damar/Desktop/grad/ImplcitTimeCourseFallWinter/')
  files <- list.files(path = "combineddata/")
  return(dataname <- files[num])
}

exponentialfunction <- function(dataname) {
  
  # get the example data:
  setwd("~/Desktop/grad/ImplcitTimeCourseFallWinter/combineddata")
  mydata <- read.csv(dataname, header = TRUE)
  mydata <- mydata[1:144, ]
  trial <- 1:144
  block <- c(rep(1,times = 20),rep(2, times = 100))
  exponentialdf <- data.frame(block,trial,mydata)
  
  # we just want 1 averaged timecourse:
  rd_full <- rowMeans(exponentialdf[,c(4:12)], na.rm=TRUE)*-1
  
  # and for the exponential, we just take the first rotation (-30)
  idx <- which(exponentialdf$schedule == 15)
  rd <- rd_full[idx]
  
  # do the fit:
  fit <- exponentialFit(signal = rd[c(1:56)])
  
  # get the trial-by-trial output:
  out <- exponentialModel(par=fit, timepoints=c(0:55))
  
  # plot data:
  plot(rd_full, col='gray')
  # plot the trial-by-trial output:
  lines(x=out$trial+21, y=out$output, col='blue')
  
  # get interpolated data:
  out2 <- exponentialModel(par=fit, timepoints=seq(0,55,.1))
  # plot the interpolated data:
  lines(x=out2$trial+20, y=out2$output, col='green')
  
}

exponentialModel <- function(par, timepoints, mode = 'learning', setN0 = NULL) {
  if (length(timepoints) == 1) {
    timepoints <- c(0:(timepoints - 1))
  }
  
  if (is.numeric(setN0)) {
    par['N0'] <- setN0
  }
  
  if (mode == 'learning') {
    output <- par['N0'] - (par['N0'] * (1 - par['lambda'])^timepoints)
  }
  if (mode == 'washout') {
    output <- par['N0'] * (par['lambda'])^timepoints
  }
  
  return(data.frame(trial = timepoints, output = output))
}

exponentialMSE <- function(par, signal, timepoints=c(0:(length(signal)-1)), mode='learning', setN0=NULL) {
  
  MSE <- mean((exponentialModel(par, timepoints, mode=mode, setN0=setN0)$output - signal)^2, na.rm=TRUE)
  
  return( MSE )
  
}

exponentialFit <- function(signal, timepoints=length(signal), mode='learning', gridpoints=11, gridfits=10, setN0=NULL) {
  
  # set the search grid:
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  asymptoteRange <- c(-1,2)*max(abs(signal), na.rm=TRUE)
  
  # define the search grid:
  if (is.numeric(setN0)) {
    searchgrid <- expand.grid('lambda' = parvals)
    lo <- c(0)
    hi <- c(1)
  }
  if (is.null(setN0)) {
    searchgrid <- expand.grid('lambda' = parvals,
                              'N0'     = parvals * diff(asymptoteRange) + asymptoteRange[1] )
    lo <- c(0,asymptoteRange[1])
    hi <- c(1,asymptoteRange[2])
  }
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=exponentialMSE, MARGIN=c(1), signal=signal, timepoints=timepoints, mode=mode, setN0=setN0)
  
  # run optimx on the best starting positions:
  allfits <- do.call("rbind",
                     apply( data.frame(searchgrid[order(MSE)[1:gridfits],]),
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=exponentialMSE,
                            method     = 'L-BFGS-B',
                            lower      = lo,
                            upper      = hi,
                            timepoints = timepoints,
                            signal     = signal,
                            mode       = mode,
                            setN0      = setN0 ) )
  
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]
  
  if (is.null(setN0)) {
    winpar <- unlist(win[1:2])
  } else {
    winpar <- c( 'lambda' = unlist(win[1]), 
                 'N0'     = setN0)
    names(winpar) <- c('lambda', 'N0')
  }
  
  # return the best parameters:
  return(winpar)
  
}

exponentialSettings <- function() {
  
  # this list determines which signals get done for each group
  groupsignals <- list(
    'fifteengroup'       = c('nocursors',  'reaches'),
    'thirtygroup'        = c('nocursors', 'reaches'),
    'fourtyfivegroup'        = c('nocursors', 'reaches'),
    'sixtygroup'        = c('nocursors', 'reaches'),
    'continuousaiming'        = c('nocursors'),
    'delayafterFBgroup'        = c('nocursors', 'reaches'),
    'delaybeforeFBgroup'        = c('nocursors', 'reaches'),
    'continuous'       = c('nocursors',  'reaches'),
    'terminal'        = c('nocursors', 'reaches'),
    'cursorjump'        = c('nocursors', 'reaches')
  )
  # this list determines which signals get done for each group
  
  
  
  
  # we used to run it on the reversal phase too, but it takes so much time...
  trialsets <- list('main'=c(1:56))
  
  baselines <- list(
    'fifteengroup'       = list( 'nocursors'=20,  'reaches'=20 ),
    'thirtygroup'        = c('nocursors'=20,          'reaches'=20),
    'fourtyfivegroup'        = c('nocursors'=20,       'reaches'=20),
    'sixtygroup'        = c('nocursors'=20,          'reaches'=20),
    'continuousaiming'        = c('nocursors'=20, 'reaches'=20, 'localization'=20),
    'delayafterFBgroup'        = c('nocursors'=20,          'reaches'=20),
    'delaybeforeFBgroup'        = c('nocursors'=20,          'reaches'=20),
    'continuous'       = c( 'nocursors'=20,  'reaches'=20 ),
    'terminal'        = c('nocursors'=20,          'reaches'=20),
    'cursorjump'        = c('nocursors'=20,       'reaches'=20)
  )
  
  schedules <- list( 
    'fifteengroup'       = list( 'nocursors'=  -1, 'reaches'= -1 ),
    'thirtygroup'        = c('nocursors'=-1,        'reaches'=-1),
    'fourtyfivegroup'        = c('nocursors'=-1,       'reaches'=-1),
    'sixtygroup'        = c('nocursors'=-1,        'reaches'=-1),
    'continuousaiming'        = c('nocursors'=-1, 'reaches'=-1, 'localication'=1)
    'delayafterFBgroup'        = c('nocursors'=-1,          'reaches'=-1),
    'delaybeforeFBgroup'        = c('nocursors'=-1,          'reaches'=-1),
    'continuous'        = c('nocursors'=-1,        'reaches'=-1),
    'terminal'        = c('nocursors'=-1,       'reaches'=-1),
    'cursorjump'        = c('nocursors'=-1,        'reaches'=-1)
  )
  
  optimxInstalled <- require("optimx")
  if (optimxInstalled) {
    useOptimx <- TRUE
  } else {
    useOptimx <- FALSE
  }
  
  settings <- list()
  settings[['groupsignals']] <- groupsignals
  settings[['trialsets']]    <- trialsets
  settings[['baselines']]    <- baselines
  settings[['schedules']]    <- schedules
  settings[['FUN']]          <- mean
  settings[['useOptimx']]    <- useOptimx
  
  return(settings)
  
}

bootstrapSemiExponentialModelsBEST <- function(bootstraps=1000) {
  
  settings <- exponentialSettings()
  
  groupsignals <- settings[['groupsignals']]
  trialsets    <- settings[['trialsets']]
  baselines    <- settings[['baselines']]
  schedules    <- settings[['schedules']]
  FUN          <- settings[['FUN']]
  useOptimx    <- settings[['useOptimx']]
  
  # loop through groups:
  for (groupname in names(groupsignals)) {
    
    if (groupname == 'fifteengroup') {
      participants <- sprintf('p%d',c(1:12))
    }
    if (groupname == 'thirtygroup') {
      participants <- sprintf('p%d',c(1:23))
    }
    if (groupname == 'fourtyfivegroup') {
      participants <- sprintf('p%d',c(1:25))
    }
    if (groupname == 'sixtygroup') {
      participants <- sprintf('p%d',c(1:53))
    }
    if (groupname == 'continuousaiming') {
      participants <- sprintf('p%d',c(1:37))
    }
    
    # do each signal for each group
    for (signalname in groupsignals[[groupname]]) {
      
      leadingzero <- FALSE
      if (signalname %in% c('localization', 'nocursors')) {
        leadingzero <- TRUE
      }

      # read in the full data set:
      df <- read.csv(sprintf('combineddata/%s_%s.csv',groupname,signalname))
      df <- df[,participants]
      
      # determine length of baseline period and schedule-direction:
      BL <- baselines[[groupname]][[signalname]]
      schedulesign <- schedules[[groupname]][[signalname]]
      
      # loop through parts of the signal we want to fit:
      for (trialset in names(trialsets)) {
        
        # get the part of the data we want to fit:
        indices <- trialsets[[trialset]] + BL
        setdf <- df[indices,]
        
        # here we store all the bootstrapped parameters:
        lambda <- c()
        N0 <- c()
        
        # we need to baseline to end of main training for reversal modeling:
        for (pp in participants) {
          setdf[,pp] <- setdf[,pp] * schedulesign
        }

        schedulelength <- dim(setdf)[1]
        if (leadingzero) {schedulelength <- schedulelength + 1}
        schedule <- 1:schedulelength
        
        # bootstrap parameters, by resampling participants:
        for (bs in c(1:bootstraps)) {
          
          cat(sprintf('group: %s, signal: %s, set: %s, bootstrap: %d/%d\n', groupname, signalname, trialset, bs, bootstraps))
          
          signal <- apply(setdf[sample(participants, replace=TRUE)], MARGIN=1, FUN=FUN, na.rm=TRUE)
          
          if (leadingzero) {signal <- c(0, signal)}
          
          par <- exponentialFit(timepoints=schedule, signal=signal)

          lambda <- c(lambda, par['lambda'])
          if (trialset == 'main') {
            N0 <- c(N0, par['N0'])
          } else {
            N0 <- c(N0, setAsymptote)
          }
          
        }
        
        write.csv(data.frame(lambda, N0), file=sprintf('model_data/%s_%s_%s_semiNEWEST.csv',groupname,signalname,trialset), quote=F, row.names=F)
        
      }
      
    }
    
  }
  
}

getexponentialParameterCIsBEST <- function(semi = TRUE) {
  settings <- exponentialSettings()
  
  groupsignals <- settings[['groupsignals']]
  trialsets <- settings[['trialsets']]
  baselines <- settings[['baselines']]
  schedules <- settings[['schedules']]
  FUN <- settings[['FUN']]
  useOptimx <- settings[['useOptimx']]
  
  group <- c()
  signal <- c()
  phase <- c()
  
  lambda <- c()
  lambda_025 <- c()
  lambda_500 <- c()
  lambda_975 <- c()
  
  N0 <- c()
  N0_025 <- c()
  N0_500 <- c()
  N0_975 <- c()
  
  # loop through groups
  for (groupname in names(groupsignals)) {
    if (groupname == 'fifteengroup') {
      participants <- sprintf('p%d', c(1:12))
    }
    if (groupname == 'thirtygroup') {
      participants <- sprintf('p%d', c(1:23))
    }
    if (groupname == 'fourtyfivegroup') {
      participants <- sprintf('p%d', c(1:25))
    }
    if (groupname == 'sixtygroup') {
      participants <- sprintf('p%d', c(1:53))
    }
    if (groupname == 'continuousaiming') {
      participants <- sprintf('p%d',c(1:37))
    }
    
    # do each signal for each group
    for (signalname in groupsignals[[groupname]]) {
      cat(sprintf('%s %s\n', groupname, signalname))
      
      leadingzero <- FALSE
      if (signalname %in% c('localization', 'nocursors')) {
        leadingzero <- TRUE
      }
      
      # # read in the full data set
      rawdf <- read.csv(sprintf('combineddata/%s_%s.csv', groupname, signalname))
      rawdf <- rawdf[,-1]
      rawdf <- rawdf[, participants]
      
      # determine length of baseline period and schedule-direction
      BL <- baselines[[groupname]][[signalname]]
      schedulesign <- schedules[[groupname]][[signalname]]
      
      # loop through parts of the signal we want to fit
      for (trialset in names(trialsets)) {
        # get the part of the data we want to fit
        indices <- trialsets[[trialset]] + BL
        setdf <- rawdf[indices, ]
        
        # baselining (commented out for now)
        for (pp in participants) {
          setdf[, pp] <- setdf[, pp] * schedulesign
        }
        schedule <- 1:schedulelength
        
        # this gets the overall parameters on the group median data
        datasignal <- apply(setdf, MARGIN = 1, FUN = FUN, na.rm = TRUE)
        if (leadingzero) {
          datasignal <- c(0, datasignal)
        }
        
        par <- exponentialFit(timepoints = schedule, signal = datasignal)
        
        # read in the bootstrapped parameter values
        if (semi) {
          sstr <- '_semiNEWEST'
        } else {
          sstr <- ''
        }
        df <- read.csv(
          sprintf('model_data/%s_%s_%s%s.csv', groupname, signalname, trialset, sstr),
          stringsAsFactors = FALSE
        )
        
        group <- c(group, groupname)
        signal <- c(signal, signalname)
        phase <- c(phase, trialset)
        
        qs <- quantile(df$lambda, probs = c(0.025, 0.500, 0.975))
        lambda <- c(lambda, as.numeric(par['lambda']))
        lambda_025 <- c(lambda_025, qs[1])
        lambda_500 <- c(lambda_500, qs[2])
        lambda_975 <- c(lambda_975, qs[3])
        
        # Exclude negative N0 values
        valid_N0 <- df$N0[df$N0 >= 0]
        if (length(valid_N0) > 0) {
          qs <- quantile(valid_N0, probs = c(0.025, 0.500, 0.975))
          N0 <- c(N0, as.numeric(par['N0']))
          N0_025 <- c(N0_025, qs[1])
          N0_500 <- c(N0_500, qs[2])
          N0_975 <- c(N0_975, qs[3])
        }
      }
    }
  }
  
  # write output
  write.csv(
    data.frame(
      group,
      signal,
      phase,
      lambda,
      lambda_025,
      lambda_500,
      lambda_975,
      N0,
      N0_025,
      N0_500,
      N0_975
    ),
    file = 'model_data/TESTINGexponentialParameterCIs.csv',
    quote = FALSE,
    row.names = FALSE
  )
}

getexponentialSaturationTrials <- function(criterion="CI") {
  
  df <- read.csv('model_data/TESTINGexponentialParameterCIs.csv', stringsAsFactors = F)
  df <- df[which(df$phase == 'main'),]
  
  settings <- exponentialSettings()
  
  groupsignals <- settings[['groupsignals']]
  trialsets    <- settings[['trialsets']]
  baselines    <- settings[['baselines']]
  schedules    <- settings[['schedules']]
  FUN          <- settings[['FUN']]
  useOptimx    <- settings[['useOptimx']]
  
  group <- c()
  signal <- c()
  avg <- c()
  lwr <- c()
  upr <- c()
  
  # loop through groups:
  for (groupname in names(groupsignals)) {

    # do each signal for each group
    for (signalname in groupsignals[[groupname]]) {
      
      # read in the full data set:
      rawdf <- read.csv(sprintf('combineddata/%s_%s.csv',groupname,signalname))
      
      leadingzero <- FALSE
      if (signalname %in% c('localization', 'nocursors')) {
        leadingzero <- TRUE
      }
      
      # determine length of baseline period and schedule-direction:
      BL <- baselines[[groupname]][[signalname]]
      schedulesign <- schedules[[groupname]][[signalname]]
      
      # loop through parts of the signal we want to fit:
      for (trialset in c('main')) {
        
        # get the part of the data we want to fit:
        indices <- trialsets[[trialset]] + BL
        setdf <- rawdf[indices,]

        schedulelength <- length(indices)
        if (leadingzero) {schedulelength <- schedulelength + 1}
        schedule <- 1:schedulelength
        
        # this gets the overal parameters on the group median data:
        trialnos <- list()
        
        for (roc in c('lambda','lambda_975','lambda_025')) {
          
          par <- c('lambda' = df[which(df$group == groupname & df$signal == signalname), roc],
                   'N0' = df[which(df$group == groupname & df$signal == signalname), 'N0'])
          
          fitdf <- exponentialModel(par = par, timepoints = schedule)
          
          if (is.numeric(criterion)) {
            crit <- (par['N0'] * criterion)
          }
          if ( criterion == 'CI' ) {
            crit <- df$N0_025[which(df$group == groupname & df$signal == signalname)]
          }
          
          trialno <- which(fitdf$output > crit)[1]
          
          # subtract 1 from trial no, as first trial depends on feedback from previous phase///
          trialno <- trialno-1
          
          trialnos[[roc]] <- trialno
          
        }

        group <- c(group, groupname)
        signal <- c(signal, signalname)
        avg <- c(avg, trialnos[['lambda']])
        lwr <- c(lwr, trialnos[['lambda_975']])
        upr <- c(upr, trialnos[['lambda_025']])
        
        cat(sprintf('%s, %s: trial %d (%d - %d)\n', groupname, signalname, trialnos[['lambda']], trialnos[['lambda_975']], trialnos[['lambda_025']]))
        
      }
      
    }
    
  }
  
  df <- data.frame(group, signal, avg, lwr, upr)
  
  write.csv(df, 'model_data/NEWESTexponentialsaturation_trialsbaselined.csv', row.names = FALSE, quote = FALSE)
  
}

plotexponentialSaturation <- function(xscale='normal', target='svg', signal='reaches') {
  
  fonts <- list(sans = "Arial", mono = "Arial")
  if (target == 'svg') {
    library('svglite')
    svglite::svglite(file=sprintf('figures/exponentialSaturation_%s.svg',signal), width=8, height=6, bg='white', system_fonts=fonts)
    
  }
  if (target == 'pdf') {
    pdf(file=sprintf('figs/exponentialSaturation_%s.pdf', signal), width=8, height=6, bg='white')
    
  }

  df <- read.csv('model_data/TESTINGexponentialParameterCIs.csv', stringsAsFactors = F)
  df <- df[which(df$phase == 'main'),]

  settings <- exponentialSettings()
  
  groups <- c('Fifteen', 'Thirty', 'Fourtyfive', 'Sixty')
  
  groupsignals <- settings[['groupsignals']]
  trialsets    <- settings[['trialsets']]
  baselines    <- settings[['baselines']]
  schedules    <- settings[['schedules']]
  FUN          <- settings[['FUN']]
  useOptimx    <- settings[['useOptimx']]
  
  
  styles <- getStyles()
  
  if (xscale == 'normal') {
    
    plot(-1000,-1000,
         xlab='trials completed in rotated phase',ylab='percentage of saturation',
         main='modeled process speeds',
         xlim=c(0,35),ylim=c(0,1.1),
         bty='n',ax=F)
    TIME <- seq(0,56,.1)  
    xcoords <- TIME
    
  }
  
  groupcolors <- c(styles$'continuous reaches'$solid,
                   styles$'continuous nocursors'$solid,
                   styles$'terminal reaches'$solid,
                   styles$'terminal nocursors'$solid,
                   styles$'cursorjump reaches'$solid,
                   styles$'cursorjump nocursors'$solid)
  
  # loop through groups:
  for (groupname in groups) {
    

    # do each signal for each group
    for (signalname in signal) {
      leadingzero <- FALSE
      if (signalname %in% c('localization', 'nocursors')) {
        leadingzero <- TRUE
      }
      
      # determine length of baseline period and schedule-direction:
      BL <- baselines[[groupname]][[signalname]]
      schedulesign <- schedules[[groupname]][[signalname]]
      
      # loop through parts of the signal we want to fit:
      for (trialset in c('main')) {
        
        # get the part of the data we want to fit:
        indices <- trialsets[[trialset]] + BL
        
        # schedule is a vector of values -1 and length the same as the signal:
        schedulelength <- length(indices)
        if (leadingzero) {schedulelength <- schedulelength + 1}
        schedule <- rep(-1, schedulelength)
        # this gets the overal parameters on the group median data:
        
        dfits <- list()
        
        #print(c(groupname,signalname))
        t_idx <- (c(0:19)*10)+1
        for (roc in c('lambda','lambda_975','lambda_025')) {
          
          par <- c('lambda'=df[which(df$group == groupname & df$signal == signalname),roc], 'N0'=df[which(df$group == groupname & df$signal == signalname),'N0'])
          scale <- df$N0_025[which(df$group == groupname & df$signal == signalname)]
          dfit <- exponentialModel(par,schedule)$output
          process <- predict(smspl,TIME)$y
          processes[[roc]] <- process
          dfits[[roc]] <- dfit
          
        }

        upr <- processes[['lambda_975']]
        lwr <- processes[['lambda_025']]
        up_idx <- which(upr >= 1)[1]
        lo_idx <- which(lwr >= 1)[1]
        print(c(up_idx,lo_idx))
        
        X <- c(xcoords[1:up_idx],rev(xcoords[1:lo_idx]))
        Y <- c(upr[1:up_idx],rev(lwr[1:lo_idx]))
        
        solid <- NA
        trans <- NA
        
        targetstyle<- sprintf("%s %s",groupname,signalname)
        if (targetstyle %in% names(styles)) {
          solid <- styles[[targetstyle]]$solid
          trans <- styles[[targetstyle]]$trans
        } else {
          if (groupname %in% names(styles)) {
            solid <- styles[[groupname]]$solid
            trans <- styles[[groupname]]$trans
          }
        }
        
        polygon(X,Y,col=trans,border=NA)
        
        avg <- processes[['lambda']]
        av_idx <- which(avg >= 1)[1]
        lines(xcoords[1:av_idx],avg[1:av_idx],col=solid)
        
      }
      
    }
    
  }
  
  if (xscale == 'normal') {
    
    polygon(c(0,20,20,0),c(1,1,2,2),col='white',border=NA)
    
    lines(c(0,35),c(1,1),col='black',lty=1,lw=2)
    text(35,1.05,'asymptote lower bound',adj=c(1,0.5))
    
    legend(22,.4,legend=c('Continuous','Terminal', 'CursorJump'),col=groupcolors,lty=c(1,1,1),bty='n')
    
    axis(side=1, at=c(0,5,10,15,20,25,30,35), labels=c('baseline',sprintf('%d',c(5,10,15,20,25,30,35))))
    axis(side=2, at=seq(0,1,0.2), labels=sprintf('%d',round(seq(0,1,0.2)*100)),las = 2)
    
  }
  
  if (target %in% c('svg','pdf','eps','tiff')) {
    dev.off()
  }
  
}

