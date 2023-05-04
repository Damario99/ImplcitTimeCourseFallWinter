readQualtricsData <- function(exp=2, part='intake') {
  
  if (exp == 1) {
    if (part == 'intake') { file <- 'fast+forward+lab+intake_April+1,+2023_12.42.csv' }
    if (part == 'task')   { file <- 'fast+forward+lab+task_April+1,+2023_12.43.csv' }
  }
  if (exp == 2) {
    if (part == 'intake') { file <- 'new+dataset+fast+forward+lab+intake_April+17,+2023_20.57.csv' }
    if (part == 'task')   { file <- 'new+dataset+fast+forward+lab+task_April+17,+2023_21.02.csv' }
  }
  
  filepath <- sprintf('data/%s',file)
  
  if (part == 'intake') {
    columnnames <- c('StartDate', 'EndDate', 'Status', 'IPAddress',    'Progress', 'Duration (in seconds)', 'Finished', 'RecordedDate', 'ResponseId', 'RecipientLastName', 'RecipientFirstName', 'RecipientEmail', 'ExternalReference', 'LocationLatitude', 'LocationLongitude', 'DistributionChannel', 'UserLanguage', 'consent', 'age', 'sex', 'height_cm', 'handedness', 'id', 'group')
    use_cols <- c('Progress', 'Finished', 'consent', 'age', 'sex', 'height_cm', 'handedness', 'id', 'group')
  }
  if (part == 'task') {
    columnnames <- c('StartDate', 'EndDate', 'Status', 'IPAddress',    'Progress', 'Duration (in seconds)', 'Finished', 'RecordedDate', 'ResponseId', 'RecipientLastName', 'RecipientFirstName', 'RecipientEmail', 'ExternalReference', 'LocationLatitude', 'LocationLongitude', 'DistributionChannel', 'UserLanguage', 'needs_vision_correction', 'has_vision_correction', 'strategy_verbalization', 'id', 'group', 'condition')
    use_cols <- c('Progress', 'Finished', 'needs_vision_correction', 'has_vision_correction', 'strategy_verbalization', 'id', 'group', 'condition')
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
  df_taks <- df_task[which(df_task$id != ""),]
  
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
  df <- merge(df_intake, df_task, by=c('id'))
  
  return(df)
  
}


getPavloviaParticipants <- function(exp=2) {
  
  # we need to look in specific folders for every experiment: 
  if (exp == 1) {folders <- c('itc3tab_ffw')}
  if (exp == 2) {folders <- c('itc4tab_expl', 'itc5tab_rot')}
  
  paths <- c()
  for (folder in folders) {
    # from each folder we need csv files with the string "_implicit_time_" in their name:
    all_paths <- c(paths, Sys.glob(sprintf('data/%s/*_implicit_time_*.csv',folder)))
  }
  
  remove_ids <- c()
  
  id <- c()
  timestamp <- c()
  path <- c()
  
  for (one_path in all_paths) {
    
    added <- FALSE
    
    # extract participant id (and pavlovia timestamp):
    id_time   <- getIDtimestamp(basename(one_path), task='implicit_time')
    
    # check if the file makes sense (just counting lines for now):
    pav_check <- checkPavloviaFile(path=one_path)
    if ( pav_check['check'] ) {
      if (checkAdapted(path=one_path)) {
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
    
    # add participant ID and file timestamp to lists:
    if (added) {
      id        <- c( id,        id_time['participant'] )
      timestamp <- c( timestamp, id_time['timestamp']   )
    }
    
  }
  
  # make a new data frame with the collected info
  df <- data.frame(id, timestamp, path)
  
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


checkAdapted <- function(path) {
  
  df <- read.csv(path)
  
  # print(str(df))
  
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
  criterion <- abs(rotation/2)
  
  if (rotation > 0) {reach_deviations <- reach_deviations * -1}
  
  return(median(reach_deviations, na.rm=TRUE) > criterion) 
  
}

convertCellToNumVector <- function(v) {
  
  # remove opening square bracket:
  v <- gsub('\\[', replacement='', x=v)
  # remove closing square bracket:
  v <- gsub(']', replacement='', x=v)
  # split by commas:
  v <- strsplit(v, ',')
  # convert to numeric:
  v <- lapply(v, FUN=as.numeric)
  # make vector:
  v <- as.vector(unlist(v))
  
  return(v)
  
}