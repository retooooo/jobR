readLog <- function(file){
  tmp <- readLines(file)
  tmp2 <- strsplit(tmp, ": ")
  steps <- which(grepl("##", tmp))
  args <- which(grepl("@@", tmp))
  args_list <- if(length(args) > 0){
    lapply(args, function(i){
      data.frame(NAME = tmp2[[i + 1]][2],
                 VALUE = tmp2[[i + 2]][2],
                 stringsAsFactors = FALSE)
    })
  }else{
    list()
  }
  steps_list <- lapply(steps, function(i){
    data.frame(STEP = tmp2[[i + 1]][2],
               STATUS = as.numeric(tmp2[[i + 2]][2]),
               RUNTIME = as.numeric(tmp2[[i + 3]][2]),
               MESSAGE = tmp2[[i + 4]][2],
               stringsAsFactors = FALSE)
  })
  return(list(jobname = tmp2[[1]][2],
              starttime = as.POSIXct(tmp2[[2]][2]),
              endtime = as.POSIXct(tmp2[[3]][2]),
              runtime = as.numeric(tmp2[[4]][2]),
              status = as.numeric(tmp2[[5]][2]),
              user = tmp2[[6]][2],
              steps = do.call("rbind", steps_list),
              args =  do.call("rbind", args_list)))
}
