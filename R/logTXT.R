logTXT <- function(jobinfos, steps, args, filename){
  log <- sapply(1:nrow(jobinfos), function(i) paste0(jobinfos$caption[i], ": ", jobinfos$value[i]))
  if(length(args) > 0){
    log <- c(log,
             "args:",
             sapply(1:length(args), function(i){
               paste0("@@ par", "\n",
                      " ", "name: ", names(args)[i], "\n",
                      " ", "value: ", args[[i]])
             })
    )
  }
  log <- c(log,
           "steps:",
           sapply(1:nrow(steps), function(i){
             tmp <- paste0("## step", "\n",
                           " ", "name: ", steps$STEP[i], "\n",
                           " ", "status: ", steps$STATUS[i], "\n",
                           " ", "runtime: ", steps$RUNTIME[i], "\n",
                           " ", "message: ", steps$MESSAGE[i])
           }))
  writeLines(log, file.path(log_folder, filename))
}
