findJobs <- function(path = getwd()){
  jobs_all <- dir(path, recursive = TRUE, include.dirs = FALSE, pattern = "job.yml", full.names = TRUE)
  jobs_list <- lapply(jobs_all, function(i){
    cfg <- config::get(file = i)
    stopifnot(c("name", "id") %in% names(cfg))
    data.frame(id = cfg$id,
               name = cfg$name,
               exec = gsub(pattern = "job.yml", replacement = "main.R", i),
               config = i,
               row.names = NULL,
               stringsAsFactors = FALSE)
  })
  jobs_dataframe <- do.call("rbind", jobs_list)
  return(jobs_dataframe[order(jobs_dataframe$id),])
}
