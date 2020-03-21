#' @export
job <- R6Class(
  classname = "job",
  public = list(
    name = NA,
    error = NA,
    log_folder = NA,
    start_time = NA,
    end_time = NA,
    silent = NA,
    steps = NA,
    args = NA,
    user = NA,
    filename = NA,
    logtype = NA,
    dependencies = NA,
    initialize = function(logfolder = NA, silent = FALSE, logtype = "html"){
      cfg <<- config::get(file = "job.yml")
      if (!is.null(cfg$packages)){
        for (p in cfg$packages){
          suppressMessages(library(package = p, character.only = TRUE))
        }
      }
      args <- commandArgs(trailingOnly = TRUE)
      if(!is.null(cfg$parameters)){
        if(length(cfg$parameters) > 0 & length(args) > 0){
          for(i in 1:min(length(cfg$parameters), length(args))){
            cfg$parameters[[i]] <<- args[[i]]
          }
        }
      }
      self$args = list()
      for(i in 1:length(cfg$parameters)){
        self$setParameter(name = names(cfg$parameters)[i], value = cfg$parameters[i])
      }
      self$logtype = logtype
      self$name = cfg$name
      self$filename = paste0(self$name, "_", format(Sys.time(), format = "%Y%m%d_%H%M%S"))
      self$silent = silent
      self$error = FALSE
      self$user = Sys.info()["user"][[1]]
      self$log_folder = logfolder
      self$start_time = Sys.time()
      self$dependencies <- data.frame(
        STEP = character(),
        DEPENDS = character()
      )
      self$steps = data.frame(
        STEP = "x",
        START_TIME = Sys.time(),
        END_TIME = Sys.time(),
        STATUS = 0,
        MESSAGE = "",
        stringsAsFactors = FALSE)[FALSE, ]
    },
    setParameter = function(name, value){
      if(is.null(value)){
        value <- 0
      }else{
        if(is.na(value)){
          value <- 0
        }
      }
      self$args[[name]] = value
    },
    execute = function(
      code,
      step_name = nrow(self$steps) + 1,
      ignore.error = FALSE,
      silent = self$silent,
      depends = c()){
      tmp <- data.frame(STEP = step_name,
           START_TIME = Sys.time(),
           END_TIME = Sys.time(),
           STATUS = 0,
           MESSAGE = "",
           stringsAsFactors = FALSE)
      dependencies <- sum(self$steps$STATUS[self$steps$STEP %in% depends])
      if(is.na(dependencies)) dependencies <- 1
      for (d in depends){
        self$dependencies <- rbind(self$dependencies, data.frame(STEP = step_name, DEPENDS = d))
      }
      if((!self$error | ignore.error) & dependencies == 0){
        if(!silent) message(paste0("Executing step '", step_name, "'"))
        tryCatch({
          eval(code)
        }, error = function(e){
          ## Step has errors
          tmp$STATUS <<- 1
          tmp$MESSAGE <<- gsub(x = e$message, pattern = "\n", " ")
          self$error = TRUE
        })
      }else{
        ## Step did not start because of erlier errors
        tmp$STATUS = 1
        tmp$MESSAGE = "Step was not executed, due to prior errors"
      }
      tmp$END_TIME = Sys.time()
      self$steps = rbind(self$steps, tmp)
    },
    finish = function(){
      self$end_time <- Sys.time()
      self$steps$RUNTIME <- round(as.numeric(self$steps$END_TIME) - as.numeric(self$steps$START_TIME), 2)
      if(!is.na(self$log_folder)){
        private$createLog(self$logtype)
      }
      self$plotDependencies(base64 = FALSE)
    },
    plotDependencies = function(base64 = FALSE){
      nodes <- self$steps[, c("STEP", "STATUS", "END_TIME", "START_TIME")]
      nodes$RUNTIME = as.numeric(nodes$END_TIME) - as.numeric(nodes$START_TIME)
      nodes$id <- 1:nrow(nodes)
      nodes$color <- "green3"
      nodes$color[nodes$STATUS == 1] <- "red2"
      nodes$label <- paste0(nodes$STEP, "\n", round(nodes$RUNTIME, 2))
      nodes <- nodes[, c("id", "color", "label")]
      tmp <- data.frame(
        STEP = self$steps$STEP,
        id = 1:nrow(self$steps))
      names(tmp) <- c("STEP", "to")
      edges_tmp <- base::merge(self$dependencies, tmp, on = "STEP")
      names(tmp) <- c("DEPENDS", "from")
      edges <- base::merge(edges_tmp, tmp, by = "DEPENDS")[, c("from", "to")]
      plotFlow(nodes = nodes, edges = edges, base64 = base64)
    }
  ),
  private = list(
    createLog = function(type){
      jobinfos <- data.frame(
        caption = c("jobname", "starttime", "endtime", "runtime", "status", "user"),
        value = c(self$name, as.character(self$start_time), as.character(self$end_time), round(as.numeric(self$end_time) - as.numeric(self$start_time), 2), self$error * 1, self$user)
      )
      filename <- paste0(self$filename, ".", type)
      if (type == "txt"){
        logTXT(jobinfos = jobinfos, steps = self$steps, args = self$args, filename = filename, log_folder = self$log_folder)
      }else if(type == "html"){
        logHTML(jobinfos = jobinfos, steps = self$steps, args = self$args, filename = filename, log_folder = self$log_folder, flowchart = self$plotDependencies(base64 = TRUE))
      }
    }
  )
)
