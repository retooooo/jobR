logHTML <- function(jobinfos, steps, args, filename, log_folder, flowchart){
  log <- c("<html style = 'font-family: monospace'>",
           "<h2>Log-File</h2>",
           "<h3>General</h3>",
           "<table>",
           "<tr>",
           paste(sapply(1:nrow(jobinfos), function(i) paste0("<td>", jobinfos$caption[i], "</td><td><b>", jobinfos$value[i], "</b></td>")), collapse = "</tr><tr>"),
           "</tr>",
           "</table>",
           "<h3>Arguments</h3>")
  if(length(args) > 0){
    log <- c(log,
             "<table>",
             "<tr>",
             paste(sapply(1:length(args), function(i) paste0("<td>", names(args)[i], "</td><td>", args[[i]], "</td>")), collapse = "</tr><tr>"),
             "</tr>",
             "</table>"
    )
  }
  steps_cols <- c("STEP", "STATUS", "RUNTIME", "MESSAGE")
  log <- c(log,
           "<h3>Process Steps</h3>",
           "<table>",
           "<tr>",
           "<th>",
           paste(steps_cols, collapse = "</th><th>"),
           "</th>",
           "</tr>",
           "<tr>",
           paste(sapply(1:nrow(steps), function(i){
             paste0("<td>",
                    paste(sapply(steps_cols, function(j) steps[i, j]), collapse = "</td><td>"),
                    "</td>")
           }), collapse = "</tr><tr>"),
           "</tr>",
           "</table>",
           flowchart,
           "</html>")
  writeLines(log, file.path(log_folder, filename))
}
