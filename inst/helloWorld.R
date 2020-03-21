require(jobR)

## Init new job
logfolder = getwd() ## Change Log Folder!
myjob <- job$new(name = "Example Task", logfolder = logfolder)

## Execute some process steps
myjob$execute(step_name = "Define variables", code = {
  var1 = 2
  var2 = 5
})
myjob$execute(step_name = "Add variables", code = {
  print(var1 + var2)
})
myjob$execute(step_name = "Strange Step", code = {
  var3 = asd
})

## Add a job parameter to the log file
myjob$setParameter("RG_MM", 201812)

## Finish Job and create Log
myjob$finish()

## Read Log for Analysis
mylog <- readLog(file.path(logfolder, myjob$filename))
str(mylog)
