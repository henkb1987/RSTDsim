# load all needed functions
source("hw_constraints.R")
source("batMaker.R")
source("GenerateInputFile.R")
source("outputParser.R")

# initialize file that will contain input changes
inputs.data <- data.frame(
  key = c(
    "@end.year",
    paste0("@fr",1:50),
    paste0("@a",1:50,".1"),
    paste0("@a",1:50,".2"),
    paste0("@a",1:50,".3"),
    paste0("@a",1:50,".4"),
    paste0("@a",1:50,".5")
  ),
  year = c(0,rep(1:50, 6)),
  new.values = c(paste0(2000, "y"), rep(1, 6 * 50)),
  on.art = 0,
  stringsAsFactors = F
)

# run model until (and including) 2000
new.path <- GenerateInputFile("henk.jif",inputs.data)
new.path <- bat.maker(gsub("\\.jif","",new.path),n = 1)
shell(paste0("C:\\Users\\manZ130194\\Documents\\STDsim\\",new.path))
outputParser("henk_0.txt")

for(yr in 1:20){
  # load results from stdsim results
  load("henk_0_results")
  # find out ART usage (ART std) in previous year
  art.usage <- distill.art.usage(results)
  art.demand <- rowSums(art.usage)
  demand <- art.demand[paste(2000 + yr)]
  # get ART capacity in yr as based on SA calculation that were done in hw_constraints.R
  capacity <- hw.data$art.capacity[hw.data$year == 2000 + yr]
  # calculate waiting time increases in next year
  # assume that if demand is k% higher than supply,
  # waiting time for everyone results in people progressing
  # through ART in (1-k%) of the time, which means their CD4 progresses faster.
  # and insert new waiting times into inputs.data
  inputs.data$new.values[grepl(paste0("@a",yr + 1), inputs.data$key, T)] <- capacity / demand
  cat("Waiting factor",capacity / demand,".\n")
  # re-run stdsim with new.values for next year
  # adjust end.year to 2000 + yr
  inputs.data$new.values[inputs.data$key == "@end.year"] <- 2000 + yr
  new.path <- GenerateInputFile("henk.jif",inputs.data)
  new.path <- bat.maker(gsub("\\.jif","",new.path),n = 1)
  shell(paste0("C:\\Users\\manZ130194\\Documents\\STDsim\\",new.path))
  outputParser("henk_0.txt")
}
load("henk_0_results")
plot(1990:2050,rowSums(distill.art.usage(results)),type="l")
