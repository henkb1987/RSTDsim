bat.maker <- function(inputfile, n = 1,i = 0){
  bat <- paste0("start /wait /min run ", inputfile," 0 ",n,"\n",
                "start /wait /min avg ", inputfile, " 0 ", n,"\n",
                "PAUSE")
  bat.path <- paste0(inputfile,i,".bat")
  cat("Wrote",bat.path,"!\n")
  write(bat,bat.path)
  return(bat.path)
}