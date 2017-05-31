GenerateInputFile <- function(input.file.path, new.inputs, run.id=0){
  input.file <- readLines(input.file.path)
  for(i in 1:nrow(new.inputs)){
    input.file <- gsub(pattern = new.inputs$key[i], replace = new.inputs$new.value[i], x = input.file)
    if(new.inputs$new.value[i] != 1){
      #cat("Inserting",new.inputs$new.value[i],"into key",new.inputs$key[i],"of inputfile",input.file.path,"\n")
    }
  }
  new.path <- paste0(gsub("\\.jif","",input.file.path),"_",run.id,".jif")
  cat("Writing filled-in input file",new.path,"\n")
  write(input.file, file=new.path)
  return(new.path)
}
