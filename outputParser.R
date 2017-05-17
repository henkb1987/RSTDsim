outputParser <- function(output.file){
  raw.file <- readLines(output.file)
  header <- raw.file[1:7]
  raw.file <- raw.file[8:length(raw.file)]
  captions.locations <- which(regexpr("\t",raw.file) == -1)
  captions <- raw.file[captions.locations]
  results <- vector("list", length(captions))
  names(results) <- rep(NA, length(results))
  for(i in 1:length(results)){
    cat("Table",i,":",captions[i],"...\n")
    names(results)[i] <- gsub(" ","",captions[i])
    columns <- strsplit(raw.file[captions.locations[i] + 1], "\t")[[1]][-1]
    n.columns <- length(columns)
    row.data <- strsplit(raw.file[(captions.locations[i]+2):min(length(raw.file),captions.locations[i+1]-1,na.rm = T)],"\t") 
    n.rows <- length(row.data)
    results[[i]] <- matrix(NA, n.rows, n.columns)
    rows <- 1:n.rows
    colnames(results[[i]]) <- columns
    for(r in 1:n.rows){
      if(length(row.data[[r]]) < n.columns){
        row.data[[r]] <- c(row.data[[r]], rep(NA, 1 + n.columns - length(row.data[[r]])))
      }
      rows[r] <- row.data[[r]][1]
      results[[i]][r,] <- row.data[[r]][2:length(row.data[[r]])]
    }
    results[[i]] <- matrix(as.numeric(results[[i]]), nrow = n.rows)
    rownames(results[[i]]) <- rows
    colnames(results[[i]]) <- columns
  }
  save(results, file=paste0(gsub(".txt","",output.file),"_results"))
  return(T)
}