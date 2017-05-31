outputParser <- function(output.file){
  raw.file <- readLines(output.file)
  header <- raw.file[1:7]
  raw.file <- raw.file[8:length(raw.file)]
  captions.locations <- which(regexpr("\t",raw.file) == -1)
  captions <- raw.file[captions.locations]
  results <- vector("list", length(captions))
  names(results) <- rep(NA, length(results))
  for(i in 1:length(results)){
    #cat("Table",i,":",captions[i],"...\n")
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
  cat("Wrote",paste0(gsub(".txt","",output.file),"_results"),"\n")
  return(T)
}
distill.art.usage <- function(results){
  pop.size <- results$`averagePOPULATIONsize(MALE/FEMALE/BOTH)`
  art <- results$`averageARTprevalence[%]`
  hiv <- results$`averageHIVprevalence[%]`
  art.size <- pop.size * 0
  age.labels <- which(colnames(art)!="")
  for(i in 1:(length(age.labels) - 1)){
    art.size[, i] <- rowSums(hiv[2:nrow(hiv), (age.labels[i] + 1):(age.labels[i] + 15)]) * 
      rowSums(art[2:nrow(art), (age.labels[i] + 1):(age.labels[i] + 15)]) * 
      pop.size[,i]
  }
  rownames(art.size) <- 1990:2050
  return(art.size)
}