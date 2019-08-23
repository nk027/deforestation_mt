
for(csv in list.files("txt", "csv$")) {
  x <- read.csv(paste0("txt/", csv), stringsAsFactors = FALSE)
  x <- x[, -1]
  
  x <- rbind(
    gsub("(*.)[.][0-9]+", "\\1", colnames(x)),
    x
  )
  
  colnames(x) <- c("variables", 
                   rep("SDM_qu", 4), rep("SDM_k5", 4), rep("SDM_k7", 4),
                   rep("CLM", 2), 
                   rep("SAR_qu", 2), rep("SAR_k5", 2), 
                   rep("SEM_qu", 2), rep("SEM_k5", 2))
  write.csv(x, paste0("txt/", csv))
}
