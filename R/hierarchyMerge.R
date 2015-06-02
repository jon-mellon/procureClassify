hierarchyMerge <- function(level.vars, cutoff = 0.005, codebook) {
  merged.cat <- rep(NA, length(level.vars[, 1]))
  oth.top <- names(which(prop.table(table(level.vars[, 1])) < cutoff))
  merged.cat[level.vars[, 1] %in% oth.top] <- "Oth"
  
  for(i in 2:ncol(level.vars)) {
    at.level <- names(which(prop.table(table(level.vars[, i])) < cutoff))
    merged.cat[level.vars[, i] %in% at.level & is.na(merged.cat)] <- level.vars[, (i - 1)][
      level.vars[, i] %in% at.level & is.na(merged.cat)]
  } 
  merged.cat[is.na(merged.cat)] <- level.vars[is.na(merged.cat), ncol(level.vars)]
  all.labs <- unique(merged.cat)
  for(i in all.labs) {
    if(i!="Oth") {
      current.code <- str_pad(i, width = 8, side = "right", pad = "0")
      library(stringr)
      
      current.lab <- codebook[codebook$code==current.code, "label"]
      current.lab <- paste0(current.code, ": ", current.lab)
      
      if(sum(grepl(i, all.labs))>1) {
        current.lab <- paste(current.lab, "(other)")
      }
      merged.cat[merged.cat==i] <- current.lab
    }
  }
  return(merged.cat)
}