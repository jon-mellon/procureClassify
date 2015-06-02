# misc functions


readProcurementBook <- function(file = "GOK_2008_2009_Itemcode_Classification.xlsx") {
  require(readxl)
  
  all.sheets <- excel_sheets(file)
  
  readSheets <- function(sheet) {
    print(sheet)
    data <- read_excel(file, sheet = sheet, skip = 4)
    header <- read_excel(file, sheet = sheet, skip = -1)[1, 3]
    
    data <- data[, grepl("Tender No.|title|value|bids", colnames(data), 
                         ignore.case = TRUE)]
    data$label <- header
    return(data)
  }
  
  all.data <- lapply(all.sheets, readSheets)
  library(data.table)
  all.data <- rbindlist(all.data)
  # all.data$Description1 <- tolower(all.data$Description1)
  #	procure$Title <- tolower(procure$Title)
  # all.data$Title <- tolower(all.data$Title)
  # all.data <- all.data[!is.na(all.data$Description1), ]
  return(all.data)
}