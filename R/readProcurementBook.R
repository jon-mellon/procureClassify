readProcurementBook <- function(file = "TenderReport_ODI_2009_2010.xlsx") {
  all.sheets <- excel_sheets(file)
  
  readSheets <- function(sheet) {
    print(sheet)
    data <- read_excel(file, sheet = sheet, skip = 2)
    header <- read_excel(file, sheet = sheet, skip = 0)[1, 2]
    
    data <- data[, grepl("description|title|value|bids", colnames(data), 
                         ignore.case = TRUE)]
    data$label <- header
    return(data)
  }
  
  all.data <- lapply(all.sheets, readSheets)
  library(data.table)
  all.data <- rbindlist(all.data)
  all.data$Description1 <- tolower(all.data$Description1)
  all.data$Title <- tolower(all.data$Title)
  all.data <- all.data[!is.na(all.data$Description1), ]
  return(all.data)
}