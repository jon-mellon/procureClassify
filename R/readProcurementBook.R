readProcurementBook <- function(file = "TenderReport_ODI_2009_2010.xlsx", 
                                skip = 2, pos.label = c(2, 2)) {
  require(readxl)
  all.sheets <- excel_sheets(file)
  
  readSheets <- function(sheet, skip) {
    print(sheet)
    data <- read_excel(file, sheet = sheet, skip = skip)
    header <- read_excel(file, sheet = sheet, 
                         skip = 0, col_names = FALSE)[pos.label[1], pos.label[2]]
    
    data <- data[, grepl("description|title|value|bids|Tender.+No", colnames(data), 
                         ignore.case = TRUE)]
    data$label <- header
    data$code <- as.character(sheet)
    colnames(data)[grepl("Tender.Title", colnames(data))] <- "Title"
    names(data) <- sub(" ", ".", names(data))
    return(data)
  }
  
  all.data <- lapply(all.sheets, readSheets, skip = skip)
  library(data.table)
  all.data <- data.frame(rbindlist(all.data), stringsAsFactors = FALSE)
  all.data[, "Description1"] <- tolower(all.data$Description1)
  all.data$Title <- tolower(all.data$Title)
  
  all.data$code.2 <- substr(all.data$code, 1,2)
  all.data$code.4 <- substr(all.data$code, 1,4)
  all.data$code.6 <- substr(all.data$code, 1,6)
  all.data$code.8 <- substr(all.data$code, 1,8)
  return(all.data)
}