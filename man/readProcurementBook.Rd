\name{readProcurementBook}
\alias{readProcurementBook}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reads manually coded procurement data for Karnataka and Odisha
}
\description{
Reads manually coded procurement data for Karnataka and Odisha
}
\usage{
readProcurementBook(file = "TenderReport_ODI_2009_2010.xlsx")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (file = "TenderReport_ODI_2009_2010.xlsx") 
{
    all.sheets <- excel_sheets(file)
    readSheets <- function(sheet) {
        print(sheet)
        data <- read_excel(file, sheet = sheet, skip = 2)
        header <- read_excel(file, sheet = sheet, skip = 0)[1, 
            2]
        data <- data[, grepl("description|title|value|bids", 
            colnames(data), ignore.case = TRUE)]
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
