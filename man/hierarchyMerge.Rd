\name{hierarchyMerge}
\alias{hierarchyMerge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Merge UNSPSC hierarchies based on frequency in data
}
\description{
Uses UNSPSC codebook and codes to classify data into useful mutually exclusive categories. 
}
\usage{
hierarchyMerge(level.vars, cutoff = 0.005, codebook)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{level.vars}{
%%     ~~Describe \code{level.vars} here~~
}
  \item{cutoff}{
%%     ~~Describe \code{cutoff} here~~
}
  \item{codebook}{
%%     ~~Describe \code{codebook} here~~
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
function (level.vars, cutoff = 0.005, codebook) 
{
    merged.cat <- rep(NA, length(level.vars[, 1]))
    oth.top <- names(which(prop.table(table(level.vars[, 1])) < 
        cutoff))
    merged.cat[level.vars[, 1] \%in\% oth.top] <- "Oth"
    for (i in 2:ncol(level.vars)) {
        at.level <- names(which(prop.table(table(level.vars[, 
            i])) < cutoff))
        merged.cat[level.vars[, i] \%in\% at.level & is.na(merged.cat)] <- level.vars[, 
            (i - 1)][level.vars[, i] \%in\% at.level & is.na(merged.cat)]
    }
    merged.cat[is.na(merged.cat)] <- level.vars[is.na(merged.cat), 
        ncol(level.vars)]
    all.labs <- unique(merged.cat)
    for (i in all.labs) {
        if (i != "Oth") {
            current.code <- str_pad(i, width = 8, side = "right", 
                pad = "0")
            library(stringr)
            current.lab <- codebook[codebook$code == current.code, 
                "label"]
            current.lab <- paste0(current.code, ": ", current.lab)
            if (sum(grepl(i, all.labs)) > 1) {
                current.lab <- paste(current.lab, "(other)")
            }
            merged.cat[merged.cat == i] <- current.lab
        }
    }
    return(merged.cat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
