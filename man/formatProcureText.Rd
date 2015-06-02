\name{formatProcureText}
\alias{formatProcureText}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
formatProcureText(procure, text.var)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{procure}{
%%     ~~Describe \code{procure} here~~
}
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
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
function (procure, text.var) 
{
    TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, 
        max = 2))
    text <- mapply(paste, procure[, text.var], collapse = " ")
    text <- stripWhitespace(text)
    text <- removePunctuation(text)
    text <- tolower(text)
    text <- Corpus(VectorSource(text))
    text <- tm_map(text, removeWords, c("the", stopwords("english")))
    text <- tm_map(text, removeNumbers)
    dtm <- DocumentTermMatrix(text, control = list(weighting = weightTf, 
        tokenize = TrigramTokenizer))
    return(dtm)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
