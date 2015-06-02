\name{procureFit}
\alias{procureFit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
procureFit(procure, random = TRUE, text.var = "text", classifier = c("svm", "maxent", "glmnet"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{procure}{
%%     ~~Describe \code{procure} here~~
}
  \item{random}{
%%     ~~Describe \code{random} here~~
}
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{classifier}{
%%     ~~Describe \code{classifier} here~~
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
function (procure, random = TRUE, text.var = "text", classifier = c("svm", 
    "maxent", "glmnet")) 
{
    dtm <- formatProcureText(procure, text.var)
    if (random) {
        procure$fit <- 1
        procure$fit[sample(1:nrow(procure), 250)] <- 0
    }
    container <- create_container(dtm, procure$label, trainSize = which(procure$fit == 
        1), testSize = which(procure$fit == 0), virgin = TRUE)
    if (classifier[1] == "svm") {
        SVM <- train_model(container, "SVM")
        predictions <- classify_model(container, SVM)
    }
    if (classifier[1] == "maxent") {
        MAXENT <- train_model(container, "MAXENT")
        predictions <- classify_model(container, MAXENT)
    }
    if (classifier[1] == "glmnet") {
        GLMNET <- train_model(container, "GLMNET")
        predictions <- classify_model(container, GLMNET)
    }
    predictions$true.label <- procure$label[procure$fit == 0]
    predictions$text <- procure[procure$fit == 0, text.var]
    return(predictions)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
