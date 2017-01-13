\name{recode113}
\alias{recode113}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
113 Selected Causes of Death
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
In order to provide a consistent ranking standard the NCHS (National Center for Health Statistics) prepared a list of 113 selected causes of death.
}
\usage{data("recode113")}
\format{
  A data frame with 135 observations on the following 6 variables.
  \describe{

    \item{\code{recode}}{a numeric vector (1-135) indicating the recode}
    \item{\code{t}}{a numeric vector (1) indicating a 'super-category' .. this code has child codes}
    \item{\code{parent}}{a numeric vector indicating the parent of this recode (equals itself if no parent)}
    \item{\code{sex}}{a numeric vector indicating if this recode is sex-limited ...

    1=Male 2=Female (ex. prostate cancer is male only)}

    \item{\code{age}}{a numeric vector indicating iof this recode is age-limited ...

    1 = 5 and over; 2 = 10-54; 3 = 28 days and over; 4 = Under 1 year;

    5 = 1-4 years; 6 = 1 year and over; 7 = 10 years and over}

    \item{\code{cause}}{a character vector describing the cause of death}

  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~

  http://www.cdc.gov/nchs/data/datalinkage/Public-use_Data_Dictionary.pdf  (Appendices 3 and 4)
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(recode113)

}
\keyword{datasets}
