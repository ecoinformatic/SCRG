\name{scrg__get_geometry_in_bbox__point_list}
\alias{scrg__get_geometry_in_bbox__point_list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{scrg__get_geometry_in_bbox_pointlist}
\description{
This function takes a series of points and an sf object, and returns a subset of
the sf object that falls within the bounding box formed by the series of points.
}
\usage{
scrg__get_geometry_in_bbox(point_list, sf_object, byrow=TRUE)
}
\arguments{
  \item{point_list}{
    The list of the latitudes and longitudes that form a bounding box.
  }
  \item{sf_object}{
    This is the geospatial collection and needs to be a variable of the class
    "sf".
  }
  \item{byrow}{
    This is an optional boolean value that determines whether the list of points
    are read as pairs (ie. byrow=TRUE) or as a list of longitudes followed by a
    list of the accompanying latitudes (ie. byrow=FALSE). This value defaults to
    'TRUE'.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a subset of `sf_object` that falls wihin a bounding box.
}
\references{
UCF Ecoinformatics Lab
}
\author{
Thomas Adriaan Hellinger
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
