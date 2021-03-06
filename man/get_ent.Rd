% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EPDr-get_functions.R
\name{get_ent}
\alias{get_ent}
\alias{.get_entity}
\alias{.get_pentity}
\alias{.get_coredrive}
\alias{.get_descr}
\alias{.get_lithology}
\alias{.get_loi}
\alias{.get_section}
\alias{.get_publent}
\title{Retrieve description for EPD entities}
\usage{
get_ent(e_, connection)

.get_entity(e_, connection)

.get_pentity(e_, connection)

.get_coredrive(e_, connection)

.get_descr(descriptor, connection)

.get_lithology(e_, connection)

.get_loi(e_, connection)

.get_section(e_, connection)

.get_publent(e_, connection)
}
\arguments{
\item{e_}{numeric Value indicating the entity number (e_) of 
the database that want to be queried.}

\item{connection}{PostgreSQLConnection Object of class 
\code{PostgreSQLConnection} as returned by function 
\code{\link[EPDr]{connect_to_epd}}.}

\item{descriptor}{character Three/four letter code for the 
description of the entity.}
}
\value{
\code{\link[EPDr]{entity-class}} object. This is an EPDr object 
with information from different tables. See documentation of the EPD: 
\url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
}
\description{
Functions in this family retrieves information relative to the 
description of the entity itself. The main function 
(\code{\link[EPDr]{get_ent}}) requires a valid connection to the 
database and the entity ID for the entity of interest. All other 
functions (starting with a dot [.]) use different arguments 
depending on the piece of information they retrieve.
}
\section{get_entity}{

This function returns an \code{\link[EPDr]{entity-class}} object with several 
information from the rest of the functions for a particular entity.
}

\section{.get_entity}{

This functions returns a data.frame with information in in the ENTITY table for the 
specified entity. This corresponds with description information 
for that entity.
}

\section{.get_pentity}{

This functions returns a data.frame with information in in the P_ENTITY table for the 
specified entity. This corresponds with details on the status of 
the data, contact person, and restriction of use for the data in 
that entity.
}

\section{.get_coredrive}{

This functions returns a data.frame with information in in the COREDRIVE table for the 
specified entity. This corresponds with details on the drirve used 
to sample the entity.
}

\section{.get_descr}{

This functions returns a data.frame with information in in the DESCR table for the 
specified entity. This corresponds with a longer description 
of the entity.
}

\section{.get_lithology}{

This functions returns a data.frame with information in in the LITHOLOGY table for 
the specified entity. This corresponds with details on the 
lithology found when drilling the entity.
}

\section{.get_loi}{

This functions returns a data.frame with information in in the LOI table for the 
specified entity. This corresponds with details on the 
loss-on-ignition for samples in that particular entity the entity.
}

\section{.get_section}{

This functions returns a data.frame with information in in the SECTION table for the 
specified entity. This corresponds with details on the sections 
used to sample the entity.
}

\section{.get_publent}{

This functions returns a data.frame with information in in the PUBLENT table for the 
specified entity. This corresponds with publications in which data 
for that entity have been published.
}

\examples{
\dontrun{
epd.connection <- connect_to_epd(host = "localhost", database = "epd",
                                 user = "epdr", password = "epdrpw")
ent.400 <- get_ent(400, epd.connection)
ent.400
}
}
