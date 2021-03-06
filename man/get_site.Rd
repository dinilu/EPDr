% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EPDr-get_functions.R
\name{get_site}
\alias{get_site}
\alias{.get_siteloc}
\alias{.get_sitedesc}
\alias{.get_siteinfo}
\alias{.get_poldiv1}
\alias{.get_poldiv2}
\alias{.get_poldiv3}
\alias{.get_igcptype}
\alias{.get_infotype}
\title{Retrieve site information for EPD entities}
\usage{
get_site(e_, connection)

.get_siteloc(site_, connection)

.get_sitedesc(site_, connection)

.get_siteinfo(site_, connection)

.get_poldiv1(poldiv1_, connection)

.get_poldiv2(poldiv2_, poldiv1_, connection)

.get_poldiv3(poldiv3_, poldiv2_, poldiv1_, connection)

.get_igcptype(igcptype, connection)

.get_infotype(icode, connection)
}
\arguments{
\item{e_}{numeric Value indicating the entity number (e_) of the 
database that want to be queried.}

\item{connection}{PostgreSQLConnection Object of class 
\code{PostgreSQLConnection} as returned by function 
\code{\link[EPDr]{connect_to_epd}}.}

\item{site_}{numeric Value indicating the site number (site_) of 
interest in the database.}

\item{poldiv1_}{character Three character string. The three 
character string are the international country code.}

\item{poldiv2_}{character Two number string. This string with 
length equal two and with numbers represent the regions code for 
administrative regions in each country. The code is not unique 
so to capture an specific region in a country you need to provide 
always country code (poldiv1_) and region code (poldiv2_).}

\item{poldiv3_}{character Three number string. This string with 
length equal three and with numbers represent the 3rd level 
administrative regions in each country.  The code is not unique 
so to capture an specific 3rd level region in a country you need 
to provide always country code (poldiv1_), region code (poldiv2_), 
and 3rd level region code (poldiv3_).}

\item{igcptype}{character Representing the IGCP type code.}

\item{icode}{character Three letter string representing the 
information code.}
}
\value{
\code{\link[EPDr]{site-class}} object. This is an EPDr object 
with information from different tables. See documentation of the EPD: 
\url{http://www.europeanpollendatabase.net/data/downloads/image/pollen-database-manual-20071011.doc}).
}
\description{
Functions in this family retrieve information relative to 
the site  where the entity has been sampled. The main function 
(\code{\link[EPDr]{get_site}}) requires a valid connection to the 
database and the entity ID for the entity of interest. All other 
functions (starting with a dot [.]) use different arguments 
depending on the piece of information they retrieve.
}
\section{get_site}{

This function returns a \code{\link[EPDr]{site-class}} object with several 
information from the rest of the functions in the group for a 
particular entity.
}

\section{.get_siteloc}{

This functions returns a data.frame with information in in the SITELOC table for the 
specified entity. This corresponds with location data for the site 
where samples were taken.
}

\section{.get_sitedesc}{

This functions returns a data.frame with information in in the SITEDESC table for the 
specified entity. This corresponds with a description of the site 
where samples were taken.
}

\section{.get_siteinfo}{

This functions returns a data.frame with information in in the SITEINFO table for the 
specified entity. This corresponds with a summary data of all type 
of information in the database for that particular entity 
(chronological, palynological, etc).
}

\section{.get_poldiv1}{

This functions returns a data.frame with information in in the POLDIV1 table for the 
specified country (poldiv1_ is the country code). This corresponds 
with data of the country in which a site belong to.
}

\section{.get_poldiv2}{

This functions returns a data.frame with information in in the POLDIV2 table for the 
specified region (poldiv2_ is the region code). This corresponds 
with data of the region in which a site belong to.
}

\section{.get_poldiv3}{

This functions returns a data.frame with information in in the POLDIV3 table for the 
specified 3rd level region (poldiv3_ is the 3rd level region code). 
This corresponds with data of the 3rd level region in which a site
belong to.
}

\section{.get_igcptype}{

This functions returns a data.frame with information in in the IGCPTYPE table for the 
specified IGCP region. This corresponds with data of the IGCP 
region in which a site belong to.
}

\section{.get_infotype}{

This functions returns a data.frame with information in in the INFOTYPE table for the 
specified info code (icode). This corresponds with a longer 
description of the info type codes returned by \code{.get_siteinfo}.
}

\examples{
\dontrun{
epd.connection <- connect_to_epd(host = "localhost", database = "epd",
                              user = "epdr", password = "epdrpw")
site.400 <- get_site(400, epd.connection)
site.400
}
}
