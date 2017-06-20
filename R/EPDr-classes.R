
# GEOCHRON ----------------------------------------------------------------

#' Class for Geochronologies of an entity
#' 
#' Class "geochron" store all information relative to datation samples
#' and analysis in a specified entity of the European Pollen Database (EPD).
#' This object is created by \code{\link[EPDr]{get_geochron}}.
#'
#' @slot geochron data.frame. Common information on geochron samples.
#' @slot aar data.frame. AAR data for samples in geochron
#' @slot c14 data.frame. C14 data for samples in geochron
#' @slot esr data.frame. ESR data for samples in geochron
#' @slot ft data.frame. FT data for samples in geochron
#' @slot kar data.frame. KAR data for samples in geochron
#' @slot pb210 data.frame. PB210 data for samples in geochron
#' @slot si32 data.frame. SI32 data for samples in geochron
#' @slot tl data.frame. TL data for samples in geochron
#' @slot useries data.frame. USERIES data for samples in geochron
#' @slot publ data.frame PUBL data where the data were published
#'
#' @export
#' @import methods
#' 
geochron <- setClass("geochron",
                       slots = c(
                         geochron = "data.frame",
                         aar = "data.frame",
                         c14 = "data.frame",
                         esr = "data.frame",
                         ft = "data.frame",
                         kar = "data.frame",
                         pb210 = "data.frame",
                         si32 = "data.frame",
                         tl = "data.frame",
                         useries = "data.frame",
                         publ = "data.frame"
                       ),
                       prototype = list(
                         geochron = data.frame(
                           e_ = NA,
                           sample_ = NA,
                           agebp = NA,
                           errorlimits = NA,
                           labnumber = NA,
                           notes = NA)[-1, ],
                         aar = data.frame(
                           e_ = NA,
                           sample_ = NA,
                           agebp = NA,
                           errorlimits = NA,
                           taxondated = NA,
                           labnumber = NA,
                           notes = NA)[-1, ],
                         c14 = data.frame(
                           e_ = NA,
                           sample_ = NA,
                           agebp = NA,
                           agesdup = NA,
                           agesdlo = NA,
                           grthanage = NA,
                           basis = NA,
                           enriched = NA,
                           labnumber = NA,
                           deltac13 = NA,
                           notes = NA)[-1, ],
                         esr = data.frame(
                           e_ = NA,
                           sample_ = NA,
                           agebp = NA,
                           errorlimits = NA,
                           labnumber = NA,
                           notes = NA)[-1, ],
                         ft = data.frame(
                           e_ = NA,
                           sample_ = NA,
                           agebp = NA,
                           errorlimits = NA,
                           labnumber = NA,
                           notes = NA)[-1, ],
                         kar = data.frame(
                           e_ = NA,
                           sample_ = NA,
                           agebp = NA,
                           errorlimits = NA,
                           labnumber = NA,
                           notes = NA)[-1, ],
                         pb210 = data.frame(
                           e_ = NA,
                           sample_ = NA,
                           agead = NA,
                           agesdup = NA,
                           agesdlo = NA,
                           grthanage = NA,
                           notes = NA)[-1, ],
                         si32 = data.frame(
                           e_ = NA,
                           sample_ = NA,
                           agebp = NA,
                           agesdup = NA,
                           agesdlo = NA,
                           grthanage = NA,
                           labnumber = NA,
                           notes = NA)[-1, ],
                         tl = data.frame(
                           e_ = NA,
                           sample_ = NA,
                           agebp = NA,
                           errorlimits = NA,
                           grainsize = NA,
                           labnumber = NA,
                           notes = NA)[-1, ],
                         useries = data.frame(
                           e_ = NA,
                           sample_ = NA,
                           agebp = NA,
                           errorlimits = NA,
                           labnumber = NA,
                           notes = NA)[-1, ],
                         publ = data.frame(
                           publ_ = NA,
                           acc_ = NA,
                           yearofpubl = NA,
                           citation = NA)[-1, ]
                         )
)


# SITE --------------------------------------------------------------------

#' Class for Site of an entity
#' 
#' Class "site" store all information relative to site where samples were
#' taken and to which a specified entity of the European Pollen Database
#' (EPD) belong to. This object is created by \code{\link[EPDr]{get_site}}.
#'
#' @slot siteloc data.frame. Information on the location (e.g. geographical
#' coordinates) for the site.
#' @slot sitedesc data.frame. Description of the site in terms of vegetation
#' and orography.
#' @slot siteinfo data.frame. This table summarize the information available
#' for that site (e.g. C14, pollen, insects, etc.)
#' @slot country data.frame. Country to which the site belong to.
#' @slot region data.frame. Regions to which the site belong to.
#' @slot region3rd data.frame. 3rd level regions to which the site belong to.
#' @slot igcptype data.frame. IGCP regions to which the site belong to.
#' @slot infotype data.frame. This table provide a longer description of the
#' information summarized in \code{@siteinfo}
#' @slot publ data.frame PUBL data where the data were published
#'
#' @export
#' @import methods
#' 
site <- setClass("site",
                     slots = c(
                       siteloc = "data.frame",
                       sitedesc = "data.frame",
                       siteinfo = "data.frame",
                       country = "data.frame",
                       region = "data.frame",
                       region3rd = "data.frame",
                       igcptype = "data.frame",
                       infotype = "data.frame",
                       publ = "data.frame"
                     ),
                     prototype = list(
                       siteloc = data.frame(
                         site_ = NA,
                         sitename = NA,
                         sitecode = NA,
                         siteexists = NA,
                         poldiv1 = NA,
                         poldiv2 = NA,
                         poldiv3 = NA,
                         latdeg = NA,
                         latmin = NA,
                         latsec = NA,
                         latns = NA,
                         latdd = NA,
                         latdms = NA,
                         londeg = NA,
                         lonmin = NA,
                         lonsec = NA,
                         lonew = NA,
                         londd = NA,
                         londms = NA,
                         elevation = NA,
                         areaofsite = NA)[-1, ],
                       sitedesc = data.frame(
                         site_ = NA,
                         sitedescript = NA,
                         physiography = NA,
                         surroundveg = NA,
                         vegformation = NA,
                         igcptype = NA)[-1, ],
                       siteinfo = data.frame(
                         site_ = NA,
                         icode = NA,
                         publ_ = NA)[-1, ],
                       country = data.frame(
                         poldiv1 = NA,
                         name = NA)[-1, ],
                       region = data.frame(
                         poldiv1 = NA,
                         poldiv2 = NA,
                         postcode = NA,
                         name = NA)[-1, ],
                       region3rd = data.frame(
                         poldiv1 = NA,
                         poldiv2 = NA,
                         poldiv3 = NA,
                         name = NA)[-1, ],
                       igcptype = data.frame(
                         igcptype = NA,
                         regionname = NA)[-1, ],
                       publ = data.frame(
                         publ_ = NA,
                         acc_ = NA,
                         yearofpubl = NA,
                         citation = NA)[-1, ]
                     )
)


# CHRON -------------------------------------------------------------------

#' Class for chronology information of an entity
#' 
#' Class "chron" store all information relative to chronologies for a
#' specified entity of the European Pollen Database (EPD). This object is
#' created by \code{\link[EPDr]{get_chron}}.
#'
#' @slot chron data.frame. Information on the different chronologies for
#' the site.
#' @slot agebound data.frame. Description of the temporal bounds or limits
#' for each chronology in  \code{@chron}.
#' @slot agebasis data.frame. This table store the information used to fit
#' the age-depth model in each chronology and to calculate ages for the
#' biological samples.
#' @slot rational data.frame. This table provide a longer explanation for
#' the rational used in each data in \code{@agebasis}.
#' @slot alsegs data.frame. Information on the annual lamination segments
#' found in the entity.
#' @slot panldpt data.frame. Detailed information (e.g., depth, thikness,
#' etc) for each annual lamination in \code{@alsegs}.
#' @slot synevent data.frame. This is a transition table that connect each
#' entity with geological events in \code{@event}.
#' @slot event data.frame. Details on the events that are present in that
#' particular entity.
#' @slot publ data.frame PUBL data where the event data were published.
#'
#' @export
#' @import methods
#' 
chron <- setClass("chron",
                  slots = c(
                    chron = "data.frame",
                    agebound = "data.frame",
                    agebasis = "data.frame",
                    rational = "data.frame",
                    alsegs = "data.frame",
                    panldpt = "data.frame",
                    synevent = "data.frame",
                    event = "data.frame",
                    publ = "data.frame"
                  ),
                  prototype = list(
                    chron = data.frame(
                      e_ = NA,
                      chron_ = NA,
                      defaultchron = NA,
                      name = NA,
                      preparedby = NA,
                      dateprepared = NA,
                      model = NA,
                      notes = NA)[-1, ],
                    agebound = data.frame(
                      e_ = NA,
                      chron_ = NA,
                      top = NA,
                      bottom = NA)[-1, ],
                    agebasis = data.frame(
                      e_ = NA,
                      chron_ = NA,
                      sample_ = NA,
                      depthcm = NA,
                      thickness = NA,
                      age = NA,
                      ageup = NA,
                      agelo = NA,
                      rcode = NA)[-1, ],
                    rational = data.frame(
                      rcode = NA,
                      rationale = NA)[-1, ],
                    alsegs = data.frame(
                      e_ = NA,
                      seg_ = NA,
                      depthtopcm = NA,
                      depthbotcm = NA)[-1, ],
                    panldpt = data.frame(
                      e_ = NA,
                      seg_ = NA,
                      sample_ = NA,
                      depthcm = NA,
                      thickness = NA,
                      counttop = NA,
                      countbot = NA)[-1, ],
                    synevent = data.frame(
                      e_ = NA,
                      event_ = NA,
                      depthcm = NA,
                      thickness = NA)[-1, ],
                    event = data.frame(
                      event_ = NA,
                      event = NA,
                      name = NA,
                      agebp = NA,
                      ageuncertup = NA,
                      ageuncertlo = NA,
                      publ = NA)[-1, ],
                    publ = data.frame(
                      publ_ = NA,
                      acc_ = NA,
                      yearofpubl = NA,
                      citation = NA)[-1, ]
                  )
)


# ENTITY ------------------------------------------------------------------

#' Class for descriptions of an entity
#' 
#' Class "entity" store all summary and description information relative 
#' to a specified entity of the European Pollen Database (EPD). This object
#' is created by \code{\link[EPDr]{get_ent}}.
#'
#' @slot entity data.frame. Information describing some characteristics of
#' the entity.
#' @slot pentity data.frame. Description contact person and restriction on
#' use of the data from this entity.
#' @slot contact data.frame. Details on the contact person.
#' @slot coredrive data.frame. Description of the coredrive used to drill
#' the entity.
#' @slot descr data.frame. This table provide a longer explanation for the
#' descriptor specified in \code{@entity}.
#' @slot lithology data.frame. Information on the lithology found in the
#' entity.
#' @slot loi data.frame. Detailed information for loss-on-ignition data for
#' that particular entity.
#' @slot section data.frame. Details on the sections used to extract the
#' entity if is a core.
#' @slot publent data.frame. Linking information between entity id and
#' publications \code{@publ}.
#' @slot publ data.frame PUBL data where the event data were published.
#'
#' @export
#' @import methods
#' 
entity <- setClass("entity",
                  slots = c(
                    entity = "data.frame",
                    pentity = "data.frame",
                    contact = "data.frame",
                    coredrive = "data.frame",
                    descr = "data.frame",
                    lithology = "data.frame",
                    loi = "data.frame",
                    section = "data.frame",
                    publent = "data.frame",
                    publ = "data.frame"
                  ),
                  prototype = list(
                    entity = data.frame(
                      e_ = NA,
                      site_ = NA,
                      sigle = NA,
                      name = NA,
                      iscore = NA,
                      issect = NA,
                      issamp = NA,
                      descriptor = NA,
                      hasanlam = NA,
                      entloc = NA,
                      localveg = NA,
                      coll_ = NA,
                      sampdate = NA,
                      depthatloc = NA,
                      icethickcm = NA,
                      sampdevice = NA,
                      corediamcm = NA,
                      c14depthadj = NA,
                      notes = NA)[-1, ],
                    pentity = data.frame(
                      e_ = NA,
                      contact_ = NA,
                      datasource = NA,
                      dataform = NA,
                      usestatus = NA,
                      datacoop = NA)[-1, ],
                    contact = data.frame(
                      worker_ = NA,
                      workeris_ = NA,
                      status = NA,
                      lastname = NA,
                      initials = NA,
                      firstname = NA,
                      suffix = NA,
                      title = NA,
                      country = NA,
                      phone = NA,
                      fax = NA,
                      emailaddr = NA,
                      address = NA)[-1, ],
                    coredrive = data.frame(
                      e_ = NA,
                      drive_ = NA,
                      drivelabel = NA,
                      drivetopcm = NA,
                      drivebotcm = NA,
                      inftopcm = NA,
                      infbotcm = NA,
                      recoverycm = NA)[-1, ],
                    descr = data.frame(
                      descriptor = NA,
                      higherdescr = NA,
                      description = NA)[-1, ],
                    lithology = data.frame(
                      e_ = NA,
                      lith_ = NA,
                      descript = NA,
                      depthtopcm = NA,
                      depthbotcm = NA,
                      loboundary = NA)[-1, ],
                    loi = data.frame(
                      e_ = NA,
                      sample_ = NA,
                      depthcm = NA,
                      thickness = NA,
                      templo = NA,
                      loilo = NA,
                      temphi = NA,
                      loihi = NA,
                      bulkdens = NA)[-1, ],
                    section = data.frame(
                      e_ = NA,
                      section_ = NA,
                      sectionlabel = NA,
                      sectiontopcm = NA,
                      sectionbotcm = NA)[-1, ],
                    publent = data.frame(
                      publ_ = NA,
                      e_ = NA)[-1, ],
                    publ = data.frame(
                      publ_ = NA,
                      acc_ = NA,
                      yearofpubl = NA,
                      citation = NA)[-1, ]
                  )
)


# SAMPLES -----------------------------------------------------------------

#' Class for biological samples of an entity
#' 
#' Class "samples" store all information about biological samples relative
#' to a specified entity of the European Pollen Database (EPD). This object
#' is created by \code{\link[EPDr]{get_samples}}.
#'
#' @slot psamples data.frame. Information describing the samples: their 
#' depth (cm), thickness (cm), analyst, date of analysis, and notes.
#' @slot analysts data.frame. Information on the worker that analyzed the 
#' data.
#' @slot pagedpt data.frame. Estimated ages for each sample according to 
#' each available chronology in the database.
#' @slot pcounts data.frame. Raw counts for each sample and each observed 
#' variable (taxon).
#' @slot pvars data.frame. Data on the identification of each variable 
#' (taxon) in \code{@pcounts}.
#' @slot syntype data.frame. Synonym type for those variables that are 
#' marked as synonyms.
#' @slot pvtrans data.frame. Alternative names for each variable. Not 
#' used in the EPD.
#' @slot pgroup data.frame. Linking table to identify each taxon (or 
#' variable) in a specific group.
#' @slot groups data.frame. Details and longer description of the 
#' groups identified in \code{@pgroup}.
#'
#' @export
#' @import methods
#' 
samples <- setClass("samples",
                   slots = c(
                     psamples = "data.frame",
                     analysts = "data.frame",
                     pagedpt = "data.frame",
                     pcounts = "data.frame",
                     pvars = "data.frame",
                     syntype = "data.frame",
                     pvtrans = "data.frame",
                     pgroup = "data.frame",
                     groups = "data.frame"
                   ),
                   prototype = list(
                     psamples = data.frame(
                       e_ = NA,
                       sample_ = NA,
                       depthcm = NA,
                       thickness = NA,
                       analyst_ = NA,
                       analydate = NA,
                       notes = NA)[-1, ],
                     analysts = data.frame(
                       worker_ = NA,
                       workeris_ = NA,
                       status = NA,
                       lastname = NA,
                       initials = NA,
                       firstname = NA,
                       suffix = NA,
                       title = NA,
                       country = NA,
                       phone = NA,
                       fax = NA,
                       emailaddr = NA,
                       address = NA)[-1, ],
                     pagedpt = data.frame(
                       e_ = NA,
                       chron_ = NA,
                       sample_ = NA,
                       agebp = NA,
                       ageup = NA,
                       agelo = NA,
                       deptime = NA)[-1, ],
                     pcounts = data.frame(
                       e_ = NA,
                       sample_ = NA,
                       var_ = NA,
                       count = NA)[-1, ],
                     pvars = data.frame(
                       var = NA,
                       accvar = NA,
                       syntype = NA,
                       varcode = NA,
                       varname = NA,
                       hvar = NA,
                       mhvar = NA,
                       auth_ = NA,
                       notes = NA)[-1, ],
                     syntype = data.frame(
                       syntype = NA,
                       description = NA)[-1, ],
                     pvtrans = data.frame(
                       var_ = NA,
                       translatesto = NA)[-1, ],
                     pgroup = data.frame(
                       set_ = NA,
                       var_ = NA,
                       groupid = NA)[-1, ],
                     groups = data.frame(
                       groupid = NA,
                       groupcode = NA,
                       groupname = NA)[-1, ]
                   )
)


# EPD.ENTITY --------------------------------------------------------------

#' Class for epd.entity objects
#' 
#' This object stores all the information available for a particular 
#' entity in the European Pollen Database (EPD). It has some additional 
#' information (e.g., in postbombzone or isingiesecke \code{@@slots}). 
#' Other \code{@@slots} are store handy information for simplicity (e.g.,
#' defaultchron or numberofchron \code{@@slots}). The rest of 
#' \code{@@slots} are the raw information as extracted from the EPD, and
#' stored in the \code{entity, site, geochron, chron, and samples classes}
#'
#' All data in this object can be changed, but the intention
#' of the object is to provide a copy of all the tables in the EPD
#' with information regarding a particular entity. When an entity
#' has not data in any particular table, the table is created empty.
#' 
#' @slot e_ numeric. Number indicating the entity number (e_) of the
#' stored entity.
#' @slot postbombzone factor. It indicate the code of the postbombzone of
#' the world where the entity is located. This is not used yet, but
#' might be handy for further implementations for datations with CLAM
#' or BACON.
#' @slot numberofchron numeric. The number of chronologies available
#' for the data in that entity.
#' @slot isingiesecke logical. Indicating whether this entity was
#' revised in Giesecke et al. (2013) and, hence, additional datations
#' are available.
#' @slot defaultchron numeric. The number of the default chronology
#' according to the EPD.
#' @slot entity entity. Entity object for the entity.
#' @slot site site. Site object for the entity.
#' @slot geochron geochron. Geochron object for the entity.
#' @slot chron chron. Chron object for the entity.
#' @slot samples samples. Samples object for the entity.
#' 
#' @references Giesecke, T., Davis, B., Brewer, S., Finsinger, W., 
#' Wolters, S., Blaaw, M., de Beaulieu, J.L., Binney, H., Fyfe, R.M.,
#' Gaillard, M.J., Gil-Romera, G., van der Knaap, W.O. Kunes, P.,
#' Kuhl, N., van Leeuwen, J.F.N, Leydet, M., Lotter, A.F., Ortu, E.,
#' Semmler, M., and Bradshaw, R.H.W (2013). Towards mapping the late
#' Quaternary vegetation change of Europe. Vegetation History and
#' Archaeobotany, 23, 75-86.
#'
#' @export
#' @import methods
#' 
epd.entity <- setClass("epd.entity", slots = c(
  e_ = "numeric",
  postbombzone = "factor",
  numberofchron = "numeric",
  isingiesecke = "logical",
  defaultchron = "numeric",
  entity = "entity",
  site = "site",
  geochron = "geochron",
  chron = "chron",
  samples = "samples"
),
prototype = list(
  e_ = numeric(0),
  postbombzone = factor(levels = c("NH1", "NH2", "NH3", "SH12", "SH3")),
  numberofchron = numeric(0),
  giesecke = logical(0),
  defaultchron = numeric(0),
  entity = new("entity"),
  site = new("site"),
  geochron = new("geochron"),
  chron = new("chron"),
  samples = new("samples")
)
)


# SAMPLESDF --------------------------------------------------------------

#' Class for samplesdf objects
#' 
#' This object stores information regarding the biological samples in an
#' entity but in vector format rather than the original format in the 
#' EPD database. Note, however, that data here comes from the table
#' \code{psamples} in the database or an \code{\link[EPDr]{epd.entity}} 
#' object. Contrary to data in \code{\link[EPDr]{epd.entity}} objects, 
#' this are intended to be manipulated and modified if necessary.
#'
#' @slot sample_ numeric. Identify numbers for each sample.
#' @slot samplelabel character. Name of each sample. Most time
#' they are a character version of the sample_ @@slot, but some others
#' are composites created by other functions in the 
#' \code{EPDr} package.
#'
#' @export
#' @import methods
#' 
samplesdf <- setClass("samplesdf",
                   slots = c(
                     sample_ = "numeric",
                     samplelabel = "character"
                   ),
                   prototype = list(
                     sample_ = numeric(0),
                     samplelabel = character(0)
                   )
)


# AGESDF --------------------------------------------------------------

#' Class for agesdf objects
#' 
#' This object stores information regarding the ages of biological 
#' samples in an entity but in vector or table format rather than 
#' the original format in the EPD database. Note, however, that data 
#' here comes from the tables \code{pagedpt} and \code{psamples} from
#' the database or an \code{\link[EPDr]{epd.entity}} object. 
#' Contrary to data in \code{\link[EPDr]{epd.entity}} objects, 
#' this are intended to be manipulated and modified if necessary.
#'
#' @slot depthcm numeric. Depth of the samples in cm.
#' @slot depthages data.frame. Estimated ages for each sample (rows) 
#' according to each chronology (columns), including chronologies
#' from Giesecke et al. (2013) if available for that entity.
#' @slot dataquality data.frame. This dataframe include a quality index
#' for each sample. By default it is empty, but it can be calculated
#' by means of specific functions like \code{\link[EPDr]{blois_quality}}.
#'
#' @export
#' @import methods
#' 
agesdf <- setClass("agesdf",
                   slots = c(
                     depthcm = "numeric",
                     depthages = "data.frame",
                     dataquality = "data.frame"
                   ),
                   prototype = list(
                     depthcm = numeric(0),
                     depthages = data.frame(),
                     dataquality = data.frame()
                   )
)


# COMMDF --------------------------------------------------------------

#' Class for commdf objects
#' 
#' This object stores information regarding the particles counts
#' in each biological sample of an entity but in vector or table 
#' format rather than the original format in the EPD database. 
#' Note, however, that data here comes from the table 
#' \code{samples} from the database or an 
#' \code{\link[EPDr]{epd.entity}} object. Contrary to data 
#' in \code{\link[EPDr]{epd.entity}} objects, this are 
#' intended to be manipulated and modified if necessary.
#'
#' @slot taxanames character. Names of the taxa included in the object.
#' @slot taxa_ numeric. Identify numbers of the taxa.
#' @slot taxaaccepted numeric. Identify numbers for the accepted taxa. 
#' This is useful to unify taxonomy across entities from different authors,
#' that may have used slightly different taxa names.
#' @slot taxamhvar numeric. Identify numbers for the higher taxa levels
#' of each taxa. This is the only taxonomical hierarchy that is available
#' in the EPD. The approach is useful, but also very limited, since there 
#' is no intuitive way to homogenize the data at certain taxonomical level
#' (e.g., at the genus level).
#' @slot taxagroupid character. Identify code (four characters code) 
#' indicating the group of taxa (e.g., TRSH for trees and shrubs or
#' HERB for herbs). Check \code{\link[EPDr]{list_taxagroups}} 
#' for the whole list and further details.
#' @slot counts data.frame. Particles counts in a matrix format (sample 
#' x taxa). This format is more intuitive and handy for biologist 
#' (ecologist or palynologist) than the original format.
#'
#' @export
#' @import methods
#' 
commdf <- setClass("commdf",
                   slots = c(
                     taxanames = "character",
                     taxa_ = "numeric",
                     taxaaccepted = "numeric",
                     taxamhvar = "numeric",
                     taxagroupid = "character",
                     counts = "data.frame"
                   ),
                   prototype = list(
                     taxanames = character(0),
                     taxa_ = numeric(0),
                     taxaaccepted = numeric(0),
                     taxamhvar = numeric(0),
                     taxagroupid = character(0),
                     counts = data.frame()
                   )
)


# NOPODF --------------------------------------------------------------

#' Class for nopodf objects
#' 
#' This object stores information regarding the particles counts
#' for no biological particles of an entity but in vector or table 
#' format rather than the original format in the EPD database. This
#' is similar, to \code{\link[EPDr]{commdf}} objects but only, 
#' for not biological particles. This may include total pollen counts, 
#' added particles to calculate pollen concentrations, sedimentation 
#' rates, etc. Check \code{list_taxa(epd.connection, "NOPO")} for the
#' whole list. Note, however, that data here comes from the table 
#' \code{samples} from the database or an 
#' \code{\link[EPDr]{epd.entity}} object. Contrary to data 
#' in \code{\link[EPDr]{epd.entity}} objects, this are 
#' intended to be manipulated and modified if necessary.
#'
#' @slot varnames character. Names of the taxa included in the object.
#' @slot var_ numeric. Identify numbers of the taxa.
#' @slot varaccepted numeric. Identify numbers for the accepted taxa. 
#' This is useful to unify taxonomy across entities from different authors,
#' that may have used slightly different taxa names.
#' @slot varmhvar numeric. Identify numbers for the higher taxa levels
#' of each taxa. This is the only taxonomical hierarchy that is available
#' in the EPD. The approach is useful, but also very limited, since there 
#' is no intuitive way to homogenize the data at certain taxonomical level
#' (e.g., at the genus level).
#' @slot vargroupid character. Identify code (four characters code) 
#' indicating the group of taxa (e.g., TRSH for trees and shrubs or
#' HERB for herbs). Check \code{\link[EPDr]{list_taxagroups}} 
#' for the whole list and further details.
#' @slot counts data.frame. Particles counts in a matrix format (sample 
#' x taxa). This format is more intuitive and handy for biologist 
#' (ecologist or palynologist) than the original format.
#'
#' @export
#' @import methods
#' 
nopodf <- setClass("nopodf",
                   slots = c(
                     varnames = "character",
                     var_ = "numeric",
                     varaccepted = "numeric",
                     varmhvar = "numeric",
                     vargroupid = "character",
                     counts = "data.frame"
                   ),
                   prototype = list(
                     varnames = character(0),
                     var_ = numeric(0),
                     varaccepted = numeric(0),
                     varmhvar = numeric(0),
                     vargroupid = character(0),
                     counts = data.frame()
                   )
)


# EPD.ENTITY.DF --------------------------------------------------------------

#' Class for epd.entity.df objects
#' 
#' This is an expansion of \code{\link[EPDr]{epd.entity}} class,
#' in which certain data have been reformated and included in the object. Hence,
#' the first @@slots are the same as in a
#' \code{\link[EPDr]{epd.entity}}. Then, additional data
#' include four @@slots for information on the processing and
#' transformation of the data, and for @@slots for reformated data.
#'
#' @slot e_ numeric. Number indicating the entity number (e_) of the
#' stored entity. Same as in a \code{\link[EPDr]{epd.entity}} 
#' object.
#' @slot postbombzone factor. It indicate the code of the postbombzone of
#' the world where the entity is located. This is not used yet, but
#' might be handy for further implementations for datations with CLAM
#' or BACON. Same as in a \code{\link[EPDr]{epd.entity}} 
#' object.
#' @slot numberofchron numeric. The number of chronologies available
#' for the data in that entity.Same as in a 
#' \code{\link[EPDr]{epd.entity}} object.
#' @slot isingiesecke logical. Indicating whether this entity was
#' revised in Giesecke et al. (2013) and, hence, additional datations
#' are available. Same as in a \code{\link[EPDr]{epd.entity}} 
#' object.
#' @slot defaultchron numeric. The number of the default chronology
#' according to the EPD. Same as in a \code{\link[EPDr]{epd.entity}} 
#' object.
#' @slot entity entity. Entity object for the entity. Same as in 
#' a \code{\link[EPDr]{epd.entity}} object.
#' @slot site site. Site object for the entity. Same as in a 
#' \code{\link[EPDr]{epd.entity}} object.
#' @slot geochron geochron. Geochron object for the entity. Same as in 
#' a \code{\link[EPDr]{epd.entity}} object.
#' @slot chron chron. Chron object for the entity. Same as in 
#' a \code{\link[EPDr]{epd.entity}} object.
#' @slot samples samples. Samples object for the entity. Same as in 
#' a \code{\link[EPDr]{epd.entity}} object.
#' @slot countstype factor. Indicating if the counts in @@slot commdf
#' are in raw counts or percentages.
#' @slot countsprocessing factor. Indicating whether the 
#' counts are those counted (or percentaged) for the samples, or
#' have been interpolated or intervaled for particular time periods
#' (or intervals). See \code{\link[EPDr]{interpolate_counts}} or
#' \code{\link[EPDr]{intervals_counts}}.
#' @slot taxatype factor. Indicating whether the taxa are the
#' same as in the original database, or they have been changed to
#' accepted taxa or higher taxonomical level.
#' @slot taxaprocessing factor. Indicating whether the taxa in @@slot 
#' commdf are those originally present in the database, or
#' they have been expanded or filtered.
#' @slot samplesdf samplesdf. Samples in vector format.
#' @slot agesdf agesdf. Ages in vector and table format.
#' @slot commdf commdf. Counts of biological particles in vector and
#' table format.
#' @slot nopodf nopodf. Counts of no-biological particles in vector
#' and table format.
#'
#' @export
#' @import methods
#' 
epd.entity.df <- setClass("epd.entity.df", contains = "epd.entity", slots = c(
  e_ = "numeric",
  postbombzone = "factor",
  numberofchron = "numeric",
  isingiesecke = "logical",
  defaultchron = "numeric",
  entity = "entity",
  site = "site",
  geochron = "geochron",
  chron = "chron",
  samples = "samples",
  countstype = "factor",
  countsprocessing = "factor",
  taxatype = "factor",
  taxaprocessing = "factor",
  samplesdf = "samplesdf",
  agesdf = "agesdf",
  commdf = "commdf",
  nopodf = "nopodf"
  ),
prototype = list(
  e_ = numeric(0),
  postbombzone = factor(levels = c("NH1", "NH2", "NH3", "SH12", "SH3")),
  numberofchron = numeric(0),
  giesecke = logical(0),
  defaultchron = numeric(0),
  entity = new("entity"),
  site = new("site"),
  geochron = new("geochron"),
  chron = new("chron"),
  samples = new("samples"),
  countstype = factor(levels = c("Counts", "Percentages")),
  countsprocessing = factor(levels = c("Raw", "Interpolated", "Ranged means")),
  taxatype = factor(levels = c("Default", "Accepted", "Higher")),
  taxaprocessing = factor(levels = c("Original", "Expanded")),
  samplesdf = new("samplesdf"),
  agesdf = new("agesdf"),
  commdf = new("commdf"),
  nopodf = new("nopodf")
)
)
