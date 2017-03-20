#' Class for Chronologies of an entity
#' 
#' Class "chronology" store in an organized and systematic way information about 
#' the chronologies for an specified entity in the European Pollen Database (EPD). This
#' object is created by \code{\link[EPDr:getChronology]{getChronology}}.
#'
#' It has different elements all of which correspond to a unique entity in the database.
#'
#' @slot e_ numeric. The entity number (e_) as in the EPD.
#' @slot restriction data.frame. Restriction of use information for that particular entity
#' in the database. It is important to know if we can freely use the data or should ask
#' for authorization
#' @slot entity data.frame. Details of the entity.
#' @slot site data.frame. Details of the site to which the entity belong to. Note that 
#' several entities can belong to the same site.
#' @slot number_of_chronologies numeric. The number of chronologies that are stored in
#' the database for this particular entity.
#' @slot default_chronology numeric. Which of the available chronologies is tagged as 
#' the default in the database.
#' @slot chron data.frame. Details about the chronologies stored in the database, like 
#' the author, full name, etc.
#' @slot agebasis data.frame. Agebasis used in the chronologies to calibrate radiocarbon 
#' dates.
#'
#' @export
#' @import methods
#' 
chronology <- setClass("chronology",
                                slots=c(
                                    e_="numeric",
                                    restriction="data.frame",
                                    entity="data.frame",
                                    site="data.frame",
                                    number_of_chronologies="numeric",
                                    default_chronology="numeric",
                                    chron="data.frame",
                                    agebasis="data.frame"
                                ),
                                prototype=list(
                                    e_=numeric(0),
                                    restriction=data.frame(
                                        e_=NA,
                                        contact_=NA,
                                        datasource=NA,
                                        dataform=NA,
                                        usestatus=NA,
                                        datacoop=NA)[-1,],
                                    entity=data.frame(
                                        e_=NA,
                                        site_=NA,
                                        sigle=NA,
                                        name=NA,
                                        iscore=NA,
                                        issect=NA,
                                        isssamp=NA,
                                        descriptor=NA,
                                        hasanlam=NA,
                                        entloc=NA,
                                        localveg=NA,
                                        coll_=NA,
                                        sampdate=NA,
                                        depthatloc=NA,
                                        icethickcm=NA,
                                        sampdevice=NA,
                                        corediamcm=NA,
                                        c14depthadj=NA,
                                        notes=NA)[-1,],
                                    site=data.frame(
                                        site_=NA,
                                        sitename=NA,
                                        sitecode=NA,
                                        siteexists=NA,
                                        poldiv1=NA,
                                        poldiv2=NA,
                                        poldiv3=NA,
                                        latdeg=NA,
                                        latmin=NA,
                                        latsec=NA,
                                        latns=NA,
                                        latdd=NA,
                                        latdms=NA,
                                        londeg=NA,
                                        lonmin=NA,
                                        lonsec=NA,
                                        lonew=NA,
                                        londd=NA,
                                        londms=NA,
                                        elevation=NA,
                                        areaofsite=NA)[-1,],
                                    number_of_chronologies=numeric(0),
                                    default_chronology=numeric(0),
                                    chron=data.frame(
                                        e_=NA,
                                        chron_=NA,
                                        defaultchron=NA,
                                        name=NA,
                                        preparedby=NA,
                                        dataprepared=NA,
                                        model=NA,
                                        notes=NA)[-1,],
                                    agebasis=data.frame(
                                        e_=NA,
                                        chron_=NA,
                                        sample_=NA,
                                        depthcm=NA,
                                        thickness=NA,
                                        age=NA,
                                        ageup=NA,
                                        agelo=NA,
                                        rcode=NA,
                                        nrow = 0)
                                )
)


#' Class for Datation of an entity
#' 
#' Class "datation" store all information about datation of an specified entity in the 
#' European Pollen Database (EPD). These objects are created by \code{\link[EPDr:getDatation]{getDatation}}.
#' 
#' Now, it include only C14 data and events, it should include in the future other
#' sort of data that are included in the EPD.
#'
#' It has different elements all of which correspond to a unique entity in the database.
#'
#' @slot e_ numeric. The entity number (e_) as in the EPD.
#' @slot restriction data.frame. Restriction of use information for that particular entity
#' in the database. It is important to know if we can freely use the data or should ask
#' for authorization
#' @slot entity data.frame. Details of the entity.
#' @slot site data.frame. Details of the site to which the entity belong to. Note that 
#' several entities can belong to the same site.
#' @slot postbomb_zone factor. Indicate the postbomb zone in which the entity (site)
#' was sampled. This information is useful when calibrating radiocarbon dates with
#' \code{clam} or \code{bacon}.
#' @slot chronology chronology. Object of class \code{\link{chronology}} for the entity.
#' @slot c14 data.frame. Details on radiocarbon (C14) data and analysis.
#' @slot events data.frame. Details about events (e.g. tephra) that could appear in
#' the entity.
#' @slot depths data.frame. Details about the samples (including depths) at which samples
#' were taken in the entity for palynological (pollen, spores, etc) analysis.
#'
#' @export
#' @import methods
#'
datation <- setClass("datation",
                              slots=c(
                                  e_="numeric",
                                  restriction="data.frame",
                                  entity="data.frame",
                                  site="data.frame",
                                  postbomb_zone="factor",
                                  chronology="chronology",
                                  c14="data.frame",
                                  events="data.frame",
                                  depths="data.frame"
                              ),
                              prototype=list(
                                  e_=numeric(0),
                                  restriction=data.frame(
                                      e_=NA,
                                      contact_=NA,
                                      datasource=NA,
                                      dataform=NA,
                                      usestatus=NA,
                                      datacoop=NA)[-1,],
                                  entity=data.frame(
                                      e_=NA,
                                      site_=NA,
                                      sigle=NA,
                                      name=NA,
                                      iscore=NA,
                                      issect=NA,
                                      isssamp=NA,
                                      descriptor=NA,
                                      hasanlam=NA,
                                      entloc=NA,
                                      localveg=NA,
                                      coll_=NA,
                                      sampdate=NA,
                                      depthatloc=NA,
                                      icethickcm=NA,
                                      sampdevice=NA,
                                      corediamcm=NA,
                                      c14depthadj=NA,
                                      notes=NA)[-1,],
                                  site=data.frame(
                                      site_=NA,
                                      sitename=NA,
                                      sitecode=NA,
                                      siteexists=NA,
                                      poldiv1=NA,
                                      poldiv2=NA,
                                      poldiv3=NA,
                                      latdeg=NA,
                                      latmin=NA,
                                      latsec=NA,
                                      latns=NA,
                                      latdd=NA,
                                      latdms=NA,
                                      londeg=NA,
                                      lonmin=NA,
                                      lonsec=NA,
                                      lonew=NA,
                                      londd=NA,
                                      londms=NA,
                                      elevation=NA,
                                      areaofsite=NA)[-1,],
                                  postbomb_zone=factor(levels=c("NH1", "NH2", "NH3", "SH12", "SH3")),
                                  chronology=chronology(),
                                  c14=data.frame(
                                      e_=NA,
                                      sample_=NA,
                                      agebp=NA,
                                      agesdup=NA,
                                      agesdlo=NA,
                                      grthanage=NA,
                                      basis=NA,
                                      enriched=NA,
                                      labnumber=NA,
                                      deltac13=NA,
                                      notes=NA,
                                      method=NA,
                                      depthcm=NA,
                                      thickness=NA,
                                      materialdated=NA,
                                      publ_=NA)[-1,],
                                  events=data.frame(
                                      event_=NA,
                                      e_=NA,
                                      depthcm=NA,
                                      thickness=NA,
                                      event=NA,
                                      name=NA,
                                      agebp=NA,
                                      ageuncertup=NA,
                                      ageuncertlo=NA,
                                      publ_=NA)[-1,],
                                  depths=data.frame(
                                      e_=NA,
                                      sample_=NA,
                                      depthcm=NA,
                                      thickness=NA,
                                      analyst=NA,
                                      analydate=NA,
                                      notes=NA,
                                      lab_ID=NA,
                                      nrow = 0)
                              )
)

#' Class for Counts of an entity
#' 
#' Class "counts" store in an organized and systematic way information about 
#' the particles counts in an specified entity in the European Pollen Database (EPD). This
#' object is created by \code{\link[EPDr:getCounts]{getCounts}}.
#'
#' It has different elements all of which correspond to a unique entity 
#'
#' @slot e_ numeric. The entity number (e_) as in the EPD.
#' @slot restriction data.frame. Restriction of use information for that particular entity
#' in the database. It is important to know if we can freely use the data or should ask
#' for authorization
#' @slot counts_type factor. Indicating the type of counts stored in slot \code{@counts}.
#' They can be one of two values "Counts" or "Percentages". "Counts" indicates that values
#' in \code{@counts} are raw counts as in the EPD. "Percentages" indicates that values in 
#' \code{@counts} are percentages calculated, for instance, with \code{\link[EPDr:trans2Percentages]{trans2Percentages}}. 
#' @slot counts_processing factor. Indicating whether the data in slot \code{@counts} has
#' been processed. It can be one of three values: "Samples", "Interpolated" or "Ranged 
#' means". "Samples" indicate that values in \code{@counts} correspond with the counts
#' for the palynological samples. "Interpolated" indicates that values in \code{@counts}
#' correspond to interpolated data at specific depth/ages among the palynological samples, 
#' using \code{\link[EPDr:interpolateCounts]{interpolateCounts}}. "Ranged means"
#' indicates that values in \code{@counts} represent mean values among all palynological samples for specified age/depth ranges calculated using \code{\link[EPDr:intervalsCounts]{intervalsCounts}}
#' @slot taxa_type factor. Indicating the taxa used to calculate values in slot
#' \code{@counts}. The EPD allow establish three levels of taxonomy: the taxonomic level
#' determined by analyst when processing the entity, "accepted" name to resolve
#' synonymies, and "higher" to collapse taxa into higher taxonomical levels (i.e.,
#' species in the same genus, genus in the same family, etc). \code{taxa_type} can take 
#' any of three values: "Samples", "Accepted" or "Higher". "Samples" indicates that 
#' taxonomy in \code{@coutns} is the same the analyst submitted to the EPD. "Accepted"
#' indicates that taxonomy in \code{@counts} is modified using the accepted taxa according
#' to last review of the EPD. "Higher" indicates that taxonomy was modified to group
#' taxa into higher taxonomical levels. "Samples" is always found when data comes
#' directly from the EPD with \code{\link[EPDr:getCounts]{getCounts}}, whereas "Accepted" and "Higher" are specified when data are transformed using \code{\link[EPDr:taxa2AcceptedTaxa]{taxa2AcceptedTaxa}} and \code{\link[EPDr:taxa2HigherTaxa]{taxa2HigherTaxa}}
#' respectively.
#' @slot taxa_processing factor. Indicating if taxonomy in \code{@counts} has been
#' processed. It can take any of three values: "Original", "Expanded", or "Taxized". 
#' "Original" indicates that taxonomy has not been modified. "Expanded" indicates that
#' taxonomy has been expanded beyond the taxa specified by the data provider, using
#' \code{\link[EPDr:filterTaxa]{filterTaxa}} or
#' \code{\link[EPDr:unifyTaxonomy]{unifyTaxonomy}}. "Taxized" indicates that taxonomy in
#' \code{@counts} has been resolved using the package \code{link[taxize]{taxize}}.
#' Function for this are on schedule but not implemented yet.
#' @slot entity data.frame. Details of the entity.
#' @slot site data.frame. Details of the site to which the entity belong to. Note that 
#' several entities can belong to the same site.
#' @slot taxa_names character. Character vector with all taxa used in \code{@counts}.
#' @slot taxa_ numeric. Numeric vector indicating the taxa number for each of the taxa 
#' used in \code{@counts}.
#' @slot taxa_accepted numeric. Numeric vector indicating the taxa number of 
#' corresponding accepted taxa for each taxa used in \code{@counts}.
#' @slot taxa_mhvar numeric. Numeric vector indicating the taxa number of 
#' corresponding higher taxa for each taxa used in \code{@counts}.
#' @slot taxa_groupid character. Character vector indicating the corresponding groupid
#' to which belong each taxa used in \code{@counts}. 
#' @slot sample_ numeric. Numeric vector indicating the sample number of each sample used
#' in \code{@counts}. Sample here is each of the palynological samples in the entity.
#' @slot sample_label character. Character vector indicating the sample name (or code)
#' of each sample used in \code{@counts}. Sample here is each of the palynological samples in the entity.
#' @slot default_ages numeric. Numeric vector indicating the ages estimated for each
#' palynological sample according to the default chronology in the EPD for that entity.
#' @slot depthcm numeric. Numeric vector indicating the depth (in cm) in which each
#' palynological sample was collected.
#' @slot counts data.frame. Data frame with counts (raw counts or percentages) for each
#' taxon at each sample (original samples, interpolated or ranged). This takes the form
#' of a age/depth (rows) by taxon (columns).
#'
#' @export 
#' @import methods
#'
counts <- setClass("counts",
                            slots=c(
                                e_="numeric",
                                restriction="data.frame",
                                counts_type="factor",
                                counts_processing="factor",
                                taxa_type="factor",
                                taxa_processing="factor",
                                entity="data.frame",
                                site="data.frame",
                                taxa_names="character",
                                taxa_="numeric",
                                taxa_accepted="numeric",
                                taxa_mhvar="numeric",
                                taxa_groupid="character",
                                sample_="numeric",
                                sample_label="character",
                                default_ages="numeric",
                                depthcm="numeric",
                                counts="data.frame"
                            ),
                            prototype=list(
                                e_=numeric(0),
                                restriction=data.frame(
                                    e_=NA,
                                    contact_=NA,
                                    datasource=NA,
                                    dataform=NA,
                                    usestatus=NA,
                                    datacoop=NA)[-1,],
                                counts_type=factor(levels=c("Counts", "Percentages")),
                                counts_processing=factor(levels=c("Samples", "Interpolated", "Ranged means")),
                                taxa_type=factor(levels=c("Samples", "Accepted", "Higher")),
                                taxa_processing=factor(levels=c("Original", "Expanded", "Taxized")),
                                entity=data.frame(
                                    e_=NA,
                                    site_=NA,
                                    sigle=NA,
                                    name=NA,
                                    iscore=NA,
                                    issect=NA,
                                    isssamp=NA,
                                    descriptor=NA,
                                    hasanlam=NA,
                                    entloc=NA,
                                    localveg=NA,
                                    coll_=NA,
                                    sampdate=NA,
                                    depthatloc=NA,
                                    icethickcm=NA,
                                    sampdevice=NA,
                                    corediamcm=NA,
                                    c14depthadj=NA,
                                    notes=NA)[-1,],
                                site=data.frame(
                                    site_=NA,
                                    sitename=NA,
                                    sitecode=NA,
                                    siteexists=NA,
                                    poldiv1=NA,
                                    poldiv2=NA,
                                    poldiv3=NA,
                                    latdeg=NA,
                                    latmin=NA,
                                    latsec=NA,
                                    latns=NA,
                                    latdd=NA,
                                    latdms=NA,
                                    londeg=NA,
                                    lonmin=NA,
                                    lonsec=NA,
                                    lonew=NA,
                                    londd=NA,
                                    londms=NA,
                                    elevation=NA,
                                    areaofsite=NA)[-1,],
                                taxa_names=character(0),
                                taxa_=numeric(0),
                                taxa_accepted=numeric(0),
                                taxa_mhvar=numeric(0),
                                taxa_groupid=character(0),
                                sample_=numeric(0),
                                sample_label=character(0),
                                default_ages=numeric(0),
                                depthcm=numeric(0),
                                counts=data.frame()
                            )
)


#' Class for Ages of an entity
#' 
#' Class "ages" store in an organized and systematic way information about 
#' estimated ages for palynological samples in an specified entity in the European
#' Pollen Database (EPD). This object is created by \code{\link[EPDr:getAges]{getAges}}.
#'
#' It has different elements all of which correspond to a unique entity 
#' 
#' @slot e_ numeric. The entity number (e_) as in the EPD.
#' @slot restriction data.frame. Restriction of use information for that particular entity
#' in the database. It is important to know if we can freely use the data or should ask
#' for authorization
#' @slot entity data.frame. Details of the entity.
#' @slot site data.frame. Details of the site to which the entity belong to. Note that 
#' several entities can belong to the same site.
#' @slot default_chronology numeric. Which of the available chronologies is tagged as 
#' the default in the database.
#' @slot giesecke logical. Indicating \code{TRUE} if there are ages revised by Giesecke
#' et al. (2013) for this entity or \code{FALSE} on the contrary.
#' @slot sample_ numeric. Numeric vector indicating the sample number of each sample used
#' in \code{@counts}. Sample here is each of the palynological samples in the entity.
#' @slot sample_label character. Character vector indicating the sample name (or code)
#' of each sample used in \code{@counts}. Sample here is each of the palynological samples
#' in the entity.
#' @slot depthcm numeric. Numeric vector indicating the depth (in cm) in which each
#' palynological sample was collected.
#' @slot depths data.frame. Details about the samples (including depths) at which samples
#' were taken in the entity for palynological (pollen, spores, etc) analysis.
#' @slot depth_ages data.frame. Data frame with ages for each palynological sample
#' according to the different chronologies and giesecke, if available.
#' @slot data_quality data.frame. Data frame with data quality index for counts on
#' each sample (original, interpolated, or ranged) according to metrics in Blois
#' et al. (2013).
#'
#' @references Giesecke, Thomas; Davis, Basil A S; Brewer, Simon; Finsinger, Walter;
#' Wolters, Steffen; Blaauw, Maarten; de Beaulieu, Jacques-Louis; Binney, Heather;
#' Fyfe, Ralph M; Gaillard, Marie-Jose; Gil-Romera, Graciela; van der Knaap, Pim Willem O;
#' Kunes, Petr; Kuhl, Norbert; van Leeuwen, Jaqueline F N; Leydet, Michelle;
#' Lotter, Andre F; Ortu, Elena; Semmler, Malte Sebastian Swen;
#' Bradshaw, Richard H W (2013). Towards mapping the late Quaternary vegetation change
#' of Europe. Vegetation History and Archaeobotany, 23(1): 75-86.
#' doi:10.1007/s00334-012-0390-y
#' @references \url{https://doi.pangaea.de/10.1594/PANGAEA.804597}
#' @references Blois, Jessica L; Williams, John W; Fitzpatrick, Matthew C; Ferrier, Simon;
#' Veloz, Samuel D; He, Feng; Liu, Zhengyu; Manion, Glenn; Otto-Bliesner, Bette (2013).
#' Modeling the Climatic Drivers of Spatial Patterns in Vegetation Composition since the
#' Last Glacial Maximum. Ecography, 36(4): 460-473. doi:10.1111/j.1600-0587.2012.07852.x.
#' 
#' @export 
#' @import methods
#'
ages <- setClass("ages",
                          slots=c(
                              e_="numeric",
                              restriction="data.frame",
                              entity="data.frame",
                              site="data.frame",
                              default_chronology="numeric",
                              giesecke="logical",
                              sample_="numeric",
                              sample_label="character",
                              depthcm="numeric",
                              depths="data.frame",
                              depth_ages="data.frame",
                              data_quality="data.frame"
                          ),
                          prototype=list(
                              e_=numeric(0),
                              restriction=data.frame(
                                  e_=NA,
                                  contact_=NA,
                                  datasource=NA,
                                  dataform=NA,
                                  usestatus=NA,
                                  datacoop=NA)[-1,],
                              entity=data.frame(
                                  e_=NA,
                                  site_=NA,
                                  sigle=NA,
                                  name=NA,
                                  iscore=NA,
                                  issect=NA,
                                  isssamp=NA,
                                  descriptor=NA,
                                  hasanlam=NA,
                                  entloc=NA,
                                  localveg=NA,
                                  coll_=NA,
                                  sampdate=NA,
                                  depthatloc=NA,
                                  icethickcm=NA,
                                  sampdevice=NA,
                                  corediamcm=NA,
                                  c14depthadj=NA,
                                  notes=NA)[-1,],
                              site=data.frame(
                                  site_=NA,
                                  sitename=NA,
                                  sitecode=NA,
                                  siteexists=NA,
                                  poldiv1=NA,
                                  poldiv2=NA,
                                  poldiv3=NA,
                                  latdeg=NA,
                                  latmin=NA,
                                  latsec=NA,
                                  latns=NA,
                                  latdd=NA,
                                  latdms=NA,
                                  londeg=NA,
                                  lonmin=NA,
                                  lonsec=NA,
                                  lonew=NA,
                                  londd=NA,
                                  londms=NA,
                                  elevation=NA,
                                  areaofsite=NA)[-1,],
                              default_chronology=numeric(0),
                              giesecke=logical(0),
                              sample_=numeric(0),
                              sample_label=character(0),
                              depthcm=numeric(0),
                              depth=data.frame(
                                  e_=NA,
                                  sample_=NA,
                                  depthcm=NA,
                                  thickness=NA,
                                  analyst_=NA,
                                  analydate=NA,
                                  notes=NA,
                                  lab_ID=NA)[-1,],
                              depth_ages=data.frame(),
                              data_quality=data.frame()
                          )
)


#' Class for Aged-Counts of an entity
#' 
#' Class "agedcounts" store in an organized and systematic way information about 
#' counts and ages for palynological samples in an specified entity in the European
#' Pollen Database (EPD). This object is created by
#' \code{\link[EPDr:getAgedCounts]{getAgedCounts}}.
#' 
#' It has different elements all of which correspond to a unique entity 
#'
#' @slot e_ numeric. The entity number (e_) as in the EPD.
#' @slot restriction data.frame. Restriction of use information for that particular entity
#' in the database. It is important to know if we can freely use the data or should ask
#' for authorization
#' @slot entity data.frame. Details of the entity.
#' @slot site data.frame. Details of the site to which the entity belong to. Note that 
#' several entities can belong to the same site.
#' @slot ages ages. Object of class \code{\link{ages}} for the entity.
#' @slot counts counts. Object of class \code{\link{counts}} for the entity.
#'
#' @export 
#' @import methods
#'
agedcounts <- setClass("agedcounts",
                                slots=c(
                                    e_="numeric",
                                    restriction="data.frame",
                                    entity="data.frame",
                                    site="data.frame",
                                    ages="ages",
                                    counts="counts"
                                ),
                                prototype=list(
                                    e_=numeric(0),
                                    restriction=data.frame(
                                        e_=NA,
                                        contact_=NA,
                                        datasource=NA,
                                        dataform=NA,
                                        usestatus=NA,
                                        datacoop=NA)[-1,],
                                    entity=data.frame(
                                        e_=NA,
                                        site_=NA,
                                        sigle=NA,
                                        name=NA,
                                        iscore=NA,
                                        issect=NA,
                                        isssamp=NA,
                                        descriptor=NA,
                                        hasanlam=NA,
                                        entloc=NA,
                                        localveg=NA,
                                        coll_=NA,
                                        sampdate=NA,
                                        depthatloc=NA,
                                        icethickcm=NA,
                                        sampdevice=NA,
                                        corediamcm=NA,
                                        c14depthadj=NA,
                                        notes=NA)[-1,],
                                    site=data.frame(
                                        site_=NA,
                                        sitename=NA,
                                        sitecode=NA,
                                        siteexists=NA,
                                        poldiv1=NA,
                                        poldiv2=NA,
                                        poldiv3=NA,
                                        latdeg=NA,
                                        latmin=NA,
                                        latsec=NA,
                                        latns=NA,
                                        latdd=NA,
                                        latdms=NA,
                                        londeg=NA,
                                        lonmin=NA,
                                        lonsec=NA,
                                        lonew=NA,
                                        londd=NA,
                                        londms=NA,
                                        elevation=NA,
                                        areaofsite=NA
                                    )[-1,],
                                    default_chronology=numeric(0),
                                    ages=ages(),
                                    counts=counts()
                                )
)



#' Class for Geochronologies of an entity
#' 
#' Class "geochron" store all information relative to datation samples and analysis in a specified entity of the European Pollen Database (EPD). This object is created by
#' \code{\link[EPDr:getGeochron]{getGeochron}}.
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
                       slots=c(
                         geochron="data.frame",
                         aar="data.frame",
                         c14="data.frame",
                         esr="data.frame",
                         ft="data.frame",
                         kar="data.frame",
                         pb210="data.frame",
                         si32="data.frame",
                         tl="data.frame",
                         useries="data.frame",
                         publ="data.frame"
                       ),
                       prototype=list(
                         geochron=data.frame(
                           e_=NA,
                           sample_=NA,
                           agebp=NA,
                           errorlimits=NA,
                           labnumber=NA,
                           notes=NA)[-1,],
                         aar=data.frame(
                           e_=NA,
                           sample_=NA,
                           agebp=NA,
                           errorlimits=NA,
                           taxondated=NA,
                           labnumber=NA,
                           notes=NA)[-1,],
                         c14=data.frame(
                           e_=NA,
                           sample_=NA,
                           agebp=NA,
                           agesdup=NA,
                           agesdlo=NA,
                           grthanage=NA,
                           basis=NA,
                           enriched=NA,
                           labnumber=NA,
                           deltac13=NA,
                           notes=NA)[-1,],
                         esr=data.frame(
                           e_=NA,
                           sample_=NA,
                           agebp=NA,
                           errorlimits=NA,
                           labnumber=NA,
                           notes=NA)[-1,],
                         ft=data.frame(
                           e_=NA,
                           sample_=NA,
                           agebp=NA,
                           errorlimits=NA,
                           labnumber=NA,
                           notes=NA)[-1,],
                         kar=data.frame(
                           e_=NA,
                           sample_=NA,
                           agebp=NA,
                           errorlimits=NA,
                           labnumber=NA,
                           notes=NA)[-1,],
                         pb210=data.frame(
                           e_=NA,
                           sample_=NA,
                           agead=NA,
                           agesdup=NA,
                           agesdlo=NA,
                           grthanage=NA,
                           notes=NA)[-1,],
                         si32=data.frame(
                           e_=NA,
                           sample_=NA,
                           agebp=NA,
                           agesdup=NA,
                           agesdlo=NA,
                           grthanage=NA,
                           labnumber=NA,
                           notes=NA)[-1,],
                         tl=data.frame(
                           e_=NA,
                           sample_=NA,
                           agebp=NA,
                           errorlimits=NA,
                           grainsize=NA,
                           labnumber=NA,
                           notes=NA)[-1,],
                         useries=data.frame(
                           e_=NA,
                           sample_=NA,
                           agebp=NA,
                           errorlimits=NA,
                           labnumber=NA,
                           notes=NA)[-1,],
                         publ=data.frame(
                           publ_=NA,
                           acc_=NA,
                           yearofpubl=NA,
                           citation=NA)[-1,]
                         )
)



#' Class for Site of an entity
#' 
#' Class "site" store all information relative to site where samples were taken and to which a specified entity of the European Pollen Database (EPD) belong to. This object is created by \code{\link[EPDr:getSite]{getSite}}.
#'
#' @slot siteloc data.frame. Information on the location (e.g. geographical coordinates) for the site.
#' @slot sitedescr data.frame. Description of the site in terms of vegetation and orography.
#' @slot siteinfo data.frame. This table summarize the information available for that site (e.g. C14, pollen, insects, etc.)
#' @slot country data.frame. Country to which the site belong to.
#' @slot region data.frame. Regions to which the site belong to.
#' @slot region3rd data.frame. 3rd level regions to which the site belong to.
#' @slot igcptype data.frame. IGCP regions to which the site belong to.
#' @slot infotype data.frame. This table provide a longer description of the information summarized in \code{@siteinfo}
#' @slot publ data.frame PUBL data where the data were published
#'
#' @export
#' @import methods
site <- setClass("site",
                     slots=c(
                       siteloc="data.frame",
                       sitedescr="data.frame",
                       siteinfo="data.frame",
                       country="data.frame",
                       region="data.frame",
                       region3rd="data.frame",
                       igcptype="data.frame",
                       infotype="data.frame",
                       publ="data.frame"
                     ),
                     prototype=list(
                       siteloc=data.frame(
                         site_=NA,
                         sitename=NA,
                         sitecode=NA,
                         siteexists=NA,
                         poldiv1=NA,
                         poldiv2=NA,
                         poldiv3=NA,
                         latdeg=NA,
                         latmin=NA,
                         latsec=NA,
                         latns=NA,
                         latdd=NA,
                         latdms=NA,
                         londeg=NA,
                         lonmin=NA,
                         lonsec=NA,
                         lonew=NA,
                         londd=NA,
                         londms=NA,
                         elevation=NA,
                         areaofsite=NA)[-1,],
                       sitedesc=data.frame(
                         site_=NA,
                         sitedescript=NA,
                         physiography=NA,
                         surroundveg=NA,
                         vegformation=NA,
                         igcptype=NA)[-1,],
                       siteinfo=data.frame(
                         site_=NA,
                         icode=NA,
                         publ_=NA)[-1,],
                       country=data.frame(
                         poldiv1=NA,
                         name=NA)[-1,],
                       region=data.frame(
                         poldiv1=NA,
                         poldiv2=NA,
                         postcode=NA,
                         name=NA)[-1,],
                       region3rd=data.frame(
                         poldiv1=NA,
                         poldiv2=NA,
                         poldiv3=NA,
                         name=NA)[-1,],
                       igcptype=data.frame(
                         igcptype=NA,
                         regionname=NA)[-1,],
                       publ=data.frame(
                         publ_=NA,
                         acc_=NA,
                         yearofpubl=NA,
                         citation=NA)[-1,]
                     )
)
