#' Title TBW
#'
#' @slot e_ numeric. TBW
#' @slot restriction data.frame. TBW
#' @slot site data.frame. TBW
#' @slot number_of_chronologies numeric. TBW
#' @slot default_chronology numeric. TBW
#' @slot chron data.frame. TBW
#' @slot agebasis data.frame. TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
chronology <- methods::setClass("chronology",
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


#' TitleTBW
#'
#' @slot e_ numeric. TBW
#' @slot restriction data.frame. TBW
#' @slot site data.frame. TBW
#' @slot postbomb_zone factor. TBW
#' @slot chronology chronology. TBW
#' @slot c14 data.frame. TBW
#' @slot events data.frame. TBW
#' @slot depths data.frame. TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
datation <- methods::setClass("datation",
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

#' Title TBW
#'
#' @slot e_ numeric. TBW
#' @slot restriction data.frame. TBW
#' @slot counts_type factor. TBW
#' @slot counts_processing factor. TBW
#' @slot taxa_type factor. TBW
#' @slot taxa_processing factor. TBW
#' @slot site data.frame. TBW
#' @slot taxa_names character. TBW
#' @slot taxa_ numeric. TBW
#' @slot taxa_accepted numeric. TBW
#' @slot taxa_mhvar numeric. TBW
#' @slot taxa_groupid character. TBW 
#' @slot sample_ numeric. TBW
#' @slot sample_label character. TBW
#' @slot default_ages numeric. TBW
#' @slot depthcm numeric. TBW
#' @slot counts data.frame. TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
counts <- methods::setClass("counts",
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

#' Title TBW
#'
#' @slot e_ numeric. TBW
#' @slot restriction data.frame. TBW
#' @slot site data.frame. TBW
#' @slot default_chronology numeric. TBW
#' @slot giesecke logical. TBW
#' @slot sample_ numeric. TBW
#' @slot sample_label character. TBW
#' @slot depthcm numeric. TBW
#' @slot depths data.frame. TBW
#' @slot depth_ages data.frame. TBW
#' @slot data_quality data.frame. TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
ages <- methods::setClass("ages",
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

#' Title TBW
#'
#' @slot e_ numeric. TBW
#' @slot restriction data.frame. TBW
#' @slot site data.frame. TBW
#' @slot ages ages. TBW
#' @slot counts counts. TBW
#'
#' @return TBW
#' 
#' @export
#'
#' @examples
#' # TBW
agedcounts <- methods::setClass("agedcounts",
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

