chronology <- setClass("chronology",
                       slots=c(
                           core_number="numeric",
                           number_of_chronologies="numeric",
                           default_chronology="numeric",
                           chron="data.frame",
                           agebasis="data.frame"
                       ),
                       prototype=list(
                           core_number=numeric(0),
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
                               notes=NA
                           )[-1,],
                           agebasis=data.frame(
                               e_=NA,
                               chron_=NA,
                               sample_=NA,
                               depthcm=NA,
                               thickness=NA,
                               age=NA,
                               ageup=NA,
                               agelo=NA,
                               rcode=NA
                           )[-1,]
                       )
)

datation <- setClass("datation",
                     slots=c(
                         core_number="numeric",
                         chronology="chronology",
                         c14="data.frame",
                         events="data.frame",
                         depths="data.frame"
                     ),
                     prototype=list(
                         core_number=numeric(0),
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
                             publ_=NA
                         )[-1,],
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
                             publ_=NA
                         )[-1,],
                         depths=data.frame(
                             e_=NA,
                             sample_=NA,
                             depthcm=NA,
                             thickness=NA,
                             analyst=NA,
                             analydate=NA,
                             notes=NA,
                             lab_ID=NA
                         )[-1,]
                     )
)