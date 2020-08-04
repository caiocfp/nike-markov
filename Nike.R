#pacotes#

library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(ChannelAttribution)
library(markovchain)
library(xlsx)
library(shape)
library(diagram)
library(igraph)
library(stringr)
library(tidyr)
library(shiny)
library(plotly)
#Dados report4#

NikeRep4  <- read.csv("bq-results-20200617-114606-xio05giiq1up.csv",sep = ",")



NikeRep4$date <- as.Date(NikeRep4$date)

str(NikeRep4)


#Unir semelhantes "pagename", "utm_source"#

unique(NikeRep4$pagename)
unique(NikeRep4$utm_source)

NikeRep4$utm_source <- ifelse(NikeRep4$utm_source == "zanox","Zanox",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source == "zoom","Zoom",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source == "criteo","Criteo",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source == "criteo,Criteo","Criteo",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source == "nikenews","Nikenews",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source == "gp_search","GP_Search",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source == "rakuten","Rakuten",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source == "::unspecified::","unspecified",NikeRep4$utm_source)

#Canal Agrupado1-Source#
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"::unspecified::","::unspecified::",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"Admatic","media",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"brand","media",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"facebook","media",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"instagram","media",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"lomadee","media",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"Rakuten","media",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"socialminer","media",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"Zanox","media",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"blue","media",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"Criteo","media",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"email_mkt_externo","crm",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"google","organic",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"googleshop","media",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"GP_Search","media",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"nfscatarina","crm",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"nfsfortalezaoff","crm",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"nfsguarulhos","crm",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"nfsnovaamerica","crm",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"nfsosasco","crm",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"nfssantoandre","crm",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"nfssaobernardo","crm",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"nfsvilavelha","crm",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"Nikenews","crm",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"rtbhouse","media",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"yahoopatrocinado","media",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"Zoom","media",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"homepage","nike",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"nikecombr","nike",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"productgrid:standardgrid","nike",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"SEARCH","nike",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"cart","nike",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"checkout","nike",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"identification","nike",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"address","nike",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"payment","nike",NikeRep4$utm_source)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"purchase","nike",NikeRep4$utm_source)

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      



#Canal Agrupado2-Source#
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	::unspecified::	"	,	"	::unspecified::	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	Admatic	"	,	"	performance	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	brand	"	,	"	brand	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	facebook	"	,	"	performance	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	instagram	"	,	"	performance	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	lomadee	"	,	"	affiliate	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	Rakuten	"	,	"	affiliate	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	socialminer	"	,	"	affiliate	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	Zanox	"	,	"	affiliate	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	blue	"	,	"	affiliate	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	Criteo	"	,	"	retargeting	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	email_mkt_externo	"	,	"	email	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	google	"	,	"	organic	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	googleshop	"	,	"	performance	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	GP_Search	"	,	"	performance	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	nfscatarina	"	,	"	email	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	nfsfortalezaoff	"	,	"	email	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	nfsguarulhos	"	,	"	email	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	nfsnovaamerica	"	,	"	email	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	nfsosasco	"	,	"	email	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	nfssantoandre	"	,	"	email	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	nfssaobernardo	"	,	"	email	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	nfsvilavelha	"	,	"	email	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	Nikenews	"	,	"	email	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	rtbhouse	"	,	"	retargeting	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	yahoopatrocinado	"	,	"	performance	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	Zoom	"	,	"	affiliate	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	homepage	"	,	"	nike	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	nikecombr	"	,	"	nike	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	productgrid:standardgrid	"	,	"	nike	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	SEARCH	"	,	"	nike	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	cart	"	,	"	nike	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	checkout	"	,	"	nike	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	identification	"	,	"	nike	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	address	"	,	"	nike	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	payment	"	,	"	nike	"	,	NikeRep4$utm_source	)
NikeRep4$utm_source <- ifelse(NikeRep4$utm_source ==	"	purchase	"	,	"	nike	"	,	NikeRep4$utm_source	)







#separar os pageview#
#podemos fazer isso, mas 
NikeRep4 <-  NikeRep4 %>%
              separate(pagename,
                       c("page1","page2","page3",
                        ">",
                        convert = T))



NikeRep4$page1 <- ifelse(NikeRep4$page1 == "", NA,NikeRep4$page1)
NikeRep4$page2 <- ifelse(NikeRep4$page2 == "", NA,NikeRep4$page2)
NikeRep4$page3 <- ifelse(NikeRep4$page3 == "", NA,NikeRep4$page3)


#Substituir paginas internas pelo homonimo#

#pagina1#
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="::unspecified::","::unspecified::",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="Admatic","media",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="brand","media",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="facebook","media",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="instagram","media",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="lomadee","media",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="Rakuten","media",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="socialminer","media",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="Zanox","media",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="blue","media",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="Criteo","media",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="email_mkt_externo","crm",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="google","organic",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="googleshop","media",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="GP_Search","media",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="nfscatarina","crm",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="nfsfortalezaoff","crm",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="nfsguarulhos","crm",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="nfsnovaamerica","crm",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="nfsosasco","crm",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="nfssantoandre","crm",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="nfssaobernardo","crm",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="nfsvilavelha","crm",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="Nikenews","crm",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="rtbhouse","media",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="yahoopatrocinado","media",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="Zoom","media",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="homepage","nike",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="nikecombr","nike",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="productgrid:standardgrid","nike",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="SEARCH","nike",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="cart","nike",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="checkout","nike",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="identification","nike",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="address","	nike",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="payment","nike",NikeRep4$page1)
NikeRep4$page1 <- ifelse(NikeRep4$page1 =="purchase","nike",NikeRep4$page1)



#pagina2#
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="::unspecified::","::unspecified::",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="Admatic","media",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="brand","media",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="facebook","media",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="instagram","media",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="lomadee","media",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="Rakuten","media",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="socialminer","media",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="Zanox","media",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="blue","media",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="Criteo","media",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="email_mkt_externo","crm",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="google","organic",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="googleshop","media",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="GP_Search","media",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="nfscatarina","crm",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="nfsfortalezaoff","crm",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="nfsguarulhos","crm",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="nfsnovaamerica","crm",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="nfsosasco","crm",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="nfssantoandre","crm",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="nfssaobernardo","crm",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="nfsvilavelha","crm",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="Nikenews","crm",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="rtbhouse","media",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="yahoopatrocinado","media",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="Zoom","media",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="homepage","nike",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="nikecombr","nike",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="productgrid","nike",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="standardgrid","nike",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="SEARCH","nike",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="cart","nike",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="checkout","nike",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="identification","nike",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="address","	nike",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="payment","nike",NikeRep4$page2)
NikeRep4$page2 <- ifelse(NikeRep4$page2 =="purchase","nike",NikeRep4$page2)




# agrupar as identidades #

NikeRep4.1 <- NikeRep4   %>%
  arrange(nike_user_id,date) %>%   #arranjar as linhas por cliente e data
  group_by(nike_user_id) %>%
  summarise(trilha = paste(c(utm_source,page2), collapse = '>'),
            conv = sum(orders),
            conv_null = ifelse(conv < 1,1,0) ) %>%
  ungroup()

NikeRep4.1 <- NikeRep4.1[-1,]


#teste agrupar identidade com "entries"#

NikeRep4.2 <- NikeRep4 %>%
           arrange(nike_user_id,date) %>%
           group_by(nike_user_id) %>%
            summarise(trilha = paste(c(utm_source,page2), collapse = '>'),
                      entries = paste(entries, collapse = '+'),
                      views = paste(pageviews, collapse = '+'),
                      conv = sum(orders),
                      conv_null = ifelse(conv < 1,1,0) ) %>%
  ungroup()

NikeRep4.2 <- NikeRep4.2[-1,]

# markov teste 1 #
nikeMark.1 <- markov_model(NikeRep4.1,
                     var_path = 'trilha',
                     var_conv = 'conv',
                     var_null = 'conv_null',
                     out_more = TRUE)



#matriz de transicao#
nikeMark.1$transition_matrix -> nikeTrans1

####canais compilados pelo e-mail, canais to###
nikeTrans1$channel_to <- factor(nikeTrans1$channel_to,
                                levels = c("(null)",
                                            "(conversion)",
                                            "media",
                                            "unspecified",
                                            "organic",
                                            "crm",
                                            "nike",
                                             "NA" ))



####canais compilados pelo e-mail, canais from###

nikeTrans1$channel_from <- factor(nikeTrans1$channel_from,
                                  levels = c("(start)",
                                             "::unspecified::",
                                             "Admatic",
                                             "brand",
                                             "facebook",
                                             "instagram",
                                             "lomadee",
                                             "Rakuten",
                                             "socialminer",
                                             "Zanox",
                                             "blue",
                                             "Criteo",
                                             "email",
                                             "google",
                                             "googleshop",
                                             "GP_Search",
                                             "rtbhouse",
                                             "yahoopatrocinado",
                                             "Zoom",
                                             "homepage",
                                             "nikecombr",
                                             "productgrid:standardgrid",
                                             "SEARCH",
                                             "cart",
                                             "checkout",
                                             "identification",
                                             "address",
                                             "payment",
                                             "purchase"))  


#canais reduzidos, canais from#
nikeTrans1$channel_from <- factor(nikeTrans1$channel_from,
                                levels = c("(start)",
                                           "media",
                                           "nikecombr",
                                           "SEARCH",
                                           "unspecified",
                                           "NA",         
                                           "organic",
                                           "crm")) 



#####Grafico inicial####

#Dummy para o Gráfico#
df_dummy <- data.frame(channel_from = c('(start)', '(conversion)', '(null)'),
                       channel_to = c('(start)', '(conversion)', '(null)'),
                       transition_probability = c(0,1,1))



#unir matriz de transicao com a dummy#
nikeTrans1 <- rbind(nikeTrans1,df_dummy)

#transformar a matriz de transicao#
nikeTrans1 <- dcast(nikeTrans1,
                    channel_from ~ channel_to,
                    value.var = 'transition_probability')



#converter o objeto, ajeitar as linhas e colunas; na=0#
nikeTrans1_matrix <- matrix(data = as.matrix(nikeTrans1[,-1]),
                            nrow = nrow(nikeTrans1[, -1]), ncol = ncol(nikeTrans1[, -1]),
                            dimnames = list(c(as.character(nikeTrans1[, 1])), c(colnames(nikeTrans1[, -1]))))

nikeTrans1_matrix[is.na(nikeTrans1_matrix)] <- 0


#grafico inicial##

nikeTrans_matrix_Graf  <- new("markovchain", transitionMatrix = nikeTrans1_matrix)




Graf1 <-   plot(nikeTrans_matrix_Graf,
           rescale = T,
           edge.arrow.size = 0.75,  
           arrow.head.size = 0.005,
           size = 5,
           shape = "none",
           label.cex = 0.3,
           asp = 0)





plotly(nikeTrans1_matrix)





write.xlsx(as.data.frame(nikeTrans1_matrix), 
           file = "C:/Users/Admin/Desktop/Projetos/AtribuicaoCanal/nikeTrans1_matrix.xlsx",
           sheetName = "Sheet1",
           col.names = T,
           row.names = T,
           append = F) 





#####mapa de calor######

#criar colunas de cor#
cols <- c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a",
          "#e29421", "#e29421", "#f05336", "#ce472e")


#maior probabilidade de transi??o#
niketrans2 <- nikeMark.1$transition_matrix
niketrans2[is.na(niketrans2)] <- 0
t <- max(niketrans2$transition_probability)


#ggplot da transio 1#
#ggplot da transi??o#
ggplot(niketrans2 , aes(y = channel_from, x = channel_to, fill = transition_probability)) +
  theme_minimal() +
  geom_tile(colour = "white", width = .9, height = .9) +
  scale_fill_gradientn(colours = cols, limits = c(0, t),
                       breaks = seq(0, t, by = t/4),
                       labels = c("0", round(t/4*1, 2), round(t/4*2, 2), round(t/4*3, 2), round(t/4*4, 2)),
                       guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_text(aes(label = round(transition_probability, 2)), fontface = "bold", size = 4) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain")) +
  ggtitle("Mapa da calor de matriz de transicao dos canais")

 

#teste 
