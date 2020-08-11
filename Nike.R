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

NikeRep10  <- read.csv("bq-results-20200731-164059-bzdz7tpr3zju.csv",sep = ",")



NikeRep10$date <- as.Date(NikeRep10$date)


#Unir semelhantes "pagename", "utm_source"#

unique(NikeRep10$pagename)
unique(NikeRep10$utm_source)

NikeRep10$utm_source <- ifelse(NikeRep10$utm_source == "zanox","Zanox",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source == "zoom","Zoom",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source == "criteo","Criteo",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source == "criteo,Criteo","Criteo",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source == "criteo,criteo","Criteo",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source == "criteo,brand","Criteo",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source == "brand,brand","brand",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source == "nikenews","Nikenews",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source == "gp_search","GP_Search",NikeRep10$utm_source)

NikeRep10$utm_source <- ifelse(NikeRep10$utm_source == "gp_search,GP_Search","GP_Search",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source == "GP_search,GP_Search","GP_Search",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source == "gp_searchhttps://busca.nike.com.br/cristiano ronaldo chuteira","GP_Search",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source == "rakuten","Rakuten",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source == ",Rakuten","Rakuten",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source == "::unspecified::","unspecified",NikeRep10$utm_source)

#Canal Agrupado1-Source#
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"::unspecified::","direct",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==  "offline","offline",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"Admatic","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"brand","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"facebook","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"instagram","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"lomadee","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"Rakuten","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"socialminer","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==  "hardmob","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==  "ntc","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"Zanox","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"blue","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"Criteo","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"email_mkt_externo","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"google","organic",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"googleshop","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"GP_Search","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==  "gp","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nfscatarina","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nfsfortalezaoff","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nfsguarulhos","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nfsnovaamerica","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nfsosasco","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nfssantoandre","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nfssaobernardo","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nfsvilavelha","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"Nikenews","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"rtbhouse","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"yahoopatrocinado","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"Zoom","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"purchase","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"gp","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"shopback","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nikecombr","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"productgrid:standardgrid","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"SEARCH","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"cart","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"checkout","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"identification","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"address","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"payment","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"purchase","nike",NikeRep10$utm_source)



#Canal Agrupado2-Source#
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"::unspecified::","direct",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"Admatic","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"brand","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"lomadee","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"Rakuten","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"socialminer","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"Zanox","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"blue","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"Criteo","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"email_mkt_externo","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"googleshop","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"GP_Search","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nfscatarina","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nfsfortalezaoff","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nfsguarulhos","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nfsnovaamerica","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nfsosasco","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nfssantoandre","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nfssaobernardo","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nfsvilavelha","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"Nikenews","crm",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"rtbhouse","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"yahoopatrocinado","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"Zoom","media",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"homepage","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"nikecombr","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"productgrid:standardgrid","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"SEARCH","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"cart","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"checkout","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"identification","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"address","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"payment","nike",NikeRep10$utm_source)
NikeRep10$utm_source <- ifelse(NikeRep10$utm_source ==	"purchase","nike",NikeRep10$utm_source)





#retirar os NA e os unspecified#
unique(NikeRep10$utm_source)

            

# agrupar as identidades #
NikeRep5.1 <- NikeRep5   %>%
  arrange(nike_user_id,date) %>%   #arranjar as linhas por cliente e data
  group_by(nike_user_id) %>%
  summarise(trilha = paste(c(utm_source), collapse = '>'),
            conv = sum(orders),
            conv_null = ifelse(conv < 1,1,0) ) %>%
  ungroup()

NikeRep5.1 <- NikeRep5.1[-1,]


# markov teste 1 #
nikeMark.1 <- markov_model(NikeRep5.1,
                           var_path = 'trilha',
                           var_conv = 'conv',
                           var_null = 'conv_null',
                           out_more = TRUE)
#matriz de transicao#
nikeMark.1$transition_matrix -> nikeTrans1

####canais compilados pelo e-mail, canais to###
unique(nikeTrans1$channel_to)

nikeTrans1$channel_to <- factor(nikeTrans1$channel_to,
                                levels = c("(start)",
                                           "(null)",
                                           "(conversion)",
                                           "media",
                                           "facebook",
                                           "google",
                                           "crm",
                                           "instagram")) 

#canais reduzidos, canais from#
unique(nikeTrans1$channel_from)

nikeTrans1$channel_from <- factor(nikeTrans1$channel_from,
                                  levels = c("(start)",
                                             "(null)",
                                             "(conversion)",
                                             "media",
                                             "facebook",
                                             "google",
                                             "crm",
                                             "instagram")) 



#####Grafico inicial####

#Dummy para o Gráfico#
df_dummy <- data.frame(channel_from = c('(start)', '(conversion)','(null)'),
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
                size = 3,
                shape = "none",
                label.cex = 0.3,
                asp = 0)

#####mapa de calor######

#criar colunas de cor#
cols <- c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a",
          "#e29421", "#e29421", "#f05336", "#ce472e")


#maior probabilidade de transi??o#
niketrans2 <- nikeMark.1$transition_matrix
niketrans2[is.na(niketrans2)] <- 0
t <- max(niketrans2$transition_probability)
colnames(niketrans2) <- c('origem','destino','probabilidade')


#ggplot da transio 1#
#ggplot da transi??o#
ggplot(niketrans2 , aes(y = origem, x = destino, fill = probabilidade)) +
  theme_minimal() +
  geom_tile(colour = "white", width = .9, height = .9) +
  scale_fill_gradientn(colours = cols, limits = c(0, t),
                       breaks = seq(0, t, by = t/4),
                       labels = c("0", round(t/4*1, 2), round(t/4*2, 2), round(t/4*3, 2), round(t/4*4, 2)),
                       guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_text(aes(label = round(probabilidade, 2)), fontface = "bold", size = 4) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain")) +
  ggtitle("Mapa da calor de matriz de transição dos canais")

#logit#


#novos dados#

NikeRep10.1 <- NikeRep10  %>%
  group_by(nike_user_id) %>%
  summarise(visualizacao = sum(pageviews),
            compras = sum(orders),
            gasto = sum(revenue))


NikeRep10.1 <- NikeRep10.1[-1,]

#subset#

NikeRep10.1$gastoUn <- NikeRep10.1$gasto/NikeRep10.1$compras
NaoNulo  <- NikeRep10.1$gastoUn != 0
NikeRep10.2 <- subset(NikeRep10.1,NaoNulo)





#MQO##

plot(x = NikeRep10.2$visualizacao,
     y = NikeRep10.2$gastoUn)

attach(Nike10.2)


#cross validation#
grau <- 1:5
cv.error10 = rep(0,5)
for(g in grau) {
  Nike.MQO=glm(gastoUn~poly(visualizacao, g),
              data = NikeRep10.2)
  cv.error10[g] = cv.glm(NikeRep10.2,
                         Nike.MQO,
                         K = 10)$delta[1]         
}


plot(grau, cv.error10, type = "b")

cv.error10

#MQ0#

Nike.MQO.1 <- glm(gastoUn ~ poly(visualizacao,2),
                  data = NikeRep10.2)

hist(Nike.MQO.1$residuals)


coeftest(Nike.MQO.1)

#funcao mqo#
mqo <- function(x) {
  (509.6367 -1122.7125*(x) + 312.0199*(x)^2)
}

Fit <- Nike.MQO.1$fitted.values
Visualização <- NikeRep10.2$visualizacao
Gasto_Uni    <- NikeRep10.2$gastoUn

MqO_teste <- cbind(Fit,Visualização,Gasto_Uni)

write.xlsx(as.data.frame(MqO_teste), 
           file = "C:/Users/Admin/Desktop/Projetos/AtribuicaoCanal/MqO_teste.xlsx",
           sheetName = "Sheet1",
           col.names = T,
           row.names = T,
           append = F) 
#lasso#


#separar treino e teste#
amostra <- sample.int(n = nrow(NikeRep10.1),
                      size = floor(.75*nrow(NikeRep10.1)),
                      replace = F)

TreinoLasso <- NikeRep10.1[amostra,]
TesteLasso  <- NikeRep10.1[-amostra,]

x = model.matrix(gastoUn~poly(visualizacao,9),TreinoLasso)
y = TreinoLasso$gastoUn

xTest = model.matrix(gastoUn~poly(visualizacao,9),TesteLasso)
yTest = TesteLasso$gastoUn

#Lambda#
grid = 10^seq(10,-2,length = 100)

#modelo#
Mod.Lasso <- glmnet(x,y, alpha = 1, lambda = grid, nfolds = 5)
plot(Mod.Lasso)

#seleção do menor lambda#
lambdaNike <- Mod.Lasso$lambda
which.min(lambdaNike)

#modelo2#
Mod.Lasso2 <- glmnet(x,y, alpha = 1, lambda = 100, nfolds = 5)
PredLasso_Niek   <- predict(Mod.Lasso2, s = 100, newx = xTest)


#Logit#

NikeRep10.1$convert <- ifelse(NikeRep10.1$compras >= 1,1,0) 

LogitNike <- glm(formula = convert ~  visualizacao,
                data = NikeRep10.1,
                family = binomial("logit"))


summary(LogitNike)
#funcao de probabilidade#
p <- function (x) {
  (exp(-3.82 + x*0.00005142))/(1 + exp(-3.82 + x*0.00005142))
}


Visualizacao <- order(NikeRep10.1$visualizacao)
Prob         <- p(Visualizacao)


LogitNike <- cbind(Prob,Visualizacao)
plot(x=Visualizacao,y = Prob)


#exportacao de 
LogitNike <- cbind(Prob,Visualizacao)
write.xlsx(as.data.frame(LogitNike), 
           file = "C:/Users/Admin/Desktop/Projetos/AtribuicaoCanal/LogitNike.xlsx",
           sheetName = "Sheet1",
           col.names = T,
           row.names = T,
           append = F) 
