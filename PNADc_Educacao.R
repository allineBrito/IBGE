library(tidyverse)
library(PNADcIBGE)
library(survey) #uso e modelos de dados de pesquisas amostrais (com pesos pós-estratificados e correção de variância)
library(srvyr)
library(dplyr)
library(tidyverse) # manipulação de dados
library(convey)
library(dineq) # regressões RIF
library(anamco)

## conjunto inicial de dados/ # Variáveis a serem lidas 
dadosPNADc <- get_pnadc(year = 2021, quarter = 4, 
                        vars = c("Ano","Trimestre","UF","UPA","Estrato","V1027","V1028","V1029","posest",
                                 "V1008","V1014","V1022","V2001","V2003","V2005","V2007","V2009","V2010",
                                 "VD2002","VD2003","VD3004","VD3005","VD3006","VD4001","VD4002","VD4003",
                                 "VD4005","VD4008","VD4009","VD4010","VD4016","VD4017","VD4019",
                                 "VD4020","VD4031","VD4035","VD4036","VD4037", "VD4015", "VD2003", "VD2004", "V3001",  "VD3005", 
                                 "V3002", "V3009A", "V3003A"))

options(scipen = 999) 

##caracteristas da pop por raça cor e sexo
totalsexo <- svymean(~V2007, dadosPNADc, na.rm = T) #proporção da população por sexo
pop_raca_ou_cor <- svytotal(x=~interaction(V2010, V2007), design=dadosPNADc, na.rm=TRUE)



#faixa_etaria
Faixaetaria0_15 <- svytotal(x=~interaction(V2007, V2009%in% c(0:15)), design=dadosPNADc, na.rm=TRUE)
Faixaetaria15_29 <- svytotal(x=~interaction(V2007, V2009%in% c(15:29)), design=dadosPNADc, na.rm=TRUE)
Faixaetaria30_59 <- svytotal(x=~interaction(V2007, V2009%in% c(30:59)), design=dadosPNADc, na.rm=TRUE)
Faixaetaria60oumais <- svytotal(x=~interaction(V2007, V2009%in% c(60:100)), design=dadosPNADc, na.rm=TRUE)

#Condição no domicílio
Família_chefiada_pela_mãe2 <- svytotal(~V2005, dadosPNADc, na.rm = T)

Família_chefiada_pela_mãe_sexo <- svytotal(~interaction(V2005, V2007), dadosPNADc, na.rm = T, type='html')


##educação

#Não sabe ler e escreber por sexo e raça ou cor
svytotal(~V3001=="Não"|V2010=="Parda"|V2010=="Preta" | V2007=="Mulher" , dadosPNADc, na.rm= T)
svyby(~V3001=="Não"|V2010=="Parda"| V2010=="Preta" |V2007=="Mulher", ~UF, dadosPNADc, svytotal, na.rm= T, vartype = "cv")

svytotal(~V3001, subset(dadosPNADc, V2009 >= 15 & V2009 <= 29), na.rm=T)
svytotal(~V3001=="Não", subset(dadosPNADc, V2009 > 60), na.rm=T)


#crianças matriculadas em creches
svytotal(~V3002=="Sim", subset(dadosPNADc, V2009 >= 4 & V2009 <= 5), na.rm=T)
svyby(~V3002=="Sim",~V2009 >= 4 & V2009 <= 5, subset(dadosPNADc, V2010=="Preta"|V2010=="Parda"), svymean, na.rm=T)

svytotal(~V3002, subset(dadosPNADc, V2009 >= 0 & V2009 <= 3), na.rm=T) #zero ??
svyby(~V3002=="Sim",~V2009 >= 0 & V2009 <= 3, subset(dadosPNADc, V2010=="Preta"|V2010=="Parda"), svymean, na.rm=T) #zero ??


#superior
svytotal(~V3003A=="Superior - graduação" , dadosPNADc, na.rm=T)

svytotal(~V3003A=="Superior - graduação", subset(dadosPNADc, 
                                                 V2009 >= 15 & V2009 <= 29 |V2010=="Parda"|V2010=="Preta" | V2007=="Mulher"), na.rm=T)
grau <- svytotal(~interaction(V2009 >= 15 & V2009 <= 29, V3003A), dadosPNADc, na.rm = T)


#renda por grau de instrução
mediaRendaEduc = svyby(~VD4020, ~VD3004, dadosPNADc, svymean, na.rm=T)
mediaRendaEduc %>%
  ggplot(aes(x=VD4020, y=VD3004))+
  geom_bar(stat='identity', colour='lightblue', fill='lightblue')



##trablho e renda
mediaRendaUFRaça <- svyby(~VD4020, ~V2010, dadosPNADc, svymean, na.rm = TRUE)
mediaRendaUFRaça %>%
  ggplot(aes(x=VD4020, y=V2010))+
  geom_bar(stat='identity', colour='lightblue', fill='lightblue')





