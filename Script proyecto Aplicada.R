
#----------------------------------------------------------#
# Proyecto Aplicada
#----------------------------------------------------------#

# Load packages
suppressMessages(library(tidyverse))
suppressMessages(library(FactoMineR))
suppressMessages(library(factoextra))
suppressMessages(library(foreign))
suppressMessages(library(corrplot))
suppressMessages(library(polycor))
suppressMessages(library(psych))
suppressMessages(library(gplots))
suppressMessages(library(viridis))
suppressMessages(library(lsr))
suppressMessages(library(DescTools))
suppressMessages(library(nlme))
suppressMessages(library(MASS))
suppressMessages(library(multilevel))
suppressMessages(library(DescTools))
suppressMessages(library(plspm))
suppressMessages(library(reshape))

#----------------------------------------------------------#
# Cargar Datos
#----------------------------------------------------------#

setwd("/Users/cesar.saavedra/Desktop/Proyecto\ Aplicada")
Datos<-read.csv("Encuesta.csv",header = T,";")

names(Datos)

bloques <- Datos %>% dplyr::select(P11:P30)
for(j in 1:ncol(bloques)){
  bloques[,j] <- factor(bloques[,j], levels = c("Totalmente de acuerdo",
                                                "De acuerdo",
                                                "Ni de acuerdo Ni en desacuerdo",
                                                "En desacuerdo",
                                                "Totalmente en desacuerdo"), ordered = T)
}; rm(j)

bloques2 <- Datos %>% dplyr::select(P6:P10)
for(j in 1:ncol(bloques)){
  bloques[,j] <- factor(bloques[,j], levels = c("Poco importante",
                                                "Nada importante",
                                                "Ni poco importante Ni importante",
                                                "Importante",
                                                "Muy importante"), ordered = T)
}; rm(j)


#----------------------------------------------------------#
# Factor Social
#----------------------------------------------------------#
PilarA1 <- Datos %>% dplyr::select(P11:P18)
PilarA1 %>% glimpse

fqTable <- PilarA1 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(PilarA1))
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("Totalmente de acuerdo",
                                                          "De acuerdo",
                                                          "Ni de acuerdo Ni en desacuerdo",
                                                          "En desacuerdo",
                                                          "Totalmente en desacuerdo"), ordered = T)
fqTable$Variable <- factor(fqTable$Variable, levels = c("P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18",
                                                        ordered = T))


gg <- fqTable %>% ggplot(aes(x = Categoria, y = Porcentaje*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ Variable) +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
show(gg)
ggsave("/Users/cesar.saavedra/Desktop/Proyecto\ Aplicada/Resultados/frecuencias_factor_social.png", 
       plot = gg, width = 22, height = 10, units = "in"); rm(fqTable, gg)

options(warn=-1)
p.chisq1 = matrix(0, nrow=ncol(PilarA1), ncol=ncol(PilarA1), byrow=T)
for(i in 1:ncol(PilarA1)){
  for(j in 1:ncol(PilarA1)){
    p.chisq1[i,j] = (chisq.test(PilarA1[,i],PilarA1[,j])$p.value)
  }
}; rm(i); rm(j)

diag(p.chisq1) = NA
colnames(p.chisq1) = colnames(PilarA1)
rownames(p.chisq1) = colnames(PilarA1)

color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
png(file = '/Users/cesar.saavedra/Desktop/Proyecto\ Aplicada/Resultados/chi_square_test_factor_social.png', 
    height = 7, width = 7, units = "in", res = 300)
heatmap.2(p.chisq1,
          main="Factor Social",
          key.title="Chi-square test",
          key.xlab="p-value",
          Rowv=F,
          Colv=NULL,
          col=color_scale,
          linecol=NULL,
          tracecol=NULL,
          density.info="density",
          denscol="blue",
          margins=c(11,11))

PilarA2 <- Datos %>% dplyr::select(P6:P10)
PilarA2 %>% glimpse

fqTable <- PilarA2 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(PilarA1))
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("Poco importante",
                                                          "Nada importante",
                                                          "Ni poco importante Ni importante",
                                                          "Importante",
                                                          "Muy importante"), ordered = T)
fqTable$Variable <- factor(fqTable$Variable, levels = c("P6", "P7", "P8", "P9", "P10",
                                                        ordered = T))

gg12 <- fqTable %>% ggplot(aes(x = Categoria, y = Porcentaje*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ Variable) +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
show(gg12)
ggsave("/Users/cesar.saavedra/Desktop/Proyecto\ Aplicada/Resultados/frecuencias_factor_social1.png", 
       plot = gg1.1, width = 22, height = 10, units = "in"); rm(fqTable, gg1.1)

options(warn=-1)
p.chisq12 = matrix(0, nrow=ncol(PilarA2), ncol=ncol(PilarA2), byrow=T)
for(i in 1:ncol(PilarA2)){
  for(j in 1:ncol(PilarA2)){
    p.chisq12[i,j] = (chisq.test(PilarA2[,i],PilarA2[,j])$p.value)
  }
}; rm(i); rm(j)

diag(p.chisq12) = NA
colnames(p.chisq12) = colnames(PilarA2)
rownames(p.chisq12) = colnames(PilarA2)

color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
png(file = '/Users/cesar.saavedra/Desktop/Proyecto\ Aplicada/Resultados/chi_square_test_factor_social1.png', 
    height = 7, width = 7, units = "in", res = 300)
heatmap.2(p.chisq12,
          main="Factor Social",
          key.title="Chi-square test",
          key.xlab="p-value",
          Rowv=F,
          Colv=NULL,
          col=color_scale,
          linecol=NULL,
          tracecol=NULL,
          density.info="density",
          denscol="blue",
          margins=c(11,11))

#----------------------------------------------------------#
# Factor Politico
#----------------------------------------------------------#
PilarB1 <- Datos %>% dplyr::select(P20:P27)
PilarB1 %>% glimpse

fqTable <- PilarB1 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(PilarB1))
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("Totalmente de acuerdo",
                                                          "De acuerdo",
                                                          "Ni de acuerdo Ni en desacuerdo",
                                                          "En desacuerdo",
                                                          "Totalmente en desacuerdo"), ordered = T)
fqTable$Variable <- factor(fqTable$Variable, levels = c("P20", "P21", "P22", "P23", "P24", "P25", "P26","P27",
                                                        ordered = T))

gg1 <- fqTable %>% ggplot(aes(x = Categoria, y = Porcentaje*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ Variable) + 
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
show(gg1)
ggsave("/Users/cesar.saavedra/Desktop/Proyecto\ Aplicada/Resultados/frecuencias_factor_politico.png", 
       plot = gg1, width = 22, height = 10, units = "in"); rm(fqTable, gg1)

options(warn=-1)
p.chisq2 = matrix(0, nrow=ncol(PilarB1), ncol=ncol(PilarB1), byrow=T)
for(i in 1:ncol(PilarB1)){
  for(j in 1:ncol(PilarB1)){
    p.chisq2[i,j] = (chisq.test(PilarB1[,i],PilarB1[,j])$p.value)
  }
}; rm(i); rm(j)

diag(p.chisq2) = NA
colnames(p.chisq2) = colnames(PilarB1)
rownames(p.chisq2) = colnames(PilarB1)

color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
png(file = '/Users/cesar.saavedra/Desktop/Proyecto\ Aplicada/Resultados/chi_square_test_factor_politico.png', 
    height = 7, width = 7, units = "in", res = 300)
heatmap.2(p.chisq2,
          main="Factor Politico",
          key.title="Chi-square test",
          key.xlab="p-value",
          Rowv=F,
          Colv=NULL,
          col=color_scale,
          linecol=NULL,
          tracecol=NULL,
          density.info="density",
          denscol="blue",
          margins=c(11,11))

#----------------------------------------------------------#
# Factor Economico
#----------------------------------------------------------#
PilarC1 <- Datos %>% dplyr::select(P28:P30)
PilarC1 %>% glimpse

fqTable <- PilarC1 %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(PilarB1))
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("Totalmente de acuerdo",
                                                          "De acuerdo",
                                                          "Ni de acuerdo Ni en desacuerdo",
                                                          "En desacuerdo",
                                                          "Totalmente en desacuerdo"), ordered = T)
fqTable$Variable <- factor(fqTable$Variable, levels = c("P28", "P29", "P30"),ordered = T)

gg3 <- fqTable %>% ggplot(aes(x = Categoria, y = Porcentaje*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ Variable)
theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
show(gg3)
ggsave("/Users/cesar.saavedra/Desktop/Proyecto\ Aplicada/Resultados/frecuencias_factor_economico.png", 
       plot = gg3, width = 22, height = 10, units = "in"); rm(fqTable, gg3)

options(warn=-1)
p.chisq3 = matrix(0, nrow=ncol(PilarC1), ncol=ncol(PilarC1), byrow=T)
for(i in 1:ncol(PilarC1)){
  for(j in 1:ncol(PilarC1)){
    p.chisq3[i,j] = (chisq.test(PilarC1[,i],PilarC1[,j])$p.value)
  }
}; rm(i); rm(j)

diag(p.chisq3) = NA
colnames(p.chisq3) = colnames(PilarC1)
rownames(p.chisq3) = colnames(PilarC1)

color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
png(file = '/Users/cesar.saavedra/Desktop/Proyecto\ Aplicada/Resultados/chi_square_test_factor_economico.png', 
    height = 7, width = 7, units = "in", res = 300)
heatmap.2(p.chisq3,
          main="Factor Economico",
          key.title="Chi-square test",
          key.xlab="p-value",
          Rowv=F,
          Colv=NULL,
          col=color_scale,
          linecol=NULL,
          tracecol=NULL,
          density.info="density",
          denscol="blue",
          margins=c(11,11))

#----------------------------------------------------------#
# Otros
#----------------------------------------------------------#
Religion <- Datos$P1
Programa <- Datos$Programa.Acad..mico
Economica1 <- Datos$P31

ggplot(data=Datos, aes(Religion, fill=Programa)) + geom_bar(position="dodge") + theme(axis.title.x = element_text(face="bold", size=10)) + 
  labs(title = "Distribucion religiosa por programa academico") + theme(plot.title = element_text(size = rel(2), colour = "black")) + 
  xlab("Religion") + ylab ("Programa academico")

ggplot(data=Datos, aes(Economica1, fill=Programa)) + geom_bar(position="dodge") + theme(axis.title.x = element_text(face="bold", size=10)) + 
  labs(title = "") + theme(plot.title = element_text(size = rel(2), colour = "black")) + 
  xlab("Religiones") + ylab ("Programa academico")

Filosofia <- subset(Datos, Datos$Programa.Acad..mico=="Filosofia")
LFilosofia <- subset(Datos, Datos$Programa.Acad..mico=="Licenciatura Filosofia")
TSocial <- subset(Datos, Datos$Programa.Acad..mico=="Trabajo Social")

Fsocial <- Datos %>% dplyr::select(P11:P18)
Fpolitico <- Datos %>% dplyr::select(P20:P27)
Feconomico <- Datos %>% dplyr::select(P28:P30)

#----------------------------------------------------------#
#Factores Filosofia
#----------------------------------------------------------#
Factorsocial <- Filosofia %>% dplyr::select(P11:P18)
Factorsocial %>% glimpse

qTable <- Factorsocial %>%
  gather(measure, value) %>%
  count(measure, value)
names(qTable) <- c("Variable", "Categoria", "Frecuencia")
qTable <- qTable %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(Factorsocial))
qTable$Categoria <- factor(qTable$Categoria, levels = c("Totalmente de acuerdo",
                                                        "De acuerdo",
                                                        "Ni de acuerdo Ni en desacuerdo",
                                                        "En desacuerdo",
                                                        "Totalmente en desacuerdo"), ordered = T)
qTable$Variable <- factor(qTable$Variable, levels = c("P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18",
                                                      ordered = T))

gg4 <- qTable %>% ggplot(aes(x = Categoria, y = Porcentaje*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ Variable) + 
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
show(gg4)
ggsave("/Users/cesar.saavedra/Desktop/Proyecto\ Aplicada/Resultados/filosofia_factor_social.png", 
       plot = gg4, width = 22, height = 10, units = "in"); rm(fqTable, gg4)

Factorpolitico <- Filosofia %>% dplyr::select(P20:P27)
Factorpolitico %>% glimpse

qTable <- Factorpolitico %>%
  gather(measure, value) %>%
  count(measure, value)
names(qTable) <- c("Variable", "Categoria", "Frecuencia")
qTable <- qTable %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(Factorpolitico))
qTable$Categoria <- factor(qTable$Categoria, levels = c("Totalmente de acuerdo",
                                                          "De acuerdo",
                                                          "Ni de acuerdo Ni en desacuerdo",
                                                          "En desacuerdo",
                                                          "Totalmente en desacuerdo"), ordered = T)
qTable$Variable <- factor(qTable$Variable, levels = c("P20", "P21", "P22", "P23", "P24", "P25", "P26","P27",
                                                      ordered = T))

gg5 <- qTable %>% ggplot(aes(x = Categoria, y = Porcentaje*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ Variable) + 
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
show(gg5)
ggsave("/Users/cesar.saavedra/Desktop/Proyecto\ Aplicada/Resultados/filosofia_factor_politico.png", 
       plot = gg5, width = 22, height = 10, units = "in"); rm(fqTable, gg5)

Factoreconomico <- Filosofia %>% dplyr::select(P28:P30)
Factoreconomico %>% glimpse

qTable <- Factoreconomico %>%
  gather(measure, value) %>%
  count(measure, value)
names(qTable) <- c("Variable", "Categoria", "Frecuencia")
qTable <- qTable %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(Factoreconomico))
qTable$Categoria <- factor(qTable$Categoria, levels = c("Totalmente de acuerdo",
                                                        "De acuerdo",
                                                        "Ni de acuerdo Ni en desacuerdo",
                                                        "En desacuerdo",
                                                        "Totalmente en desacuerdo"), ordered = T)
qTable$Variable <- factor(qTable$Variable, levels = c("P28", "P29", "P30",
                                                      ordered = T))

gg6 <- qTable %>% ggplot(aes(x = Categoria, y = Porcentaje*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ Variable) + 
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
show(gg6)
ggsave("/Users/cesar.saavedra/Desktop/Proyecto\ Aplicada/Resultados/filosofia_factor_economico.png", 
       plot = gg6, width = 22, height = 10, units = "in"); rm(fqTable, gg6)

#----------------------------------------------------------#
#Factores Licenciatura Filosofia
#----------------------------------------------------------#
Factorsocial1 <- LFilosofia %>% dplyr::select(P11:P18)
Factorsocial1 %>% glimpse

qTable <- Factorsocial1 %>%
  gather(measure, value) %>%
  count(measure, value)
names(qTable) <- c("Variable", "Categoria", "Frecuencia")
qTable <- qTable %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(Factorsocial1))
qTable$Categoria <- factor(qTable$Categoria, levels = c("Totalmente de acuerdo",
                                                        "De acuerdo",
                                                        "Ni de acuerdo Ni en desacuerdo",
                                                        "En desacuerdo",
                                                        "Totalmente en desacuerdo"), ordered = T)
qTable$Variable <- factor(qTable$Variable, levels = c("P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18",
                                                      ordered = T))

gg7 <- qTable %>% ggplot(aes(x = Categoria, y = Porcentaje*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ Variable) + 
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
show(gg7)
ggsave("/Users/cesar.saavedra/Desktop/Proyecto\ Aplicada/Resultados/lfilosofia_factor_social.png", 
       plot = gg7, width = 22, height = 10, units = "in"); rm(fqTable, gg7)

Factorpolitico1 <- LFilosofia %>% dplyr::select(P20:P27)
Factorpolitico1 %>% glimpse

qTable <- Factorpolitico1 %>%
  gather(measure, value) %>%
  count(measure, value)
names(qTable) <- c("Variable", "Categoria", "Frecuencia")
qTable <- qTable %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(Factorpolitico1))
qTable$Categoria <- factor(qTable$Categoria, levels = c("Totalmente de acuerdo",
                                                        "De acuerdo",
                                                        "Ni de acuerdo Ni en desacuerdo",
                                                        "En desacuerdo",
                                                        "Totalmente en desacuerdo"), ordered = T)
qTable$Variable <- factor(qTable$Variable, levels = c("P20", "P21", "P22", "P23", "P24", "P25", "P26","P27",
                                                      ordered = T))

gg8 <- qTable %>% ggplot(aes(x = Categoria, y = Porcentaje*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ Variable) + 
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
show(gg8)
ggsave("/Users/cesar.saavedra/Desktop/Proyecto\ Aplicada/Resultados/lfilosofia_factor_politico.png", 
       plot = gg8, width = 22, height = 10, units = "in"); rm(fqTable, gg8)

Factoreconomico1 <- LFilosofia %>% dplyr::select(P28:P30)
Factoreconomico1 %>% glimpse

qTable <- Factoreconomico1 %>%
  gather(measure, value) %>%
  count(measure, value)
names(qTable) <- c("Variable", "Categoria", "Frecuencia")
qTable <- qTable %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(Factoreconomico1))
qTable$Categoria <- factor(qTable$Categoria, levels = c("Totalmente de acuerdo",
                                                        "De acuerdo",
                                                        "Ni de acuerdo Ni en desacuerdo",
                                                        "En desacuerdo",
                                                        "Totalmente en desacuerdo"), ordered = T)
qTable$Variable <- factor(qTable$Variable, levels = c("P28", "P29", "P30",
                                                      ordered = T))

gg9 <- qTable %>% ggplot(aes(x = Categoria, y = Porcentaje*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~ Variable) + 
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
show(gg9)
ggsave("/Users/cesar.saavedra/Desktop/Proyecto\ Aplicada/Resultados/lfilosofia_factor_economico.png", 
       plot = gg9, width = 22, height = 10, units = "in"); rm(fqTable, gg9)


#----------------------------------------------------------#
#Factores Trabajo Social
#----------------------------------------------------------#



#----------------------------------------------------------#
#Sacar la base por programa academico
j=c()
for (i in 1:length(Datos$Programa.Acad..mico)) {
  if(Datos$Programa.Acad..mico[i]=="Filosofia"){
    j=c(j,i)
  }
}
BaseFilosofia=Datos[j,]
j=c()
for (i in 1:length(Datos$Programa.Acad..mico)) {
  if(Datos$Programa.Acad..mico[i]=="Licenciatura Filosofia"){
    j=c(j,i)
  }
}
BaseLicFilosofia=Datos[j,]
j=c()
for (i in 1:length(Datos$Programa.Acad..mico)) {
  if(Datos$Programa.Acad..mico[i]=="Trabajo Social"){
    j=c(j,i)
  }
}
BaseTSocial=Datos[j,]


#----------------------------------------------------------#
# Alfa de Cronbach 
#----------------------------------------------------------#
library(Rcmdr)
suppressMessages(library(nlme))
suppressMessages(library(MASS))
suppressMessages(library(multilevel))


Datos1<-read.csv("Encuesta2.csv",header = T,";")

#----------------------------------------------------------#
# Alfa Cuestionario
#----------------------------------------------------------#
reliability(cov(Datos1[,c("P6","P7","P8","P9","P10","P11","P12","P13","P14",
                          "P15","P16","P17","P18","P20","P21","P22","P23","P24","P25","P26","P27",
                          "P28","P29","P30")], use="complete.obs"))

#----------------------------------------------------------#
# ALfa por factores
#----------------------------------------------------------#

# Alfa Social
Social <- Datos1 %>% dplyr::select(P6:P10, P11:P18)
reliability(cov(Social[,c("P6","P7","P8","P9","P10","P11",
                               "P12","P13","P14","P15","P16","P17","P18")], use="complete.obs"))

# Alfa Politica
Politica <- Datos1 %>% dplyr::select(P20:P27)
reliability(cov(Politica[,c("P20","P21","P22","P23","P24","P25","P26","P27")], use="complete.obs"))

#Alfa Economia
Economia <- Datos1 %>% dplyr::select(P28:P30)
reliability(cov(Economia[,c("P28","P29","P30")], use="complete.obs"))

#----------------------------------------------------------#


