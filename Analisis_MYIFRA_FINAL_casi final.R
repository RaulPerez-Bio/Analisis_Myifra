#Paquetes instalados----

install.packages("FactoMineR")
install.packages("factoextra")
install.packages("ggplot2")
install.packages("doBy")
install.packages("MVTests")
install.packages("MASS")
install.packages("klaR")
install.packages("dplyr")
install.packages("caTools")
install.packages("randomForest")

#DIGITALIZACION DE DATA MYIFRA----

getwd()
setwd("D:/DISCO D/Tesis_M_fraseri/Analisis_Data_MYIFRA_R/Data")
data <-read.csv("DATA_MYIFRA_FINAL.csv",)

#PREPARACION DE BASE DE DATOS-----

library(dplyr)
library(doBy)

data <- data[-c(294,295,298,403),c(1:39)]
data1 <- data[data$edad%in%c("A","D"),c(3,5,8,11,25:31,35)]
sapply(data1,class)
data1$codigo <- as.factor(data1$codigo)
data1$edad <- as.factor(data1$edad)
data1$ala <- as.numeric(data1$ala)
data1$cola <- as.numeric(data1$cola)

summarise_all(data1,funs(sum(is.na(.))))

#Matriz de correlacion de Pearson ----

round(cor(data1[,5:11],use = "na.or.complete" ),3)

#Regresion lineal----

lm(cola ~ ala + sexo,data = data1)
data1$cola<-ifelse(is.na(data1$cola),
                   round(5.4113+(data1$ala*0.8241)
                         + ifelse(data1$ala=="H", -0.3445, 0.1808),0),
                   data1$cola)

lm(peso ~ ala + sexo,data = data1)
data1$peso<-ifelse(is.na(data1$peso), 
                   round( 1.02202+(data1$ala*0.18754)
                          + ifelse(data1$sexo=="H", 0.10556, 0.05525),1),
                   data1$peso)

lm(tarso ~ ala + sexo,data = data1)
data1$tarso <- ifelse(is.na(data1$tarso),
                      round(15.60461 + (data1$ala * 0.10964)
                    + ifelse(data1$sexo == "H", -0.06456, -0.07254), 1),
                      data1$tarso)

lm(l_pico ~ peso + sexo,data1)
data1$l_pico<-ifelse(is.na(data1$l_pico),
                     round(7.09965+(data1$peso*0.11385)
                           + ifelse(data1$sexo=="H", -0.09162, -0.04763),1),
                     data1$l_pico)

lm(a_pico ~ cola + sexo,data1)
data1$a_pico<-ifelse(is.na(data1$a_pico),
                    round(4.126833+(data1$cola*-0.001561)
                  + ifelse(data1$sexo=="H", 0.045559,0.129566),1),
                    data1$a_pico)

lm(n_pico ~ a_pico + sexo,data1)
data1$n_pico<-ifelse(is.na(data1$n_pico),
                     round( 4.6911+(data1$a_pico*0.2108)
                   + ifelse(data1$sexo=="H", -0.1240, -0.1108),1),
                     data1$n_pico)

summaryBy(ala + cola + tarso + l_pico + a_pico + n_pico + peso ~ sexo,
          FUN=c(mean,sd),data1)

data1 <-read.csv("data1.csv",)

data2 <- data1[data1$sexo%in%c("H","M"),]
data2 <- data2[data2$anho%in%c("2019"),]

#.......................................................
#ANALISIS DE COMPONENTES PRINCIPALES (PCA)----
#.......................................................

library(FactoMineR)

pca <- PCA(data2[,c(5:11)],scale.unit = TRUE, ncp = 7)

#EXPLICACION DE LOS VALORES DEL PCA ----

#-Eigenvalores----

round(pca$eig,2) 

#-Loading----

round(pca$var$coord,3)

#Valores de Calidad de la representación y de Contribución---- 
#de las variables

round(pca$var$cos2,3)

round(pca$var$contrib,2)

#-Grafico de sedimentacion de la contribucion ---- 

library(ggplot2)
library(factoextra)
library(gridExtra)

#Crear etiquetas para el grafi_sedi

etiquetas <- data.frame(variables = names(pca$var$contrib[,1]),
                        contribucion = pca$var$contrib[,1])
etiquetas <- etiquetas[order(etiquetas$contribucion, 
                             decreasing = TRUE), ]

#Grafi_sedi
grafi_sedi<-fviz_contrib(pca, choice = "var", 
                         axes = 1, top = 7,
                         title ="") + 
            ylab("Contribución (%) de las variables en Dim 1") +
            xlab(" ")+
            theme_classic() + 
            scale_y_continuous(expand = c(0, 0, 0, 0), limits = c(0,31)) +
            coord_cartesian(ylim = c(0, NA)) + 
            geom_text(data = etiquetas, 
            aes(x = 1:7, y = contribucion, 
                label = round(contribucion, 2)),
                  size = 3, vjust = -1)


#Crear etiquetas para el grafi_Sedi2

etiquetas2 <- data.frame(variables = names(pca$var$contrib[,2]),
                        contribucion = pca$var$contrib[,2])
etiquetas2 <- etiquetas2[order(etiquetas2$contribucion, 
                               decreasing = TRUE), ]

#Grafi_Sedi2
grafi_sedi2<-fviz_contrib(pca, choice = "var", axes = 2, top = 7,
                         title ="") +
              ylab("Contribución (%) de las variables en Dim 2") + 
              xlab(" ") +
              theme_classic() +
              scale_y_continuous(expand = c(0, 0, 0, 0), limits = c(0,62)) +
              coord_cartesian(ylim = c(0, NA)) +
              geom_text(data = etiquetas2, 
                        aes(x = 1:7, y = contribucion, 
                        label = round(contribucion, 2)),
                          size = 3, vjust = -1)

# Organizar los dos gráficos en una cuadrícula

grid <- grid.arrange(grafi_sedi, grafi_sedi2, ncol = 2)

#Exportar grafico 

ggsave("D:/DISCO D/Tesis_M_fraseri/Analisis_Data_MYIFRA_R/Plots/grafi_sedi.png",
       grid, width = 6, height = 4, dpi = 3000)

#-Circulo de correlacion----

circulo_cor<-fviz_pca_var(pca, col.var ="contrib",
             col.circle = "#000000",
             gradient.cols = c( "#006100","#FFFF00", "#FF2600"),
             repel = T,
             title ="", 
             labelsize = 4,
             labeldistance = 950,) +
  labs(color = "Contribución") + 
  theme_light()


#Exportar grafico

ggsave("D:/DISCO D/Tesis_M_fraseri/Analisis_Data_MYIFRA_R/Plots/circulo_cor.png",
       circulo_cor, width = 6, height = 5, dpi = 3000, bg= "white")

#-Biplot----

biplot<-fviz_pca_biplot(pca, col.ind = "#9ec0da", 
                col.var = "contrib", 
                gradient.cols = c("#006100", "#FFFF00", "#FF2600"),
                title = " ", adj=.5,
                repel = F,) + 
  theme_light()


#Exportar grafico

ggsave("D:/DISCO D/Tesis_M_fraseri/Analisis_Data_MYIFRA_R/Plots/biplot.png",
       biplot, width = 6, height = 6, dpi = 3000,  bg= "white")

#...............................................
# data para el ANALISIS DICRIMINATE------------
#...............................................

data2 <- data2[,-c(8:10,12)]

sapply(data2,class)

data2$sexo<-as.factor(data2$sexo)

data2.m <- data2[c(37,12,6,18,40,9,34,8,21,7,41,16),]#ultima prueba

data2.m <- data2[data2$sexo%in%c("M"),][sample(nrow(data2[data2$sexo%in%c("M"),]), size=12), ]

data19 <-anti_join(data2[data2$sexo%in%c("M"),], data2.m,)

data24 <- rbind(data2[data2$sexo%in%c("H"),],data2.m)

#Visualizar promedios y desviacion estandar----

resumen<-summaryBy(ala + cola + tarso + peso ~ sexo,
          FUN=c(mean,sd, min, max,length), data2,)

cbind(sexo=resumen[,1],round(resumen[,2:3],0),
      tarso.mean=round(resumen[,4],1),peso.mean=round(resumen[,5],1),
      round(resumen[,6:21],2))


#prueba de Shapiro para evaluar la normalidad ---- 
# de cada variable en funcion al sexo

library(reshape2)
library(knitr)

datos_tidy <- melt(data24[,4:8], value.name = "valor")
datos_tidy %>% group_by(variable, sexo) %>%
  summarise(p_value_Shapiro.test = round(shapiro.test(valor)$p.value,3))

#Prueba Mbox para medir la homogeneidad de covarianzas----

library(MVTests)

BoxM(data= as.matrix((data24[,5:8])), group = data24$sexo)


#Analisis Discrimante----

library(MASS) 
library(ggplot2)


ADlineal <- lda(sexo ~ ala + cola + tarso + peso,
                data = data24)

round(ADlineal$scaling,2)

ADcuad <- qda(sexo ~ ala + cola + tarso + peso
              , data = data24)

round(ADcuad$scaling,2)

# Elaborar grafico de barras
 
plot(ADlineal, image.colors = c("darkgoldenrod1", "skyblue2"))

ldapred <- predict(ADlineal, data24)
df <- as.data.frame(ldapred$x)
df$sexo <- data24$sexo
histograma.lda<-ggplot(df, aes(x = LD1, fill = factor(sexo))) + 
              geom_histogram(position = "dodge", alpha = 1) +
              xlab("LAD") +
              ylab(" ") +
              theme_classic(base_size = 18) +
              theme(legend.position = "top") +
              guides(fill = guide_legend(title = "Sexo")) +
              scale_y_continuous(limits = c(0,5), 
                                 breaks = seq(0,5,1),expand = c(0, 0))+
              scale_x_continuous(limits = c(-3,3), 
                                 breaks = seq(-3,4,1),expand = c(0, 0))

ggsave("D:/DISCO D/Tesis_M_fraseri/Analisis_Data_MYIFRA_R/Plots/histograma.lda.png",
      histograma.lda, width = 6, height = 4, dpi = 3000, bg= "white")

#Analisis discrimante usando la validacion cruzada----

ADlinealcv <- lda(sexo ~ ala + cola + tarso + peso,
                  CV= TRUE,data = data24)

100*round(ADlinealcv$posterior,2)

ADcuadcv <- qda(sexo ~ ala + cola + tarso + peso,
                CV= TRUE,data = data24)

100*round(ADcuadcv$posterior,2)

#matriz de confusion

table(data24$sexo, ADlinealcv$class,
      dnn = c("Sexo observado", "Sexo predicho"))

table(data24$sexo, ADcuadcv$class,
      dnn = c("Sexo observado", "Sexo predicho"))

#matriz de confusion en porcentaje

N.M <- length(data24$sexo[data24$sexo == "M"])
N.H <- length(data24$sexo[data24$sexo == "H"])

round(100*table(data24$sexo, ADlinealcv$class,
                dnn = c("Sexo observado", "Sexo predicho"))/c( N.H, N.M),2)

round(100*table(data24$sexo, ADcuadcv$class,
                dnn = c("Sexo observado", "Sexo predicho"))/c( N.H, N.M),2)

#graficos de analisis discriminante----

library(klaR)

partimat( sexo ~ ala + cola + tarso + peso, data = data24, 
          method = "lda", nplots.vert = 2, 
          nplots.hor = 3, main= " ",image.colors = c("darkgoldenrod1", "skyblue2"),
          col.mean = "firebrick") 

partimat( sexo ~ ala + cola + tarso + peso,
          data = data24, method = "qda",nplots.vert = 2, 
          nplots.hor = 3, main= " ", image.colors = c("darkgoldenrod1", "skyblue2"),
          col.mean = "firebrick")

#Prueba de capacidad de prediccion con individuos sexados----

#-10 individuos mal sexados----

library(caTools)

sapply(data19, class)

muestra.data10 <- sample.split(data19$sexo,SplitRatio = .50)

data10.msex <- subset(data19,muestra.data10== F)

predl10 <-predict(ADlineal,newdata = data10.msex)
predcd10<-predict(ADcuad,newdata = data10.msex)

round(100*predl10$posterior,1)
round(100*predcd10$posterior,1)

#matriz de confusion para ver como se clasificaron

table(data10.msex$sexo, predl10$class,
      dnn = c("Sexo observado", "Sexo predicho"))

table(data10.msex$sexo, predcd10$class,
      dnn = c("Sexo observado", "Sexo predicho"))

N.M1 <- length(data10.msex$sexo[data10.msex$sexo == "M"])
N.H1 <- length(data10.msex$sexo[data10.msex$sexo == "H"])

round(100*table(data10.msex$sexo, predl10$class,
                dnn = c("Sexo observado", "Sexo predicho"))/c( N.H1,N.M1),2)

round(100*table(data10.msex$sexo, predcd10$class,
                dnn = c("Sexo observado", "Sexo predicho"))/c( N.H1, N.M1),2)

#19 indviduos mal sexados-----

data19

predl19 <-predict(ADlineal,newdata = data19)
predcd19 <-predict(ADcuad,newdata = data19)

round(100*predl19$posterior,1)
round(100*predcd19$posterior,1)

table(data19$sexo,predl19$class,
      dnn = c("sexo observado","sexo predicho"))

table(data19$sexo,predcd19$class,
      dnn = c("sexo observado","sexo predicho"))

N.M2 <- length(data19$sexo[data19$sexo == "M"])
N.H2 <- length(data19$sexo[data19$sexo == "H"])

round(100*table(data19$sexo, predl19$class,
                dnn = c("Sexo observado", "Sexo predicho"))/c( N.H2,N.M2),2)

round(100*table(data19$sexo, predcd19$class,
                dnn = c("Sexo observado", "Sexo predicho"))/c( N.H2, N.M2),2)


#Prueba de capacidad de prediccion en datos de museo-----

data.mu <- data1[data1$codigo%in%c("U"),c(1:7,11,12)]
data.mu<- subset(data.mu,anho!="2019")

sapply(data.mu,class)

data.mu$sexo<- as.factor(data.mu$sexo)

predl.mu<-predict(ADlineal,newdata= data.mu)
predcd.mu<-predict(ADcuad,newdata=data.mu)

round(100*predl.mu$posterior,1)
round(100*predcd.mu$posterior,1)

table(data.mu$sexo,predl.mu$class,
      dnn = c("sexo observado","sexo predicho"))

table(data.mu$sexo,predcd.mu$class,
      dnn = c("sexo observado", "sexo predicho"))

N.M.mu <- length(data.mu$sexo[data.mu$sexo == "M"])
N.H.mu <- length(data.mu$sexo[data.mu$sexo == "H"])
N.D.mu <- length(data.mu$sexo[data.mu$sexo == "D"])

round(100*table(data.mu$sexo,predl.mu$class,
                dnn = c("Sexo observado", "Sexo predicho"))/c(N.D.mu, N.H.mu,N.M.mu),2)

round(100*table(data.mu$sexo,predcd.mu$class,
                dnn = c("Sexo observado", "Sexo predicho"))/c(N.D.mu, N.H.mu, N.M.mu),2)

#Adultos desconocidos del 2019----

data.a19 <- data1[data1$sexo%in%c("D"),c(1:7,11,12)]
data.a19 <- data.a19[data.a19$edad%in%c("A"),]

predl.a19<-predict(ADlineal,newdata= data.a19)
predcd.a19<-predict(ADcuad,newdata=data.a19)

round(100*predl.a19$posterior,1)
round(100*predcd.a19$posterior,2)

table(data.a19$sexo,predl.a19$class,
      dnn = c("sexo observado","sexo predicho"))

table(data.a19$sexo,predcd.a19$class,
      dnn = c("sexo observado", "sexo predicho"))

N.D <- length(data.a19$sexo[data.a19$sexo == "D"])

round(100*table(data.a19$sexo,predl.a19$class,
                dnn = c("Sexo observado", "Sexo predicho"))/N.D,2)

round(100*table(data.a19$sexo,predcd.a19$class,
                dnn = c("sexo observado", "sexo predicho"))/N.D,2)

#lambda de Wilks----

y<-cbind(data24$ala,data24$cola,data24$peso,data24$tarso ) 

manova.res <- manova(y ~ sexo, data = data24)

summary(manova.res, test = "Wilks")

#BOSQUE ALEATORIO (METODO NO PARAMETRICO) ----

library(randomForest)

sapply(data.a19,class)

data.a19$sexo <- as.factor(data.a19$sexo)
data24.rf<-data24[,-c(1:3)]

rf <- randomForest(sexo ~ ., data=data24.rf)


pred <-predict(rf, newdata=data.a19)

data19.rf<-cbind(data.a19[,1:4],pred,data.a19[,5:9])
options(max.print =10000)

table(data.a19[,4], pred,
           dnn = c("sexo observado","sexo predicho"))

N.RF <- length(pred)

round(100*table(data.a19[,4], pred,
                dnn = c("sexo observado","sexo predicho"))/N.RF,2)

#Comparacion...

table(pred,predl.a19$class,
      dnn = c("bosque aleatorio","lineal"))

table(pred,predcd.a19$class,
      dnn = c("bosque aleatorio","cuadratic"))

round(100*table(pred,predl.a19$class,
      dnn = c("bosque aleatorio","lineal"))/c(N.RF,N.D),2)

round(100*table(pred,predcd.a19$class,
      dnn = c("bosque aleatorio","cuadratic"))/c(N.RF,N.D),2)

#FIN-----

#Graficos de barras----
str(data)
data.p<-data[data$edad%in%c("P"),c(5,14,22:24,33,34)]
data.p<-data.p[data.p$codigo_ciclo%in%c("FCF"),]
sapply(data.p,class)
summary(data.p)
data.p$osificacion <- gsub("D", 9, data.p$osificacion)
data.p$mes<- as.numeric(data.p$mes)
data.p$osificacion<- as.numeric(data.p$osificacion)
summarise_all(data.p,funs(sum(is.na(.))))



library(ggplot2)

ggplot(data.p, aes(x=mes, y=osificacion, fill=osificacion)) +
  geom_col(position="fill", width=0.6) +
  labs(x="Mes", y="Porcentaje", fill="Tipo") +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_fill_manual(values=c("#29516f","#3e627d","#53738b",
                             "#69859a","#7e96a8","#94a8b7",
                             "#a9b9c5","#becad3","#d4dce2")) 
 

data_pct <- data.p %>% group_by(mes) %>%
  mutate(pct = 100 * osificacion / sum(osificacion))%>%
  mutate(osificacion = factor(osificacion, levels = c(1:7)))
  
ggplot(data_pct, aes(x = mes, y = pct, fill = factor(osificacion))) +
  geom_col(position = "fill", width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(),expand = c(0,0)) +
  scale_x_discrete(limits = unique(data_pct$mes), 
                     labels = c("Mar", "Abr", "May", "Jun", "Ago", "Sep", "Nov", "Dic")) +
  labs(x = "Mes", y = "Porcentaje de osificación", fill = "Tipo") +
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))



