# 4ta parte:
### VARIABLES AMBIENTALES, MATRICES DE DISTANCIA, MANTEL ####

# cargo bases de datos ambientales:
# datos charcos físicos
Ambientales_xch_insitu<-read_csv("ambientales_charcos/Ambientales_xch_insitu.csv")
var_ambient_fisicas<-as.data.frame(Ambientales_xch_insitu)
str(var_ambient_fisicas)
tail(var_ambient_fisicas)
colnames(var_ambient_fisicas)
var_ambient_fisicas<-var_ambient_fisicas[,-c(4,5)]
is.na.data.frame(var_ambient_fisicas)
which(is.na.data.frame(var_ambient_fisicas)==TRUE) # muchos NA, ver que hacer!
table(var_ambient_fisicas[,c(1,2)]) # veo cual es el mes con mayor hidroperiodo, i.e: con mas charcos activos
# idealmente el mes con mas charcos activos coincide con el mes de mayor registro de individuos(por charcos)/especies(por um por charco) 
# tengo que quedarme con el mismo mes del cual hice los analisis de estructura de la red
# 2006 coinciden los meses: mes 6
# 2007 coinciden los meses: mes 8
# 2008 coinciden los meses: mes 8
# 2009 coinciden los meses: mes 7
# 2012 coinciden los meses: mes 7

# subsetear por año, y después se puede hacer un colmeans, o similar para obtener un promedio de las variables por año
# tb se puede hacer una disimilaridad entre anos, o un CV de los colmeans, como proxy de heterogeneidad ambiental
var_ambient_fisicas_x_ano_lista<-hace_riq_x_ano(var_ambient_fisicas, colum.anos = 1)

#funcion que identifica el mes para cada año con mayor charcos activos (que coincide con los de mayores registros usados para el resto de los analisis de estructura), y que sea con el mes que seleccione para hacer las matrices
hace_bases_1mes_x_ano<-function(base, lista.base.anos, anos=1,mes=2){
  tabla<-table(base[,c(anos,mes)])
  out<-list()
  for (i in 1:length(rownames(tabla))) {
    out[[i]]<-subset(lista.base.anos[[i]],lista.base.anos[[i]][,2]==as.numeric(colnames(tabla)[which(tabla[i,]==max(tabla[i,]))]))
  }
  names(out)<-names(lista.base.anos)
  out
}
var_ambient_fisicas_x_ano_lista_1mes<-hace_bases_1mes_x_ano(var_ambient_fisicas,var_ambient_fisicas_x_ano_lista,anos = 1,mes = 2)

#_________________#

for (i in 1:length(var_ambient_fisicas_x_ano_lista_1mes)) { # saco columna 1 y 2, que es la columna con el ano y el mes
  var_ambient_fisicas_x_ano_lista_1mes[[i]]<-var_ambient_fisicas_x_ano_lista_1mes[[i]][,-c(1,2)]
}

# extrae el dato de la cantidad de charcos activos como una variable de hidroperiodo, complementaria a la cantidad de lluvias, etc.
Ncharcos<-lapply(var_ambient_fisicas_x_ano_lista_1mes, nrow)
Ncharcos<-unlist(Ncharcos)
# optimizar etsa funcion ya que puede que el charco aparezca, pero si prof media ==0, sacar
# subseteo cada base sacando charcos con prof media==0 y ahi cuento el nrow

hace_var_fisicas_mediasxano<-function(lista,charcos=1){
  out<-NULL
  for (i in 1:length(lista)) {
  out.t<-colMeans(lista[[i]][,-charcos], na.rm = TRUE)
  out<-rbind(out,out.t)
    }
  rownames(out)<-names(lista)
  out
}

var_fisicas_mediasxano<-hace_var_fisicas_mediasxano(var_ambient_fisicas_x_ano_lista_1mes)
colMeans(var_fisicas_mediasxano, na.rm = TRUE) # esto no es nada
summary(var_fisicas_mediasxano) # tampoco nada

# proxy de heterogeneidad por ano- matriz de dist. ch x ch por ano:
# 1. obtener un promedio...general de la matriz
hace_heterog_metacom_xano<-function(lista){
  require(vegan)
  out<-NULL
  out.1<-list()
  out.2<-vector()
  for (i in 1:length(lista)) {
    lista[[i]]<-as.matrix(lista[[i]])
    row.names(lista[[i]])<-NULL
    row.names(lista[[i]])<-lista[[i]][,1]
    lista[[i]]<-lista[[i]][,5:12] #2005 y 2006, tiene NA en las 1eras 3 variables, genera sesgo en comparacion con el resto, donde si son inclu'idas...si las dejo igual sacar columna 1 que es el nombre de los charcos
    #lista[[i]]<-lista[[i]][,-1]
    out.1[[i]]<-as.matrix(vegdist(lista[[i]],method = "bray",na.rm = TRUE))
    out.2[i]<-mean(out.1[[i]][upper.tri(out.1[[i]])],na.rm=TRUE)
  }
  names(out.1)<-names(lista)
  names(out.2)<-names(lista)
  out.1
  out.2
  out<-list("heterogeneidad"=out.2,"matri_dist_bray"=out.1)
  out
}

distancia_ch_xano<-hace_heterog_metacom_xano(var_ambient_fisicas_x_ano_lista_1mes)
heterogeneidad<-distancia_ch_xano$heterogeneidad # que tan heterogeneo es cada ano en relacion a que tan parecidos o diferentes son las caarct fisicas de los charcos

# proxy de heterogeneidad alternativo: heterogeneidad_2
# sd y coef. de variacion de las variables f'isicas en cada ano
for (i in 1:length(var_ambient_fisicas_x_ano_lista)) { # saco columna 1, que es la columna de los nombres de los charcos 
  var_ambient_fisicas_x_ano_lista[[i]]<-var_ambient_fisicas_x_ano_lista[[i]][,-1]
}

hace_heterog_metacom_xano_2<-function(lista_sub.bases,ch=1){
  require(EnvStats)
  cv_sin_na<-function(x) cv(x, na.rm = TRUE)
  sd_sin_na<-function(x) sd(x, na.rm = TRUE)
  out.sd<-matrix(NA,nrow = length(lista_sub.bases),ncol = ncol(lista_sub.bases[[5]][,-ch]))
  rownames(out.sd)<-names(lista_sub.bases); colnames(out.sd)<-colnames(lista_sub.bases[[5]][,-ch])
  out.cv<-matrix(NA,nrow = length(lista_sub.bases),ncol = ncol(lista_sub.bases[[5]][,-ch]))
  rownames(out.cv)<-names(lista_sub.bases); colnames(out.cv)<-colnames(lista_sub.bases[[5]][,-ch])
  for (i in 1:length(lista_sub.bases)) {
    #for (j in 1:ncol(lista_sub.bases[[i]])) {
      out.sd[i,]<-apply(lista_sub.bases[[i]][,-ch], 2, sd_sin_na)
      out.cv[i,]<-apply(lista_sub.bases[[i]][,-ch], 2, cv_sin_na)
      #out.cv[i,]<-cv(lista_sub.bases[[i]][,j], na.rm = TRUE)
    #}
  }
#out.cv
#out.sd
out<-list(out.sd, out.cv)
names(out)<-c("desvio","coef. de var.")
out
}

var_fisicas_heterogxano_2<-hace_heterog_metacom_xano_2(var_ambient_fisicas_x_ano_lista_1mes)
View(var_fisicas_heterogxano_2$`coef. de var.`)

# fisicas estandarizada (centrada y ds=1)
fisicas.st<-scale(var_fisicas_mediasxano,scale=TRUE)# otra forma: decostand(var.meteor_x_ano,MARGIN = 2,"standardize")
summary(fisicas.st) # media=0 y ds=1
# exploracion ambientales
pairs(na.omit(fisicas.st),pch=19, upper.panel = panel.cor, diag.panel = panel.hist)
cor(na.omit(fisicas.st))
# saco de la base las variables muy correlacionadas

# OTRAS FORMAS DE OBTENER UN PROXY DE HETEROGENEDIAD POR ANO:
# promedio de los coef de var de las variables de cada sub-bases por ano?
# tiene sentido hacer el desvio y el cv de la matriz de distancia? 
# seria como que tanto se agrupan (dispersan en torno a la media), porque puede que la media sea baja, pero en verdad que haya dos grandes grupos de charcos parecidos entre ellos

# 2. ? 

# ACP, para describir y bien puedo retener el PC1
ACP_var.fisicas<-prcomp(na.omit(fisicas.st[,-c(1,2,6,10)])) #saco las var. correlacionadas
summary(ACP_var.fisicas) # por lo que veo del out, en todo caso em convendria retener PC1 y PC2
resum_var.fisicas<-ACP_var.fisicas$x[,1:2]
biplot(ACP_var.fisicas)
abline(v=0,col="dark green", lty=8); abline(h=0,col="dark green", lty=8)

#_____________________________________________________________________________
# datos meteorologicos
var_met_1_2002_7_2020 <- read_csv("ambientales_charcos/var.met._1.2002_7.2020.csv")
var_meteorologicas <-as.data.frame(var_met_1_2002_7_2020)
str(var_meteorologicas)
for (i in 2:9) {
  #var_meteorologicas[,i]<-as.numeric(na.omit(var_meteorologicas[,i]))
  var_meteorologicas[,i]<-as.numeric(var_meteorologicas[,i])
}

library(zoo);library(lubridate)
yrmo <- as.yearmon(var_meteorologicas[,1], "%Y-%m")
year(as.Date(as.Date (yrmo),format="%y/%m/%d"));month(as.Date(as.Date (yrmo),format="%y/%m/%d"))
var_meteorologicas<-cbind(year(as.Date(as.Date (yrmo),format="%y/%m/%d")),var_meteorologicas[,-1])
head(var_meteorologicas)
colnames(var_meteorologicas)<-c("ano",colnames(var_meteorologicas[2:9]))
# base var. meteo con los meses para hacer promedios diferenciales por estacion, etc
var_meteorologicas_c.meses<-cbind(var_meteorologicas[,1],month(as.Date(as.Date (yrmo),format="%y/%m/%d")),var_meteorologicas[,-1])
colnames(var_meteorologicas_c.meses)[1:2]<-c("ano","meses")
head(var_meteorologicas_c.meses)

# hacer promedios, desvio y CV por ano
hace_estadisticos_meteo_xano<-function(base){
  require(EnvStats)
  anos<-unique(base[,1])
  out<-NULL
  for (i in 1:length(anos)) {
    a<-subset(base, base[,1]==unique(base[,1])[i])
    out.t<-colMeans(na.omit(a[,-1]))
    out.t2<-apply(na.omit(a[,-1]),MARGIN = 2,sd)
    names(out.t2)<-paste("sd",colnames(base[,-1]),sep = "_")
    out.t3<-apply(na.omit(a[,-1]),MARGIN = 2,cv)
    names(out.t3)<-paste("cv",colnames(base[,-1]),sep = "_")
    out.f<-c(out.t,out.t2,out.t3)
    out<-rbind(out,out.f)
  }
  rownames(out)<-anos
  out
}

var.meteor_x_ano<-hace_estadisticos_meteo_xano(var_meteorologicas)
head(var.meteor_x_ano)
tail(var.meteor_x_ano)

# ambiental estandarizada (centrada y ds=1)
ambient.st<-scale(var.meteor_x_ano[,1:7],scale=TRUE)# otra forma: decostand(var.meteor_x_ano,MARGIN = 2,"standardize")
summary(ambient.st) # media=0 y ds=1
# exploracion ambientales
pairs(ambient.st,pch=19, upper.panel = panel.cor, diag.panel = panel.hist)
cor(ambient.st)
# saco de la base las variables Tmax_media y Tmin_media, por estar muy correlacionadas con Tmax y Tmin respectivamente

# ACP, para describir y bien puedo retener el PC1
which((colnames(ambient.st)=="Tmax_media(ºC)")==TRUE);which((colnames(ambient.st)=="TMín_media(ºC)")==TRUE)
ACP_var.ambient<-prcomp(ambient.st[,-c(4,6)])
summary(ACP_var.ambient) # por lo que veo del out, en todo caso me convendria retener PC1 y PC2
resum_var.ambient<-ACP_var.ambient$x[,1:2]
biplot(ACP_var.ambient)

#____________________________
# VARIABLES BIÓTICAS QUE DAN CUENTA DE LAS CONDICIONES: RIQUEZA Y BIOMASA
bm_todas <- read_csv("bm_todas.csv")
bm_todas<-as.data.frame(bm_todas)

# biomasa total y coef. de var por año
hace_biom_xano<-function(base,colum.anos=1, col.biom=6){
    out<-data.frame()
    biom.total<-vector()
    n.muestras<-vector()
    biom.promedio<-vector()
    biom.sd<-vector()
    biom.max<-vector()
    biom.cv<-vector()
    base<-base[which(is.na(bm_todas[,col.biom])==FALSE),]
    for (i in 1:length(unique(na.omit(base[,colum.anos])))) {
      sub.base<-subset(base,base[,colum.anos]==(unique(na.omit(base[,colum.anos])))[i])
      n.muestras[i]<-nrow(sub.base)
      biom.total[i]<-sum(sub.base[,col.biom])
      biom.promedio[i]<-mean(sub.base[,col.biom])
      biom.sd[i]<-sd(sub.base[,col.biom])
      biom.max[i]<-max(sub.base[,col.biom])
      biom.cv[i]<-(sd(sub.base[,col.biom])/mean(sub.base[,col.biom])) #*100, tb podr'ia haber usado la fun del EnvStats
    }
    out<-cbind(n.muestras,biom.total,biom.promedio,biom.sd,biom.max,biom.cv)
    rownames(out)<-unique(na.omit(base[,colum.anos]))
    out
  }
var.biomasa<-hace_biom_xano(bm_todas)

# riqueza total y coef. de var por año
lapply(lapply(matri_abund_sppxch_lista , rowSums), function(x) which(x==0)) # compruebo que el nro de filas de las matrices matri_abund_sppxch_lista se corresponde con la riqueza total, ya que todas las spp tienen al menos un individuo
unlist(lapply(lapply(matri_abund_sppxch_lista , rowSums), function(x) which(x==0)))
Riq.total_x_ano<-unlist(lapply(matri_abund_sppxch_lista , nrow)) # otra forma: unlist(lapply(matri_abund_sppxch_lista , function(x) dim(x)[1] ))

# coeficiente de variacion de la riqueza por ano?? otener riqueza por charco, hacer el vector y estimar promedio anual y sd, y cv

var.biom_riq<-cbind(var.biomasa[,-c(1:2)],Riq.total_x_ano)
cor(var.biom_riq)
biom_riq.st<-scale(var.biom_riq,scale=TRUE)

# se podr'ia hacer una matriz de distancia ch x ch por ano para ver que tan heterogeneos son los 
# charcos en cuanto a riqueza y biomasa y ver si esta matriz se correlaciona con la matriz de distancia en condiciones ambientales fisicas!!

#____________________________
# variables alternativas:
# 1. Eficiencia en el Uso de la precipitación (EUP) como un marco integrador del clima y la respuesta de la vegetación: 
# (Relaciones entre la precipitación, producción de biomasa e índices espectrales de la vegetación: alcances y limitaciones. Fernando Paz Pellat y Heriberto Díaz Solís)
# EUP= Bm/P -> precipitación (P), total o promedio, y la biomasa aérea anual Bm (equivalente a la producción primaria neta anual)
# años que vienen de años atrás de sequía, puede que les cueste más recuperarse, y por lo tanto presentar menor EUP 

EUP<-var.biomasa[,3]/var.meteor_x_ano[4:19,1]
hist(EUP,breaks = 15)
matplot(rownames(var.biomasa),cbind(scale(EUP,scale=TRUE),scale(var.meteor_x_ano[4:19,1],scale=TRUE)))

# ETo expresa el poder evaporante de la atmósfera en una localidad y época del año específicas, y no considera ni las características del cultivo, ni los
# factores del suelo. Desde este punto de vista, el método FAO Penman-Monteith se recomienda como el único método de determinación de ETo con parámetros climáticos. 
# el valor de ETo es medio fijo en las regiones, no sé si espero que varie mucho entre años
# es muuuy complejo de estimar, ídem balance hídrico.

# 3. ver fotos satelitales y determinar un índice de cobertura anual?? 

####_______FIN 4ta parte________###
# Doctorado - ultima modificacion: 30 abril 2021