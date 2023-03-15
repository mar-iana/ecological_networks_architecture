# subir cada una de las bases de datos, por predador, transponer, agregar columna de año y columna de depredador
# rbind todas las bases...

# idealemnte hay muestra de ergagópilas de todos los depredadores tomadas las mismas fechas, 
# la columna de especies presas esta compuesta por los mismas especies y en el mismo orden
# funciones que pueden ser útiles: match y %in%

### subo todas las bases de datos,, la edito según función "hace_base.chinchillas"
# antes modifico base de pseudalopex, porque a diferencia de las anteriores tiene datos de semillas tambien
# le tengo que sacar en vez de 2 filas, 4.
# y las pego por filas...
# luego con modifciación de función co.g y decidiendo como unificar fechas, 
# genero las redes troficas (predador x presa) para cada año (saco de las redes de cada ano las presas con abundancia 0 en todas las columnas) 

hace_base.chinchillas<-function(base_original){
  library("lubridate")
  M<-as.data.frame(base_original)
  out.t<-NULL
  fecha<-vector()
  sitio<-vector()
    for (i in 6:ncol(M)) {
    matriz<-M[-c(1:3),c(2,5,i)] # aca es donde modifique a raiz de que modifique las bases originales y a raiz de que voy a incluir la clase en el out que genero
    rep(colnames(M)[i],nrow(matriz))->fecha
    rep(M[3,i],nrow(matriz))->sitio
    rep(deparse(substitute(base_original)),nrow(matriz))->predador
    subbase<-as.matrix(cbind(fecha,predador,sitio,matriz))
    #subbase<-as.matrix(cbind(fecha,sitio,matriz))
    out.t<-rbind(out.t,subbase)
  }
  out.t<-cbind(year(as.Date(out.t[,1],format="%d/%m/%y")),month(as.Date(out.t[,1],format="%d/%m/%y")), out.t)
  #out.t<-as.data.frame(out.t)
  colnames(out.t)<-c("ano","mes","fecha","predador","sitio","clase","especie","abundancia")
  row.names(out.t)<-1:nrow(out.t)
  out.t
}


Pseudalopex_sp_base.modif<-Pseudalopex_sp[-c(3,4),]

hace_base.chinchillas(Athene_cunicularia)->Athene_cunicularia_df
hace_base.chinchillas(Bubo_virginianus)->Bubo_virginianus_df
hace_base.chinchillas(Falco_sparverius)->Falco_sparverius_df
hace_base.chinchillas(Glaucidium_nanum)->Glaucidium_nanum_df
hace_base.chinchillas(Pseudalopex_sp_base.modif)->Pseudalopex_sp_df
hace_base.chinchillas(Tyto_alba)->Tyto_alba_df
#hace_base.chinchillas(Dusicyon_sp)->Dusicyon_sp_df # estas bases al final no las uso, porque estos predadores aparecieron solo en un ano (91-92)
#hace_base.chinchillas(Dusicyon_sp2)->Dusicyon_sp2_df

# ver algo asi, para dividir de marzo a febrero
kk<-ifelse (as.numeric(as.character(Athene_cunicularia_df.2$mes))<3,-1,0)
kkk<-as.numeric(as.character(Athene_cunicularia_df.2$ano))+kk

# UNIR EN UNA LISTA LAS BASES DE CADA DEPREDADOR, MODIFICADAS POR FUNCION hace_base.chinchillas
lista_incidencia_predadores<-list(Athene_cunicularia_df,Bubo_virginianus_df,Falco_sparverius_df,Glaucidium_nanum_df,Pseudalopex_sp_df,Tyto_alba_df)#Dusicyon_sp_df,Dusicyon_sp2_df,
names(lista_incidencia_predadores)<-c("Athene_cunicularia_df","Bubo_virginianus_df","Falco_sparverius_df","Glaucidium_nanum_df","Pseudalopex_sp_df","Tyto_alba_df")#"Dusicyon_sp_df","Dusicyon_sp2_df",
length(lista_incidencia_predadores)
class(lista_incidencia_predadores[[1]][,1])

# antes de dividir por ano, tengo que tener en cuenta que el ano "meteorologico" va de marzo a febrero,
# de alguna forma tengo que indicar eso para poder subdividir de marzo a feb de cada ano y ano+1

redefine_ano_marz.feb<-function(lista_bases_predadores, ano=1, mes=2){
  out<-list()
  for (i in 1:length(lista_bases_predadores)) {
    ano_segun_mes<-as.numeric(as.character(lista_bases_predadores[[i]][,ano]))+
      (ifelse(as.numeric(as.character(lista_bases_predadores[[i]][,mes]))<3,-1,0))
    out[[i]]<-cbind(ano_segun_mes,lista_bases_predadores[[i]])
    colnames(out[[i]])[1]<-"ano_c"
  }
  names(out)<-names(lista_bases_predadores)
  out
}

lista_incidencia_predadores_ano.correct<-redefine_ano_marz.feb(lista_incidencia_predadores)

hace_presas_agregadas_por_ano<-function(lista_bases_predadores,ab=9,ano=1,spp=8,pred=5){ #ab, es la columna donde estan las abundancias de las presas en cada obs
  library("dbplyr")
  out<-list()
    for (i in 1:length(lista_bases_predadores)) {
   base.temp<-cbind(lista_bases_predadores[[i]][,-ab],as.numeric(as.character(lista_bases_predadores[[i]][,ab])))
   out[[i]]<-aggregate(as.numeric(na.omit(base.temp[,ab])) ~ base.temp[,1] + base.temp[,spp]+ base.temp[,pred], base.temp,sum)
   colnames(out[[i]])<-c("ano","presa","predador","abundancia")  
   }
  names(out)<-names(lista_bases_predadores)
  out
}
  
#rep(deparse(substitute(base_original)),nrow(matriz))->predador
#lista_xpred_agregada_xano<-hace_presas_agregadas_por_ano(lista_incidencia_predadores)
lista_xpred_agregada_xano<-hace_presas_agregadas_por_ano(lista_incidencia_predadores_ano.correct)
head(lista_xpred_agregada_xano[[1]])
base_presas.predadores_agregada_unif<-rbind(lista_xpred_agregada_xano[[1]],lista_xpred_agregada_xano[[2]],lista_xpred_agregada_xano[[3]],lista_xpred_agregada_xano[[4]],lista_xpred_agregada_xano[[5]],lista_xpred_agregada_xano[[6]])#,lista_xpred_agregada_xano[[7]],lista_xpred_agregada_xano[[8]])
head(base_presas.predadores_agregada_unif)
dim(base_presas.predadores_agregada_unif)
# saco de la base las presas con abundancia 0
base_presas.predadores_agregada_unif<-base_presas.predadores_agregada_unif[-(which(base_presas.predadores_agregada_unif$abundancia==0)),]
unique(base_presas.predadores_agregada_unif$predador) # 6 predadores
unique(base_presas.predadores_agregada_unif$ano) # 28
length(unique(base_presas.predadores_agregada_unif$presa)) #108 presas
which(base_presas.predadores_agregada_unif$presa=="Tyto_alba")
#_________________

#función que divide en tantas bases por año y las une en una lista
hace_riq_x_ano<-function(base,vector.anos,columna=1){
  out<-list()
  for (i in 1:length(vector.anos)) {
    out[[i]]<-subset(base,base[,columna]==vector.anos[i])
  }
  names(out)<-vector.anos
  out
}

todos_los_anos<-as.numeric(as.character(unique(base_presas.predadores_agregada_unif$ano)))
todos_los_anos<-todos_los_anos[order(todos_los_anos)]
base_incid_lista_x_ano<-hace_riq_x_ano(base_presas.predadores_agregada_unif,todos_los_anos,1)
length(base_incid_lista_x_ano)

#ESTIMACION DE RED DE TROFICA METODO: 
# modificacion funcion ab_comm 
hace_presas_x_pred<-function(M, spp_en=2, comm_en=3, ab_en=4){ #comm es el predador
  comm<-sort(unique(M[,comm_en]))
  spp<-sort(unique(M[,spp_en]))
  out<-matrix(NA,length(spp),length(comm))
  colnames(out)<-comm
  rownames(out)<-spp
  for(i in spp){
    for(j in comm){
      out[i,j]<-ifelse(length(which(M[,spp_en]==i & M[,comm_en]==j))!=0,(M[which(M[,spp_en]==i & M[,comm_en]==j),ab_en]),0)
    }
  }
  out
}

#hace_presas_x_pred<-function(M, spp_en=2, comm_en=3, ab_en=4){ #comm es el predador
 # comm<-sort(unique(M[,comm_en]))
  #spp<-sort(unique(M[,spp_en]))
  #out<-matrix(NA,length(spp),length(comm))
  #colnames(out)<-comm
  #rownames(out)<-spp
  #for(i in spp){
   # for(j in comm){
    #  out[i,j]<-ifelse(M[which(M[,spp_en]==i & M[,comm_en]==j)!=named integer(0),kk[,ab_en])
      #    }
  #}
  #out
#}

hace_presas_x_pred(base_incid_lista_x_ano$`1987`)->kk

hace_lista_presas_x_pred<-function(lista_bases_x_ano){
  out<-list()
  for (i in 1:length(lista_bases_x_ano)) {
    out[[i]]<-hace_presas_x_pred(lista_bases_x_ano[[i]],spp_en=2, comm_en=3, ab_en=4)
      }
names(out)<-names(lista_bases_x_ano)
  out
  }

presas_x_pred_lista<-hace_lista_presas_x_pred(base_incid_lista_x_ano)
length(presas_x_pred_lista)
#presas_x_pred_lista<-presas_x_pred_lista[-29] #esto si trabajo con los anos como son en verdad

#saca_ab.zero<-function(lista_redes_x_ano){
 # out<-list()
  #for (i in 1:length(lista_redes_x_ano)){
   # out[[i]]<-lista_redes_x_ano[[i]][-which(rowSums(lista_redes_x_ano[[i]])==0),]
    #        }
  #names(out)<-names(lista_redes_x_ano)
  #out
  #}

#presas_x_pred_lista<-saca_ab.zero(presas_x_pred_lista)

###________________________________________________________

# para evaluar tamano de las matrices, ya que 2015, tiene un unico predador, por lo tanto sacar esa matriz
para_saber_dimensiones<-function(lista_con_matrices){
  out<-list()
  for (i in 1:length(lista_con_matrices)) {
    out[[i]]<-dim(lista_con_matrices[[i]])
  }
  names(out)<-names(lista_con_matrices)
  out
}

#veo que matrices tienen un tamano (promedio filas y columnas) menor a 10
#...para decidir si las saco o no
para_saber_dimensiones(presas_x_pred_lista)->dim_matrices
#dim_matrices<-unlist(dim_matrices)
dim_matrices
min(dim_matrices)

#para saber nro total de links (edges)
lapply(presas_x_pred_lista_bin, sum)

## arma matrices pesadas nulas, paquete del vegan nullmodels, hay que ver cómo separarlas, porque el out
# es un gran data frame que hay que ir cortando por columna...

hace_matrices_nulas_W<-function(lista.redes,nsimul=2000,burn=500,thiny=500){
  #detach("package:bipartite", unload=TRUE)
  library(vegan)
  out.1<-vector()
  out.2<-list()
  out.3<-list()
  for (i in 1:length(lista.redes)) {
    out.1[i]<-dim(lista.redes[[i]])[2]
    modelo<-nullmodel(lista.redes[[i]], "quasiswap_count")#out.3[[i]]
    simulacion<-simulate(modelo, nsim=nsimul, burnin=burn, thin=thiny)
    out.2[[i]]<-as.data.frame(simulacion) #tiene pegadas las matrices nulas en columnas, con el nrow=nrow de red original y ncol=ncol original * nsimul
    }
    out.1
    out.2
   for (j in 1:length(out.2)) {
          cat("va en la red: ", j, "\n")  
      out.3[[j]]<-split.default(out.2[[j]], rep(1:nsimul, each=out.1[j]))
    }
    out.3
    names(out.3)<-names(lista.redes)
    out.3
}

matrices_nulas_W<-hace_matrices_nulas_W(presas_x_pred_lista)  

#compruebo si se mantiene el total de filas y columnas entre la matriz original y las n simuladas
apply(presas_x_pred_lista$`1987`,1,sum)[1]
apply(matrices_nulas_W$`1987`$`1`,1,sum)[1]
apply(matrices_nulas_W$`1987`$`2`,1,sum)[1]
apply(presas_x_pred_lista$`1987`,2,sum)[1]
apply(matrices_nulas_W$`1987`$`1`,2,sum)[1]
apply(matrices_nulas_W$`1987`$`2`,2,sum)[1]

#_______________________________________________________________________

# una vez que tengo las redes por ano, en el objeto "presas_x_pred_lista" 
# estimo distintas metricas de red y roles siguiendo script "redes_co.ocurr_plantas"

### METRCIAS ESTRUCTURALES GLOBALES DE LAS REDES X ANO ####

# 1) Conectividad, tamaño, distribución de grado, nro de componentes,distancia caracteristica, motivos
# 1.1- Distribucion de grado
library("bipartite")
hist(degree(presas_x_pred_lista$`1987`))
degreedistr(presas_x_pred_lista$`1987`)->oo
class(oo$`lower level dd fits`)
oo$`lower level dd fits`[,c(1,4)]
row.names(oo$`lower level dd fits`)
#output: tabla de valores, donde aparece el AIC de cada tipo de distribucion de grado
#lower level o higher hace referencia a las entradas de la matriz, cuando es bipartita
# voy a extraer del out el AIC de cada distribucion: "exponential","power law","truncated power law"

#leer papaer de la funcion degreedistr para ver que es lower y higher level y ver si tiene sentido solo para redes unipartitas
# Jordano, P., Bascompte, J. and Olesen, J. M. (2003)
hace_distr.grado_bipartita<-function(lista_de_redes){
  detach("package:sna", unload=TRUE)
  library(bipartite)
  library(sna)
  out<-data.frame() #matrix(NA,length(lista_redes),6)
      for (i in 1:length(lista_de_redes)) { 
    #dist.grado<-degreedistr(lista_de_redes[[i]])
    #aic.1<-dist.grado$`higher level dd fits`[,5]
    #aic.2<-dist.grado$`lower level dd fits`[,5]
    aic[i,1:6]<-c(degreedistr(lista_de_redes[[i]],plot.it = FALSE)$`higher level dd fits`[,5],degreedistr(lista_de_redes[[i]],plot.it = FALSE)$`lower level dd fits`[,5])
          }
  out<-rbind(out,aic)
rownames(out)<-names(lista_de_redes)
colnames(out)<-c("exponential_h","power law_h","truncated power law_h", "exponential_l","power law_l","truncated power law_l")
  out
  }

extrae_distr_grado_bipartita(presas_x_pred_lista)->distribucion_de_grado
distribucion_de_grado<-t(distribucion_de_grado)

# 1.2- Distancia promedio/Diametro/Tamano/Conectividad
library("igraph")
graph.incidence(presas_x_pred_lista$`1987`,directed = FALSE,weighted = TRUE)->oo
graph.incidence(presas_x_pred_lista$`1987`,directed = TRUE, mode= "out",weighted = TRUE)->o
average.path.length(oo) # dist.promedio, otra forma: mean_distance(oo)
diameter(o,weights=E(o)$weight) #diametro, si se especifica NULL, toma los pesos de que ya están en la matriz (que no es binaria)
size<-gsize(oo) #tamano
N<-dim(presas_x_pred_lista$`1987`)[1] + dim(presas_x_pred_lista$`1987`)[2] #cantidad total de nodos
C<-(sum(ifelse(presas_x_pred_lista$`1987`>0,1,0)))/(dim(presas_x_pred_lista$`1987`)[1] * dim(presas_x_pred_lista$`1987`)[2]) ##conectividad
transitivity(oo,type="global") # coeficiente de agrupamiento

# hace funcion cuyo out es una matriz de las distintas metricas de redes estimadas por año: 
# las columnas son los años y las filas las metricas de red 
hace_metricas_varias_red.trofica<-function(lista_redes.coex){
  library("igraph")
  out<-data.frame()
  out.grafo<-list()
  out.grafo.NO.dirigida<-list() #algunas metricas, ej: dist. promedio tiene sentido hacerlo con la red NO dirigida
  dist.promedio<-vector()
  diametro<-vector()
  dimension<-vector()
  conectividad<-vector()
  coef.agrupamiento<-vector()
  for (i in 1:length(lista_redes.coex)) {
    out.grafo[[i]]<-graph.incidence(lista_redes.coex[[i]],directed = TRUE, mode= "out",weighted = TRUE)
    out.grafo.NO.dirigida[[i]]<-graph.incidence(lista_redes.coex[[i]],directed = FALSE,weighted = TRUE)
    average.path.length(out.grafo.NO.dirigida[[i]])->dist.promedio[i] # distancia promedio
    diameter(out.grafo[[i]],weights=NULL)->diametro[i] # diametro
    gsize(out.grafo[[i]])->dimension[i] # nro. de links
    conectividad[i]<-(sum(ifelse(lista_redes.coex[[i]]>0,1,0)))/(dim(lista_redes.coex[[i]])[1] * dim(lista_redes.coex[[i]])[2]) ##conectividad
    transitivity(out.grafo[[i]],type="global")->coef.agrupamiento[i] # coeficiente de agrupamiento
      }
  out<-rbind(out,dist.promedio,diametro,dimension,conectividad,coef.agrupamiento)
  colnames(out)<-names(lista_redes.coex)
  rownames(out)<-c("dist.promedio","diametro","tamaño","conectividad","coef.agrupamiento")
  out
}

metricas_globales_x_ano_rt<-hace_metricas_varias_red.trofica(presas_x_pred_lista)

#1.3 CENTRALIZACION GLOBAL...
# ver bien que es lo que hace en Freeman 1979
hace_centraliz_global<-function(lista_redes){
  library("igraph")
  out<-data.frame()
  out.grafo<-list()
  #out.grafo.NO.dirigida<-list() #algunas metricas, ej: dist. promedio tiene sentido hacerlo con la red NO dirigida
  centarliz_bc<-vector()
  maximo_theo_bc<-vector()
  centarliz_clo<-vector()
  maximo_theo_clo<-vector()
  centarliz_eigen<-vector()
  maximo_theo_eigen<-vector()
  centarliz_degree<-vector()
  maximo_theo_degree<-vector()
  for (i in 1:length(lista_redes)) {
    #out.grafo[[i]]<-graph.incidence(lista_redes[[i]],directed = TRUE, mode= "out",weighted = TRUE)
    out.grafo[[i]]<-graph.incidence(lista_redes[[i]],directed = FALSE,weighted = TRUE)
    centarliz_bc[i]<-centr_betw(out.grafo[[i]], directed = TRUE)$centralization
    maximo_theo_bc[i]<-centr_betw(out.grafo[[i]], directed = TRUE)$theoretical_max
    centarliz_clo[i]<-centr_clo(out.grafo[[i]], mode = "all")$centralization
    maximo_theo_clo[i]<-centr_clo(out.grafo[[i]], mode = "all")$theoretical_max
    centarliz_eigen[i]<-centr_eigen(out.grafo[[i]], directed = TRUE)$centralization
    maximo_theo_eigen[i]<-centr_eigen(out.grafo[[i]], directed = TRUE)$theoretical_max
    centarliz_degree[i]<-centr_degree(out.grafo[[i]], mode = "all")$centralization
    maximo_theo_degree[i]<-centr_degree(out.grafo[[i]], mode = "all")$theoretical_max
  }
  out<-rbind(out,centarliz_bc, maximo_theo_bc, centarliz_clo, maximo_theo_clo, centarliz_eigen, maximo_theo_eigen, centarliz_degree, maximo_theo_degree)
  colnames(out)<-names(lista_redes)
  rownames(out)<-c("centarliz_bc", "maximo_theo_bc", "centarliz_clo", "maximo_theo_clo", "centarliz_eigen", "maximo_theo_eigen", "centarliz_degree", "maximo_theo_degree")
  out
}

centralidad_global<-hace_centraliz_global(presas_x_pred_lista)
head(centralidad_global)

# uno todo en un gran data frame...
metricas_estructura.1<-rbind(distribucion_de_grado, metricas_globales_x_ano_rt,centralidad_global)
tail(metricas_estructura.1)
#_____________________________

# 2) ANIDAMIENTO 
# pruebas
oecosimu(presas_x_pred_lista$`1987`,nestfun=nestednodf, method= "quasiswap",2000, 1000, 1000)->anid.1987 #la funcion "oecosimu" me genera la cant. de modelos nulos para la funcion que yo quiera, transforma las matrices a binarias siempre
anid.1987$oecosimu$z #valores de z (valores de una distribucion normal), para comparar con otras matrices, son tres valores de z tiene (sentido si la red fuera una red bipartita)
anid.1987$oecosimu$z[3] # 
visweb(anid.1987$statistic$comm); anid.1987$oecosimu$method; anid.1987$oecosimu$statistic #da el grado de anidamiento crudo
as.mcmc.oecosimu(oecosimu(presas_x_pred_lista$`1987`,nestfun=nestednodf, method= "quasiswap",2000, 1000, 1000)) # evalua si esta bien el nro de simul. o otros parametros?

# 2.1- ANIDAMIENTO binario PARA LA LISTA CON TODAS LAS MATRICES (oecosimu solo es binario)
hace_anid.bin_x_ano<-function(lista_redes.coex){
  require(bipartite)
  require(vegan)
  out.modelos<-list()
  out<-vector()
  for(i in 1:length(lista_redes.coex)){
    cat("va en la red: ", i, "\n")  
    out.modelos[[i]]<-oecosimu(lista_redes.coex[[i]],nestfun=nestednodf, method= "quasiswap",nsimul=2000, 500, 500)
    out.modelos[[i]]$oecosimu$z[3]->out[i] # depende si queriero el anid.col, o anid.row, o NODF el nro entre corchetes [...]
  }
  #names(out.modelos)<-names(lista_redes.coex)
  # out.modelos
  names(out)<-names(lista_redes.coex)
  out
}

anid.bin_x_ano_rt<-hace_anid.bin_x_ano(presas_x_pred_lista)
anid.bin_x_ano_rt

# 2.2- ANIDAMIENTO pesado PARA LA LISTA CON TODAS LAS MATRICES
#funcion que calcula el Znodf pesado a aprtir de las matrices originales (obs), y de las matrices nulas del objeto: matrices_nulas_W
hace_anid.W_x_ano<-function(lista.redes, lista.redes.nulas){
  anid.w<-function(x) nestednodf(x, weighted = TRUE)  
  z<-vector()
  for (i in 1:length(lista.redes)) {
    cat("va en la red: ", i, "\n")  
    out.2<- nestednodf(lista.redes[[i]], weighted = TRUE)
    anid.matrices.nulas<-lapply(lista.redes.nulas[[i]], anid.w)
    nodf.matrices.nulas <- lapply(anid.matrices.nulas, function(x) x$statistic[3])
    nodf.matrices.nulas<-unlist(nodf.matrices.nulas)
    z[i]<-(out.2$statistic[3] - mean(nodf.matrices.nulas))/sd(nodf.matrices.nulas)
  }
  names(z)<-names(lista.redes)
  z
}

anid.w_x_ano_rt_veg<-hace_anid.W_x_ano(presas_x_pred_lista, matrices_nulas_W)
anid.w_x_ano_rt_veg

# 2da version de la funcion, mas compleja, pero da lo mismo
#hace_anid.W_x_ano<-function(lista.redes, lista.redes.nulas){
 # anid.w<-function(x) nestednodf(x, weighted = TRUE)  
  #out.2<-list()
  #anid.matrices.nulas<-list()
  #nodf.matrices.nulas<-list()
  #z<-vector()
  #for (i in 1:length(lista.redes)) {
   # cat("va en la red: ", i, "\n")  
    #out.2[[i]]<- nestednodf(lista.redes[[i]], weighted = TRUE)
    #anid.matrices.nulas[[i]]<-lapply(lista.redes.nulas[[i]], anid.w)
    #nodf.matrices.nulas[[i]] <- lapply(anid.matrices.nulas[[i]], function(x) x$statistic[3])
    #nodf.matrices.nulas[[i]]<-unlist(nodf.matrices.nulas[[i]])
    #z[i]<-(out.2[[i]]$statistic[3] - mean(nodf.matrices.nulas[[i]]))/sd(nodf.matrices.nulas[[i]])
  #}
  #names(z)<-names(lista.redes)
  #z
#}
########_______________________________________________________

############## 3) MODULARIDAD
# 3.1- MODULAR (reescribir en formato .txt las matrices binarias de cada ano) 
hace_redes_binarias<-function(lista_redes.coex){
  out<-list()
  for (i in 1:length(lista_redes.coex)) {
    out[[i]]<-ifelse(lista_redes.coex[[i]]>0,1,0)
  }
  names(out)<-names(lista_redes.coex)
  out
}

presas_x_pred_lista_bin<-hace_redes_binarias(presas_x_pred_lista)

for (i in 1:length(presas_x_pred_lista_bin)) {
  write.table(presas_x_pred_lista_bin[[i]], paste(names(presas_x_pred_lista_bin)[i], ".txt", sep=""), row.names = FALSE, col.names = FALSE)
}

# hacer 2000 simulaciones, con la metrica de Barber, para redes bipartitas
# initial temeprature factor= 2 , cooling factor=1.01 , nro. of iteration factor=1 
# levantar en el R, el OUT del modular que consiste en un txt para cada ano, 
# y para lo cual tengo que establecer el directorio de donde esta el out 
setwd("/Users/mariana/Desktop/Analisis_DOC_2020/base_pred.presa_relaschinchillas/txt_para_modular/resultsSa")
# extraer la columna 3 de cada el txt de cada ano (es el valor de modularidad de cada simulacion, o sea que voy a tener tantas filas como simulaciones- ej.2000)
# ver script: levanta_out_MODULAR
out_modular<-function(vector_nombres){
  out.1<-NULL
  out.2<-NULL
  est.nulos<-data.frame()
  for (i in vector_nombres) {
    cat("va en red: ", i, "\n")    
    nombre_archivo.1<-paste("OUT_1",i,".txt", sep="")
    out.a.1<-read.table(nombre_archivo.1, header = T, dec=".")
    out.a.1<-out.a.1[,3]
    out.1<-cbind(out.1, out.a.1)
    nombre_archivo.2<-paste("OUT_2",i,".txt", sep="")
    out.a.2<-read.table(nombre_archivo.2, header = T, dec=".")
    out.a.2<-out.a.2[,3]
    out.2<-cbind(out.2, out.a.2)
  }
  colnames(out.1)<-vector_nombres
  colnames(out.2)<-vector_nombres
  media1<-apply(out.1,2,mean)
  desvio1<-apply(out.1,2,sd)
  media2<-apply(out.2,2,mean)
  desvio2<-apply(out.2,2,sd)
  est.nulos<-t(rbind(media1,desvio1,media2,desvio2))
}

MODULAR_estad.nulos_rt<-out_modular(names(presas_x_pred_lista))
mod_MODULAR_rt<-read.table("OUT_MOD.txt", header = T, dec=".")

# como el out del modular estan de cualquier orden, quiero ordenarlo para que me coincida
# con el resto de las bases, de forma creciente segun el ano
# para eso sigo los siguientes pasos:
strsplit(as.character(mod_MODULAR_rt[,1]), ".txt")->ano # me saca parte de texto de los elemntos de un vector character
unlist(strsplit(as.character(mod_MODULAR_rt[,1]), ".txt"))->ano # me transforma el out anterior en un vector en ve de una lista que por defecto te da
as.numeric(ano)->ano #transformo el vector anterior en numerico para poder ordenarlo
mod_MODULAR_rt<-cbind(ano,mod_MODULAR_rt[,-1])
mod_MODULAR_rt<-mod_MODULAR_rt[order(mod_MODULAR_rt$ano),] #de esta forma ordeno por la columna que eligo, por defecto esta funcion ordena creciente, 
hist(mod_MODULAR_rt[,3], breaks = 10)
# z modelo nulo 1 (modularidad estandarizada)
z.mod_MODULAR.1_rt<-(mod_MODULAR_rt[,3]-MODULAR_estad.nulos_rt[,1])/MODULAR_estad.nulos_rt[,2]
hist(z.mod_MODULAR.1_rt, breaks = 10)
which(z.mod_MODULAR.1_rt>2) # 0
# z modelo nulo 2
z.mod_MODULAR.2_rt<-(mod_MODULAR_rt[,3]-MODULAR_estad.nulos_rt[,3])/MODULAR_estad.nulos_rt[,4]
hist(z.mod_MODULAR.2_rt, breaks = 10)
which(z.mod_MODULAR.2_rt>2) # 0
cor.test(z.mod_MODULAR.1_rt,z.mod_MODULAR.2_rt) # 0.9911569
#z.mod_MODULAR<-cbind(z.mod_MODULAR.1,z.mod_MODULAR.2)

# 3.2- MODULARIDAD estimada con paquete igraph, funcion spinglass.community  ##
#...(VER FUNCION DEL IGRAPH WALK.COMMUNITY...)
detach("package:bipartite", unload=TRUE)
detach("package:sna", unload=TRUE)
library("igraph")
#el comando graph.incidence me lleva una matriz bipartita a un grafo
#si la matriz no es bipartita hay que usar la funcion: graph.adjacency

#EJEMPLO PARA UNA MATRIZ...
graph.incidence(presas_x_pred_lista$`2011`,directed = TRUE, mode = "out", weighted = TRUE)->kk
is.connected(kk) 
components(kk)
component_distribution(kk) 
count_components(kk) 
spinglass.community(kk)->pp #algoritmo que estima la modularidad
pp$membership #a que modulo pertenece cada nodulo
pp$modularity #te da el valor de modularidad de toda la red
#la modularidad estimada con el IGRAPH NO FUNCIONA PARA REDES DESCONECTADAS (lo que se puede hacer en este caso es trabajar con el componente de mayor tamano, pero no es el caso, ya que est'an todas conectadas)

modul_igraph<-function(lista.redes){
  #detach("package:bipartite", unload=TRUE)
  #detach("package:sna", unload=TRUE)
  require(igraph)
  out<-NULL
  out.t<-list()
  out.graph<-list()
  modul.grafo<-vector()
  num.modulos<-vector()
  for(i in 1:length(lista.redes)){
    cat("va en la red: ", i, "\n")  
    out.graph[[i]]<-graph.incidence(lista.redes[[i]],directed = TRUE,mode= "out",weighted = TRUE)
    out.t[[i]]<-spinglass.community(out.graph[[i]])
    modul.grafo[i]<-out.t[[i]]$modularity
    num.modulos[i]<-length(out.t[[i]]$csize)
  }
  out<-cbind(out,modul.grafo,num.modulos)
  rownames(out)<-names(lista.redes)
  out
}

modularidad_igraph<-modul_igraph(presas_x_pred_lista)

###__________________________________________________
#### 3.3- Modularidad estimada con paquete rnetcarto
# para extraer el z con matrices binarias uso el oecosimu
# con matrices pesadas me pasa lo mismo que con la funcion nestednodf, para la cual voy a 
# tener que correr 2000 matrices nulas de abundancia con funcion nullmodels del bipartite, y correrlas en la funcion netcarto
# para obtener el z: se hace el (obs - mean de la distrib de las 1000 matrices nulas) / sd
library(rnetcarto)
?netcarto

# funcion para obtener la modularidad observada de cada ano: elemento 2 del out para cada ano
hace_modul_netcarto<-function(lista_redes.coex){ 
  require(rnetcarto)
  out.f<-list()
  out<-list()
  out.2<-vector()
  for(i in 1:length(lista_redes.coex)){
    cat("va en la red: ", i, "\n")  
    out[[i]]<-netcarto(lista_redes.coex[[i]], bipartite = TRUE)
    out.2[i]<-out[[i]][[2]]
  }
  names(out.2)<-names(lista_redes.coex)
  out.2
  names(out)<-names(lista_redes.coex)
  out
  out.f<-list(out,out.2)
  out.f
}

hace_modul_netcarto(presas_x_pred_lista)->modularidad_netcartoo_lista_rt
modularidad_netcartoo_lista_rt[[2]] # vector con el valor de modularidad de cada matriz (por ano)
modularidad_netcartoo_lista_rt[[1]]$`1987`[2] #modul de cada ano (el mismo dato del vector de arriba)
modularidad_netcartoo_lista_rt[[1]]$`1987`[1] # tabla con roloes topologicos (z y c) y membresia de cada spp a los modulos

# para estandarizar la modul del netcarto, tengo que hacer modelos nulos
# si estoy trabajando con matrices binarias puedo usar el oecosimu
# si es con matrices pesadas, uso el nullmodels (similar a como voy a hacer con la fun. nestednodf)

# modelos nulos con rnetcartoo, para obtener los valores estandarizados de modularidad (z)
# ejemplo para una matriz
detach("package:igraph", unload=TRUE)
mod_netcart_1ano<-function(x) netcarto(x, bipartite = T)[[2]]
mod_netcart_1ano_oecosim<-oecosimu(presas_x_pred_lista_bin$`1987`,nestfun=mod_netcart_1ano, method= "quasiswap", statistic="modularity",nsimul=100, 500, 500) 
# da lo mismo si uso la lista de redes binarias o pesadas, poruqe igual el oecosimu transforma todo a binario
### OJO: investigar bien que esta haciendo porque da valores de Z-modularidad absurdamente altos

# funcion para correr el netcarto y ya extraer el z (usando el oecosimu), por lo tanto es una
# funcion para trabajar con matrices binarias
hace_oecosimu_netcarto<-function(lista_redes.coex){
  require(bipartite)
  require(rnetcarto)
  out<-list()
  mod<-function(x) netcarto(x, bipartite = T)[[2]]
  z_netcarto_bin<-vector()
  for(i in 1:length(lista_redes.coex)){
    cat("va en la red: ", i, "\n")  
    out[[i]]<-oecosimu(lista_redes.coex[[i]],nestfun=mod, method= "quasiswap",statistic="modularity", nsimul=2000, 500, 500) #cambiar el 100 x 2000
    z_netcarto_bin[i]<-out[[i]]$oecosimu$z
  }
  names(z_netcarto_bin)<-names(lista_redes.coex)
  z_netcarto_bin
}

#z.mod_netcarto_bin_rt<-hace_oecosimu_netcarto(presas_x_pred_lista_bin) 
z.mod_netcarto_bin_rt # apago funcion porque demora, dan muy coherentes los Z a diferencia de que con el sistema de charcos

#funcion para correr netcarto incluyendo la lista que contiene las matrices simuladas para cada red original
hace_null.w_netcarto<-function(lista.redes,lista.redes.nulas ){
  #detach("package:igraph", unload=TRUE)
  mod.w<-function(x) netcarto(x, bipartite = TRUE)[[2]]  
  z_netcarto_w<-vector()
  #out<-list()
  out.matrix<-list()
  #out2<-NULL
  for(i in 1:length(lista.redes)){
    cat("va en la red: ", i, "\n")
    #out[[i]]<-netcarto(lista.redes[[i]], bipartite = TRUE)
    out.t<-netcarto(lista.redes[[i]], bipartite = TRUE)
    out.matrix[[i]]<-lapply(lista.redes.nulas[[i]],as.matrix)
    #netcarto.matrices.nulas[[i]]<-lapply(out.matrix[[i]], mod.w)
    netcarto.matrices.nulas<-lapply(out.matrix[[i]], mod.w)
    #modul.matrices.nulas[[i]]<-lapply(netcarto.matrices.nulas, function(x) x[[2]])    
    #modul.matrices.nulas<-lapply(netcarto.matrices.nulas, function(x) x[[2]])    
    #modul.matrices.nulas<-unlist(netcarto.matrices.nulas[[i]])      
    modul.matrices.nulas<-unlist(netcarto.matrices.nulas)      
      #z_netcarto_w[i]<-(out[[i]][[2]] - mean(modul.matrices.nulas[[i]]))/sd(modul.matrices.nulas[[i]])
      z_netcarto_w[i]<-(out.t[[2]] - mean(modul.matrices.nulas))/sd(modul.matrices.nulas)
           }
       names(z_netcarto_w)<-names(lista.redes)
       z_netcarto_w
     }
    
z.mod_netcarto_w_rt2<-hace_null.w_netcarto(presas_x_pred_lista,matrices_nulas_W)
#___________________________________________________

# 3.4- funcion computeModules del bipartite, esta disenado para matrices pesadas pero bipartitas
# de lo contrario ver script de tesis_2012 con funciones para trabajr con listas y generar los modelos nulos
?computeModules

# 2) modularidad con computemodules...(bipartite)
hace_modul_computemod<-function(lista_redes){
  require(bipartite)
  out<-list()
  for(i in 1:length(lista_redes)){
    cat("va en la red: ", i, "\n")  
    out[[i]]<-computeModules(lista_redes[[i]])
  }
  names(out)<-names(lista_redes)
  out
}

modul.w_computemod<-hace_modul_computemod(presas_x_pred_lista)
modul.w_computemod$`1987`@likelihood # visulaizar el resultado
plotModuleWeb(modul.w_computemod$`1987`) # plot para la red de 1987

# Modelo nulo, para la funcion computemodules del bipartite, este modelo nulo aleatoriza la red, 
# basado en individuos, por eso da valores de modularidad elevados, hay que ver si es correcto usar el metodo "r2d"
# para este sistema de estudio, o utilizar algun otro metodo...de hecho probar con 
# otros metodos, ejemplo "shuffle.web", y estudiar como aleatoriza las matrices cada modelo nulo

#Funcion computemoduls, con las matrices nulas
hace_z.computemodul_w<-function(lista.redes, lista.redes.nulas){ 
  library(bipartite)
  #out1<-list()        
  z.mod <-vector()
  #out2<-NULL
  for(i in 1:length(lista.redes)){
    cat("va en la red: ", i, "\n")  
    #out1[[i]]
    mod.obs<-computeModules(lista.redes[[i]])
    out.nulos<-lapply(lista.redes.nulas[[i]], computeModules)
    like.nulls_w <- sapply(out.nulos, function(x) x@likelihood)
    #like.nulls_w <-unlist(like.nulls_w)
    z.mod[i]<-(mod.obs@likelihood - mean(like.nulls_w))/sd(like.nulls_w)
        #z.mod[i]<-(out1[[i]]@likelihood - mean(like.nulls_w))/sd(like.nulls_w)
      }
    names(z.mod)<-names(lista.redes)
  z.mod
}

z_mod.w_computemod_rt<-hace_z.computemodul_w(presas_x_pred_lista, matrices_nulas_W)
z_mod.w_computemod_rt
hist(z_mod.w_computemod_rt, breaks = 15, col = "orange", xlab = "modularity_w (coputemodules)", main = "")

#_____________________________
# 4) base de datos con MÉTRICAS ESTRUCTURALES GLOBALES DE LAS REDES DE CADA ANO
# unir todas las métrcias globales de las redes anuales en un único data frame:
# falta agregar la modularidad de las redes pesadas con rnetcarto y nullmodels, pero no pude ejecutar la función
estructura_redes.global_rt<-cbind(t(metricas_estructura.1),anid.bin_x_ano_rt,anid.w_x_ano_rt,z.mod_MODULAR.1_rt,z.mod_netcarto_bin_rt, z.mod_netcarto_w_rt2 ,z_mod.w_computemod_rt)
colnames(estructura_redes.global_rt)<-c(rownames(metricas_estructura.1),"anid_BIN","anid_W" ,"z_mod.BIN_MODULAR.1","z_mod.BIN_netcarto","z_mod.W_netcarto" ,"z_mod.W_computemod")
#________________________________________________________

# ROLES TOPOLOGICOS:

# 1) Centralidad de los nodos (BC, DC, EC, CC)

# ROLES TOPOLOGICOS:

# 1) Centralidad de los nodos (BC, DC, EC, CC)
library("sna")
prueba.BC<-betweenness(presas_x_pred_lista$`1987`, gmode="digraph",cmode="directed",ignore.eval=TRUE) #ej. para una red

# AVERIGUAR: indegree, outdegree o freeman (para la función degree)???

hace_cent_x_ano_rt<-function(lista_redes.troficas){
  out<-list()
  #detach("package:bipartite", unload=TRUE)
  #detach("package:igraph", unload=TRUE)
  library("sna")
  for (i in 1:length(lista_redes.troficas)) {
    BC<-betweenness(lista_redes.troficas[[i]],gmode="digraph",cmode="directed",ignore.eval=TRUE)
    DC<-degree(lista_redes.troficas[[i]],gmode="digraph",cmode="freeman",ignore.eval=TRUE)
    CC<-closeness(lista_redes.troficas[[i]],gmode="digraph",cmode="directed",ignore.eval=TRUE)
    EC<-evcent(lista_redes.troficas[[i]],gmode="digraph",ignore.eval=TRUE, use.eigen=TRUE)
    out[[i]]<-cbind(BC,DC,CC,EC)
    rownames(out[[i]])<-c(rownames(lista_redes.troficas[[i]]), colnames(lista_redes.troficas[[i]]))
  }
  names(out)<-names(lista_redes.troficas)
  out
}

cent_x_ano_lista_rt<-hace_cent_x_ano_rt(presas_x_pred_lista)
head(cent_x_ano_lista_rt$`1999`) # como es una red dirigida el out es un listado de las presas mas los predadores...
#como saber si une los predadores al final o al ppio?? deben ser al final porque son los de DC considerablemente mayor

# 2) NESTEDRANK
# ver si tiene sentido para una red unipartita y simetrica
nestedrank(presas_x_pred_lista$`1987`, return.matrix = TRUE)->kk
kk$`higher level`; kk$`lower level`#PREDADORES Y PRESAS RESPECTIVAMENTE

#3) CZVALUES 
# es del {bipartite}, funciona con el out del computemodules
czvalues(modul.w_computemod$`1987`, weighted = TRUE, level = "higher")->prueba_roles_cyz_1987_PRED
czvalues(modul.w_computemod$`1987`, weighted = TRUE, level = "lower")->prueba_roles_cyz_1987_PRESA
prueba_roles_cyz_1987_PRED
prueba_roles_cyz_1987_PRESA

# 4) estimar Z y C, a partir de Modularidad, con paquete rnetcartoo, o computeModules
# sacar del objeto modularidad_netcartoo_lista la columna de 5 de cada ano donde esta el rol de las spp.
modularidad_netcartoo_lista_rt
modularidad_netcartoo_lista_rt[[1]]$`1987`[[1]]
class(modularidad_netcartoo_lista_rt[[1]]$`1987`[[1]])
dim(modularidad_netcartoo_lista_rt[[1]]$`1987`[[1]]) # da 73 roles de un total de 129 nodos!!
# ademas no se cual es el criterio de los que pone en el listado y cuales no!, 
# porque solo aparacen tyto y falco de los depredadores que se supone que son los que tienen mas enlaces,
# capaz que tiene que ver con el hecho de ser bipartita, pero ni idea!

extrae_rol_spp_netcarto<-function(lista_out.netcartoo,spp_en=1,rol_en=5){
  out<-list()
  for (i in 1:length(lista_out.netcartoo[[1]])) {
    out[[i]]<-lista_out.netcartoo[[1]][[i]][[1]]
  }
  names(out)<-names(lista_out.netcartoo[[1]])
  out
}

roles_netcartoo_xano<-extrae_rol_spp_netcarto(modularidad_netcartoo_lista)
#_____________________________________
#NO ME CONVENCE LO QUE ESTA HACIENDO EL NETCARTO!
# si insisto ver py de redes de charcos.

####______________________________________

# variables meteorologicas
head( ENSO_1980_2020)
colnames(ENSO_1980_2020)<-c("ano", month.name)
#rownames(ENSO_1980_2020)<-ENSO_1980_2020[,1]
ENSO_promedio<-cbind(ENSO_1980_2020[6:35,1], apply(ENSO_1980_2020[6:35,-(1:4)], 1, mean))
colnames(ENSO_promedio)<-c("Años","SOI")
plot(ENSO_promedio, type = "l", col="blue",lwd = 2)
# hacer luego funcion que promedie por ano, pero empezando el ano de marzo a febrero