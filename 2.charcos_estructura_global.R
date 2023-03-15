# 2da parte: 
### METRICAS ESTRUCTURALES GLOBALES DE LAS REDES X ANO ####
# una vez que tengo las redes de co-existencia, se estiman metricas estructurales de redes:

# 1) Conectividad, tamaño, distribución de grado, nro de componentes,distancia caracteristica, motivos
# 1.1- Distribucion de grado
#leer papaer de la funcion degreedistr para ver que es lower y higher level y ver si tiene sentido solo para redes unipartitas
# Jordano, P., Bascompte, J. and Olesen, J. M. (2003)
extrae_distr_grado<-function(lista_de_redes){
  library(bipartite)
  #out<-data.frame() #matrix(NA,length(lista_redes),6)
  aic<-data.frame()
  for (i in 1:length(lista_de_redes)) { 
    aic[i,1:3]<-c(degreedistr(lista_de_redes[[i]],plot.it = FALSE)$`higher level dd fits`[,5])
  }
  #out<-rbind(out,aic)
  aic
  rownames(aic)<-names(lista_de_redes)
  colnames(aic)<-c("exponential","power law","truncated power law")
  aic
}

extrae_distr_grado(redes_coex.positivas_lista)->distribucion_de_grado_plantas
head(distribucion_de_grado_plantas)
which((distribucion_de_grado_plantas[2,]==min(distribucion_de_grado_plantas[2,]))==TRUE) # me da para la fila 2 (o cualquiera que especifique), la columna con el minimo AIC

# 1.2- Distancia promedio/Diametro/Tamano/Conectividad
library("igraph")
graph_from_adjacency_matrix(redes_coex.positivas_lista$`2015`,mode = "undirected",weighted = TRUE,diag = FALSE)->oo
average.path.length(oo) # dist.promedio, otra forma: mean_distance(oo)
diameter(oo,weights=NULL) #diametro, si se especifica NULL, toma los pesos de que ya están en la matriz (que no es binaria)
N<-ncol(redes_coex.positivas_lista$`2015`) #tamano
# RESOLVER SI EN TAMAÑO INCLUYO NRO DE NODOS O DE ENLACES: EN ESE CASO:
gsize(oo)
(2*(sum(ifelse(redes_coex.positivas_lista$`2015`>0,1,0))/2))/(N*(N-1)) ##conectividad, no iria el 2* ya que esta completa la matriz...
(sum(ifelse(redes_coex.positivas_lista$`2015`>0,1,0)))/(N*(N-1)) ##conectividad, para mi ser'ia as'i, pero hay que verlo en detalle...
# ya que es una red sim'etrica
transitivity(oo,type="global", weights = TRUE) # coeficiente de agrupamiento
transitivity(oo,type="local", weights = TRUE) # coeficiente de agrupamiento

# hace funcion cuyo out es una matriz de las distintas metricas de redes estimadas por año: las columnas son los años y las filas las metricas de red 
hace_metricas_varias_red<-function(lista_redes.coex){
  library("igraph")
  out<-data.frame()
  out.grafo<-list()
  dist.promedio<-vector()
  diametro<-vector()
  dimension<-vector()
  conectividad<-vector()
  coef.agrupamiento<-vector()
  for (i in 1:length(lista_redes.coex)) {
    out.grafo[[i]]<-graph_from_adjacency_matrix(lista_redes.coex[[i]],mode = "undirected",weighted = TRUE,diag = FALSE)
    average.path.length(out.grafo[[i]])->dist.promedio[i]
    diameter(out.grafo[[i]],weights=NULL)->diametro[i]
    ncol(lista_redes.coex[[i]])->dimension[i]
    (2*(sum(ifelse(lista_redes.coex[[i]]>0,1,0))/2))/(dimension[i]*(dimension[i]-1))->conectividad[i]
    transitivity(out.grafo[[i]],type="global")->coef.agrupamiento[i]
    #out<-rbind(out,dist.promedio,diametro,dimension, conectividad,coef.agrupamiento)
  }
  out<-rbind(out,dist.promedio,diametro,dimension, conectividad,coef.agrupamiento)
  colnames(out)<-names(lista_redes.coex)
  rownames(out)<-c("dist.promedio","diametro","tamaño","conectividad","coef.agrupamiento")
  out
}
detach("package:bipartite", unload=TRUE)
detach("package:vegan", unload=TRUE)
metricas_grafos_varias<-hace_metricas_varias_red(redes_coex.positivas_lista)

####1.3 CENTRALIZACION GLOBAL...
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
    out.grafo[[i]]<-graph_from_adjacency_matrix(lista_redes[[i]],mode = "undirected",weighted = TRUE,diag = FALSE)
    centarliz_bc[i]<-centr_betw(out.grafo[[i]])$centralization
    maximo_theo_bc[i]<-centr_betw(out.grafo[[i]])$theoretical_max
    centarliz_clo[i]<-centr_clo(out.grafo[[i]])$centralization
    maximo_theo_clo[i]<-centr_clo(out.grafo[[i]])$theoretical_max
    centarliz_eigen[i]<-centr_eigen(out.grafo[[i]])$centralization
    maximo_theo_eigen[i]<-centr_eigen(out.grafo[[i]])$theoretical_max
    centarliz_degree[i]<-centr_degree(out.grafo[[i]])$centralization
    maximo_theo_degree[i]<-centr_degree(out.grafo[[i]])$theoretical_max
  }
  out<-rbind(out,centarliz_bc, maximo_theo_bc, centarliz_clo, maximo_theo_clo, centarliz_eigen, maximo_theo_eigen, centarliz_degree, maximo_theo_degree)
  colnames(out)<-names(lista_redes)
  rownames(out)<-c("centarliz_bc", "maximo_theo_bc", "centarliz_clo", "maximo_theo_clo", "centarliz_eigen", "maximo_theo_eigen", "centarliz_degree", "maximo_theo_degree")
  out
}

centralidad_global<-hace_centraliz_global(redes_coex.positivas_lista)
head(centralidad_global)

# uno todo en un gran data frame...
metricas.1_estructura<-rbind(t(distribucion_de_grado_plantas), metricas_grafos_varias,centralidad_global)
head(metricas.1_estructura)
tail(metricas.1_estructura)
#_______________________#

# 2) ANIDAMIENTO

hace_z.anid_x_ano<-function(lista.redes, lista.redes.nulas, peso_VoF=TRUE){
  require(vegan)
  anid.w<-function(x) nestednodf(x, weighted = peso_VoF)  
  z<-vector()
  for (i in 1:length(lista.redes)) {
    cat("va en la red: ", i, "\n")  
    out.2<- nestednodf(lista.redes[[i]], weighted = peso_VoF)
    anid.matrices.nulas<-lapply(lista.redes.nulas[[i]], anid.w)
    nodf.matrices.nulas <- lapply(anid.matrices.nulas, function(x) x$statistic[3])
    nodf.matrices.nulas<-unlist(nodf.matrices.nulas)
    z[i]<-(out.2$statistic[3] - mean(nodf.matrices.nulas))/sd(nodf.matrices.nulas)
  }
  names(z)<-names(lista.redes)
  z
}

# 2.1- ANIDAMIENTO BINARIO, usando la lista de matrices nulas binarias para obtener el Z NODF
detach("package:bipartite", unload=TRUE)
z.nodf.bin<-hace_z.anid_x_ano(redes_coex.positivas_BIN_lista, redes_coex_nulas.positive_BIN, peso_VoF = FALSE)
z.nodf.bin

# 2.2- ANIDAMIENTO pesado PARA LA LISTA CON TODAS LAS MATRICES
z.nodf.W<-hace_z.anid_x_ano(redes_coex.positivas_lista, redes_coex_nulas.positive, peso_VoF = TRUE )
z.nodf.W
cor.test(z.nodf.bin,z.nodf.W) # 0.82

#_________________

### plus: Normalised Nestedness metric- NODFc (Song et al.2017)
library(maxnodf)
?maxnodf
maxnodf(redes_coex.positivas_lista$`2008`,quality=2)[[1]] # en caso de ser muy lento con quality=2, dejar por defecto =0
maxnodf(redes_coex.positivas_BIN_lista$`2008`,quality=2)[[1]] # da exactamente lo mismo para la red pesada y binaria
maxnodf(redes_coex.positivas_lista$`2012`,quality=2)[[1]] # no trabaja con redes desconectadas, i.e.: con nodos aislados, donde la suma total de fila o columna es 0

hace_NODFc_x_ano<-function(lista.redes, Q=2){
    require(maxnodf)
    hace_maxnodf<-function(x) maxnodf(x, quality = Q)[[1]]  
      nodf_c<-lapply(lista.redes, hace_maxnodf)
      nodf_c<-unlist(nodf_c)
      names(nodf_c)<-names(lista.redes)
      nodf_c
}

NODF_c<-hace_NODFc_x_ano(redes_coex.positivas_lista)

############## 3) MODULARIDAD

#### Modularidad estimada con paquete rnetcarto
# funcion para obtener la modularidad observada de cada charco: elemento 2 del out para cada charco, y el elemento 1 los roles
hace_modul_netcarto<-function(lista_redes.coex){ 
  require(rnetcarto)
  out.f<-list()
  out<-list()
  out.3<-list()
  out.2<-vector()
  for(i in 1:length(lista_redes.coex)){
    cat("va en la red: ", i, "\n")  
    out[[i]]<-netcarto(lista_redes.coex[[i]], bipartite = FALSE) #bipartite=FALSE, ya que la redes de coexist de plantas de cada ano son redes unipartitas
    out.3[[i]]<-out[[i]][[1]]
    out.2[i]<-out[[i]][[2]]
  }
  names(out.2)<-names(lista_redes.coex)
  out.2
  names(out.3)<-names(lista_redes.coex)
  out.3
  out.f<-list(out.3,out.2)
  names(out.f)<-c("Roles.topo","Modularidad_raw")
  out.f
}

hace_modul_netcarto(redes_coex.positivas_lista)->modularidad_roles

hace_nro.de_modulos<-function(lista_salida_del_netcarto){
  out<-vector()
  for (i in 1:length(lista_salida_del_netcarto$Roles.topo)) {
    out[i]<-length(unique((modularidad_roles$Roles.topo[[i]])[,2]))
  }
  names(out)<-names(lista_salida_del_netcarto$Roles.topo)
  out
}

hace_nro.de_modulos(modularidad_roles)
dim_componentes<-cbind(dim_comp_mayor=lapply(modularidad_roles$Roles.topo,nrow),dim_tot=lapply(matri_abund_sppxch_lista,nrow),dif=(unlist(lapply(modularidad_roles$Roles.topo,nrow)))-(unlist(lapply(matri_abund_sppxch_lista,nrow))))#,(unlist(lapply(modularidad_roles$Roles.topo,nrow))-unlist(lapply(matri_abund_sppxch_lista,nrow))))
which(dim_componentes[,3]==0) # los anos cuyas redes estan conectadas: 2006, 2007, 2008, 2010, 2015
is.connected(graph_from_adjacency_matrix(redes_coex.positivas_lista$`2020`))
components(graph_from_adjacency_matrix(redes_coex.positivas_lista$`2009`))
aa<-spinglass.community(graph_from_adjacency_matrix(redes_coex.positivas_lista$`2010`)) # solo funciona con redes conectadas
length(unique(aa$membership))
# el netcarto est(ima la modularidad teniendo en cuenta solo los componentes conectados, i.e.: "ignora" los nodos aislados

#funcion para correr netcarto binario y pesadao, y estimar z.mod bin, para lo cual necesito usar la lista de lista con las redes coex nnulas binarias
hace_z.mod_x_ano<-function(lista.redes,lista.redes.nulas ){
  mod<-function(x) netcarto(x, bipartite = FALSE)[[2]]  
  z_netcarto<-vector()
  out.matrix<-list()
    for(i in 1:length(lista.redes)){
    cat("va en la red: ", i, "\n")
    out.t<-netcarto(lista.redes[[i]], bipartite = FALSE)
    out.matrix[[i]]<-lapply(lista.redes.nulas[[i]],as.matrix)
    netcarto.matrices.nulas<-lapply(out.matrix[[i]], mod)
    modul.matrices.nulas<-unlist(netcarto.matrices.nulas)      
    z_netcarto[i]<-(out.t[[2]] - mean(modul.matrices.nulas))/sd(modul.matrices.nulas)
  }
  names(z_netcarto)<-names(lista.redes)
  z_netcarto
}

# 3.1. MODULARIDAD BINARIA
detach("package:igraph", unload=TRUE)
z.mod_bin<-hace_z.mod_x_ano(redes_coex.positivas_BIN_lista,redes_coex_nulas.positive_BIN)
z.mod_bin
cor.test(z.mod_bin,z.nodf.bin)#-0.42
###________

# 3.2. MODULARIDAD PESADAS, con lista de redes nulas
# z.mod_w<-hace_null_netcarto(redes_coex.positivas_lista,redes_coex_nulas.positive)
z.mod_w<-hace_z.mod_x_ano(redes_coex.positivas_lista,redes_coex_nulas.positive)
cor.test(z.mod_bin,z.mod_w) # 0.69
cor.test(z.mod_w ,z.nodf.W) # -0.57 
###________

### 4. UNO LAS METRICAS DE ESTRUCTURA GLOBAL EN UN GRAN DATA FRAME
metricas.2_estructura_patrones<-cbind(z.nodf.bin, z.nodf.W,z.mod_bin, z.mod_w, nro_modulos=hace_nro.de_modulos(modularidad_roles)) #NODF_c
head(metricas.2_estructura_patrones)
#write.csv(patrones,file = "patrones_red.co-ocurr_charcos.csv")
#patrones_estructura_global<-cbind(t(metricas.1_estructura),metricas.2_patrones)
  
####_______FIN 2da parte________###

# guarda los objetos que quiera en un nuevo espacio de .RData
#save(redes_coex.positivas_lista,redes_coex_nulas.positive,redes_coex.positivas_BIN_lista , file = "prueba.RData")

