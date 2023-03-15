# 1era parte:
### Estimación REDES CO-OCURRENCIA PLANTAS CHARCOS 
# carga base de datos: donde están todas las spp de plantas observadas por año, por charco y por um
# exploro base madre:
#library(readr)
# base_madre_lista_spp_veg <- read_csv("base_madre_lista_spp_veg.csv")
head(base_madre_lista_spp_veg)
riqueza<-as.data.frame(base_madre_lista_spp_veg)
table(riqueza[,1:2])
anos_totales<-unique(na.omit(riqueza$año))

# elimino species NA o las spp nombradas como 0 (en la base de 2016 y 2918) en la base antes de hacer las matrices, removiendo esas filas
riqueza[which((is.na(riqueza[,5]))==TRUE),] # al parecer base con 2020, ninguna spp NA!
riqueza[which((riqueza[,5]=="0")==TRUE),]
riqueza<-riqueza[-(which((riqueza[,5]=="0")==TRUE)),]
#riqueza<-riqueza[-(rownames(riqueza[which((is.na(riqueza[,5]))==TRUE),])),]

#______________#
#función que divide en tantas bases por año y las une en una lista
hace_riq_x_ano<-function(base,colum.anos=1){
  out<-list()
  for (i in 1:length(unique(na.omit(base[,colum.anos])))) {
    out[[i]]<-subset(base,base[,colum.anos]==(unique(na.omit(base[,colum.anos])))[i])
  }
  names(out)<-as.character(unique(na.omit(base[,colum.anos])))
  out
}
riqueza_x_ano_lista<-hace_riq_x_ano(riqueza)

#tengo anos donde hay mas de un mes (ej: unique(riqueza_x_ano_lista$`2007`$mes)), cuando el muestreo de primavera fue muy bajo, puedo tener en cuenta muestreos de meses anteriores, como por ej. en 2007 y 2008
#para eso voy a incluir el muestreo con mayor numero de indiv. involucrados en cada ano, entonces:
#2006-uso muestreo de mes 6; #2007 y 2008-uso muestreo del mes 8; 2009 y 2010->uso muestreo del mes 7

#funcion que identifica el mes para cada año con mayor registro, y que sea con el mes que seleccione para hacer las matrices
# como es de esperar un mayor registro de spp coincide con mas cahrcos activos
hace_bases_1mes_x_ano<-function(base, lista.base.anos, anos=1,mes=2){
  tabla<-table(base[,c(anos,mes)])
  out<-list()
  for (i in 1:length(rownames(tabla))) {
    out[[i]]<-subset(lista.base.anos[[i]],lista.base.anos[[i]][,2]==as.numeric(colnames(tabla)[which(tabla[i,]==max(tabla[i,]))]))
  }
  names(out)<-names(lista.base.anos)
  out
}
riqueza_x_ano_lista_1mes<-hace_bases_1mes_x_ano(riqueza,riqueza_x_ano_lista,anos = 1,mes = 2)
#_________________#

## funcion que calcula para cada ano los meses en donde hubo mas charcos muestreados
calcula_meses_con_mas_charcos<-function(lista_bases_x_ano, meses_en=2,ch_en=3){
    out<-list()
    meses_mas_ch<-vector()
    for (i in 1:length(lista_bases_x_ano)) {
      tabla<-table(lista_bases_x_ano[[i]][,c(meses_en,ch_en)])
      tabla.2<-ifelse(tabla>0,1,0)
      suma_meses<-rowSums(tabla.2)
      out[i]<-which(suma_meses==max(suma_meses))
      names(out)[i]<-names(which(suma_meses==max(suma_meses)))
    }
    meses_mas_ch<-as.numeric(names(out))
    names(meses_mas_ch)<-names(riqueza_x_ano_lista)
    meses_mas_ch
}
  
calcula_meses_con_mas_charcos(riqueza_x_ano_lista)
table(riqueza_x_ano_lista[[1]][,c(2,3)])

###ESTIMACION DE RED DE CO-OCURRENCIA METODO: Borthagaray et al. 2014

#genera la red a partir de desviaciones significativas en la co-ocurrencia, se basa en el calculo de probabilidades de
#ocurrencia en un parche a partir de los datos de individuos y no de la incidencia de la especie.
red_desde_coex<-function(M, especies_en, comunidades_en, abundancia){
  spp<-sort(unique(M[,especies_en]))
  out<-matrix(NA,length(spp),length(spp))
  colnames(out)<-spp
  rownames(out)<-spp
  P<-probs(M, especies_en, comunidades_en)
  Co<-co.g(M, comunidades_en, especies_en, abundancia)
  n.par<-unique(M[,comunidades_en])
  for(i in spp){
    for(j in spp){
      if(i>j){
        id.i<-which(rownames(P)==i)
        id.j<-which(rownames(P)==j)
        Pij<-P[id.i]*P[id.j,]
        Cobs<-length(which(Co[which(rownames(Co)==j),]+Co[which(rownames(Co)==i),]==2))
        Cesp<-sum(Pij)
        Var<-sum((1-Pij)*Pij)
        z<-(Cobs-Cesp)/(Var^.5)
        cat("valor estimado de Pij  ", c(Cobs,Cesp,Var), "\n")
        out[which(rownames(out)==i),which(colnames(out)==j)]<-z
      }
    }
  }
    out
}

###
# Esta funcion calcula la probabilidad de observar a cada especie en cada parche a partir del 
# numero de individuos de la especie en la metacomunidad y del numero de individuos observados en el parche independientemente de la especie
probs<-function(M, especies_en, comunidades_en){
  spp<-unique(M[,especies_en])
  com<-sort(unique(M[,comunidades_en]))
  out<-matrix(0,length(spp),length(com))
  colnames(out)<-com
  rownames(out)<-spp
  JS<-tapply(M[,especies_en],M[,especies_en],length) # Numero de individuos por especie en la metacomunidad
  JP<-tapply(M[,comunidades_en],M[,comunidades_en],length) # Numero de individuos por parche sin considerar especies
  N<-sum(JP)
  for(i in spp){
    Ji<-JS[which(names(JS)==i)]# n total de individuos de la especie i en la metacomunidad
    for(j in com){
      Jj<-JP[which(names(JP)==j)]#n total de individuos en el parche j sin considerar identidad de especies
      ifelse((N-Ji)>Jj,(Pij<-1-exp((lchoose((N-Ji),Jj))-(lchoose(N,Jj)))),(Pij<-1))
      id.j<-which(colnames(out)==j)
      id.i<-which(rownames(out)==i)
      out[id.i,id.j]<-Pij
    }
  }
  out
}
####
# Genera matriz de presencia de especies por parche (o ano), se usa tanto para la funcion red_desde_coex,
# como para hacer matriz de spp x año y luego aleatorizar para obener matrices nulas (ver más adelante)
co.g<-function(M, comunidades_en, especie_en, abundancia){
  SPP<-unique(M[,especie_en])
  COMM<-unique(M[,comunidades_en])
  out<-matrix(0,length(SPP),length(COMM))
  colnames(out)<-COMM
  rownames(out)<-SPP
  for (i in 1:length(SPP)){
    for(j in 1:length(COMM)){
      if(length(which(M[,especie_en]==SPP[i] & M[,comunidades_en]==COMM[j]))>0){
        JJ<-which(colnames(out)==COMM[j])
        II<-which(rownames(out)==SPP[i])
        if(abundancia==FALSE)out[II,JJ]<-1
        if(abundancia==TRUE)out[II,JJ]<-length(which(M[,especie_en]==SPP[i] & M[,comunidades_en]==COMM[j]))
      }
    }
  }
  out
}

# funcion que tome las listas de riqueza por año y creae matrices de co-ocurrencia de plantas por anos, 
# para lo cual utiliza 3 funciones anteriores (Borthagaray et al. 2014)
hace_matriz.coex_x_ano<-function(lista_base_riq, especies_en=5, comunidades_en=3, abundancia=TRUE){
  out<-list()
  for (i in 1:length(lista_base_riq)) {
    out[[i]]<-red_desde_coex(lista_base_riq[[i]],especies_en,comunidades_en,abundancia)
  }
  names(out)<-names(lista_base_riq)
  out
}
matriz.coex_x_ano_lista<-hace_matriz.coex_x_ano(riqueza_x_ano_lista_1mes) #por defecto abund=TRUE
#______________________#
#funcion que modifica matrices del out hace_matriz.coex_x_ano, para que sean matrices simetricas y diag=0
hace_matriz_simetricas<-function(lista_matri_coex,valor.diag=0){
  out<-list()
  for (i in 1:length(lista_matri_coex)) {
    out[[i]]<-lista_matri_coex[[i]]
    out[[i]][upper.tri(out[[i]])]<-t(out[[i]])[upper.tri(t(out[[i]]))]
    diag(out[[i]])<-0 #valor.diag
  }
  names(out)<-names(lista_matri_coex)
  out
}

matriz.coex_x_ano_lista<-hace_matriz_simetricas(matriz.coex_x_ano_lista)
isSymmetric(matriz.coex_x_ano_lista$`2008`)#TRUE
###
# funcion que crea matriz de co-existencia positiva: transforma links significativos (=>2) a links efectivos (1, o dejando el valore de Z), y los no signif. (< 2) son 0
hace_redes.coex_positivas<-function(lista_matri_simetricas,umbral=2){
  out<-list()
  for (i in 1:length(lista_matri_simetricas)) {
    #out[[i]]<-lista_matri_simetricas[[i]]
    out[[i]]<-ifelse(lista_matri_simetricas[[i]]<umbral,0,lista_matri_simetricas[[i]])
  }
  names(out)<-names(lista_matri_simetricas)
  out
}
redes_coex.positivas_lista<-hace_redes.coex_positivas(matriz.coex_x_ano_lista)

# explorando las matrices... 
para_saber_dimensiones<-function(lista_con_matrices){
  out<-list()
  for (i in 1:length(lista_con_matrices)) {
    out[[i]]<-dim(lista_con_matrices[[i]])
  }
  names(out)<-names(lista_con_matrices)
  out
}
dim_matrices<-para_saber_dimensiones(redes_coex.positivas_lista)
dim_matrices<-unlist(dim_matrices);dim_matrices;min(dim_matrices)
sapply(redes_coex.positivas_lista, function(x) dim(x)[2]) #otra forma de saber la dim, sirve ya que son matrices simetricas
#_________________#
### CREA MATRICES BINARIAS
hace_redes_binarias<-function(lista_redes.coex){
  out<-list()
  for (i in 1:length(lista_redes.coex)) {
    out[[i]]<-ifelse(lista_redes.coex[[i]]>0,1,0)
  }
  names(out)<-names(lista_redes.coex)
  out
}

redes_coex.positivas_BIN_lista<-hace_redes_binarias(redes_coex.positivas_lista)
sapply(redes_coex.positivas_BIN_lista, sum)#para saber nro total de links (edges)
#_________________#

### MATRICES PESADAS NULAS

# 1.genero matrices pesadas nulas, para eso debo obtener las matrices de abundancia spp x charco de cada año (0.), las cuales aleatorizo!!
# 2.una vez que tengo las 2000 matri de abund aleatorias, modifico la función redes_desde_coex, para que la entrada sea una matriz de abun y no un listado de incidencia
# 3.a las matrices de coex nulas debo hacerlas simétricas y con diag 0 también y 
# 4.luego hacer las redes nulas con links positivos (>2)

# 0.con la función co.g hago matriz de abundancia para poder luego aleatorizar en base a un modelo nulo (quassiswap) y generar las 2000 matrices nulas
hace_base_x_ano_con_abundancia<-function(lista.bases.x.ano){
  out<-list()
  for (i in 1:length(lista.bases.x.ano)) {
    out[[i]]<-co.g(lista.bases.x.ano[[i]],comunidades_en = 3,especie_en = 5,abundancia = TRUE)
  }
  names(out)<-names(lista.bases.x.ano)
  out
}

hace_base_x_ano_con_abundancia(riqueza_x_ano_lista_1mes)->matri_abund_sppxch_lista
matri_abund_sppxch_lista$`2005`
#length(which((riqueza_x_ano_lista_1mes$`2005`$Especie=="Eleocharis_viridans" & riqueza_x_ano_lista_1mes$`2005`$Charco==11)==TRUE))

# 1.funcion que hace matrices nulas pesadas (con datos de abund.), paquete del vegan nullmodels, 
# el out es un gran data frame que hay que ir cortando por columna sabiendo el ncol de la matriz original
hace_matrices_nulas_W<-function(lista.redes,nsimul=2000,burn=500,thiny=500){
  #detach("package:bipartite", unload=TRUE)
  library(vegan)
  out.1<-vector()
  out.2<-list()
  out.3<-list()
  for (i in 1:length(lista.redes)) {
    out.1[i]<-dim(lista.redes[[i]])[2]
    modelo<-nullmodel(lista.redes[[i]], "quasiswap_count")
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
detach("package:bipartite", unload=TRUE)
matrices_abund_nulas_W<-hace_matrices_nulas_W(matri_abund_sppxch_lista)  

# comparar suma de la matriz origianal vs alguna de las matrices nulas
# compruebo si se mantiene el total de filas y columnas entre la matriz original y las n simuladas
dim(matrices_abund_nulas_W$`2005`$`1`)
apply(matri_abund_sppxch_lista$`2011`,1,sum)[1];apply(matrices_abund_nulas_W$`2011`$`1`,1,sum)[1]
apply(matri_abund_sppxch_lista$`2011`,2,sum)[1];apply(matrices_abund_nulas_W$`2011`$`1`,2,sum)[1]
#otra forma de hacer lo mismo:
rowSums(matri_abund_sppxch_lista$`2005`); rowSums(matrices_nulas_W$`2005`$`1`)
colSums(matri_abund_sppxch_lista$`2005`); colSums(matrices_nulas_W$`2005`$`1`)
#________________#
# 2. modificacion de la funcion "hace_matriz.coex_x_ano" y funciones asociadas, redes_desde_coex y probs
# para que lo haga redes con z-coexistencia para la lista de 2000 matrices nulas de cada ano

# modificacion de redes_desde_coex donde la "entrada" ya es una matriz de abundancia spp x ch,
# que incluye el probs, y el out ya es la matriz simetrica y con diag=0
red_desde_coex_para_lista.nulas<-function(M.sppxch){
  spp<-sort(rownames(M.sppxch))
  out<-matrix(NA,length(spp),length(spp))
  colnames(out)<-spp
  rownames(out)<-spp
  P<-probs_modif_para.nulas(M.sppxch)
  n.par<-colnames(M.sppxch)
  for(i in spp){
    for(j in spp){
      if(i>j){
        id.i<-which(rownames(P)==i)
        id.j<-which(rownames(P)==j)
        Pij<-P[id.i]*P[id.j,]
        coex.obs<-length(which(M.sppxch[which(rownames(M.sppxch)==j),]+M.sppxch[which(rownames(M.sppxch)==i),]==2))
        coex.esp<-sum(Pij)
        Var<-sum((1-Pij)*Pij)
        z<-(coex.obs-coex.esp)/(Var^.5)
        #cat("valor estimado de Pij  ", c(coex.obs,coex.esp,Var), "\n")
        out[which(rownames(out)==i),which(colnames(out)==j)]<-z
        out[which(rownames(out)==j),which(colnames(out)==i)]<-z
        diag(out)<-0
      }
    }
  }
    out
}
#jj<-red_desde_coex_para_lista.nulas(matrices_nulas_W$`2005`$`1`); isSymmetric(jj)

# modificacion de funcion probs donde la "entrada" ya es una matriz de abundancia spp x ch
probs_modif_para.nulas<-function(M){ #especies_en, comunidades_en){
  M<-M[order(row.names(M)),]
  spp<-row.names(M)
  com<-sort(colnames(M))
  #spp<-unique(M[,especies_en])
  #com<-sort(unique(M[,comunidades_en]))
  out<-matrix(0,length(spp),length(com))
  colnames(out)<-com
  rownames(out)<-spp
  JS<-rowSums(M)
  JP<-colSums(M)
  #JS<-tapply(M[,especies_en],M[,especies_en],length) # Numero de individuos por especie en la metacomunidad
  #JP<-tapply(M[,comunidades_en],M[,comunidades_en],length) # Numero de individuos por parche sin considerar especies
  N<-sum(JP)
  for(i in spp){
    Ji<-JS[which(names(JS)==i)]# n total de individuos de la especie i en la metacomunidad
    for(j in com){
      Jj<-JP[which(names(JP)==j)]#n total de individuos en el parche j sin considerar identidad de especies
      ifelse((N-Ji)>Jj,(Pij<-1-exp((lchoose((N-Ji),Jj))-(lchoose(N,Jj)))),(Pij<-1))
      id.j<-which(colnames(out)==j)
      id.i<-which(rownames(out)==i)
      out[id.i,id.j]<-Pij
    }
  }
  out
}
#probs_modif_para.nulas(matrices_nulas_W$`2005`$`1`)->kk

#________________#
# funcion que hace redes_desde_coex, pero para la entrada que es una lista que contiene para cada ano una lista de matrices nulas
hace_matriz.coex_x_ano_NULAS<-function(lista_de_lista.m.nulas){
  out.1<-list()
  for (i in 1:length(lista_de_lista.m.nulas)) {
    cat("va en la red: ", i, "\n")
    out.1[[i]]<-lapply(lista_de_lista.m.nulas[[i]], red_desde_coex_para_lista.nulas) ## donde dice probs hacer fucni'on que tengo que crear... 
  }
  names(out.1)<-names(lista_de_lista.m.nulas)
  out.1
}  
#redes_coex_nulas<-hace_matriz.coex_x_ano_NULAS(matrices_abund_nulas_W)

# 2016 y 2018 quedaron con una spp==0, la modifico de matrices_abund_sppxch_lista, y de ahi a matrices_abund_nulas_W 
# y vuelvo a correr el hace_matriz.coex_x_ano_NULAS, solo con estas dos redes y luego sustituyo

# si corrigo la base desde riqueza, eliminando las spp con nombre NA o 0, los pasos que siguen no es necesario hacerlos
which(rownames(matri_abund_sppxch_lista$`2016`)=="0")
which(rownames(matri_abund_sppxch_lista$`2018`)=="0")
matri_abund_sppxch_lista$`2016`<-matri_abund_sppxch_lista$`2016`[-40,]
matri_abund_sppxch_lista$`2018`<-matri_abund_sppxch_lista$`2018`[-50,]
k<-list(matri_abund_sppxch_lista$`2016`,matri_abund_sppxch_lista$`2018`)
kk<-hace_matrices_nulas_W(k) 
names(kk)<-c(2016,2018)
kkk<-hace_matriz.coex_x_ano_NULAS(kk)

redes_coex_nulas$`2016`<-kkk$`2016`
redes_coex_nulas$`2018`<-kkk$`2018`

#_________________________#
# 4. hace redes nulas de co/ocurrencia positivas
# para eso usar funcion: hace_redes.coex_positivas
hace_una_matriz_coex.positive<-function(M, umbral=2){
  M<-ifelse(M<umbral,0,M)
}

hace_redes_nulas.coex_positivas<-function(lista_de_listas.coex.nulas, umbral=2){
  out.posit<-list()
  for (j in 1:length(lista_de_listas.coex.nulas)) {
    out.posit[[j]]<-lapply(lista_de_listas.coex.nulas[[j]], hace_una_matriz_coex.positive)
  }
  names(out.posit)<-names(lista_de_listas.coex.nulas)
  out.posit
}
redes_coex_nulas.positive<-hace_redes_nulas.coex_positivas(redes_coex_nulas)
#__________________________#
 
### HACE MATRICES NULAS BINARIAS...
hace_una_matriz_binaria<-function(M, umbral=2){
  M<-ifelse(M<umbral,0,M)
}

hace_redes_nulas.binarias<-function(lista_de_listas.coex.nulas){
  out.bin<-list()
  for (j in 1:length(lista_de_listas.coex.nulas)) {
    out.bin[[j]]<-lapply(lista_de_listas.coex.nulas[[j]], hace_una_matriz_binaria)
  }
  names(out.bin)<-names(lista_de_listas.coex.nulas)
  out.bin
}
redes_coex_nulas.positive_BIN<-hace_redes_nulas.binarias(redes_coex_nulas.positive)

###_________FIN 1era parte________###
# Doctorado - ultima modificacion: 19 abril 2021

# guarda los objetos que quiera en un nuevo espacio de .RData
?save.image
#save(redes_coex_lista,redes_coex_nulas.positive,redes_para_netcarto,redes_para_netcarto_NULAS,redes_coex_BIN_lista , file = "prueba.RData")

#save(base_incid_plantas_charcos_anos,matri_abund_sppxch_lista,matrices_nulas_W ,redes_coex_lista,redes_coex_nulas,redes_coex_nulas.positive,redes_coex_BIN_lista,redes_coex_nulas.posit.binarias,file = "DOC_charcos.RData")


