# 3ra parte:
### ROLES TOPOLOGICOS DE LAS SPP X ANO ####

# 1) Centralidad de los nodos (BC, DC, EC, CC)
hace_cent_x_ano<-function(lista_redes.coex, ignorar.pesos=TRUE){
  out<-list()
  library("sna")
  for (i in 1:length(lista_redes.coex)) {
    BC<-betweenness(lista_redes.coex[[i]],gmode="graph",cmode="undirected",ignore.eval=ignorar.pesos)
    DC<-degree(lista_redes.coex[[i]],gmode="graph",cmode="undirected",ignore.eval=ignorar.pesos)
    CC<-closeness(lista_redes.coex[[i]],gmode="graph",cmode="undirected",ignore.eval=ignorar.pesos)
    EC<-evcent(lista_redes.coex[[i]],gmode="graph",ignore.eval=TRUE, use.eigen=ignorar.pesos)
    out[[i]]<-cbind(BC,DC,CC,EC)
    rownames(out[[i]])<-rownames(lista_redes.coex[[i]])
  }
  names(out)<-names(lista_redes.coex)
  out
}
detach("package:bipartite", unload=TRUE)
detach("package:igraph", unload=TRUE)
cent.nodo_x_ano_lista<-hace_cent_x_ano(redes_coex.positivas_lista, ignorar.pesos = TRUE)
head(cent.nodo_x_ano_lista$`2011`) # closeness da 0 en las redes desconectadas: 2005,2009,2011:2014,2016:2019
#graph_from_adjacency_matrix(redes_coex.positivas_lista$`2018`,mode = "undirected",weighted = TRUE,diag = FALSE)->oo
#is.connected(oo) 
cent.nodo_x_ano_lista.W<-hace_cent_x_ano(redes_coex.positivas_lista, ignorar.pesos = FALSE)

###__________________
# resolver que se hace con los rols de las spp que no est'an!! 
# resolver que hago con el closeness y las redes descoencatdas??

# 2) estimar Z y C, a partir de Modularidad, con paquete rnetcartoo
# sacar del objeto modularidad_roles$Roles.topo la columna de 5 de cada ano donde esta el rol de las spp., o los vectores connectivity y participation
# CUIDADO!! #cosas a resolver: 
# 1.en las redes desconectadas, no aparecen las spp que no estan conectadas, ver como resolver eso! 
# 2.hago una lista unica de spp, y que se pone en el rol de las spp que no estan en det anos?? 
dim(modularidad_roles$Roles.topo$`2006`);dim(redes_coex.positivas_lista$`2006`)
dim(modularidad_roles$Roles.topo$`2005`);dim(redes_coex.positivas_lista$`2005`)

Roles_netcarto<-modularidad_roles$Roles.topo

ordena_spp_roles_alf<-function(lista_roles_ano,nombres_spp=1){
  out<-list()
  for (i in 1:length(lista_roles_ano)) {
    out[[i]]<-lista_roles_ano[[1]][order(lista_roles_ano[[i]]),]
     }
  out
  names(out)<-names(lista_roles_ano)
  out
}

Roles_netcarto_ord_alf<-ordena_spp_roles_alf(Roles_netcarto)

## SEGUIR DE A PARTIR DE ACA!!!!!!!
# veo cual spp me falta en cada ano en el out de roles_netcarto, y veo con que valor la agrego, la agego al final y luego vuelvo a correr la funcion de ordena_alf
which((rownames(redes_coex.positivas_lista$`2005`)%in%Roles_netcarto_ord_alf$`2005`[,1])==FALSE)
rownames(redes_coex.positivas_lista$`2005`)[which((rownames(redes_coex.positivas_lista$`2005`)%in%Roles_netcarto_ord_alf$`2005`[,1])==FALSE)]
kk<-as.data.frame( matrix(0,nrow = length(jj[,1]),ncol = ncol(Roles_netcarto$`2005`)))
kk[,1]<-rownames(redes_coex.positivas_lista$`2005`)[which((rownames(redes_coex.positivas_lista$`2005`)%in%Roles_netcarto_ord_alf$`2005`[,1])==FALSE)]
colnames(kk)<-NULL;rownames(kk)<-NULL
#kk[,5]<-as.factor(kk[,5])
jj<-Roles_netcarto_ord_alf$`2005`
names(kk)<-names(jj)
ll<-rbind(jj,kk, deparse.level = 1)
ll # en la columna de role pone NA, si quiero que quede un 0 cambiar class de esa columna  
jj[,5]<-as.character(jj[,5])

## hacer esto para todas si es que voy a dejar en 0 esas que no son...el problema son los vectores que tienen valores negativos
# lo otro es poner un NA...
# la columna module la tendria que sacar...el mismo error voy a tener si incluyo todas las spp. todos los anos!

# otra forma, de encotrar las especies que no paarecen en el rnetcartoo, no tan clara:
kk<-roles_netcartoo_xano$`2011`[order(roles_netcartoo_xano$`2011`[,1]),]
which(rownames(cent_x_ano_lista$`2011`)!=kk[,1])
which(rownames(cent_x_ano_lista$`2011`[-26])!=kk[,1])
###________________###



# NESTEDRANK del bipartite
# ver si tiene sentido para una red unipartita y simetrica
?nestedrank
nestedrank(redes_coex.positivas_lista$`2011`, return.matrix = TRUE)->kk
kk$`higher level`; kk$`lower level`# dan lo mismo, asi que en este caso que se trata de una red unipartita simetrica me quedo con uno de esos vectores
View(kk$nested.matrix)
dim(redes_coex.positivas_lista$`2011`)
length(kk$`lower level`)

#_______________________________________________________________________________
###### COSAS A DESARROLLAR, PROFUNDIZAR: #####

###MOTIVOS...explorar en la literatura más...
library("sna")
triad.classify(M_dist_canelones,mode="graph")
triad.census(M_dist_canelones,mode="graph")
dyad.census(m3)

# ROLES TOPOLOGICOS:
# otras ideas para los roles topologicos...?
get.diameter(oo) #nodos implicados en ese diametro (distancia mas larga formada por las dist. mas cortas entre nodos)
as.vector(get.diameter(oo))->id # vector con los nodos implicados en el diametro
id
redes_coex_lista$`2011`[id,id]


transitivity(oo,type="local", weights = TRUE) # coeficiente de agrupamiento, de cada nodo, a diferencia del global que es en general de la red



####_______FIN 3ra parte________###
# Doctorado - ultima modificacion: 27 agosto 2020