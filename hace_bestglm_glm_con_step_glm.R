# columnmas de MM: atributos+sitio+metacom
# rownames(MM): spp

### bestglm para varias comunidades
#colnames(MM) ## MM is a data.frame
comunidades<-16:73
  A<-matrix(0,nrow=length(comunidades),ncol=15)
  colnames(A)<-colnames(MM)[1:15] ## traits
  rownames(A)<-colnames(MM)[comunidades] ## community richness
  for (i in 16:73){
  M.best.i<-bestglm(MM[,c(1:15,i)],family = poisson, nvmax =5)
  id.a<-match(names(M.best.i$BestModel$coefficients)[-1],colnames(A))
  A[i-15,id.a]<-(M.best.i$BestModel$coefficients)[-1]
} 
  A

#########################
#A->A.nvmax.4  
apply(ifelse(A.nvmax.4!=0,1,0),2,sum)
#A->A.nvmax.5  
apply(ifelse(A.nvmax.5!=0,1,0),2,sum)
#A->A.nvmax.6
apply(ifelse(A.nvmax.6!=0,1,0),2,sum)

###################################################################
## hace glm para cada comunidad
dim(MM) ## MM es data.frame
comunidades<-16:73
B<-matrix(0,nrow=length(comunidades),ncol=16)
colnames(B)<-c(colnames(MM)[1:15],"r2") ## traits
# Bd+Mo+ciegos+Bbn+OgSf+EySz+EyPo+PfPo+PfSh+FsSf+MxLg+BdEg+Mass+Dieta

rownames(B)<-colnames(MM)[comunidades] ## community richness
for (i in 16:73){
  N.j<-MM[,i]
  #M.temp<-(glm(N.j~Bd+Mo+ciegos+Bbn+OgSf+EySz+EyPo+PfPo+PfSh+FsSf+MxLg+BdEg+Mass+Dieta,family = poisson,offset=metacomm, data=MM))
  M.temp<-(glm(N.j~Bd+ciegos+OgSf+PfSh+MxLg+Dieta,family = poisson,offset=metacomm, data=MM)) #
 # p<-coef(M.temp)
  p.value<-(summary(M.temp))$coefficients[,4]
  #print(p)
  r2<-(M.temp$null.deviance-M.temp$deviance)/M.temp$null.deviance
  B[i-15,16]<-r2
 # id.a<-match(names(p)[-1],colnames(A))
  id.p<-match(names(p.value)[-1],colnames(A))
  #B[i-15,id.a]<-p[-1]
  B[i-15,id.p]<-p.value[-1]
 
}
B

###
B->B.p.values

#########
B.step<-matrix(0,nrow=length(comunidades),ncol=16)
colnames(B.step)<-c(colnames(MM)[1:15],"r2") ## traits
rownames(B.step)<-colnames(MM)[comunidades] ## community richness
for (i in 16:73){
  N.j<-MM[,i]
  #M.temp<-(glm(N.j~Bd+Mo+ciegos+Bbn+OgSf+EySz+EyPo+PfPo+PfSh+FsSf+MxLg+BdEg+Mass+Dieta,family = poisson,offset=metacomm, data=MM))
  M.temp<-(glm(N.j~Bd+Mo+ciegos+Bbn+OgSf+EySz+EyPo+PfPo+PfSh+FsSf+MxLg+BdEg+Mass+Dieta,family = poisson,offset=metacomm, data=MM))
  M.temp.step<-step(M.temp)
  p<-coef(M.temp.step)
  print(p)
  r2<-(M.temp.step$null.deviance-M.temp.step$deviance)/M.temp.step$null.deviance
  B.step[i-15,16]<-r2
  id.a<-match(names(p)[-1],colnames(MM)[1:15])
  B.step[i-15,id.a]<-p[-1]
}
B.step
#########


##########
#B; B.step
a<-data.frame(cbind(B[-c(2,5,10,21,38,45,54),1:16], ## B tiene los coeficientes del glm
                    poder_RN_obs[-c(2,5,10,21,38,45,54)],
                    R_sitio_amb.atr[-c(2,5,10,21,38,45,54),]))
k<-as.vector(which(B[,16]>0.5));k
a<-data.frame(cbind(B[-c(k),1:16], ## B tiene los coeficientes del glm
                    poder_RN_obs[-c(k)],
                    R_sitio_amb.atr[-c(k),]))

#a<-data.frame(cbind(B, poder_RN_obs,R_sitio_amb.atr))

names(a)<-c(colnames(B),"poder",colnames(R_sitio_amb.atr))

head(a)
#ggplot(a)+aes(poder, Oral)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()

span.plot=1
Bd.plot<-ggplot(a)+aes(Closenn, Bd)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
M0.plot<-ggplot(a)+aes(Closenn, Mo)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
ciegos.plot<-ggplot(a)+aes(Closenn, ciegos)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
Bdn.plot<-ggplot(a)+aes(Closenn, Bbn)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
OgSf.plot<-ggplot(a)+aes(Closenn, OgSf)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
EySz.plot<-ggplot(a)+aes(Closenn, EySz)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
EyPo.plot<-ggplot(a)+aes(Closenn, EyPo)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
PfPo.plot<-ggplot(a)+aes(Closenn, PfPo)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
PfSh.plot<-ggplot(a)+aes(Closenn, PfSh)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
FsSf.plot<-ggplot(a)+aes(Closenn, FsSf)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
MxLg.plot<-ggplot(a)+aes(Closenn, MxLg)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
BdLSh.plot<-ggplot(a)+aes(Closenn, BdLSh)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
BdEg.plot<-ggplot(a)+aes(Closenn, BdEg)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
Mass.plot<-ggplot(a)+aes(Closenn, Mass)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
Dieta.plot<-ggplot(a)+aes(Closenn, Dieta)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
r2.plot<-ggplot(a)+aes(Closenn, r2)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
library(gridExtra)
grid.arrange(Bd.plot,ciegos.plot,OgSf.plot,PfSh.plot,
              MxLg.plot, Dieta.plot,  ncol=3, nrow=2) #MxLg

grid.arrange(Bd.plot, M0.plot,ciegos.plot, Bdn.plot, OgSf.plot,EySz.plot,EyPo.plot,
             PfPo.plot, PfSh.plot,MxLg.plot, Mass.plot, 
             Dieta.plot,  ncol=6, nrow=2)

######
a<-data.frame(cbind(RN.part_2, poder_RN_obs,R_sitio_amb.atr))

names(a)<-c(colnames(RN.part_2),"poder",colnames(R_sitio_amb.atr))

head(a)
span.plot=1
Efecto.rasgo.plot<-ggplot(a)+aes(Closenn, Efecto.rasgo)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
Efecto.metacomunidad.plot<-ggplot(a)+aes(Closenn,  Efecto.metacomunidad)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
Efecto.conjunto.plot<-ggplot(a)+aes(Closenn, Efecto.conjunto)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()
Deriva.rasgos.plot<-ggplot(a)+aes(Closenn, Deriva.rasgos)+ geom_point()+geom_smooth(span = span.plot)+ theme_classic()

grid.arrange(Efecto.rasgo.plot, Efecto.metacomunidad.plot,
             Efecto.conjunto.plot,Deriva.rasgos.plot,
             ncol=2, nrow=2)

