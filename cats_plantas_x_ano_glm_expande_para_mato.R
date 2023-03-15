### GLM CON MATRIZ EXPANDE Y GRAFICO DE LAS FUNCIONES QUE RELACIONA COMO 
# CAMBIA EL COEFICIENTE DE SELECCIÓN DE UN ATRIBUTO EN FUNCIÓN DE VAR. AMBIENTALES

# 1) 1ero genero matriz con la que voy a trabajar:
# agrega N en t-1 como fila (también se podría N en t-2), y rasgos dummy vulven a categoría donde se asigna un nro a cada nivel
# 2) 2do se corre glm con matriz de expande
# 3) 3ero agrega efectos aleatorios: sp.id y/o año
# 4) grafico resultados: lambda Atributo vs var. ambiental a partir de los parámetros del modelo glm

# 1)_______________________________________________________
# bases a usar:
# 1) M_expande 1136 x 32, base de datos completa a usar, sin efecto de la memoria, pero con potenciales efectos aleatorios
# 2) metacomm_expande (rep. x 16 años de colsum(L)) 
# 3) M_expande_mem 1065 x 33, M_expande, sacando todas las filas correspondiente al 1er año (2005), y agregandole vector de memoria, N en t-1

# 2)_______________________________________________________
# GLMs
library(MuMIn)
library(lme4)

### 1er modelo sin memoria 
modelo1_expande<-glm(N~log.Heigth+FPC+SLA+forma.disp+tamano.semilla+Luvias_mm+TAire_media_C+Tmax_media_C+
                       IntV_prom_km.h+log.Heigth__x__Luvias_mm+log.Heigth__x__TAire_media_C+log.Heigth__x__Tmax_media_C+log.Heigth__x__IntV_prom_km.h+
                       FPC__x__Luvias_mm+FPC__x__TAire_media_C+FPC__x__Tmax_media_C+FPC__x__IntV_prom_km.h+SLA__x__Luvias_mm+SLA__x__TAire_media_C+
                       SLA__x__Tmax_media_C+SLA__x__IntV_prom_km.h+forma.disp__x__Luvias_mm+forma.disp__x__TAire_media_C+
                       forma.disp__x__Tmax_media_C+forma.disp__x__IntV_prom_km.h+tamano.semilla__x__Luvias_mm+
                       tamano.semilla__x__TAire_media_C+tamano.semilla__x__Tmax_media_C+  tamano.semilla__x__IntV_prom_km.h,
                     family="poisson", offset = metacomm_expande/sum(metacomm),data=M_expande_num)

summary(modelo1_expande)
## estimamos R2 de la poisson: 0.120065
r2.modelo1_expande<-(modelo1_expande$null.deviance - modelo1_expande$deviance)/modelo1_expande$null.deviance
# trato de ir simplificando (achicando) el modelo:
# drop1(modelo1_expande, test="Chisq")->modelo1_expande.DROP
# modelo1_expande.DROP
step(modelo1_expande, direction = "both")->modelo1_expande_step
summary(modelo1_expande_step) ### este es el modelo definitivo sin memoria

### 2do modelo con memoria (hay que sacar 2005 del offset metacomm_expande)
modelo1_expande_memoria <- glm(formula = N ~ FPC + SLA + forma.disp + Luvias_mm + TAire_media_C + 
                                 Tmax_media_C + IntV_prom_km.h + log.Heigth__x__Luvias_mm + 
                                 log.Heigth__x__TAire_media_C + log.Heigth__x__Tmax_media_C + 
                                 log.Heigth__x__IntV_prom_km.h + FPC__x__TAire_media_C + FPC__x__Tmax_media_C + 
                                 FPC__x__IntV_prom_km.h + SLA__x__Luvias_mm + SLA__x__TAire_media_C + 
                                 SLA__x__IntV_prom_km.h + forma.disp__x__Luvias_mm + forma.disp__x__TAire_media_C + 
                                 forma.disp__x__IntV_prom_km.h + tamano.semilla__x__TAire_media_C + 
                                 tamano.semilla__x__Tmax_media_C + tamano.semilla__x__IntV_prom_km.h + memoria_t1, 
                               family = "poisson", data = M_expande_mem_num, offset = metacomm_expande[-(1:71)]/sum(metacomm))
summary(modelo1_expande_memoria)
AIC(modelo1_expande_memoria, modelo1_expande_step) # modelo con mem tiene mucho menor AIC, por lo que retenemos el modelo con mem
# plot de memoria como var explicativa:
ggplot(data=M_expande_mem_num, aes(log(memoria_t1), log(N))) + geom_point(size=3, shape=19, color="navy")+
  geom_smooth(span=1)+ labs(x="Memoria del sistema (N t-1)", y="N") + theme_classic(base_size = 15)#, title="Media rasgo vs gradiente")+
theme_classic(base_size = 15)
#kk <- glm(formula = N ~ memoria_t1, family = "poisson", data = M_expande_mem_num, offset = metacomm_expande[-(1:71)]/sum(metacomm))
# AIC: modelo1_expande_memoria- 35296.85 , kk- 39912.70
# ya que la var memoria presenta una alta significancia, sigo el retso de las exploraciones y analisis con la matriz que tiene la variable memoria

#________________________
### MIXTO: con efectos aleatorios y memoria
# parto del modelo con memoria ya que fue el que mejor con una diferencia muy significativa de AIC
modelo1_mem_expande_mixto<-glmer(N~log.Heigth+FPC+SLA+forma.disp+tamano.semilla+Luvias_mm+TAire_media_C+Tmax_media_C+
                             IntV_prom_km.h+log.Heigth__x__Luvias_mm+log.Heigth__x__TAire_media_C+log.Heigth__x__Tmax_media_C+log.Heigth__x__IntV_prom_km.h+
                             FPC__x__Luvias_mm+FPC__x__TAire_media_C+FPC__x__Tmax_media_C+FPC__x__IntV_prom_km.h+SLA__x__Luvias_mm+SLA__x__TAire_media_C+
                             SLA__x__Tmax_media_C+SLA__x__IntV_prom_km.h+forma.disp__x__Luvias_mm+forma.disp__x__TAire_media_C+
                             forma.disp__x__Tmax_media_C+forma.disp__x__IntV_prom_km.h+tamano.semilla__x__Luvias_mm+
                             tamano.semilla__x__TAire_media_C+tamano.semilla__x__Tmax_media_C+  tamano.semilla__x__IntV_prom_km.h +memoria_t1 + 
                             (1|spp_var.aleat)+ (1|ano_var.aleat),family="poisson", offset = metacomm_expande[-(1:71)]/sum(metacomm),data=M_expande_mem_num)
plot(modelo1_mem_expande_mixto)
summary(modelo1_mem_expande_mixto)

# falla la convergencia...probamos escalando las variables de la matriz, pero sigue teniendo problema para converger...(ver mas abajo en script completo)
# por lo tanto seguimos adelante con la base de datos sin escalar y obviamos el detalle de que los modelos no convergen!

# limpiamos el modelo a partir del summary(modelo1_mem_expande_mixto), hacemos una reducción de variables, 
# igual debemos retener los efectos simples de las variables, aunque no sean significativas si es que aparecen en interacciones significativas
modelo2_mem_expande<-glmer(N~FPC+SLA+IntV_prom_km.h+log.Heigth+Luvias_mm+TAire_media_C+Tmax_media_C+forma.disp+tamano.semilla+
                           log.Heigth__x__Luvias_mm+log.Heigth__x__TAire_media_C+ FPC__x__TAire_media_C+FPC__x__Tmax_media_C+
                             FPC__x__IntV_prom_km.h+SLA__x__Luvias_mm+SLA__x__TAire_media_C+SLA__x__IntV_prom_km.h+forma.disp__x__Luvias_mm+
                             forma.disp__x__TAire_media_C+forma.disp__x__Tmax_media_C+forma.disp__x__IntV_prom_km.h+
                             tamano.semilla__x__Luvias_mm+tamano.semilla__x__Tmax_media_C+tamano.semilla__x__IntV_prom_km.h+memoria_t1 + 
                             (1|spp_var.aleat)+ (1|ano_var.aleat),family="poisson", offset = metacomm_expande[-(1:71)]/sum(metacomm),data=M_expande_mem_num)
summary(modelo2_mem_expande)
AIC(modelo1_mem_expande_mixto, modelo2_mem_expande) # modelo1_mem_expande_mixto- 11360.78, modelo2_mem_expande- 11356.84

# 3)_______________________________________________________________________________________________________
# Plots de la variación de los coef. de selección en función de las variables ambientales
# el modelo final es: modelo2_mem_expande
colnames(M_expande_mem_num)
coeffs(modelo2_mem_expande)->p ; p
par(mfrow=c(4,5), mar=c(4,2,2,1))
### CON VIENTO
# 1.FPC vs Viento:
curve(p[1]+p[2]+(p[4]+p[15]+(p[18]*mean(M_expande_mem_num$SLA)+p[22]*mean(M_expande_mem_num$forma.disp)+p[25]*mean(M_expande_mem_num$tamano.semilla)))*x, col="red", lwd=3,from=min(R$`IntV_prom(km/h)`), 
      to=max(R$`IntV_prom(km/h)`), xlab="Inten. del viento (km/h)")#ylab="FPC (lambda)",
title("FPC")
# 2.SLA vs Viento:
curve(p[1]+p[3]+(p[4]+p[18]+(p[15]*mean(M_expande_mem_num$FPC))+(p[22]*mean(M_expande_mem_num$forma.disp))+(p[25]*mean(M_expande_mem_num$tamano.semilla)))*x, col="red", lwd=3,from=min(R$`IntV_prom(km/h)`), 
      ylab="SLA (lambda)",to=max(R$`IntV_prom(km/h)`), xlab="Inten. del viento (km/h)")
title("SLA")
# 3.log altura vs Viento:
curve(p[1]+p[5]+(p[4]+p[15]*mean(M_expande_mem_num$FPC)+p[18]*mean(M_expande_mem_num$SLA)+(p[22]*mean(M_expande_mem_num$forma.disp))+(p[25]*mean(M_expande_mem_num$tamano.semilla)))*x, col="red", lwd=3,from=min(R$`IntV_prom(km/h)`), 
      ylab="Log_Altura (lambda)",to=max(R$`IntV_prom(km/h)`), xlab="Inten. del viento (km/h)")
title("log(altura)")
# 4.forma dispersión vs Viento:
curve(p[1]+p[9]+(p[4]+p[22]+p[15]*mean(M_expande_mem_num$FPC)+p[18]*mean(M_expande_mem_num$SLA)+(p[25]*mean(M_expande_mem_num$tamano.semilla)))*x, col="red", lwd=3,from=min(R$`IntV_prom(km/h)`), 
      ylab="Forma semilla (lambda)",to=max(R$`IntV_prom(km/h)`), xlab="Inten. del viento (km/h)")
title("Forma semilla")
# 5.tamano vs Viento:
curve(p[1]+p[10]+(p[4]+p[25]+p[15]*mean(M_expande_mem_num$FPC)+p[18]*mean(M_expande_mem_num$SLA)+(p[22]*mean(M_expande_mem_num$forma.disp)))*x, col="red", lwd=3,from=min(R$`IntV_prom(km/h)`), 
      ylab="Tamaño semilla (lambda)",to=max(R$`IntV_prom(km/h)`), xlab="Inten. del viento (km/h)")
title("Tamaño semilla")
### CON LLUVIAS
# 6.FPC vs Lluvia:
curve(p[1]+p[2]+(p[6]+p[16]*mean(M_expande_mem_num$SLA)+p[11]*mean(M_expande_mem_num$log.Heigth)+p[19]*mean(M_expande_mem_num$forma.disp)+p[23]*mean(M_expande_mem_num$tamano.semilla))*x, col="red", lwd=3,from=min(R$Luvias_mm), 
      ylab="FPC (lambda)",to=max(R$Luvias_mm), xlab="Lluvias (mm)")
# 7.SLA vs Lluvia:
curve(p[1]+p[3]+(p[6]+p[16]+p[11]*mean(M_expande_mem_num$log.Heigth)+p[19]*mean(M_expande_mem_num$forma.disp)+p[23]*mean(M_expande_mem_num$tamano.semilla))*x, col="red", lwd=3,from=min(R$Luvias_mm), 
      ylab="SLA (lambda)",to=max(R$Luvias_mm), xlab="Lluvias (mm)")
# 8.log altura vs Lluvia:
curve(p[1]+p[5]+(p[6]+p[11]+p[16]*mean(M_expande_mem_num$SLA)+p[19]*mean(M_expande_mem_num$forma.disp)+p[23]*mean(M_expande_mem_num$tamano.semilla))*x, col="red", lwd=3,from=min(R$Luvias_mm), 
      ylab="Log_Altura (lambda)",to=max(R$Luvias_mm), xlab="Lluvias (mm)")
# 9.forma dispersión vs Lluvia:
curve(p[1]+p[9]+(p[6]+p[19]+p[16]*mean(M_expande_mem_num$SLA)+p[11]*mean(M_expande_mem_num$log.Heigth)+p[23]*mean(M_expande_mem_num$tamano.semilla))*x, col="red", lwd=3,from=min(R$Luvias_mm), 
      ylab="Forma semilla (lambda)",to=max(R$Luvias_mm), xlab="Lluvias (mm)")
# 10.tamano vs Lluvia:
curve(p[1]+p[10]+(p[6]+p[23]+p[16]*mean(M_expande_mem_num$SLA)+p[11]*mean(M_expande_mem_num$log.Heigth)+p[19]*mean(M_expande_mem_num$forma.disp))*x, col="red", lwd=3,from=min(R$Luvias_mm), 
      ylab="Tamaño semilla (lambda)",to=max(R$Luvias_mm), xlab="Lluvias (mm)")

### CON TAire_media_C
# 11.FPC vs `TAire_media(ºC)`:
curve(p[1]+p[2]+(p[7]+p[13]+p[17]*mean(M_expande_mem_num$SLA)+p[12]*mean(M_expande_mem_num$log.Heigth)+p[20]*mean(M_expande_mem_num$forma.disp))*x, 
      col="red", lwd=3,from=min(R$`TAire_media(ºC)`), 
      ylab="FPC (lambda)",to=max(R$`TAire_media(ºC)`), xlab="Temp. Aire media (°C)")
# 12.SLA vs `TAire_media(ºC)`:
curve(p[1]+p[3]+(p[7]+p[17]+p[13]*mean(M_expande_mem_num$FPC)+p[12]*mean(M_expande_mem_num$log.Heigth)+p[20]*mean(M_expande_mem_num$forma.disp))*x, col="red", lwd=3,from=min(R$`TAire_media(ºC)`), 
      ylab="SLA (lambda)",to=max(R$`TAire_media(ºC)`), xlab="Temp. Aire media (°C)")
# 13.log altura vs `TAire_media(ºC)`:
curve(p[1]+p[5]+(p[7]+p[12]+p[13]*mean(M_expande_mem_num$FPC)+p[17]*mean(M_expande_mem_num$SLA)+p[20]*mean(M_expande_mem_num$forma.disp))*x, col="red", lwd=3,from=min(R$`TAire_media(ºC)`), 
      ylab="Log_Altura (lambda)",to=max(R$`TAire_media(ºC)`), xlab="Temp. Aire media (°C)")
# 14.forma dispersión vs `TAire_media(ºC)`:
curve(p[1]+p[9]+(p[7]+p[20]+p[13]*mean(M_expande_mem_num$FPC)+p[17]*mean(M_expande_mem_num$SLA)+p[12]*mean(M_expande_mem_num$log.Heigth))*x, col="red", lwd=3,from=min(R$`TAire_media(ºC)`), 
      ylab="Forma semilla (lambda)",to=max(R$`TAire_media(ºC)`), xlab="Temp. Aire media (°C)")
# 15.tamano vs `TAire_media(ºC)`:
curve(p[1]+p[10]+(p[7]+p[13]*mean(M_expande_mem_num$FPC)+p[17]*mean(M_expande_mem_num$SLA)+p[12]*mean(M_expande_mem_num$log.Heigth)+p[20]*mean(M_expande_mem_num$forma.disp))*x, col="red", lwd=3,from=min(R$`TAire_media(ºC)`), 
      ylab="Tamaño semilla (lambda)",to=max(R$`TAire_media(ºC)`), xlab="Temp. Aire media (°C)")

### CON Tmax_media(ºC)
# 16.FPC vs `Tmax_media(ºC)`:
curve(p[1]+p[2]+(p[8]+p[14]+p[21]*mean(M_expande_mem_num$forma.disp)+p[24]*mean(M_expande_mem_num$tamano.semilla))*x, col="red", lwd=3,from=min(R$`Tmax_media(ºC)`), 
      ylab="FPC (lambda)",to=max(R$`Tmax_media(ºC)`), xlab="Temp. Max media (°C)")
# 17.SLA vs`Tmax_media(ºC)`:
curve(p[1]+p[3]+(p[8]+p[14]*mean(M_expande_mem_num$FPC)+p[21]*mean(M_expande_mem_num$forma.disp)+p[24]*mean(M_expande_mem_num$tamano.semilla))*x, col="red", lwd=3,from=min(R$`Tmax_media(ºC)`), 
      ylab="SLA (lambda)",to=max(R$`Tmax_media(ºC)`), xlab="Temp. Max media (°C)")
# 18.log altura vs `Tmax_media(ºC)`:
curve(p[1]+p[5]+(p[8]+p[14]*mean(M_expande_mem_num$FPC)+p[21]*mean(M_expande_mem_num$forma.disp)+p[24]*mean(M_expande_mem_num$tamano.semilla))*x, col="red", lwd=3,from=min(R$`Tmax_media(ºC)`), 
      ylab="Log_Altura (lambda)",to=max(R$`Tmax_media(ºC)`), xlab="Temp. Max media (°C)")
# 19.forma dispersión vs `Tmax_media(ºC)`:
curve(p[1]+p[9]+(p[8]+p[21]+p[14]*mean(M_expande_mem_num$FPC)+p[24]*mean(M_expande_mem_num$tamano.semilla))*x, col="red", lwd=3,from=min(R$`Tmax_media(ºC)`), 
      ylab="Forma semilla (lambda)",to=max(R$`Tmax_media(ºC)`), xlab="Temp. Max media (°C)")
# 20.tamano vs `Tmax_media(ºC)`:
curve(p[1]+p[10]+(p[8]+p[24]+p[14]*mean(M_expande_mem_num$FPC)+p[21]*mean(M_expande_mem_num$forma.disp))*x, col="red", lwd=3,from=min(R$`Tmax_media(ºC)`), 
      ylab="Tamaño semilla (lambda)",to=max(R$`Tmax_media(ºC)`), xlab="Temp. Max media (°C)")
