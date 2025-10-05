#Librerias
library(tidyverse)
library(scales)
library(readxl)
library(ggcorrplot)
library(visreg)
library(nortest)
library(lmtest)
library(ggthemes)
library(ggpmisc)

#base de datos

suelo=read.csv("suelo.csv",sep="|") %>% 
  select(KEYPPAL,UM_USUELO,AR_TRAN,AR_PERM,AR_INFAGR_SN,
         AR_INFAGR,AR_USUELO)

productor=read.csv("productor.csv",sep="|") %>% 
  select(KEYPPAL,NIVEL_ESTUD,EDAD_PROD,DED_PROD,PROD_SEX) %>% 
  rename(Estudios=2,Edad=3,Dedicacion=4,Sexo=5)

lote=read.csv("Lotes.csv",sep="|") %>% 
  select(KEYPPAL_L,KEYPPAL,CUL_TIPO,CANT_CUL)

cultivo=read.csv("cultivos.csv",sep="|") %>%
  select(KEYPPAL_CU,KEYPPAL_L,CUL_TIPO,SIEMBRA_MES,
         SIEMBRA_ANIO,COSECHA_MES,
         AR_SEMOPLAN,REND_OP_2019)

cosecha=read.csv("cosecha.csv",sep="|") %>% 
  select(KEYPPAL_CU,AR_COS,COSECHA_CANT,REND_DEF,PORC_VENTA,
         PRECIO_VENUM,PRECIO_X_KILO) %>% 
  rename(Area_cosecha=2,Cantidad_cosechada=3,Rendimiento=4,"%venta"=5)

#Base de datos final

Base_datos_ENA=suelo %>% inner_join(productor,by="KEYPPAL") %>%
  inner_join(lote,by="KEYPPAL")
  

Base_datos_cosecha_ENA=cultivo %>% 
  inner_join(cosecha,by="KEYPPAL_CU") %>% inner_join(lote,by="KEYPPAL_L")%>% 
  group_by(KEYPPAL) %>% 
  summarise(Ventas=log(sum(PRECIO_X_KILO*Cantidad_cosechada*1000))) %>% 
  inner_join(suelo,by="KEYPPAL") %>% inner_join(productor,by="KEYPPAL")%>% 
  filter(!is.na(Ventas))




  select(-KEYPPAL_CU,-KEYPPAL_L,-KEYPPAL) %>%
  mutate(Ventas=log(PRECIO_X_KILO*Cantidad_cosechada*1000)) 
#Modelos

Modelo_final=lm(Ventas~Estudios+Sexo+Edad+AR_USUELO,Base_datos_cosecha_ENA)
cor(Base_datos_cosecha_ENA)
summary(Modelo_final)
plot(Modelo_final,which=1)
plot(Modelo_final,which=2)
plot(Modelo_final,which=3)
plot(Modelo_final,which=5)
#Pruebas modelo
                                     
