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
         SIEMBRA_ANIO,COSECHA_MES,EFEC_CLIM_PER_COSECHA,
         AR_SEMOPLAN,REND_OP_2019) 

#Base de datos final

Base_datos_ENA=suelo %>% inner_join(productor,by="KEYPPAL")

Base_datos_lotes_ENA=lote 
                                     
