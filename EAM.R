library(tidyverse)
library(scales)
library(readxl)
library(ggcorrplot)
library(visreg)
library(nortest)
library(lmtest)
library(ggthemes)
library(ggpmisc)


Base_de_datos_EAM=read_xlsx("EAM.XLSX") %>%
  select(dpto,c7r10c2,c3r19c3,c3r35c1,c3r10c1,
         r4csap)

            