#Pruebas de Sebastian

#universidad del valle
%>% select(DIRECTORIO,Municipio,Estrato,`Ingreso del hogar`,
           Arriendo_estimacion,`Cantidad de personas en el hogar`,
           Sexo,Edad,`Ultimo grado alcanzado`,
           ,sastifacion,PERCAPITA)

Modelo_final=lm(`Ingreso del hogar`~Sexo+Tiempo_trabajado+`Ultimo grado alcanzado`+
                  `Cantidad de personas en el hogar`+Edad,Base_datos)

summary(Modelo_final)
plot(Modelo_final,which=1)
plot(Modelo_final,which=2)
plot(Modelo_final,which=3)
plot(Modelo_final,which=5)