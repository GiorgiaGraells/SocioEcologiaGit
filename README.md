Socio Ecologia
================

## GitHub Documents

Antes de partir actualizar base de datos de las encuestas:
**/EncuestaAvesRegistro/respuestas\_COMPLETO.csv**

Distintos graficos exploratorios:

  - **Preliminar general.R**
  - **Preliminar por ambiente.R**
  - **Preliminar por especie.R**

En estos mismo archivos se incluyen gráficos de boxplot para mejor
representacion de los datos con 95% IC. Para esto se trabaja con la
variable **AmbOrigen**, donde se hace la encuesta, y **Ambiente** que es
el ambiente por el cual se pregunta

### Análisis

Primer intento de analisis de vías: **PLS\_PathAnalysis.R** a partir de:
**Prep datos outer model.R** y **Post\_PathAnalysis.R**

Analisis preliminares utilizando ordered logistic regression, para
presentar como alternativa de 55 réplicas **RegLog.Rmd**. Intento de
analisis de poder en **Poder\_OLR.R**.

#### Diciembre 2020

Hubo una confusion con un arreglo realizado a la base de datos de las
encuestas y fue solucionado. En un principio existian las opciones de
SSEE para las aves hasta “nada le agrada”, guardado en la tablet como
valor 5 y con una columna extra con la opcion “otro”. Se agregó la
opción “otro” con valor 6 a la pregunta de SSEE y se eliminó la columna
“otro” por no contar con informaciono suficiente como para tanerla como
columna. Cuando se eliminaron los NA para realizar la regresion
logistica, se borraron datos con valor 6 porque no habian sido pasados a
“Otro” en la columna llamada “AgradoCH”, lo mismo para “DesagradoCH”.

Esto fue arreglado en el código de **RegLog.Rmd**

Nuevos modelos considerando la variable de los encuestados como un
factor aleatorio. Se utilizó la funcion clmm en el docuemnto
**CumMixM.Rmd**, junto con esto esta el documento **Ecuacion.Rmd** que
describe la ecuacion del modelo ambiental seleccionado y **Curvas.R**
desde donde se extrae la iformacin de los modelos para armar las
predicciones y los graficos para las variables seleccionadas.
