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
