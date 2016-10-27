Introducción y Puesta en Marcha en R
========================================================
author: Asier Goikoetxea eta Gorka Kobeaga
date: 2016/10/27
autosize: true

Contenidos de la Sesión
========================================================

- Presentación sobre R y algunas nociones básicas
    - R y R studio
    - Operaciones y Estadística Básica
    - Gráficos
    - Librerías (paquetes), funciones, recursos para seguir adelante...
- Taller práctico con datos públicos

Introducción
========================================================
- Qué es R
- Porqué es importante
    - Libre, gratuito, multi-plataforma
    - Comunidad muy activa y subiendo mucho en popularidad [stackoverflow](http://blog.revolutionanalytics.com/2015/07/in-celebration-of-100000-r-questions-on-stackoverflow.html) [stackoverflow2](http://blog.revolutionanalytics.com/2015/07/the-most-popular-programming-languages-on-stackoverflow.html) y [github](http://pypl.github.io/PYPL.html) 
    - Ecosistema de R: crear documentos con Latex/Markdown, informes o presentaciones pdf/html
- Qué limitaciones tiene: uso de memoria.

Presentación basada en el libro [YaRrr! Pirate's guide to R](http://nathanieldphillips.com/thepiratesguidetor/)

R y R studio
========================================================

CRAN y Rstudio.com
![RStudio](rstudio.png)

Programación
========================================================

Programar es un proceso artesanal:
- Hay infinitas formas correctas de escribir un mismo programa
- Criterios:
    - utilizar los menos codigos posibles
    - optimizar la velocidad
    - optimizar el uso de memoria o CPU...
    - que el codigo se entienda lo mejor posible

Instalar y Trabajar con Paquetes
========================================================

- R Base
- Instalar paquetes o extensiones
    - CRAN, GitHub


```r
# ******* instalar el paquete YaRrr! *******
#install.packages("devtools"); library(devtools)
#install.github("ndphillips/yarrr", build_vignette=TRUE)
library(yarrr)
```

Leer Datos
=========================================================
- Basico: read.csv / readlines / read.table / readRDS 
- Librerías: excel / hoja de calculo GDrive / SQL / Leer de la web


```r
palabras <- read.csv("unigram_ws.csv")
head(palabras)
```

```
  X Content Frequency
1 1     the    333505
2 2      to    191753
3 3     and    168449
4 4       a    166493
5 5      of    140199
6 6       i    115619
```

Explorar Nuestro Dataset
=========================================================


```r
nrow(pirates)
```

```
[1] 1000
```

```r
names(pirates)
```

```
 [1] "id"              "sex"             "age"            
 [4] "height"          "weight"          "headband"       
 [7] "college"         "tattoos"         "tchests"        
[10] "parrots"         "favorite.pirate" "sword.type"     
[13] "eyepatch"        "sword.time"      "beard.length"   
[16] "fav.pixar"       "grogg"          
```

=========================================================

Head / Tail

```r
head(pirates[, 1:6])
```

```
  id    sex age height weight headband
1  1   male  28 173.11   70.5      yes
2  2   male  31 209.25  105.6      yes
3  3   male  26 169.95   77.1      yes
4  4 female  31 144.29   58.5       no
5  5 female  41 157.85   58.4      yes
6  6   male  26 190.20   85.4      yes
```

Operaciones: coger una muestra de Datos
=========================================================
Filtrar por Columnas [ , X] / Filtrar por Filas [X, ]
Filtrar por condiciones lógicas: subset()
Asignar valor a una variable '<-' / 'alt -'

```r
muestra <- pirates[5:15 ,1:3] 
subset(muestra, sex=="male")
```

```
   id  sex age
6   6 male  26
10 10 male  30
12 12 male  20
14 14 male  26
```

Operaciones: Condiciones logicas
=========================================================

Comparar dos variables u objetos:


```r
12==2 #12!=2
```

```
[1] FALSE
```

```r
12>=2
```

```
[1] TRUE
```

```r
12<2
```

```
[1] FALSE
```

Operaciones: Condiciones logicas
=========================================================

%in%


```r
laborable <- c("lunes", "martes", "miercoles","jueves", "viernes")
hoy <- "jueves"
hoy%in%laborable
```

```
[1] TRUE
```

Operaciones: Estadística Descriptiva
=========================================================


```r
summary(pirates$age)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  11.00   24.00   27.00   27.36   31.00   46.00 
```

```r
table(pirates$sex)
```

```

female   male  other 
   464    490     46 
```

Operaciones: Tablas
=========================================================


```r
table(pirates$sex, pirates$sword.type)
```

```
        
         banana cutlass sabre scimitar
  female     21     379    31       33
  male       22     414    32       22
  other       3      37     4        2
```

Operaciones: Estadística Descriptiva
=========================================================
mean, median, range, var, sd, iqr, max, min

```r
aggregate(age ~ sex, data = pirates, FUN = mean)
```

```
     sex      age
1 female 29.92241
2   male 24.96735
3  other 27.00000
```

Operaciones: Estadística Descriptiva
=========================================================


```r
aggregate(age ~ sex + sword.type, data = pirates, FUN = mean)
```

```
      sex sword.type      age
1  female     banana 31.14286
2    male     banana 24.54545
3   other     banana 27.00000
4  female    cutlass 29.91821
5    male    cutlass 25.11353
6   other    cutlass 26.91892
7  female      sabre 28.35484
8    male      sabre 24.21875
9   other      sabre 28.50000
10 female   scimitar 30.66667
11   male   scimitar 23.72727
12  other   scimitar 25.50000
```

Gráficos: Histograma
=========================================================


```r
hist(x=pirates$tattoos, main="Frequencia del número de Tatuajes de los Piratas", xlab = "Número de Tatuajes", ylab = "Frequencia", col="skyblue", border="white")
```

![plot of chunk unnamed-chunk-12](Introducción y Puesta en Marcha en R-figure/unnamed-chunk-12-1.png)

Diagrama de Cajas (BoxPlot)
=========================================================


```r
boxplot(age ~ sword.type, data= pirates, col= c("red", "blue", "green", "orange"))
```

![plot of chunk unnamed-chunk-13](Introducción y Puesta en Marcha en R-figure/unnamed-chunk-13-1.png)

Diagrama de Dispersión (ScatterPlot)
=========================================================


```r
plot(x= pirates$weight, y= pirates$height, main= "Relación entre Peso y la Altura de los Piratas", xlab= "Peso (kg)", ylab = "Altura (cm)", pch=16, col=gray(.0,.4))
linea <- lm(height ~ weight, data = pirates); abline(linea, col="blue", lwd=3)
```

![plot of chunk unnamed-chunk-14](Introducción y Puesta en Marcha en R-figure/unnamed-chunk-14-1.png)

Gráficos Avanzados
=========================================================
- ggplot2
- gráficos interactivos:
    - Manipulate, rCharts, GoogleVis
- mapas:
    - Leaflet, google maps

Otros gráficos
=========================================================


```r
library(circlize)
chordDiagram(matrix(sample(10), nrow = 2,ncol = 5))
```

![plot of chunk unnamed-chunk-15](Introducción y Puesta en Marcha en R-figure/unnamed-chunk-15-1.png)

Otros gráficos
=========================================================


```r
# Create a pirateplot showing the distribution of ages
pirateplot(formula = age ~ sword.type,data = pirates, main = "Pirateplot of ages by favorite sword")
```

![plot of chunk unnamed-chunk-16](Introducción y Puesta en Marcha en R-figure/unnamed-chunk-16-1.png)

Loops
=========================================================
- Repetir un trozo de codigo varias veces

```r
trabajo <- 0
for (dia in laborable) {
    trabajo <- 8 + trabajo
    mensaje <- paste0(dia, ": suma de horas trabajadas => ", trabajo)
    print(mensaje)
}
```

```
[1] "lunes: suma de horas trabajadas => 8"
[1] "martes: suma de horas trabajadas => 16"
[1] "miercoles: suma de horas trabajadas => 24"
[1] "jueves: suma de horas trabajadas => 32"
[1] "viernes: suma de horas trabajadas => 40"
```

GSUB
=========================================================
- Sustituir caracteres por otros en un texto: literal / [regex](http://regexr.com/)


```r
gsub("trabajadas", "programando", mensaje)
```

```
[1] "viernes: suma de horas programando => 40"
```

```r
gsub("[aeiou]", "_", mensaje)
```

```
[1] "v__rn_s: s_m_ d_ h_r_s tr_b_j_d_s => 40"
```

Recursos para seguir aprendiendo
=========================================================
- [Yarrr! Pirate's guide to R](http://nathanieldphillips.com/thepiratesguidetor/)
- [R Programing for data science](https://leanpub.com/rprogramming)
- [R-Bloggers](https://www.r-bloggers.com/)

y mucho más...
