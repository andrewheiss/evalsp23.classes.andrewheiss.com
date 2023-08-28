# Codigo practico 3 EC 2023: Matrices de correlación y tamaños de efecto
# Autor(a):
# Fecha:

# 1. Cargar librerías -----------------------------------------------------

pacman::p_load(tidyverse, # Manipulacion datos
               sjmisc, # Descriptivos
               sjPlot, # Tablas
               kableExtra, #Tablas
               GGally, # Correlaciones
               corrplot) # Correlaciones

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

# 2. Importar datos ---------------------------------------------------------

load(url("https://multivariada.netlify.app/assignment/data/proc/ELSOC_ess_merit2016.RData")) #Cargar base de datos

# 3. Explorar datos -------------------------------------------------------

# 3.1 Dimensiones y columnas

names(proc_elsoc)
dim(proc_elsoc)

# 3.2 Descriptivos

sjmisc::descr(proc_elsoc,
              show = c("label","range", "mean", "sd", "NA.prc", "n")) %>%
  kable(.,"markdown")

# 4. Procesamiento ---------------------------------------------------------

# 4.1 Tratamiento NAs ----

## a) Listwise

proc_elsoc_original <- proc_elsoc # respaldo base original
dim(proc_elsoc)

sum(is.na(proc_elsoc)) # contar NAs total

colSums(is.na(proc_elsoc)) # contar NAs por columna

proc_elsoc <- na.omit(proc_elsoc) # remover NAs
dim(proc_elsoc) # verificar


## b) Pairwise

mean(proc_elsoc_original$pmerit); mean(proc_elsoc$edad); mean(proc_elsoc$ess)

mean(proc_elsoc_original$pmerit, na.rm = TRUE); mean(proc_elsoc$edad, na.rm = TRUE); mean(proc_elsoc$ess, na.rm = TRUE)

# 5. Matrices de correlación ----------------------------------------------

# 5.1 Matriz simple ----

M <- cor(proc_elsoc_original, use = "complete.obs")
M

# 5.2 Tabla reportable ----

sjPlot::tab_corr(proc_elsoc_original,
                 triangle = "lower") # listwise


sjPlot::tab_corr(proc_elsoc_original,
                 na.deletion = "pairwise", # espeficicamos tratamiento NA pairswise
                 triangle = "lower")

# 5.3 Graficar ----

corrplot.mixed(M) # matriz

ggpairs(proc_elsoc) # matriz y scatter

sjPlot::plot_scatter(proc_elsoc, edad, ess) # solo scatterplot

# 6. Tamaños de efecto ----------------------------------------------------

# Criterios según Cohen (1988):
# < ±0.1 = Muy pequeño
# ±0.1–0.3 = Pequeño
# ±0.3–0.5 = Moderado
# >±0.5 = Grande


# Interpretación ejemplo:
# El coeficiente de correlación de Pearson entre estatus social subjetivo y
# años de educación es positivo y moderado (r = 0.3) según Cohen (1988).

# 7. Correlación Spearman -------------------------------------------------

# 7.1 Frecuencias ----

sjmisc::frq(proc_elsoc$mesfuerzo)

sjmisc::frq(proc_elsoc$mtalento)

# 7.2 Cálculo correlación ----

cor.test(proc_elsoc$mesfuerzo, proc_elsoc$mtalento,
         method = "spearman") #especificamos metodo spearman

# 8. R2 -------------------------------------------------------------------

coef_r <- M[4,3] # seleccionamos el coef de nuestra matriz

coef_r

coef_r^2 # cálculo coef de determinacion R2
