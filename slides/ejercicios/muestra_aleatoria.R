pacman::p_load(googlesheets4, dplyr)


# Data Dados
data_dados <- read_sheet("https://docs.google.com/spreadsheets/d/1YrMd_ds5zHgQWrdjYcX5Diwv7bQHDzf5r2A0oYWriyA")
names(data_dados)
summary(data_dados)
data_dados %>% select(caso1:caso5) %>% slice(1L) %>% sd() # check

sd(data_dados$Prom)# sd de los promedios, para comparar con los SEs

hist(data_dados$Prom, breaks = 15)

# Datos simulados, 15 muestras 30 casos c/u

df <- as.data.frame(matrix(round(runif(n=450, min=1, max=6), 0), nrow=15))

dim(df)

df$promedio <- df %>% rowMeans() # agregar promedio

hist(df$promedio)
sd(df$promedio)

sd <- apply(df, 1, sd)  

df$sd_muestras <-sd
