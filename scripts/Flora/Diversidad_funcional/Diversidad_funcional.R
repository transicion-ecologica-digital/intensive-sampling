# Paquetes necesarios
packages = c(
  "ggplot2", "ggmcmc", "agricolae", "fitdistrplus", "MASS", "cowplot", "qqplotr", 
  "ggeffects", "GGally", "broom", "doBy", "corrplot", "DHARMa", "pROC", "multcomp", 
  "multcompView", "car", "broom.mixed", "glmmTMB", "gamlss.dist", "bayesplot", 
  "reshape2", "gridExtra", "brms", "emmeans", "DirichletReg", "readxl", "tidyr", 
  "dplyr", "writexl", "stats", "ggrip_modif", "data.table", "jsonlite", "curl", 
  "tidyverse", "vegan", "FD", "AICcmodavg", "nlme", "GA", "gawdis", "lme4", 
  "pbkrtest", "lmerTest", "lavaan", "readr", "betapart", "dplyr", "tidyverse", "mgcv", 
  "geosphere", "performance", "ggpubr", "patchwork", "tidymv", "sjPlot","MuMIn")

invisible(lapply(packages, require, character.only = TRUE))

#===============================================================================
#                   ORDENAMIENTO DE BASES DE DATOS FLORA 2024 Y 2025
#===============================================================================

#2024

library(DBI)
library(RPostgres)
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "your-database",
  host = "your-IP",      # o la IP del servidor (ej. 192.168.1.50)
  port = 5432,             # 5432 es el puerto por defecto de Postgres
  user = "your-user",
  password = "your-password" # Ver nota de seguridad abajo
)

datos <- dbReadTable(con, "int_herbaceous_species")

dbDisconnect(con)

#Seleccionar solo datos del año 2024
data_2024 = datos %>%
  filter(format(as.Date(date, format="%Y-%m-%d"), "%Y") == "2024")

# Crear columna de ID de microestación + date
data = data_2024%>%
  mutate(ID_micro_date = paste(verbatim_microstation, date, sep = "_"))
#View(data)

# Reorganizar en formato largo incluyendo ID_micro_date solamente para las especies con cobertura verde  (vivas)

data_long = data %>%
  dplyr::select(ID_micro_date,
                ends_with("_species"),
                ends_with("_total_cover_perc")) %>%
  tidyr::pivot_longer(
    cols = -ID_micro_date,
    names_to = c("Estrato", ".value"),
    names_pattern = "(shrub|tree|scrub|herb)_(.*)"
  ) %>%
  dplyr::rename(Specie = species, Cover = total_cover_perc) %>%
  dplyr::filter(!is.na(Specie) & Specie != "" & !is.na(Cover))

#View(data_long)

# Agregar peso de subcuadrant (1/8)
data_long = data_long %>%
  mutate(Cover_micro_adj = Cover / 8)
#View(data_long)

# Sumar cobertura ajustada por especie en cada microestación-date
cover_species_micro_date_2024= data_long %>%
  group_by(ID_micro_date, Specie) %>%
  summarise(Specie_Cover_Microstation_Date = sum(Cover_micro_adj, na.rm = TRUE), .groups = "drop")
#View(cover_species_micro_date_2024)


#2025
#Seleccionar solo datos del año 2024
data_2025 = datos %>%
  filter(format(as.Date(date, format="%Y-%m-%d"), "%Y") == "2025")

# Crear columna de ID de microestación + date
data = data_2025%>%
  mutate(ID_micro_date = paste(verbatim_microstation, date, sep = "_"))
#View(data)

# Reorganizar en formato largo incluyendo ID_micro_date solamente para las espcies con cobertura verde (vivas)
data_long = data %>%
  dplyr::select(ID_micro_date,
                ends_with("_species"),
                ends_with("_total_cover_perc")) %>%
  tidyr::pivot_longer(
    cols = -ID_micro_date,
    names_to = c("Estrato", ".value"),
    names_pattern = "(shrub|tree|scrub|herb)_(.*)"
  ) %>%
  dplyr::rename(Specie = species, Cover = total_cover_perc) %>%
  dplyr::filter(!is.na(Specie) & Specie != "" & !is.na(Cover))

#View(data_long)

# Agregar peso de subcuadrant (1/8)
data_long = data_long %>%
  mutate(Cover_micro_adj = Cover / 8)
#View(data_long)

# Sumar cobertura ajustada por especie en cada microestación-date
cover_species_micro_date_2025= data_long %>%
  group_by(ID_micro_date, Specie) %>%
  summarise(Specie_Cover_Microstation_Date = sum(Cover_micro_adj, na.rm = TRUE), .groups = "drop")
#View(cover_species_micro_date_2025)

#Guardar bases de datos para manipular en excel
#write.csv(cover_species_micro_date_2024, "especies_cover2024", row.names = FALSE)
#write.csv(cover_species_micro_date_2025, "especies_cover2025", row.names = FALSE)

#Ajuste de la tabla en excel para que queden las columnas asi:
# "ID_micro_date", "verbatim_microstation", "date", "Specie", "cover"

#Nombre de  base de datos: "Micro_date_cobertura.csv"


#ORDENAMIENTO DE BASES PARA CALCULOS DE DIVERSIDAD FUNCIONAL

setwd("Inserte_su_directorio")
especies_2024_2025 = read.csv("Micro_fecha_cobertura.csv", header=T,  sep = ",", stringsAsFactors = T)

#View(especies_2024_2025)
#colnames(especies_2024_2025)

#Eliminar NAs
especies_2024_2025 = especies_2024_2025 %>% 
  filter(
    !is.na(specie),
    specie != "",
    !is.na(cover)
  )

#Pasar especies a columnas según columna "ID_micro_date"
sp_cover_date = especies_2024_2025 %>% pivot_wider(names_from = specie, values_from = cover, values_fill = 0)
#View(sp_cover_date)

#write.csv(sp_cover_date, "sp_cover_date", row.names = FALSE)


#Calculo de Diversidad Funcional
setwd("Inserte_su_directorio")
#Base de rasgos
traits = read.csv("sp_traits.csv", header=T,  sep = ",", stringsAsFactors = T, row.names = 1)
#base de cobertura formato wide
cover =  read.csv("sp_cover_fecha.csv", header=T,  sep = ",", stringsAsFactors = T)
#View(cover)
#View(traits)

#Prepar datos

#Solo herbaceas y mata
traits = traits[traits$Strata %in% c("Her", "Mat"), ]
#View(traits)
nrow(traits)
ncol(cover)
ncol(traits)

#Creacion de matriz de traits
traits.matrix = traits[,c(1:9)]
#View(traits.matrix)

#Transformar cover a una matriz para poder incluirla en la función dbFD
ncol(cover)
#view(cover)
cover.matrix = as.matrix(cover[,4:186])
#View(cover.matrix)
#Agregar columna Micro_date
rownames(cover.matrix)=cover$ID_micro_date
#View(cover.matrix)

#chequear que las especies sean las mismas en ambos set de datos
identical(sort(colnames(cover.matrix)), sort(rownames(traits.matrix)))
#Si da False, Identificar cuales son las especies que no coinciden en nombres
col_names = colnames(cover.matrix)
row_names = rownames(traits.matrix)
# Species in col_names but not in row_names
diff1 = setdiff(col_names, row_names)
print(diff1)
# Species in row_names but not in col_names
diff2 = setdiff(row_names, col_names)
print(diff2)

#re-chequear
col_names = colnames(cover.matrix)
row_names = rownames(traits.matrix)
ncol(cover.matrix)
nrow(traits.matrix)

##Hacer que también coincidan el orden de las columnas y tablas 
common_species = intersect(colnames(cover.matrix), rownames(traits.matrix))
traits.matrix = traits.matrix[common_species, ]
cover.matrix  = cover.matrix[, common_species]
#View(cover.matrix)
#View(traits.matrix)
ncol(cover.matrix)
nrow(traits.matrix)
dim(traits.matrix)
dim(cover.matrix)

traits.matrix = data.frame(lapply(traits.matrix, as.numeric), row.names = rownames(traits.matrix))

#Verificar si quedaron verbatim_microstationes con cobertura de 0 especies de herbaceas, mata o arbustos o con menos de 2 especies presentes
row_sums = rowSums(cover.matrix)
# Identificar las filas con suma = 0
zero_sites = which(row_sums == 0)
# Mostrar los nombres o índices de las comunidades vacías
zero_sites
#View(cover.matrix)
#Eliminar esas verbatim_microstationes
cover.matrix = cover.matrix[row_sums > 0, ]


###Indices de Diversidad Funcional Herbaceas y mata para ambos años
#Calculo de Indices de Diversidad Funcional
library(FD)
FD_SierraNevada_2024_2025 = dbFD(traits.matrix, cover.matrix, w.abun = TRUE, stand.x = TRUE,
                                 calc.CWM = F, calc.FDiv = T, corr = "cailliez")

FD = as.data.frame(FD_SierraNevada_2024_2025)
View(FD)

#Agregar columna verbatim_microstation y date
FD$verbatim_microstation = rownames(FD)
#View(FD)

#exportar
#write.csv(FD, "FD_SierraNevada_2024_2025.csv", row.names = T)

###################### Indices de diversidad taxonómica ########################

sp_cover =  read.csv("sp_cover_fecha.csv", header=T,  sep = ",", stringsAsFactors = T, row.names = 1)
colnames(sp_cover)

#Eliminar columnas micro y date para que queden solo las especies
sp_cover = sp_cover[, !(names(sp_cover) %in% c("micro", "date"))]
#View(sp_cover)


#  Calcular índices de diversidad
taxo_results = data.frame(
  Sitio = rownames(sp_cover),
  Shannon = diversity(sp_cover, index = "shannon"),      # índice de Shannon (H')
  Simpson = diversity(sp_cover, index = "simpson")      # índice de Simpson (1 - D)
)

# Revisar resultados
print(taxo_results)
#View(taxo_results)

#Exportar
#write.csv(taxo_results, "Shannon-Simpson.csv", row.names = FALSE)


#===============================================================================

#               ANÁLISIS DE DIVERSIDAD FUNCIONAL Y TAXONÓMICA DE 
#              FLORA EN verbatim_microstationES DE SIERRA NEVADA, GRANADA        
#          DATOS COMBINADOS POR AÑO (2024 y 2025), date Y MICROESTACIÓN

#===============================================================================

#Preparación de datos

setwd("Inserte_su_directorio")
Div =  read.csv("DivFlora_SierraNevada_2024_2025.csv", header=T,  sep = ",", stringsAsFactors = T)
#View(Div)

#Transformar NAs en 0
Div[is.na(Div)] = 0

#Pasar a factor o numerico
Div$Microestacion = factor(Div$Microestacion)
Div$Parcela = factor(Div$Parcela)
Div$Tratamiento = factor(Div$Tratamiento)
Div$Cota = factor(Div$Cota)       # Cota categórica 
Div$Cota_num = as.numeric(as.character(Div$Cota))  # Cota numerica
Div$Orientacion = factor(Div$Orientacion)

#View(Div)

#Para estos análisis Filtrar Solamente Camarate (Orientación NORTE) y Cañar (Orientación SUR)
Div_Can_Cam = Div[Div$Parcela %in% c("Camarate", "Canar"), ]
#View(Div_Can_Cam)

#plots rapidos de cajas solo para visualizar
ggplot(Div_Can_Cam, aes(x = factor(Parcela), y = FDis)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  theme_minimal() 

#################### DISTRIBUCIÓN DE VARIABLES RESPUESTA ######################

#Cambiar solamente la variable (índices)

normal=fitdist(Div_Can_Cam$FDiv,"norm")
poisson=fitdist(Div_Can_Cam$FDiv,"pois")
negbinom=fitdist(Div_Can_Cam$FDiv,"nbinom")
gamma=fitdist(Div_Can_Cam$FDiv, "gamma")

cdf.sp=cdfcomp(list(normal, poisson, negbinom, gamma), main="", legendtext =c("Normal", "Poisson", "BinNeg", "Gamma"),fitcol = c("orange", "blue", "green", "purple"), plotstyle ="ggplot")+
  geom_line(linewidth=1.2)+
  theme(axis.title=element_text(size=18), 
        axis.text = element_text(size=16), 
        legend.position = c(0.70,0.25),
        legend.text=element_text(size=16))
qq.sp=qqcomp(list(normal, poisson, negbinom, gamma), main="",fitcol = c("orange","blue", "green", "purple"), plotstyle 	="ggplot")+
  geom_line(linewidth=1.2)+
  theme_bw()+
  theme(axis.title=element_text(size=18), 
        axis.text = element_text(size=16),
        legend.position ="none")
#graficos de distribuciónes
plot(cdf.sp)
plot(qq.sp)
gofstat(list(normal, poisson, negbinom, gamma))$aic

#Todas las  variables respuesta (índices de diversidad) tienen una distribución
#tendiendo a la normalidad

########################### ANALISIS DE DATOS #################################

#INDICES DE DIVERSIDAD FUNCIONAL

#DISPERSIÓN FUNCIONAL (FDis)

#FDis: Es la propagación de los valores de los rasgos ponderado por la abundancia (cobertura)
# de las especies al centrioide del espacio funcional (9 rasgos = 9 dimensional)
#(Mason y Mouillot, 2013; De Frutos et al., 2015)

#Mircoestación como variable aleatoria (para tener en cuenta las dates de visita) controla la pseudoreplicación y autocorrelación
#Cota, Tratamiento y Orientación como variables fijas (explicativas)
#FDis (índice de diversidad funcional como variable respuesta)

#lmer por distribución normal

mFDis = lmer(FDis ~ Cota_num * Tratamiento * Orientacion + (1 | Microestacion), REML = T, data = Div_Can_Cam) 
summary(mFDis)

#Plot rapido
plot(ggeffect(mFDis, terms = c("Cota_num", "Tratamiento", "Orientacion")))

#Residuos 
plot(residuals(mFDis) ~ fitted(mFDis))
qqnorm(residuals(mFDis)); qqline(residuals(mFDis))

Anova(mFDis, type = "III")
r.squaredGLMM(mFDis)

#Solo lm cota cara norte
mod_norte = Div_Can_Cam %>%
  filter(Orientacion == "Norte") %>%
  lm(FDis ~ Cota_num, data = .)
mod_norte

#Solo lm cota cara sur
mod_sur = Div_Can_Cam %>%
  filter(Orientacion == "Sur") %>%
  lm(FDis ~ Cota_num, data = .)
mod_sur

norte_stats = glance(mod_norte)
sur_stats   = glance(mod_sur)

R2_norte = round(norte_stats$r.squared, 3)
R2_sur   = round(sur_stats$r.squared, 3)

p_norte  = signif(norte_stats$p.value, 3)
p_sur    = signif(sur_stats$p.value, 3)

norte_stats
sur_stats

# Obtener predicciones del modelo para graficar
pred = ggpredict(
  mFDis,
  terms = c("Cota_num", "Tratamiento", "Orientacion")
)
pred_df = as.data.frame(pred)

# Colores para tratamiento
colores = c("Control" = "#d62730", "Refugio" = "#1f77b4")
# Lineas para orientación
lineas = c("Norte" = "twodash", "Sur"   = "solid")

#Gráfico
p = ggplot(pred_df, aes(x = x, y = predicted)) +
  geom_line(aes(color = group, linetype = facet), linewidth = 1.8) +
  scale_color_manual(values = colores, name = "Tratamiento") +
  scale_linetype_manual(values = lineas, name = "Orientación") +
  labs(x = "Altura (m s.n.m.)",
       y = "Dispersión funcional (FDis)") +
  scale_y_continuous(limits = c(0.00, 0.20)) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color="black", size=1.2),
    axis.text = element_text(color="black"),
    axis.title = element_text(color="black"),
    legend.title = element_text(color="black"),
    legend.text = element_text(color="black"),
    legend.box = "vertical"
  )

#Agregado de los estadisticos R² y p-value
info_text = paste0(
  "Norte:  R² = ", R2_norte, "   |   p  = ", p_norte, "\n",
  "Sur:      R² = ", R2_sur,   "   |   p = ", p_sur
)

# Figura final 
# Calcular posición dentro del gráfico
x_pos = max(pred_df$x) * 0.98
y_pos = min(pred_df$predicted) + 0.1 * (max(pred_df$predicted) - min(pred_df$predicted))
p_final_FDis = p +
  annotate(
    "text",
    x = x_pos,
    y = y_pos,
    label = paste0(
      "Norte: R² = ", R2_norte, " | p value = ", p_norte, "\n",
      "Sur: R² = ", R2_sur,   " | p value = ", p_sur
    ),
    hjust = 0.8,
    vjust = 3,
    size = 4.5,
    color = "black"
  )

p_final_FDis


#RIQUEZA FUNCIONAL (FRIC)

#FRic: Mide el tamaño del espacio funcional (9 dimensiones) ocupado por todas
#las especies de una comunidad (microestación). No tiene en cuenta las abundancias (cobertura)
#(Mason y Mouillot, 2013)

#Mircoestación como variable aleatoria (para tener en cuenta las dates de visita) controla la pseudoreplicación y autocorrelación
#Cota, Tratamiento y Orientación como variables fijas (explicativas)
#FRic (índice de diversidad funcional como variable respuesta)

#lmer por distribucipn normal

mFRic = lmer(FRic ~ Cota_num * Tratamiento * Orientacion + (1 | Microestacion), REML = T, data = Div_Can_Cam) 
summary(mFRic)

#Plot rapido
plot(ggeffect(mFRic, terms = c("Cota_num", "Tratamiento", "Orientacion")))

#Residuos 
plot(residuals(mFRic) ~ fitted(mFRic))
qqnorm(residuals(mFRic)); qqline(residuals(mFRic))

Anova(mFRic, type = "III")
r.squaredGLMM(mFRic)

#Solo lm cota cara norte
mod_norte = Div_Can_Cam %>%
  filter(Orientacion == "Norte") %>%
  lm(FRic ~ Cota_num, data = .)
mod_norte

#Solo lm cota cara sur
mod_sur = Div_Can_Cam %>%
  filter(Orientacion == "Sur") %>%
  lm(FRic ~ Cota_num, data = .)
mod_sur

norte_stats = glance(mod_norte)
sur_stats   = glance(mod_sur)

R2_norte = round(norte_stats$r.squared, 3)
R2_sur   = round(sur_stats$r.squared, 3)

p_norte  = signif(norte_stats$p.value, 3)
p_sur    = signif(sur_stats$p.value, 3)

norte_stats
sur_stats

# Obtener predicciones del modelo para graficar
pred = ggpredict(
  mFRic,
  terms = c("Cota_num", "Tratamiento", "Orientacion")
)
pred_df = as.data.frame(pred)

# Colores para tratamiento
colores = c("Control" = "#d62730", "Refugio" = "#1f77b4")
# Lineas para orientación
lineas = c("Norte" = "twodash", "Sur"   = "solid")

#Gráfico
p = ggplot(pred_df, aes(x = x, y = predicted)) +
  geom_line(aes(color = group, linetype = facet), size = 1.8) +
  scale_color_manual(values = colores, name = "Tratamiento") +
  scale_linetype_manual(values = lineas, name = "Orientación") +
  labs(x = "Altura (m s.n.m.)",
       y = "Riqueza funcional (FRic)") +
  scale_y_continuous(limits = c(0, 0.6)) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color="black", size=1.2),
    axis.text = element_text(color="black"),
    axis.title = element_text(color="black"),
    legend.title = element_text(color="black"),
    legend.text = element_text(color="black"),
    legend.box = "vertical"
  )

#Agregado de los estadisticos R² y p-value
info_text = paste0(
  "Norte: R² = ", R2_norte, "   |   p  = ", p_norte, "\n",
  "Sur: R² = ", R2_sur,   "   |   p = ", p_sur
)

# Figura final 
# Calcular posición dentro del gráfico
x_pos = max(pred_df$x) * 0.98
y_pos = min(pred_df$predicted) + 0.1 * (max(pred_df$predicted) - min(pred_df$predicted))
p_final_FRic = p +
  annotate(
    "text",
    x = x_pos,
    y = y_pos,
    label = paste0(
      "Norte: R² = ", R2_norte, " | p value = ", p_norte, "\n",
      "Sur: R² = ", R2_sur,   " | p value = ", p_sur
    ),
    hjust = 0.8,
    vjust = 2.5,
    size = 4.5,
    color = "black"
  )

p_final_FRic

#Resumen Resultados Diversidad Funcional
plot_DivFuncional = cowplot::plot_grid(p_final_FDis, p_final_FRic, ncol = 2, labels = "AUTO")
plot_DivFuncional

#La altura (cota) parece la unica variable que hace que aumente la diversidad funcional
#Los tratamientos y la orientación no muestran efectos ni diferencias significativas según los modelos lmer

#Conclusiónes rápidas
# A mayor altura de montaña, mas diferentes son las especies de las verbatim_microstationes
# A mayor altura (ambientes más hostiles) más diferencia en el uso de los recursos y mayor resiliencia a cambios
# A simple vista, parece que la cara norte es la que más pendiente muestra en la cota
# Esto puede deberse a que la mayor aridez en la cara norte, también ayuda a aumentar la diversidad funcional (Gross et al., 2024)


#INDICES DE DIVERSIDAD TAXONÓMICA

#Riqueza de especies

#Mircoestación como variable aleatoria (para tener en cuenta las dates de visita) controla la pseudoreplicación y autocorrelación
#Cota, Tratamiento y Orientación como variables fijas (explicativas)
#Riqueza (variable respuesta)

#lmer por distribución normal

mRiqueza = lmer(Riqueza ~ Cota_num * Tratamiento * Orientacion + (1 | Microestacion), REML = T, data = Div_Can_Cam) 
summary(mRiqueza)

#Plot rapido
plot(ggeffect(mRiqueza, terms = c("Cota_num", "Tratamiento", "Orientacion")))

#Residuos 
plot(residuals(mRiqueza) ~ fitted(mRiqueza))
qqnorm(residuals(mRiqueza)); qqline(residuals(mRiqueza))

Anova(mRiqueza, type = "III")
r.squaredGLMM(mRiqueza)

#Solo lm cota cara norte
mod_norte = Div_Can_Cam %>%
  filter(Orientacion == "Norte") %>%
  lm(Riqueza ~ Cota_num, data = .)
mod_norte

#Solo lm cota cara sur
mod_sur = Div_Can_Cam %>%
  filter(Orientacion == "Sur") %>%
  lm(Riqueza ~ Cota_num, data = .)
mod_sur

norte_stats = glance(mod_norte)
sur_stats   = glance(mod_sur)

R2_norte = round(norte_stats$r.squared, 3)
R2_sur   = round(sur_stats$r.squared, 3)

p_norte  = signif(norte_stats$p.value, 3)
p_sur    = signif(sur_stats$p.value, 3)

norte_stats
sur_stats

# Obtener predicciones del modelo para graficar
pred = ggpredict(
  mRiqueza,
  terms = c("Cota_num", "Tratamiento", "Orientacion")
)
pred_df = as.data.frame(pred)

# Colores para tratamiento
colores = c("Control" = "#d62730", "Refugio" = "#1f77b4")
# Lineas para orientación
lineas = c("Norte" = "twodash", "Sur"   = "solid")

#Gráfico
p = ggplot(pred_df, aes(x = x, y = predicted)) +
  geom_line(aes(color = group, linetype = facet), size = 1.8) +
  scale_color_manual(values = colores, name = "Tratamiento") +
  scale_linetype_manual(values = lineas, name = "Orientación") +
  labs(x = "Altura (m s.n.m.)",
       y = "Riqueza de especies") +
  scale_y_continuous(limits = c(0, 10)) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color="black", size=1.2),
    axis.text = element_text(color="black"),
    axis.title = element_text(color="black"),
    legend.title = element_text(color="black"),
    legend.text = element_text(color="black"),
    legend.box = "vertical"
  )

#Agregado de los estadisticos R² y p-value
info_text = paste0(
  "Norte:  R² = ", R2_norte, "   |   p  = ", p_norte, "\n",
  "Sur:      R² = ", R2_sur,   "   |   p = ", p_sur
)

# Figura final 
# Calcular posición dentro del gráfico
x_pos = max(pred_df$x) * 0.98
y_pos = min(pred_df$predicted) + 0.1 * (max(pred_df$predicted) - min(pred_df$predicted))
p_final_Riqueza = p +
  annotate(
    "text",
    x = x_pos,
    y = y_pos,
    label = paste0(
      "Norte: R² = ", R2_norte, " | p value = ", p_norte, "\n",
      "Sur: R² = ", R2_sur,   " | p value = ", p_sur
    ),
    hjust = 0.8,
    vjust = 3,
    size = 4.5,
    color = "black"
  )

p_final_Riqueza


#Índice de Shannon (H')
#considera tanto la cantidad como la distribución (equitatividad o dominancia) de las especies en la comunidad
#Alto shannon: Comunidad más diversa, mucha riqueza y sin especies dominantes (coberturas equitativas)
#Bajo shannon: Comunidad menos diversa, poca riqueza y especies dominantes

#Mircoestación como variable aleatoria (para tener en cuenta las dates de visita) controla la pseudoreplicación y autocorrelación
#Cota, Tratamiento y Orientación como variables fijas (explicativas)
#Shannon(variable respuesta)

#lmer por distribución normal

mShannon = lmer(Shannon ~ Cota_num * Tratamiento * Orientacion + (1 | Microestacion), REML = T, data = Div_Can_Cam) 
summary(mShannon)

#Plot rapido
plot(ggeffect(mShannon, terms = c("Cota_num", "Tratamiento", "Orientacion")))

#Residuos 
plot(residuals(mShannon) ~ fitted(mShannon))
qqnorm(residuals(mShannon)); qqline(residuals(mShannon))

Anova(mShannon, type = "III")
r.squaredGLMM(mShannon)

#Solo lm cota cara norte
mod_norte = Div_Can_Cam %>%
  filter(Orientacion == "Norte") %>%
  lm(Shannon ~ Cota_num, data = .)
mod_norte

#Solo lm cota cara sur
mod_sur = Div_Can_Cam %>%
  filter(Orientacion == "Sur") %>%
  lm(Shannon ~ Cota_num, data = .)
mod_sur

norte_stats = glance(mod_norte)
sur_stats   = glance(mod_sur)

R2_norte = round(norte_stats$r.squared, 3)
R2_sur   = round(sur_stats$r.squared, 3)

p_norte  = signif(norte_stats$p.value, 3)
p_sur    = signif(sur_stats$p.value, 3)

norte_stats
sur_stats

# Obtener predicciones del modelo para graficar
pred = ggpredict(
  mShannon,
  terms = c("Cota_num", "Tratamiento", "Orientacion")
)
pred_df = as.data.frame(pred)

# Colores para tratamiento
colores = c("Control" = "#d62730", "Refugio" = "#1f77b4")
# Lineas para orientación
lineas = c("Norte" = "twodash", "Sur"   = "solid")

#Gráfico
p = ggplot(pred_df, aes(x = x, y = predicted)) +
  geom_line(aes(color = group, linetype = facet), size = 1.8) +
  scale_color_manual(values = colores, name = "Tratamiento") +
  scale_linetype_manual(values = lineas, name = "Orientación") +
  labs(x = "Altura (m s.n.m.)",
       y = "Shannon (H')") +
  scale_y_continuous(limits = c(0, 2)) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color="black", size=1.2),
    axis.text = element_text(color="black"),
    axis.title = element_text(color="black"),
    legend.title = element_text(color="black"),
    legend.text = element_text(color="black"),
    legend.box = "vertical"
  )

#Agregado de los estadisticos R² y p-value
info_text = paste0(
  "Norte:  R² = ", R2_norte, "   |   p  = ", p_norte, "\n",
  "Sur:      R² = ", R2_sur,   "   |   p = ", p_sur
)

# Figura final 
# Calcular posición dentro del gráfico
x_pos = max(pred_df$x) * 0.98
y_pos = min(pred_df$predicted) + 0.1 * (max(pred_df$predicted) - min(pred_df$predicted))
p_final_Shannon = p +
  annotate(
    "text",
    x = x_pos,
    y = y_pos,
    label = paste0(
      "Norte: R² = ", R2_norte, " | p value = ", p_norte, "\n",
      "Sur: R² = ", R2_sur,   " | p value = ", p_sur
    ),
    hjust = 0.8,
    vjust = 3,
    size = 4.5,
    color = "black"
  )

p_final_Shannon

#Índice de Simpson (D)
#Probabilidad de que dos individuos de una comunidad seleccionados al azar sean de la misma especie
#Relación con la dominancia
#Alto simpson: mayor dominancia 
#Bajo simpson: menor dominancia

#Mircoestación como variable aleatoria (para tener en cuenta las dates de visita) controla la pseudoreplicación y autocorrelación
#Cota, Tratamiento y Orientación como variables fijas (explicativas)
#Simpson (variable respuesta)

#lmer por distribución normal

mSimpson = lmer(Simpson ~ Cota_num * Tratamiento * Orientacion + (1 | Microestacion), REML = T, data = Div_Can_Cam) 
summary(mSimpson)

#Plot rapido
plot(ggeffect(mSimpson, terms = c("Cota_num", "Tratamiento", "Orientacion")))

#Residuos 
plot(residuals(mSimpson) ~ fitted(mSimpson))
qqnorm(residuals(mSimpson)); qqline(residuals(mSimpson))

Anova(mSimpson, type = "III")
r.squaredGLMM(mSimpson)

#Solo lm cota cara norte
mod_norte = Div_Can_Cam %>%
  filter(Orientacion == "Norte") %>%
  lm(Simpson ~ Cota_num, data = .)
mod_norte

#Solo lm cota cara sur
mod_sur = Div_Can_Cam %>%
  filter(Orientacion == "Sur") %>%
  lm(Simpson ~ Cota_num, data = .)
mod_sur

norte_stats = glance(mod_norte)
sur_stats   = glance(mod_sur)

R2_norte = round(norte_stats$r.squared, 3)
R2_sur   = round(sur_stats$r.squared, 3)

p_norte  = signif(norte_stats$p.value, 3)
p_sur    = signif(sur_stats$p.value, 3)

norte_stats
sur_stats

# Obtener predicciones del modelo para graficar
pred = ggpredict(
  mSimpson,
  terms = c("Cota_num", "Tratamiento", "Orientacion")
)
pred_df = as.data.frame(pred)

# Colores para tratamiento
colores = c("Control" = "#d62730", "Refugio" = "#1f77b4")
# Lineas para orientación
lineas = c("Norte" = "twodash", "Sur"   = "solid")

#Gráfico
p = ggplot(pred_df, aes(x = x, y = predicted)) +
  geom_line(aes(color = group, linetype = facet), size = 1.8) +
  scale_color_manual(values = colores, name = "Tratamiento") +
  scale_linetype_manual(values = lineas, name = "Orientación") +
  labs(x = "Altura (m s.n.m.)",
       y = "Simpson (D)") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color="black", size=1.2),
    axis.text = element_text(color="black"),
    axis.title = element_text(color="black"),
    legend.title = element_text(color="black"),
    legend.text = element_text(color="black"),
    legend.box = "vertical"
  )

#Agregado de los estadisticos R² y p-value
info_text = paste0(
  "Norte:  R² = ", R2_norte, "   |   p  = ", p_norte, "\n",
  "Sur:      R² = ", R2_sur,   "   |   p = ", p_sur
)

# Figura final 
# Calcular posición dentro del gráfico
x_pos = max(pred_df$x) * 0.98
y_pos = min(pred_df$predicted) + 0.1 * (max(pred_df$predicted) - min(pred_df$predicted))
p_final_Simpson = p +
  annotate(
    "text",
    x = x_pos,
    y = y_pos,
    label = paste0(
      "Norte: R² = ", R2_norte, " | p value = ", p_norte, "\n",
      "Sur: R² = ", R2_sur,   " | p value = ", p_sur
    ),
    hjust = 0.8,
    vjust = 3,
    size = 4.5,
    color = "black"
  )

p_final_Simpson

#Resumen Resultados Diversidad Funcional
plot_DivTaxonomica = cowplot::plot_grid(p_final_Riqueza, p_final_Shannon, p_final_Simpson, ncol = 3, labels = "AUTO")
plot_DivTaxonomica

#La altura (cota) parece la unica variable que hace que aumenta también la diversidad taxonomica
#Los tratamientos y la orientación no muestran efectos ni diferencias significativas según los modelos lmer

#Conclusiónes rápidas
#Similar con la diversidad funcional, la diversidad taxonomica aumenta en funcion de la cota
# A simple vista la altura en la orientación norte parece tener un efecto mayor sobre la diversidad que en la orientación sur

#================================================================================

#Conclusión final en base a todos los resultados

# La aridez y la altura de la orientación norte parecen ser ambientes que aumentan diversidad funcional y taxonómica de la flora

#================================================================================

