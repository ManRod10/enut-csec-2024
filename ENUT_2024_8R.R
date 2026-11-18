
#Procesamiento ENUT -----

#system.time({
library(dplyr)
library(data.table)
library(readxl)
library(reshape2)
library(purrr)
library(tidyr)
library(stringr)
library(openxlsx)
library(writexl)
library(haven)

# Rutas ------------------------------------------------------------------------
# RUTA_DANE es la carpeta que contiene ENUT/, GEIH/ y Pobreza/. Se toma de la
# variable de entorno RUTA_DANE (por ejemplo, definida en ~/.Renviron). Si no
# existe, se asume la estructura DANE/ENUT/Code: dos niveles por encima del
# directorio de trabajo, que debe ser la carpeta de este repositorio.
if (!exists("RUTA_DANE")) {
  RUTA_DANE <- Sys.getenv("RUTA_DANE",
                          unset = normalizePath(file.path(getwd(), "..", ".."), winslash = "/"))
}
path <- file.path(RUTA_DANE, "ENUT")
setwd(path)

filex <- list.files(path = "Raw/ENUT_2024_CSV/",recursive = T,full.names = T)

homologo <- read_excel("Anex/Homologación ENUT-CSEC.xlsx",sheet = "Anexo 1")
colnames(homologo) <- c("cod","des","act_CSEC","des_CSEC")

idx <- c("DIRECTORIO","SECUENCIA_P","ORDEN") 

#enut <-  lapply(filex[1:10], read_sas) 

enut <- lapply(filex[1:10], function(x) {
  print(x)
  #df <- read.csv(x,sep = ";")
  df <- fread(x,sep = ";")
  names(df) <- toupper(names(df))
  
  df
})

educ <- enut[[6]] |> data.frame()
geo <- enut[[1]] |> data.frame()
demo <- enut[[3]] |> data.frame()
salud <- enut[[4]] |> data.frame()
uso_tiempo <- enut[[9]] |> data.frame()

geo <- geo[,c("DIRECTORIO","CLASE","REGION2",
              "FEX_C","P4030S1A1")]


educ <- educ[,c(idx,"P6210","P6210S1","P1158"  )]

colx <- colnames(uso_tiempo)

colsx <- colnames(uso_tiempo)[grep("S",colx)]

preguntas <- data.frame(
  pregunta = sub("S.*", "", colsx),                       # Lo que está antes de la "S"
  opcion1 = sub(".*S", "", colsx),                     # Lo que está después de la "S"
  opcion2 = sub(".*S([0-9]+)A.*", "\\1", colsx),       # Número entre S y A
  item = sub(".*A", "", colsx)                         # Lo que está después de la "A"
)


qadmin <- preguntas |> filter(pregunta == "P1140" & (item %in% c(1:4)))
qcitasm <- preguntas |> filter(pregunta == "P1131" & (item %in% c(1:4)))

preguntas<-preguntas[preguntas$item %in% c(1,2),]
preguntas <- preguntas |> filter(pregunta != "P1140")
preguntas <- preguntas |> filter(pregunta != "P1131")

preguntas <- rbind(preguntas,qadmin)
preguntas <- rbind(preguntas,qcitasm)


preguntas$cod <- paste0(preguntas$pregunta,"S",preguntas$opcion2)
preguntas$cod2 <- paste0(preguntas$pregunta,"S",preguntas$opcion1)

df <- merge(homologo,preguntas,by="cod",all = T)

op5 <- df[is.na(df$cod2),]
df <- df[!is.na(df$act_CSEC),]

colnames(preguntas)[1] <-"cod"
colnames(preguntas)[5] <-"cod0"

op5 <- merge(op5[,colnames(homologo)],preguntas,by="cod",all.x = T)

colnames(op5) <- colnames(df)

df <- rbind(df,op5)
df <- df[!is.na(df$cod2),]


##         Procesamiento Demografia-----
### Edad --------
# demo <- demo |>
#   mutate(Redad = case_when(
#     P6040 >= 10 & P6040 <= 14 ~ "De 10 a 14 años",
#     P6040 >= 15 & P6040 <= 18 ~ "De 15 a 18 años",
#     P6040 >= 19 & P6040 <= 28 ~ "De 19 a 28 años",
#     P6040 >= 29 & P6040 <= 38 ~ "De 29 a 38 años",
#     P6040 >= 39 & P6040 <= 48 ~ "De 39 a 48 años",
#     P6040 >= 49 & P6040 <= 60 ~ "De 49 a 60 años",
#     P6040 > 60               ~ "Más de 60 años",
#     TRUE                     ~ NA_character_
#   ))
# 


demo <- demo |>
  mutate(Redad = case_when(
    P6040 >= 10 & P6040 <= 14 ~ "De 10 a 14 años",
    P6040 >= 15 & P6040 <= 17 ~ "De 15 a 17 años",
    P6040 >= 18 & P6040 <= 29 ~ "De 18 a 29 años",
    P6040 >= 30 & P6040 <= 39 ~ "De 30 a 39 años",
    P6040 >= 40 & P6040 <= 49 ~ "De 40 a 49 años",
    P6040 >= 50 & P6040 <= 59 ~ "De 50 a 59 años",
    P6040 >= 60 & P6040 <= 75 ~ "De 60 a 75 años",
    P6040 > 75               ~ "Más de 76 años",
    TRUE                     ~ NA_character_
  ))

### Tipo de hogar --------------

demo$idh <- paste0(demo$DIRECTORIO,"-",demo$SECUENCIA_P)
uso_tiempo$idh <- paste0(uso_tiempo$DIRECTORIO,uso_tiempo$SECUENCIA_P)

chogar <- demo |> 
  group_by(idh) |> 
  summarise(
    PT = n(),  
    tiene_jefe = any(P425 == 1),
    sexo_jefe = P6020[P425 == 1][1],  
    tiene_pareja = any(P425 == 2),
    tiene_hijos = any(P425 == 3),
    tiene_extensos = any(P425 %in% 4:9),
    tiene_compuestos = any(P425 %in% 10:14),
    .groups = "drop"
  ) |> 
  mutate(
    thogar = case_when(
      PT == 1 & sexo_jefe == 1 ~ "1.Unipersonal (Hombre)",
      PT == 1 & sexo_jefe == 2 ~ "2.Unipersonal (Mujer)",
      
      !tiene_pareja & tiene_hijos & sexo_jefe == 1 
      & !tiene_extensos  & !tiene_compuestos
      ~ "5.Monoparental (Hombre) con hijos",
      
      !tiene_pareja & tiene_hijos & 
        sexo_jefe == 2 & !tiene_extensos & !tiene_compuestos
      ~ "6.Monoparental (Mujer) con hijos",
      
      tiene_pareja & !tiene_hijos & 
        !tiene_extensos & !tiene_compuestos
      ~ "3.Nuclear (sin hijos)",
      
      tiene_pareja & tiene_hijos & 
        !tiene_extensos & !tiene_compuestos ~ "4.Nuclear (con hijos)",
      
      tiene_extensos & !tiene_compuestos ~ "7.Extenso",
      tiene_compuestos ~ "8.Compuesto"
    )
  )

demo <- merge(demo,chogar[,c("idh","thogar")], by = "idh",all.x = T)


# etapas_hogar <- demo %>%
#   mutate(
#     es_jefe = P425 == 1,
#     es_pareja = P425 == 2,
#     es_hijo = P425 == 3
#   ) %>%
#   group_by(idh) %>%
#   summarise(
#     tiene_jefe = any(es_jefe),
#     tiene_pareja = any(es_pareja),
#     edad_mujer = P6040[which((es_jefe | es_pareja) & P6020 == 2)][1],
#     n_hijos = sum(es_hijo),
#     edad_max_hijo = ifelse(n_hijos > 0, max(P6040[es_hijo]), NA),
#     edad_min_hijo = ifelse(n_hijos > 0, min(P6040[es_hijo]), NA),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     etapa = case_when(
#       tiene_jefe & tiene_pareja & n_hijos == 0  & edad_mujer <= 40 ~ "1. Pareja joven sin hijos",
#       tiene_jefe & tiene_pareja & n_hijos > 0 & edad_max_hijo <= 5 ~ "2. Etapa inicial",
#       tiene_jefe & tiene_pareja & edad_max_hijo >= 6 & edad_max_hijo <= 12 ~ "3. Etapa de expansión",
#       tiene_jefe & tiene_pareja & edad_max_hijo >= 13 & edad_max_hijo <= 18 ~ "4. Etapa de consolidación (a)",
#       tiene_jefe & tiene_pareja & edad_max_hijo > 18 & edad_min_hijo <= 18 ~ "5. Etapa de consolidación (b)",
#       tiene_jefe & tiene_pareja & edad_min_hijo > 18 ~ "6. Etapa de salida",
#       tiene_jefe & tiene_pareja & n_hijos == 0 & edad_mujer > 40  ~ "7. Pareja mayor sin hijos",
#       TRUE ~ NA_character_
#     )
#   )

etapas_hogar <- demo %>%
  mutate(
    es_jefe = P425 == 1,
    es_pareja = P425 == 2,
    es_hijo = P425 == 3
  ) %>%
  group_by(idh) %>%
  summarise(
    tiene_jefe = any(es_jefe),
    tiene_pareja = any(es_pareja),
    edad_mujer = {
      mujeres <- P6040[(es_jefe | es_pareja) & P6020 == 2]
      if (length(mujeres) == 1) {
        mujeres[1]
      } else if (length(mujeres) > 1) {
        min(mujeres, na.rm = TRUE)  
      } else {
        min(P6040[es_jefe | es_pareja], na.rm = TRUE)
      }
    },
    n_hijos = sum(es_hijo),
    edad_max_hijo = ifelse(n_hijos > 0, max(P6040[es_hijo], na.rm = TRUE), NA),
    edad_min_hijo = ifelse(n_hijos > 0, min(P6040[es_hijo], na.rm = TRUE), NA),
    .groups = "drop"
  ) %>%
  mutate(
    etapa = case_when(
      tiene_jefe & tiene_pareja & n_hijos == 0 & edad_mujer <= 40 ~ "1. Pareja joven sin hijos",
      tiene_jefe & tiene_pareja & n_hijos > 0 & edad_max_hijo <= 5 ~ "2. Etapa inicial",
      tiene_jefe & tiene_pareja & edad_max_hijo >= 6 & edad_max_hijo <= 12 ~ "3. Etapa de expansión",
      tiene_jefe & tiene_pareja & edad_max_hijo >= 13 & edad_max_hijo <= 18 ~ "4. Etapa de consolidación (a)",
      tiene_jefe & tiene_pareja & edad_max_hijo > 18 & edad_min_hijo <= 18 ~ "5. Etapa de consolidación (b)",
      tiene_jefe & tiene_pareja & edad_min_hijo > 18 ~ "6. Etapa de salida",
      tiene_jefe & tiene_pareja & n_hijos == 0 & edad_mujer > 40 ~ "7. Pareja mayor sin hijos",
      TRUE ~ NA_character_
    )
  )

demo <- merge(demo,etapas_hogar[,c("idh","etapa")], by = "idh",all.x = T)

demo$campesino <- ifelse(demo$P1173_1 == 1,1,NA)

## Salud ----

salud <-salud |> select(idx,starts_with("P1169")) 

salud$discapacidad <- apply(salud[, 4:ncol(salud)], 1, function(x) any(x %in% c(1, 2))) * 1

## Educacion ----
educ <- educ %>%
  mutate(educacion = case_when(
    P6210 %in% c(1, 2) ~ "Ninguno y preescolar",
    P6210 == 3 ~ "Primaria",
    P6210 %in% c(4,5) ~ "Secundaria - Media",
    P6210 %in% c(6,7,8,9,10,11,12) ~ "Superior y posgrado"
  ))

# educ <- educ %>%
#   mutate(educacion3 = case_when(
#     P1158 %in% c(1,2) ~ "Ninguno y preescolar",
#     P1158 == 3~ "Primaria",
#     P1158 %in% c(4,5) ~ "Secundaria - Media",
#     P1158 %in% c(6,7,8,9) ~ "Superior y posgrado"
#   ))

educ <- educ %>%
  mutate(educacion3 = case_when(
    P1158 %in% c(1) ~ "Ninguno y preescolar",
    P1158 %in% c(2) ~ "Primaria",
    P1158 == 3~ "Secundaria - Media",
    P1158 %in% c(4,5,6,7,8,9) ~ "Superior y posgrado"
  ))

geo <- geo |> mutate(
  estrato = case_when(
    P4030S1A1 %in% 1:2 ~ "Estrato 1 y 2",
    P4030S1A1 %in% 3:4 ~ "Estrato 3 y 4",
    P4030S1A1 %in% 5:6 ~ "Estrato 5 y 6",
    !(P4030S1A1 %in% (1:6)) ~ "Sin información del estrato socioeconómico"))


educ$educa <- ifelse(is.na(educ$educacion),
                     educ$educacion3,
                     educ$educacion)

educ |> filter(DIRECTORIO == 73903) |> select(SECUENCIA_P,ORDEN)


uso_tiempo <- merge(uso_tiempo,educ,
                    by = idx,all.x = T)

uso_tiempo |> filter(DIRECTORIO == 73903) |> select(SECUENCIA_P,ORDEN)

uso_tiempo <- merge(uso_tiempo,
                    salud[,c(idx,"discapacidad")],
                    by = idx,all.x = T)

uso_tiempo |> filter(DIRECTORIO == 73903) |> select(SECUENCIA_P,ORDEN)

uso_tiempo <- merge(uso_tiempo,demo[,c(idx,"P6020","Redad","P6040",
                                       "thogar","etapa","P1173","campesino")],
                    by = idx,all.x = T)

uso_tiempo |> filter(DIRECTORIO == 73903) |> select(SECUENCIA_P,ORDEN)

uso_tiempo <- merge(uso_tiempo,
                    geo, by = "DIRECTORIO",all.x = T)


uso_tiempo |> filter(DIRECTORIO == 73903) |> select(SECUENCIA_P,ORDEN)


demo |> filter(DIRECTORIO == 73903) |> select(SECUENCIA_P,ORDEN)

uso_tiempo$REGION2 <- as.character(uso_tiempo$REGION2)
uso_tiempo$REGION <- uso_tiempo$REGION2


## Algoritmo de Calculo --------
#
#Horas anuales dedicadas al trabajo doméstico y de 
#Cuidado no remunerado (TDCNR) por la población de 10 años y/o funcionalidad
#
# Proceso: 
# 1. Se identifican las preguntas de cada funcionalidad apartir del archivo
# Homologacion 
# 2. Se pasan las variables de minutos a horas 
# (se idetifican con la terminacion 2 y 4)
# 3. Se suma el tiempo de dedicado de cada persona para las preguntas
# relacionadas
# 4. En funcion de la desagregación requerida se agregan los resultados
#   una variable (sexo/P6020), sexo mas otra vriable, 
# 5. Los datos se expanden usando el factor de expansion y por se 
# pasan de frecuencia diaria a anual (7 dias y 52 semanas) y se llevan a
# miles /1000

#uso_tiempo |> filter(!(REGION2 %in% c(7,8))) |> dim()

## REGION1

####################################################
#
###################################################

#uso_tiempo <- uso_tiempo |> filter(!(is.na(REGION1))) 

csec <- function(x,y) {
  
  # uso_tiempo <- if (z == 1) {
  #   filter(uso_tiempo, !(REGION %in% c(7, 8)))
  # } else {
  #   uso_tiempo
  # }
  
  #  x = 3
  columnas <- df |> 
    filter(act_CSEC == x) |> 
    pull(cod2) |> 
    unique()
  
  db <- uso_tiempo |>
    select(all_of(columnas), P6020,Redad,
           FEX_C.y,REGION,CLASE,P4030S1A1,educacion,educa,
           educacion3,thogar,etapa,estrato,P1173,campesino,discapacidad) |>  
    mutate(across(all_of(columnas), ~ na_if(., 0))) |>
    mutate(across(ends_with(c("2", "4")), ~ . / 60)) |>
    mutate(suma_actividades = 
             rowSums(across(all_of(columnas)), na.rm = TRUE))
  
  if (y == "P6020") {
    
    valx <- db |> 
      group_by(P6020) |> 
      summarize(val = 
                  sum(suma_actividades * FEX_C.y, na.rm = TRUE) * 7 * 52 / 1000,
                .groups = "drop")
    
    valx <- data.frame(t(valx))[2,]
    colnames(valx)<-c("Hombres","Mujeres")
  }
  
  else {
    valx <- db |> filter(!is.na(.data[[y]])) |>
      group_by(.data[[y]],P6020) |> 
      summarize(val = sum(suma_actividades * FEX_C.y, na.rm = TRUE) * 7 * 52 / 1000, 
                .groups = "drop")
    
    valx <- dcast(valx,formula = as.formula(paste(y, "~ P6020")),value.var = "val")
    
    colnames(valx)<-c("Grupo", "Hombres", "Mujeres")
    row.names(valx) <- valx$Grupo
    valx<-valx[,-1]
  }
  
  valx$Total = rowSums(valx,na.rm = T)
  
  return(valx)
}

sexo<-lapply(X = c(1:6), FUN = csec, y = "P6020") 
clase<-lapply(X = c(1:6), FUN = csec, y = "CLASE") 
reg<-lapply(X = c(1:6), FUN = csec, y = "REGION") 

hogar<-lapply(X = c(1:6), FUN = csec, y = "thogar") 
educacion<-lapply(X = c(1:6), FUN = csec, y = "educa") 
edad<-lapply(X = c(1:6), FUN = csec, y = "Redad")  
etapa<-lapply(X = c(1:6), FUN = csec, y = "etapa")  
estrato<-lapply(X = c(1:6), FUN = csec, y = "estrato")  


etnia<-lapply(X = c(1:6), FUN = csec, y = "P1173") 
#campesino<-lapply(X = c(1:6), FUN = csec, y = "campesino") 
#discapacidad<-lapply(X = c(1:6), FUN = csec, y = "discapacidad") 


## Exportar Resultados -----
# Se realiza la exportacion de los resultados, a las 
# ubicaciones requeridas


annex_file <- loadWorkbook("Output/Resultado_ENUT_2024_8R.xlsx")

colx <- c(3,7,11,15,19,23)


vars <- list(sexo, clase, reg, hogar, educacion, 
             edad,etapa,estrato,etnia)
#starts <- c(11,22,25,34,43,48,56,64,69)
starts <- c(11,22,25,34,43,48,57,65,70)

for (i in 1:6) {
  for (j in seq_along(vars)) {
    writeData(annex_file, sheet = "Cuadro 1",
              vars[[j]][[i]], startCol = colx[i],
              startRow = starts[j], colNames = FALSE, rowNames = FALSE)
  }
  saveWorkbook(annex_file, "Output/Resultado_ENUT_2024_8R.xlsx", overwrite = TRUE)
}
#})
## Tiempo por pregunta ------------
# Desagregación por preguntas según la funcionalidad 
# Es similar al ejercicio anterior, pero no se realiza una
# suma del total de actividades CSEC por fila, se realiza unicamente
# segun por preguntas (actividades ENUT)

csec_q <- function(x,y){
  
  columnas <- df |> 
    filter(act_CSEC == x) |> 
    pull(cod2) |> 
    unique()
  
  temp <- uso_tiempo |> 
    select(all_of(columnas), P6020, Redad, FEX_C.y, REGION, CLASE,
           P4030S1A1, educacion, educa, 
           educacion3, thogar, etapa, estrato) |> 
    mutate(across(ends_with(c("2", "4")), ~ . / 60)) |>
    pivot_longer(cols = all_of(columnas),
                 names_to = "Actividad_original",
                 values_to = "valor") |> 
    mutate(Grupo = str_extract(Actividad_original, "^P\\d+S\\d+"),
           valor_ponderado = 
             valor * FEX_C.y * 7 * 52 / 1000)
  
  if ( y == "P6020") {
    temp <- temp |> 
      group_by(Grupo,P6020) |> 
      summarise(horasm = 
                  sum(valor_ponderado, na.rm = TRUE),
                .groups = "drop") |>
      pivot_wider(names_from = P6020, 
                  values_from = horasm)
    
    temp$Total <- temp$`1` + temp$`2`
    colnames(temp)[c(2,3)]<-c("Hombres","Mujeres")
  }
  
  else { 
    temp <- temp |> 
      group_by(Grupo,P6020,.data[[y]]) |> 
      summarise(horasm = 
                  sum(valor_ponderado, na.rm = TRUE),
                .groups = "drop") |>
      pivot_wider(names_from = P6020, 
                  values_from = horasm)
    
    temp$Total <- temp$`1` + temp$`2`
    colnames(temp)[c(3,4)]<-c("Hombres","Mujeres")
  }
  return(temp)
  
}


### Exportar resultados por pregunta------
# Se exportan los resultados


vars <- c("P6020", "CLASE", "REGION", 
          "thogar", "educa", "Redad", "etapa", "estrato")
nombres <- c("sexo", "clase", "reg", 
             "hogar", "educacion", "edad", "etapa", "estrato")

for (i in seq_along(vars)) {
  res <- lapply(1:6, FUN = csec_q, y = vars[i])
  names(res) <- paste0("Funcionalidad", 1:6)

  write_xlsx(res, path = paste0("Output/",
                                "Desagregacion_Funcionalidades_segun_", nombres[i], "_2024_8R.xlsx"))
}



# Cantidad de personas y horas

csec_h <- function(x,y,cuadro5 = F, w = 0) {
  
  if (w == 0){ 
    columnas <- df |> 
      filter(act_CSEC == x) |> 
      pull(cod2) |> 
      unique()
    
    db <- uso_tiempo |>
      select(all_of(columnas), P6020,Redad,
             FEX_C.y,REGION,CLASE,P4030S1A1,educacion,educa,
             educacion3,thogar,etapa,estrato,P1173,campesino,discapacidad) |>  
      mutate(across(all_of(columnas), ~ na_if(., 0))) |>
      mutate(across(ends_with(c("2", "4")), ~ . / 60)) |>
      mutate(suma_actividades = 
               rowSums(across(all_of(columnas)), na.rm = TRUE))
    
    if (y == "P6020") {
      
      valx <- db |> filter(suma_actividades > 0) |>
        group_by(P6020) |> 
        summarize(horas =
                    sum(suma_actividades*FEX_C.y, na.rm = TRUE)*7,
                  personas = sum(FEX_C.y),
                  .groups = "drop") |>
        mutate(promedio = horas/personas) |> 
        as.data.table()
      
      valx <- data.table::dcast(valx, . ~ P6020, value.var = c("horas", "personas", "promedio"))
      
      valx$`M/H` <- valx$promedio_2/valx$promedio_1
      
      valx <- valx[,-1]
      
    }
    
    else {
      
      valx <- db |> filter(!is.na(.data[[y]]) & suma_actividades > 0) |>
        group_by(.data[[y]],P6020) |> 
        summarize(horas = 
                    sum(suma_actividades*FEX_C.y, na.rm = TRUE)*7,
                  personas = sum(FEX_C.y),
                  .groups = "drop") |> 
        mutate(promedio = horas/personas) |> as.data.table()
      
      valx <- melt(valx, id.vars = c(y, "P6020"), 
                   variable.name = "variable", 
                   value.name = "valor") |> as.data.table()
      
      valx[, varname := paste(variable, P6020, sep = "_")]
      
      valx <- dcast(valx, formula = as.formula(paste(y, "~ varname")), value.var = "valor")
      
      valx$`M/H` <- valx$promedio_2/valx$promedio_1
      
      colnames(valx)[1]<-"Grupo"
      row.names(valx) <- valx$Grupo
      valx<-valx[,-1]
    }
    
    if (cuadro5 == F) {
      return(valx)
    }
    
    if (cuadro5 == T) {
      return(valx[,c(5:7)])
    }
  }
  
  if (w == 1){  
    columnas <- df |> 
      pull(cod2) |> 
      unique()
    
    db <- uso_tiempo |>
      select(all_of(columnas), P6020,Redad,
             FEX_C.y,REGION,CLASE,P4030S1A1,educacion,educa,
             educacion3,thogar,etapa,estrato,P1173,campesino,discapacidad) |>  
      mutate(across(all_of(columnas), ~ na_if(., 0))) |>
      mutate(across(ends_with(c("2", "4")), ~ . / 60)) |>
      mutate(suma_actividades = 
               rowSums(across(all_of(columnas)), na.rm = TRUE))
    
    if (y == "P6020") {
      
      valx <- db |> filter(suma_actividades > 0) |>
        group_by(P6020) |> 
        summarize(horas =
                    sum(suma_actividades*FEX_C.y, na.rm = TRUE)*7,
                  personas = sum(FEX_C.y),
                  .groups = "drop") |>
        mutate(promedio = horas/personas) |> 
        as.data.table()
      
      valx <- data.table::dcast(valx, . ~ P6020, value.var = c("horas", "personas", "promedio"))
      
      valx$`M/H` <- valx$promedio_2/valx$promedio_1
      
      valx <- valx[,-1]
      
    }
    
    else {
      
      valx <- db |> filter(!is.na(.data[[y]]) & suma_actividades > 0) |>
        group_by(.data[[y]],P6020) |> 
        summarize(horas = 
                    sum(suma_actividades*FEX_C.y, na.rm = TRUE)*7,
                  personas = sum(FEX_C.y),
                  .groups = "drop") |> 
        mutate(promedio = horas/personas) |> as.data.table()
      
      valx <- melt(valx, id.vars = c(y, "P6020"), 
                   variable.name = "variable", 
                   value.name = "valor") |> as.data.table()
      
      valx[, varname := paste(variable, P6020, sep = "_")]
      
      valx <- dcast(valx, formula = as.formula(paste(y, "~ varname")), value.var = "valor")
      
      valx$`M/H` <- valx$promedio_2/valx$promedio_1
      
      colnames(valx)[1]<-"Grupo"
      row.names(valx) <- valx$Grupo
      valx<-valx[,-1]
    }
    
    if (cuadro5 == F) {
      return(valx)
    }
    
    if (cuadro5 == T) {
      return(valx[,c(5:7)])
    }
  }
  
  
  
}

## Exportar resultados ####

vars <- c("P6020", "CLASE", "REGION", 
          "thogar", "educa", "Redad", "etapa", "estrato",
          "discapacidad")

nombres <- c("sexo", "clase", "reg", 
             "hogar", "educacion", "edad", "etapa", "estrato",
             "discapacidad")

# for (i in seq_along(vars)) {
#   res <- lapply(1:6, FUN = csec_h, y = vars[i])
#   names(res) <- paste0("Funcionalidad", 1:6)
#   
#   write_xlsx(res, path = paste0("Output/",
#                                 "Horas_Semanales_segun_", nombres[i], "_2024.xlsx"))
# }




sexo<-lapply(X = c(1:6), FUN = csec_h, y = "P6020",cuadro5 = T) 
sexo <- append(sexo,list(csec_h(x = 1, y = "P6020", w = 1,cuadro5 = T )))

clase<-lapply(X = c(1:6), FUN = csec_h, y = "CLASE", cuadro5 = T) 
clase <- append(clase,list(csec_h(x = 1, y = "CLASE", w = 1,cuadro5 = T )))

reg<-lapply(X = c(1:6), FUN = csec_h, y = "REGION", cuadro5 = T) 
reg <- append(reg,list(csec_h(x = 1, y = "REGION", w = 1,cuadro5 = T )))


hogar<-lapply(X = c(1:6), FUN = csec_h, y = "thogar", cuadro5 = T) 

filas <- c(
  "1.Unipersonal (Hombre)",
  "2.Unipersonal (Mujer)",
  "3.Nuclear (sin hijos)",
  "4.Nuclear (con hijos)",
  "5.Monoparental (Hombre) con hijos",
  "6.Monoparental (Mujer) con hijos",
  "7.Extenso",
  "8.Compuesto"
)

hogar <- lapply(hogar, function(x) {
  x <- x[match(filas, rownames(x)), , drop = FALSE]
  rownames(x) <- filas
  x
})

hogar <- append(hogar,list(csec_h(x = 1, y = "thogar", w = 1,cuadro5 = T )))

educacion<-lapply(X = c(1:6), FUN = csec_h, y = "educa", cuadro5 = T) 
educacion <- append(educacion,list(csec_h(x = 1, y = "educa", w = 1,cuadro5 = T )))

edad<-lapply(X = c(1:6), FUN = csec_h, y = "Redad", cuadro5 = T)  
edad <- append(edad,list(csec_h(x = 1, y = "Redad", w = 1,cuadro5 = T )))

etapa<-lapply(X = c(1:6), FUN = csec_h, y = "etapa",  cuadro5 = T)  
etapa <- append(etapa,list(csec_h(x = 1, y = "etapa", w = 1,cuadro5 = T )))

estrato<-lapply(X = c(1:6), FUN = csec_h, y = "estrato",  cuadro5 = T)  
estrato <- append(estrato,list(csec_h(x = 1, y = "estrato", w = 1,cuadro5 = T )))

etnia<-lapply(X = c(1:6), FUN = csec_h, y = "P1173",  cuadro5 = T) 

filas <- as.character(1:6)

etnia <- lapply(etnia, function(x) {
  x <- x[match(filas, rownames(x)), , drop = FALSE]
  rownames(x) <- filas
  x
})

etnia <- append(etnia,list(csec_h(x = 1, y = "P1173", w = 1,cuadro5 = T )))



#campesino<-lapply(X = c(1:6), FUN = csec_h, y = "campesino",  cuadro5 = T) 
#discapacidad<-lapply(X = c(1:6), FUN = csec_h, y = "discapacidad", cuadro5 = T) 

annex_file <- loadWorkbook("Output/Resultado_ENUT_2024_8R.xlsx")

colx <- c(3,7,11,15,19,23,27)


vars <- list(sexo, clase, reg, hogar, educacion, edad,etapa,estrato,etnia)
starts <- c(11,22,25,34,43,48,56,64,69)
#starts <- c(11,22,25,34,43,48,56,64,69)

for (i in 1:7) {
  for (j in seq_along(vars)) {
    writeData(annex_file, sheet = "Cuadro 5",
              vars[[j]][[i]], startCol = colx[i],
              startRow = starts[j], colNames = FALSE, rowNames = FALSE)
  }
  saveWorkbook(annex_file, "Output/Resultado_ENUT_2024_8R.xlsx", overwrite = TRUE)
}


csec_people <- function(x,y,w = 0) {
  
  if (w == 0){
    columnas <- df |> 
      filter(act_CSEC == x) |> 
      pull(cod2) |> 
      unique()
    
    db <- uso_tiempo |>
      select(all_of(columnas), P6020,Redad,P6040,
             FEX_C.y,REGION,CLASE,P4030S1A1,educacion,educa,
             educacion3,thogar,etapa,estrato,P1173,campesino,discapacidad) |>  
      mutate(across(all_of(columnas), ~ na_if(., 0))) |>
      mutate(across(ends_with(c("2", "4")), ~ . / 60)) |>
      mutate(suma_actividades = 
               rowSums(across(all_of(columnas)), na.rm = TRUE))
    
    if (y == "P6020") {
      
      valx <- db |>
        group_by(P6020) |>
        summarize(
          den = sum(FEX_C.y * (P6040 > 9) , na.rm = TRUE),
          num = sum(FEX_C.y * (suma_actividades > 0) * (P6040 > 9), na.rm = TRUE),
          val = (num/den)*100, 
          .groups = "drop")
      
      valx <- data.frame(t(valx))[4,]
      colnames(valx)<-c("Hombres","Mujeres")
      return(valx)
    }
    
    else {
      valx0 <- db |> filter(!is.na(.data[[y]])) |>
        group_by(.data[[y]],P6020) |> 
        summarize(        
          den = sum(FEX_C.y * (P6040 > 9), na.rm = TRUE),
          num = sum(FEX_C.y * (suma_actividades > 0) * (P6040 > 9) , na.rm = TRUE),
          val = (num/den)*100, 
          .groups = "drop")
      
      valx <- dcast(valx0,formula = as.formula(paste(y, "~ P6020")),value.var = "val")
      
      colnames(valx)<-c("Grupo", "Hombres", "Mujeres")
      row.names(valx) <- valx$Grupo
      valx<-valx[,-1]
      valx1 <- valx 
      
      valx <- dcast(valx0,formula = as.formula(paste(y, "~ P6020")),value.var = "den")
      
      colnames(valx)<-c("Grupo", "Hombres", "Mujeres")
      row.names(valx) <- valx$Grupo
      valx<-valx[,-1]
      valx2 <- valx 
      
      valx <- dcast(valx0,formula = as.formula(paste(y, "~ P6020")),value.var = "num")
      
      colnames(valx)<-c("Grupo", "Hombres", "Mujeres")
      row.names(valx) <- valx$Grupo
      valx<-valx[,-1]
      valx3 <- valx 
      
      return(valx1)
    }
  }
  
  
  if (!is.na(w)){
    columnas <- df |> 
      pull(cod2) |> 
      unique()
    
    db <- uso_tiempo |>
      select(all_of(columnas), P6020,Redad,P6040,
             FEX_C.y,REGION,CLASE,P4030S1A1,educacion,educa,
             educacion3,thogar,etapa,estrato,P1173,campesino,discapacidad) |>  
      mutate(across(all_of(columnas), ~ na_if(., 0))) |>
      mutate(across(ends_with(c("2", "4")), ~ . / 60)) |>
      mutate(suma_actividades = 
               rowSums(across(all_of(columnas)), na.rm = TRUE))
    
    if (y == "P6020") {
      
      valx <- db |>
        group_by(P6020) |>
        summarize(
          den = sum(FEX_C.y * (P6040 > 9) , na.rm = TRUE),
          num = sum(FEX_C.y * (suma_actividades > 0) * (P6040 > 9), na.rm = TRUE),
          val = (num/den)*100, 
          .groups = "drop")
      
      valx <- data.frame(t(valx))[4,]
      colnames(valx)<-c("Hombres","Mujeres")
      return(valx)
    }
    
    else {
      valx0 <- db |> filter(!is.na(.data[[y]])) |>
        group_by(.data[[y]],P6020) |> 
        summarize(        
          den = sum(FEX_C.y * (P6040 > 9), na.rm = TRUE),
          num = sum(FEX_C.y * (suma_actividades > 0) * (P6040 > 9) , na.rm = TRUE),
          val = (num/den)*100, 
          .groups = "drop")
      
      valx <- dcast(valx0,formula = as.formula(paste(y, "~ P6020")),value.var = "val")
      
      colnames(valx)<-c("Grupo", "Hombres", "Mujeres")
      row.names(valx) <- valx$Grupo
      valx<-valx[,-1]
      valx1 <- valx 
      
      valx <- dcast(valx0,formula = as.formula(paste(y, "~ P6020")),value.var = "den")
      
      colnames(valx)<-c("Grupo", "Hombres", "Mujeres")
      row.names(valx) <- valx$Grupo
      valx<-valx[,-1]
      valx2 <- valx 
      
      valx <- dcast(valx0,formula = as.formula(paste(y, "~ P6020")),value.var = "num")
      
      colnames(valx)<-c("Grupo", "Hombres", "Mujeres")
      row.names(valx) <- valx$Grupo
      valx<-valx[,-1]
      valx3 <- valx 
      
      return(valx1)
    }
  }
  
  
}


sexo<-lapply(X = c(1:6), FUN = csec_people, y = "P6020") 
sexo <- append(sexo,list(csec_people(x = 1, y = "P6020", w = 1)))

clase<-lapply(X = c(1:6), FUN = csec_people, y = "CLASE") 
clase <- append(clase,list(csec_people(x = 1, y = "CLASE", w = 1)))


reg<-lapply(X = c(1:6), FUN = csec_people, y = "REGION") 
reg <- append(reg,list(csec_people(x = 1, y = "REGION", w = 1)))

hogar<-lapply(X = c(1:6), FUN = csec_people, y = "thogar") 
hogar <- append(hogar,list(csec_people(x = 1, y = "thogar", w = 1)))

educacion<-lapply(X = c(1:6), FUN = csec_people, y = "educa") 
educacion <- append(educacion,list(csec_people(x = 1, y = "educa", w = 1)))

edad<-lapply(X = c(1:6), FUN = csec_people, y = "Redad")  
edad <- append(edad,list(csec_people(x = 1, y = "Redad", w = 1)))

etapa<-lapply(X = c(1:6), FUN = csec_people, y = "etapa")  
etapa <- append(etapa,list(csec_people(x = 1, y = "etapa", w = 1)))

estrato<-lapply(X = c(1:6), FUN = csec_people, y = "estrato")  
estrato <- append(estrato,list(csec_people(x = 1, y = "estrato", w = 1)))

etnia<-lapply(X = c(1:6), FUN = csec_people, y = "P1173")  
etnia <- append(etnia,list(csec_people(x = 1, y = "P1173", w = 1)))

annex_file <- loadWorkbook("Output/Resultado_ENUT_2024_8R.xlsx")

colx <- c(3,6,9,12,15,18,21)


vars <- list(sexo, clase, reg, hogar, educacion, edad,etapa,estrato, etnia)
starts <- c(11,21,24,33,42,47,56,64,69)
#starts <- c(11,22,25,34,43,48,56,64,69)

for (i in 1:7) {
  for (j in seq_along(vars)) {
    writeData(annex_file, sheet = "Cuadro 6",
              vars[[j]][[i]], startCol = colx[i],
              startRow = starts[j], colNames = FALSE, rowNames = FALSE)
  }
  saveWorkbook(annex_file, "Output/Resultado_ENUT_2024_8R.xlsx", overwrite = TRUE)
}
