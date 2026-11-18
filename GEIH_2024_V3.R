################################################################################
# Valoración del TDCNR por funcionalidad CSEC - 2024
#
# V3: script autocontenido. Ya no hay que correr la ENUT en la misma sesión.
#   - V2 llamaba a csec_q() del script ENUT_2024.R, que lee los objetos globales
#     `df` (homologación) y `uso_tiempo`. Como este script también usa `df` para
#     la GEIH, había que correr V2 hasta la línea 189, luego ENUT_2024.R y luego
#     el resto. Correrlo de arriba abajo fallaba.
#   - Ahora la sección 3 calcula aquí mismo las horas ENUT por pregunta, con la
#     misma lógica que csec_q(x, y = "P6020") de ENUT_2024.R. Todos los objetos
#     de la ENUT llevan prefijo `enut_`, así que no chocan con `df` de la GEIH.
#   - Se quitaron dos líneas de depuración de V2 (`aaa |> ... View()`, que
#     fallaba porque `aaa` no existe, y `tmp <- df[...]`).
#
# Secciones:
#   0. Parámetros
#   1. Insumos
#   2. GEIH: ingreso por hora y salario promedio por código CSEC  -> `horas`
#   3. ENUT: horas anuales por pregunta y funcionalidad           -> `res`
#   4. Valoración: salario promedio por funcionalidad             -> `sfuncional`
################################################################################

library(dplyr)
library(data.table)
library(readxl)
library(tidyr)
library(stringr)
library(writexl)

# 0. Parámetros -----------------------------------------------------------------

# RUTA_DANE es la carpeta que contiene ENUT/, GEIH/ y Pobreza/. Se toma de la
# variable de entorno RUTA_DANE (por ejemplo, definida en ~/.Renviron). Si no
# existe, se asume la estructura DANE/ENUT/Code: dos niveles por encima del
# directorio de trabajo, que debe ser la carpeta de este repositorio.
if (!exists("RUTA_DANE")) {
  RUTA_DANE <- Sys.getenv("RUTA_DANE",
                          unset = normalizePath(file.path(getwd(), "..", ".."), winslash = "/"))
}
path <- paste0(RUTA_DANE, "/")
setwd(path)

# ENUT_2024.R (línea 337) quita a las personas sin REGION1 antes de calcular.
# Son 29.006 personas, 1,55 millones expandidas (2,9 %). TRUE reproduce lo que
# hacía el flujo V2 + ENUT_2024.R; FALSE las incluye (como ENUT_2024_8R.R).
ENUT_FILTRA_REGION1 <- TRUE

# 1. Insumos ---------------------------------------------------------------------

valoracion <- readxl::read_excel("ENUT/Raw/Otros/Base2021.xlsx")

homologo2 <- read_excel("ENUT/Anex/Homologación ENUT-CSEC.xlsx", sheet = "Anex2")
colnames(homologo2)[2] <- "Grupo"

qp6370 <- read_excel("ENUT/Raw/Otros/identif_rama_oficio_nal_2024_v2.xlsx")
cor_claudia <- read_excel("ENUT/Raw/Otros/CSEC_Correlativa_2024_26062026.xlsx")
colnames(cor_claudia)[3] <- "SECUENCIA_P"

# 2. GEIH: ingreso por hora ------------------------------------------------------

archivos <- list.files(paste0(path, "GEIH/2024/"), recursive = T, full.names = T)

ocupa <- archivos[
  grepl("^Ocupados.CSV", basename(archivos),
        ignore.case = TRUE)]

otros <- archivos[
  grepl("Otros ingresos e impuestos.CSV", basename(archivos),
        ignore.case = TRUE)]

ocu1 <- lapply(ocupa, fread) |>
  rbindlist(fill = T, ignore.attr = T)

ocu1 <- ocu1 |> filter(P6430 %in% c(1,3,4))

otrosx <- lapply(otros, fread) |>
  rbindlist(fill = T, ignore.attr = T)

pobreza <- fread("Pobreza/Pobreza2024.csv")

pobreza <- pobreza |> filter(p6430 %in% c(1,3,4))
colnames(pobreza) <- toupper(colnames(pobreza))

pobreza$IMPA[is.na(pobreza$IMPA)] <- 0
pobreza$IMPAES[is.na(pobreza$IMPAES)] <- 0
pobreza$IEES[is.na(pobreza$IEES)] <- 0

pobreza$imputados <- pobreza$IMPA + pobreza$IMPAES

idx <- c("DIRECTORIO",
         "SECUENCIA_P",
         "ORDEN",
         "HOGAR",
         "MES")

df <- merge(ocu1, otrosx, by = idx, all.x = T)
df <- merge(df, pobreza[,c("DIRECTORIO","SECUENCIA_P","ORDEN","imputados","IEES")],
            by = c("DIRECTORIO","SECUENCIA_P","ORDEN"), all.x = T)

qp6370$MES <- qp6370$MES |> as.integer()

df <- merge(df, qp6370,
            by = c("DIRECTORIO","SECUENCIA_P","ORDEN","HOGAR","MES"), all.x = T)

vars_usadas <- c(
  "P6500", "P6510S2", "P6510S1", "P6590S1", "P6600S1", "P6610S1", "P6620S1",
  "P6585S1A2", "P6585S1A1", "P6585S2A2", "P6585S2A1",
  "P6585S3", "P6585S3A2", "P6585S3A1", "P6585S4", "P6585S4A2", "P6585S4A1",
  "P6545S2", "P6545S1", "P6580S2", "P6580S1",
  "P6630S1A1", "P6630S2A1", "P6630S3A1", "P7510S6"
)

df <- df %>%
  mutate(across(
    all_of(vars_usadas),
    ~ as.numeric(ifelse(is.na(.), 0, .))
  ))

df <- df |> mutate(sys = P6500 +
                     if_else(P6510S2 == 1, P6510S1, 0) +
                     P6590S1 +
                     P6600S1 +
                     P6610S1 +
                     P6620S1 +
                     if_else(P6585S1A2 == 2, P6585S1A1, 0) +
                     if_else(P6585S2A2 == 2, P6585S2A1, 0) +
                     if_else(P6585S3 == 1 & P6585S3A2 == 1, -P6585S3A1, 0) +
                     if_else(P6585S4 == 1 & P6585S4A2 == 1, -P6585S4A1, 0) +
                     if_else(P6545S2 == 2, P6545S1, 0) +
                     if_else(P6580S2 == 2, P6580S1, 0) +
                     P6630S1A1 / 12 +
                     P6630S2A1 / 12 +
                     P6630S3A1 / 12 +
                     if_else(P7510S6 == 1, P6500 / 12, 0) +
                     if_else(P7510S6 == 1, P6500 * 0.01, 0))

df <- df %>%
  mutate(resultado = P6500 +
           ifelse(P6510S2 == 1, P6510S1, 0) +
           P6590S1 + P6600S1 + P6610S1 + P6620S1 +
           ifelse(P6585S1A2 == 2, P6585S1A1, 0) +
           ifelse(P6585S2A2 == 2, P6585S2A1, 0) +
           ifelse(P6585S3 == 1 & P6585S3A2 == 1, -P6585S3A1, 0) +
           ifelse(P6585S4 == 1 & P6585S4A2 == 1, -P6585S4A1, 0) +
           ifelse(P6545S2 == 2, P6545S1, 0) +
           ifelse(P6580S2 == 2, P6580S1, 0) +
           P6630S1A1/12 + P6630S2A1/12 + P6630S3A1/12 +
           ifelse(P7510S6 == 1, P6500/12, 0) +
           ifelse(P7510S6 == 1, P6500*0.01, 0))

df <- df %>%
  mutate(
    aportes = coalesce(
      if_else(
        P6800 >= 48,
        P6500 * 0.085 + P6500 * 0.12 + P6500 * 0.00522 + P6500 * 0.04,
        P6500 * 0.12 + P6500 * 0.00522 + P6500 * 0.04
      ),
      0
    )
  )

df$aportes[is.na(df$aportes)] <- 0
df$IEES[is.na(df$IEES)] <- 0
df$imputados[is.na(df$imputados)] <- 0

df <- df |> mutate(csec_code = if_else(P6430 %in% c(1,3,4,5),1,0))

df <- df |> mutate(systotal = if_else(csec_code == 1 | P6430 == 1 |
                                        P6430 == 2 | P6430 == 3 | P6430 == 8, sys + aportes, sys))

df <- df |> mutate(systotal = if_else(systotal >= imputados, systotal, imputados))

df$systotal[is.na(df$systotal)] <- 0
df$systotal <- df$systotal + df$IEES

df <- df %>%
  mutate(
    ing_hora = if_else(
      P6430 %in% c(1, 2, 3, 8),
      # Si P6430 es 1,2,3 u 8
      (systotal * 12) / (P6800 * (52 - 3)),   # = * 49
      # Si no, intenta Ingresos_ConIees/(P6850*4.33); si "error", usa el fallback
      coalesce(
        na_if(na_if(systotal / (P6850 * 4.33),  Inf), -Inf),
        (systotal * 12) / (P6800 * (52 - 3))
      )
    )
  )

df <- merge(df, cor_claudia, by = c("DIRECTORIO","SECUENCIA_P","ORDEN","MES"), all.x = T)

df <- df |> filter(!is.na(CCSEC_1))

horas <- df |> group_by(CCSEC_1) |>
  reframe(valor = sum(ing_hora*FEX_C18.x/12, na.rm = T),
          people = sum(FEX_C18.x/12, na.rm = T),
          vp = valor/people)

colnames(horas)[1] <- "COD_CSEC"
colnames(valoracion)[1] <- "COD_CSEC"

horas$COD_CSEC[horas$COD_CSEC == "Generalista"] <- "GENERALISTA"

horas3 <- merge(horas, valoracion, by = "COD_CSEC")

horas3$dif <- horas3$vp/horas3$DEV-1

#write_xlsx(horas3, "ENUT/Output/Valoracion_2024_V3.xlsx")

# 3. ENUT: horas por pregunta ----------------------------------------------------
# Equivale a csec_q(x, y = "P6020") de ENUT_2024.R, con los mismos archivos:
#   uso del tiempo = C8.csv (enut[[9]] en ENUT_2024.R)
#   personas       = C3.csv (enut[[3]]): aporta el sexo P6020
#   vivienda       = C1.csv (enut[[1]]): aporta REGION1
# El factor es FEX_C del archivo de uso del tiempo. ENUT_2024.R usa FEX_C.y
# (el de C1), pero los dos son idénticos persona por persona (verificado
# 2026-11-18), así que el resultado es el mismo.

enut_dir <- "ENUT/Raw/ENUT_2024_CSV/"

leer_enut <- function(archivo) {
  d <- fread(paste0(enut_dir, archivo), sep = ";")
  names(d) <- toupper(names(d))
  as.data.frame(d)
}

enut_ut   <- leer_enut("C8.csv")
enut_pers <- leer_enut("C3.csv") |> select(DIRECTORIO, SECUENCIA_P, ORDEN, P6020)
enut_viv  <- leer_enut("C1.csv") |> select(DIRECTORIO, REGION1)

# Homologación preguntas ENUT -> funcionalidad CSEC (igual que ENUT_2024.R)
enut_homologo <- read_excel("ENUT/Anex/Homologación ENUT-CSEC.xlsx", sheet = "Anexo 1")
colnames(enut_homologo) <- c("cod","des","act_CSEC","des_CSEC")

enut_colsx <- grep("S", colnames(enut_ut), value = TRUE)

enut_preguntas <- data.frame(
  pregunta = sub("S.*", "", enut_colsx),                 # antes de la "S"
  opcion1  = sub(".*S", "", enut_colsx),                 # después de la "S"
  opcion2  = sub(".*S([0-9]+)A.*", "\\1", enut_colsx),   # número entre S y A
  item     = sub(".*A", "", enut_colsx)                  # después de la "A"
)

enut_qadmin  <- enut_preguntas |> filter(pregunta == "P1140" & (item %in% c(1:4)))
enut_qcitasm <- enut_preguntas |> filter(pregunta == "P1131" & (item %in% c(1:4)))

enut_preguntas <- enut_preguntas[enut_preguntas$item %in% c(1,2),]
enut_preguntas <- enut_preguntas |> filter(!(pregunta %in% c("P1140","P1131")))
enut_preguntas <- rbind(enut_preguntas, enut_qadmin, enut_qcitasm)

enut_preguntas$cod  <- paste0(enut_preguntas$pregunta, "S", enut_preguntas$opcion2)
enut_preguntas$cod2 <- paste0(enut_preguntas$pregunta, "S", enut_preguntas$opcion1)

enut_homolog <- merge(enut_homologo, enut_preguntas, by = "cod", all = T)

enut_op5 <- enut_homolog[is.na(enut_homolog$cod2),]
enut_homolog <- enut_homolog[!is.na(enut_homolog$act_CSEC),]

colnames(enut_preguntas)[1] <- "cod"
colnames(enut_preguntas)[5] <- "cod0"

enut_op5 <- merge(enut_op5[,colnames(enut_homologo)], enut_preguntas, by = "cod", all.x = T)
colnames(enut_op5) <- colnames(enut_homolog)

enut_homolog <- rbind(enut_homolog, enut_op5)
enut_homolog <- enut_homolog[!is.na(enut_homolog$cod2),]

# Base de personas con sexo, región y factor
enut_base <- enut_ut |>
  merge(enut_pers, by = c("DIRECTORIO","SECUENCIA_P","ORDEN"), all.x = T) |>
  merge(enut_viv, by = "DIRECTORIO", all.x = T)

if (ENUT_FILTRA_REGION1) {
  enut_base <- enut_base |> filter(!is.na(REGION1))
}

# Horas por pregunta (Grupo) para la funcionalidad x, con FEX_C.
# Los ítems que terminan en 2 o 4 son minutos y se pasan a horas.
horas_enut_pregunta <- function(x) {

  columnas <- enut_homolog |>
    filter(act_CSEC == x) |>
    pull(cod2) |>
    unique()

  enut_base |>
    select(all_of(columnas), P6020, FEX_C) |>
    mutate(across(all_of(columnas) & ends_with(c("2", "4")), ~ . / 60)) |>
    pivot_longer(cols = all_of(columnas),
                 names_to = "Actividad_original",
                 values_to = "valor") |>
    mutate(Grupo = str_extract(Actividad_original, "^P\\d+S\\d+"),
           valor_ponderado = valor * FEX_C) |>
    group_by(Grupo, P6020) |>
    summarise(horasm = sum(valor_ponderado, na.rm = TRUE),
              .groups = "drop") |>
    pivot_wider(names_from = P6020, values_from = horasm,
                names_prefix = "sexo_") |>
    transmute(Grupo,
              Hombres = sexo_1,
              Mujeres = sexo_2,
              Total = sexo_1 + sexo_2)
}

res <- lapply(1:6, FUN = horas_enut_pregunta) |> rbindlist()

# 4. Valoración ------------------------------------------------------------------

res <- merge(res, homologo2, by = "Grupo")

horas <- horas |> as.data.frame()

res$COD_CSEC[res$COD_CSEC == "Generalista"] <- "GENERALISTA"

res1 <- res |> filter(Tipo != 0) |> merge(horas, by = "COD_CSEC")

res1 |> dplyr::group_by(Funcionalidad) |> dplyr::summarize(val = sum(vp*Total)/sum(Total))

res1 <- res1 |> select(Grupo, Funcionalidad, Total, vp)

# Actividades Tipo 0: se valoran con el promedio de varios códigos CSEC.
# La asignación de abajo depende de que haya exactamente 5 filas, en este orden.
res2 <- res |> filter(Tipo == 0)
stopifnot(nrow(res2) == 5)
res2$Sub <- c("a","b","c","c","C")

res2 <- rbind(res2, res2)
res2$COD_CSEC <- c(33,32,55,55,55,58,33,52,52,52) |> as.character()

res2 <- merge(res2, horas, by.x = "COD_CSEC", by.y = "COD_CSEC", all.x = T)

res2 <- res2 |> dplyr::mutate(Grupo = substr(Grupo, start = 1, stop = nchar(Grupo)-2)) |>
  dplyr::group_by(Grupo, Funcionalidad) |> dplyr::summarize(
    vp = sum(people*vp)/sum(people), Total = mean(Total))

restotal <- rbind(res1, res2)

sfuncional <- restotal |> dplyr::group_by(Funcionalidad) |> dplyr::summarize(sum(Total*vp)/sum(Total))

sfuncional

#write_xlsx(sfuncional, "ENUT/Output/Salarios_Funcionales_2024_V3.xlsx")
