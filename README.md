# TDCNR por funcionalidad CSEC – ENUT 2024 y GEIH 2024

Rutinas en R que miden y valoran el trabajo doméstico y de cuidado no remunerado (TDCNR) de 2024 para la Cuenta Satélite de Economía del Cuidado (CSEC).

- **Documento metodológico (online):** <https://manrod10.github.io/enut-csec-2024/> (fuente: [`docs/index.html`](docs/index.html))
- **Documento metodológico (PDF):** [`docs/Metodologia_TDCNR_CSEC_2024.pdf`](docs/Metodologia_TDCNR_CSEC_2024.pdf)

## Rutinas

| Rutina | Fuente | Qué produce | Cobertura regional |
| --- | --- | --- | --- |
| `ENUT_2024.R` | ENUT 2024 | Cuadros 1, 5 y 6 (horas anuales, horas semanales por participante y tasa de participación en TDCNR) | 6 regiones clásicas (`REGION1`) |
| `ENUT_2024_8R.R` | ENUT 2024 | Los mismos cuadros | 8 regiones (`REGION2`): agrega Orinoquía y Amazonía |
| `GEIH_2024_V3.R` | GEIH 2024 y ENUT 2024 | Ingreso laboral por hora por código CSEC y salario promedio por funcionalidad | Nacional |

Las tres rutinas son independientes y pueden correrse en cualquier orden.

## Datos

Este repositorio **no incluye datos**. Las rutinas leen los microdatos de la ENUT 2024 y la GEIH 2024, la base de pobreza y las tablas de homologación desde una carpeta externa, `RUTA_DANE`, con esta estructura:

```
RUTA_DANE/
├── ENUT/
│   ├── Code/               (este repositorio)
│   ├── Raw/ENUT_2024_CSV/  (C1.csv a C9.csv)
│   ├── Raw/Otros/          (correlativas y base de referencia 2021)
│   ├── Anex/               (Homologación ENUT-CSEC.xlsx)
│   └── Output/             (plantillas de cuadros y resultados)
├── GEIH/2024/              (una carpeta por mes con Ocupados.CSV y Otros ingresos e impuestos.CSV)
└── Pobreza/Pobreza2024.csv
```

## Configuración de rutas

Cada rutina define `RUTA_DANE` de una de dos formas:

1. **Variable de entorno.** Si existe `RUTA_DANE`, se usa su valor. Por ejemplo, agregue esta línea al archivo `~/.Renviron` y reinicie R:

   ```
   RUTA_DANE=D:/datos/DANE
   ```

2. **Estructura por defecto.** Si la variable no existe, se asume que el repositorio está en `RUTA_DANE/ENUT/Code` y que R se abre con el repositorio como directorio de trabajo. `RUTA_DANE` queda dos niveles arriba.

## Paquetes de R

`dplyr`, `data.table`, `readxl`, `reshape2`, `purrr`, `tidyr`, `stringr`, `openxlsx`, `writexl` y `haven`.

```r
install.packages(c("dplyr", "data.table", "readxl", "reshape2", "purrr",
                   "tidyr", "stringr", "openxlsx", "writexl", "haven"))
```
