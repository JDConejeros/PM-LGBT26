# 00_Algoritmos.R
# PM-LGBT26 — Algoritmos binarios para construir grupos objetivo en fuentes secundarias
# Grupos:
#   PM-LGB cisgénero: 60+ años, orientación sexual no heterosexual, identidad cisgénero
#   PM-Trans: 40+ años, identidad de género distinta al sexo asignado al nacer
#
# Requiere: haven, dplyr (opcional)

suppressPackageStartupMessages({
  if (!requireNamespace("haven", quietly = TRUE)) install.packages("haven")
  library(haven)
})

# Rutas base (ajustar si cambia estructura del proyecto)
path_data <- function(...) file.path("02_Data", ...)

# ---------------------------------------------------------------------------
# Funciones auxiliares
# ---------------------------------------------------------------------------

na_sogi <- function(x) {
  x %in% c(-99, -88, 88, 89, 9, 99, NA)
}

# ---------------------------------------------------------------------------
# SENAMA 2025 — Encuesta Nacional Personas Mayores (8ª versión)
# ---------------------------------------------------------------------------
# LIMITACIÓN: la base no incluye variables SOGI. No es posible construir
# PM-LGB cisgénero ni PM-Trans con definición del plan de análisis.

algoritmo_senama <- function(d) {
  d$edad_60_mas <- ifelse(!is.na(d$S4) & d$S4 >= 60, 1L, 0L)
  d$sexo_mujer <- ifelse(d$S21 == 1 | d$SEXO == 2, 1L, 0L)

  d$pm_lgb_cis <- NA_integer_
  d$pm_trans <- NA_integer_
  d$grupo_construible_pm_lgb <- 0L
  d$grupo_construible_pm_trans <- 0L

  attr(d, "notas") <- "Sin variables SOGI en SENAMA8. pm_lgb_cis y pm_trans quedan NA."
  d
}

# ---------------------------------------------------------------------------
# INE — Encuesta Web Diversidades 2025
# ---------------------------------------------------------------------------
# LIMITACIÓN: edad disponible solo en tramos (tr_edad). PM-Trans 40+ usa
# tr_edad %in% c(3,4), lo que incluye personas de 30-39 años.

algoritmo_ine <- function(d) {
  # Edad
  d$edad_60_mas <- ifelse(d$tr_edad == 4, 1L, 0L)
  d$edad_40_mas_aprox <- ifelse(d$tr_edad %in% c(3L, 4L), 1L, 0L)

  # Orientación sexual no heterosexual
  d$os_no_hetero <- case_when_ine_os(d$m02_var09)
  if ("sgos_div_sexual" %in% names(d)) {
    d$os_no_hetero <- ifelse(is.na(d$os_no_hetero), d$sgos_div_sexual, d$os_no_hetero)
  }

  # Trans: identidad distinta a sexo asignado al nacer
  d$trans_ig_distinta_sa <- case_when_ine_trans(d$m02_var01, d$m02_var05)
  if ("sgos_trans" %in% names(d)) {
    d$trans_ig_distinta_sa <- ifelse(is.na(d$trans_ig_distinta_sa), d$sgos_trans, d$trans_ig_distinta_sa)
  }

  # Cisgénero (para PM-LGB)
  d$cisgenero <- case_when_ine_cis(d$m02_var01, d$m02_var05)

  d$pm_lgb_cis <- ifelse(
    d$edad_60_mas == 1L & d$os_no_hetero == 1L & d$cisgenero == 1L, 1L,
    ifelse(is.na(d$edad_60_mas) | is.na(d$os_no_hetero) | is.na(d$cisgenero), NA_integer_, 0L)
  )

  d$pm_trans <- ifelse(
    d$edad_40_mas_aprox == 1L & d$trans_ig_distinta_sa == 1L, 1L,
    ifelse(is.na(d$edad_40_mas_aprox) | is.na(d$trans_ig_distinta_sa), NA_integer_, 0L)
  )

  d$grupo_construible_pm_lgb <- 1L
  d$grupo_construible_pm_trans <- 1L
  attr(d, "notas") <- "PM-Trans usa tramo 30-59 como aproximación de 40+."
  d
}

case_when_ine_os <- function(x) {
  ifelse(x %in% c(1L, 2L, 3L, 4L, 5L, 7L), 1L,
         ifelse(x == 6L, 0L,
                ifelse(x %in% c(88L, 99L), NA_integer_, NA_integer_)))
}

case_when_ine_trans <- function(ig, sa) {
  trans_id <- ig %in% c(3L, 4L, 5L, 6L)
  cis_match <- (ig == 1L & sa == 1L) | (ig == 2L & sa == 2L)
  ifelse(trans_id | (!is.na(ig) & !is.na(sa) & !cis_match & !ig %in% c(88L, 99L)), 1L,
         ifelse(ig %in% c(88L, 99L) | sa %in% c(88L, 99L), NA_integer_, 0L))
}

case_when_ine_cis <- function(ig, sa) {
  cis <- (ig == 1L & sa == 1L) | (ig == 2L & sa == 2L)
  ifelse(cis, 1L,
         ifelse(ig %in% c(88L, 99L) | sa %in% c(88L, 99L), NA_integer_, 0L))
}

# ---------------------------------------------------------------------------
# CASEN 2015
# ---------------------------------------------------------------------------
# LIMITACIÓN: sin sexo asignado al nacer ni categoría trans explícita en r22.
# PM-Trans no construible con definición estricta.

algoritmo_casen2015 <- function(d) {
  d$edad_60_mas <- ifelse(!is.na(d$edad) & d$edad >= 60, 1L, 0L)
  d$edad_40_mas <- ifelse(!is.na(d$edad) & d$edad >= 40, 1L, 0L)

  d$os_no_hetero <- ifelse(d$r21 %in% c(2L, 3L, 4L), 1L,
                           ifelse(d$r21 %in% c(1L), 0L, NA_integer_))

  # Proxy cis: masculino/femenino en r22 (excluye "Otro")
  d$cisgenero <- ifelse(d$r22 %in% c(1L, 2L), 1L,
                        ifelse(d$r22 == 3L, 0L, NA_integer_))

  d$pm_lgb_cis <- ifelse(
    d$edad_60_mas == 1L & d$os_no_hetero == 1L & d$cisgenero == 1L, 1L,
    ifelse(is.na(d$edad_60_mas) | is.na(d$os_no_hetero) | is.na(d$cisgenero), NA_integer_, 0L)
  )

  d$pm_trans <- NA_integer_
  d$grupo_construible_pm_lgb <- 1L
  d$grupo_construible_pm_trans <- 0L
  attr(d, "notas") <- "PM-Trans no identificable: r22 no distingue trans y no hay sexo asignado."
  d
}

# ---------------------------------------------------------------------------
# CASEN 2017
# ---------------------------------------------------------------------------

algoritmo_casen2017 <- function(d) {
  d$edad_60_mas <- ifelse(!is.na(d$edad) & d$edad >= 60, 1L, 0L)
  d$edad_40_mas <- ifelse(!is.na(d$edad) & d$edad >= 40, 1L, 0L)

  d$os_no_hetero <- ifelse(d$r23 %in% c(2L, 3L, 4L), 1L,
                           ifelse(d$r23 == 1L, 0L, NA_integer_))

  d$cisgenero <- ifelse(d$r24 %in% c(1L, 2L), 1L,
                        ifelse(d$r24 == 3L, 0L, NA_integer_))

  d$trans_ig <- ifelse(d$r24 == 3L, 1L,
                       ifelse(d$r24 %in% c(1L, 2L), 0L, NA_integer_))

  d$pm_lgb_cis <- ifelse(
    d$edad_60_mas == 1L & d$os_no_hetero == 1L & d$cisgenero == 1L, 1L,
    ifelse(is.na(d$edad_60_mas) | is.na(d$os_no_hetero) | is.na(d$cisgenero), NA_integer_, 0L)
  )

  d$pm_trans <- ifelse(
    d$edad_40_mas == 1L & d$trans_ig == 1L, 1L,
    ifelse(is.na(d$edad_40_mas) | is.na(d$trans_ig), NA_integer_, 0L)
  )

  d$grupo_construible_pm_lgb <- 1L
  d$grupo_construible_pm_trans <- 1L
  attr(d, "notas") <- "Proxy trans: r24==3 (Transgénero). Sin sexo asignado al nacer."
  d
}

# ---------------------------------------------------------------------------
# CASEN 2022
# ---------------------------------------------------------------------------

algoritmo_casen2022 <- function(d) {
  d$edad_60_mas <- ifelse(!is.na(d$edad) & d$edad >= 60, 1L, 0L)
  d$edad_40_mas <- ifelse(!is.na(d$edad) & d$edad >= 40, 1L, 0L)

  d$os_no_hetero <- ifelse(d$os1 %in% c(2L, 3L, 4L), 1L,
                           ifelse(d$os1 == 1L, 0L, NA_integer_))

  d$cisgenero <- ifelse(d$genero %in% c(1L, 2L) & !is.na(d$genero), 1L,
                        ifelse(d$genero %in% c(3L, 4L, 5L, 6L), 0L, NA_integer_))

  d$trans_ig <- ifelse(d$trans == 1L | d$genero %in% c(3L, 4L, 5L, 6L), 1L,
                       ifelse(d$trans == 2L | d$genero %in% c(1L, 2L), 0L, NA_integer_))

  d$pm_lgb_cis <- ifelse(
    d$edad_60_mas == 1L & d$os_no_hetero == 1L & d$cisgenero == 1L, 1L,
    ifelse(is.na(d$edad_60_mas) | is.na(d$os_no_hetero) | is.na(d$cisgenero), NA_integer_, 0L)
  )

  d$pm_trans <- ifelse(
    d$edad_40_mas == 1L & d$trans_ig == 1L, 1L,
    ifelse(is.na(d$edad_40_mas) | is.na(d$trans_ig), NA_integer_, 0L)
  )

  d$grupo_construible_pm_lgb <- 1L
  d$grupo_construible_pm_trans <- 1L
  attr(d, "notas") <- "Módulo SOGI aplica si os_presente==1. Sin sexo asignado; cis/trans vía genero/trans."
  d
}

# ---------------------------------------------------------------------------
# ENSSEX 2022-2023
# ---------------------------------------------------------------------------

algoritmo_enssex <- function(d) {
  d$edad_60_mas <- ifelse(!is.na(d$p4) & d$p4 >= 60, 1L, 0L)
  d$edad_40_mas <- ifelse(!is.na(d$p4) & d$p4 >= 40, 1L, 0L)

  # Orientación sexual (preferir p134; fallback variables derivadas MINSAL)
  d$os_no_hetero <- ifelse(d$p134 %in% c(1L, 2L, 3L, 5L), 1L,
                           ifelse(d$p134 == 4L, 0L, NA_integer_))
  if ("orientacion_sexual_separado" %in% names(d)) {
    d$os_no_hetero <- ifelse(
      is.na(d$os_no_hetero),
      ifelse(d$orientacion_sexual_separado %in% c(2L, 4L, 5L), 1L,
             ifelse(d$orientacion_sexual_separado == 1L, 0L, NA_integer_)),
      d$os_no_hetero
    )
  }

  # Cisgénero: concordancia p1 (sexo asignado) y p3 (género)
  d$cisgenero <- ifelse(
    (d$p1 == 1L & d$p3 == 1L) | (d$p1 == 2L & d$p3 == 2L), 1L,
    ifelse(d$p3 %in% c(3L, 4L, 5L, 6L), 0L,
           ifelse(d$p1 %in% c(9L) | d$p3 %in% c(7L, 8L, 9L), NA_integer_, NA_integer_))
  )

  d$trans_ig <- ifelse(
    d$p3 %in% c(3L, 4L, 5L, 6L), 1L,
    ifelse(d$p3 %in% c(1L, 2L), 0L, NA_integer_)
  )
  if ("genero" %in% names(d)) {
    d$trans_ig <- ifelse(is.na(d$trans_ig) & d$genero == 2L, 1L, d$trans_ig)
    d$cisgenero <- ifelse(is.na(d$cisgenero) & d$genero == 1L, 1L, d$cisgenero)
  }

  d$pm_lgb_cis <- ifelse(
    d$edad_60_mas == 1L & d$os_no_hetero == 1L & d$cisgenero == 1L, 1L,
    ifelse(is.na(d$edad_60_mas) | is.na(d$os_no_hetero) | is.na(d$cisgenero), NA_integer_, 0L)
  )

  d$pm_trans <- ifelse(
    d$edad_40_mas == 1L & d$trans_ig == 1L, 1L,
    ifelse(is.na(d$edad_40_mas) | is.na(d$trans_ig), NA_integer_, 0L)
  )

  d$grupo_construible_pm_lgb <- 1L
  d$grupo_construible_pm_trans <- 1L
  d
}

# ---------------------------------------------------------------------------
# Carga y aplicación
# ---------------------------------------------------------------------------

cargar_y_etiquetar <- function(fuente = c("senama", "ine", "casen2015", "casen2017", "casen2022", "enssex")) {
  fuente <- match.arg(fuente)
  d <- switch(
    fuente,
    senama = read_sav(path_data("SENAMA8", "Base Senama.sav")),
    ine = read_dta(path_data("INE-DIV25", "bbdd_ewd_2025v1.dta")),
    casen2015 = read_dta(path_data("CASEN", "2015", "casen_2015.dta")),
    casen2017 = read_dta(path_data("CASEN", "2017", "casen_2017.dta")),
    casen2022 = read_dta(path_data("CASEN", "2022", "casen_2022.dta")),
    enssex = read_dta(path_data("BBDD_ENSSEX_2022_2023", "20240516_ENSSEX_data.dta"))
  )

  d <- switch(
    fuente,
    senama = algoritmo_senama(d),
    ine = algoritmo_ine(d),
    casen2015 = algoritmo_casen2015(d),
    casen2017 = algoritmo_casen2017(d),
    casen2022 = algoritmo_casen2022(d),
    enssex = algoritmo_enssex(d)
  )

  d
}

# Ejemplo de uso:
# d_casen22 <- cargar_y_etiquetar("casen2022")
# table(d_casen22$pm_lgb_cis, useNA = "ifany")
# table(d_casen22$pm_trans, useNA = "ifany")
# attr(d_casen22, "notas")
