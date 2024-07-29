
# resultados electorales servel
# https://www.servel.cl/centro-de-datos/resultados-electorales-historicos-gw3/

# primera vuelta
# https://app.powerbi.com/view?r=eyJrIjoiODIyYWQ5NjctYTY3OS00NDQwLWJlMjQtMzA0ZjNjYmVlM2I2IiwidCI6ImVhZjg3OWJkLWQzZWMtNDY1MC1iMTI5LTEzZGZkZjQ4NTlmZSJ9

# segunda vuelta
# https://app.powerbi.com/view?r=eyJrIjoiODVjNjE1NWYtMDUxZS00OTA0LTk3MjAtNjk1OWI3MmVjNWUzIiwidCI6ImVhZjg3OWJkLWQzZWMtNDY1MC1iMTI5LTEzZGZkZjQ4NTlmZSJ9


library(dplyr)
library(readxl)
library(janitor)
library(stringr)
library(textclean)
library(tidyr)


# cargar datos ----
cut_comunas <- read.csv2("datos_originales/comunas_chile_cut.csv")

primera_vuelta_0 <- read_excel("datos_originales/2021_11_Presidencial_1V_Datos_Eleccion.xlsx")
segunda_vuelta_0 <- read_excel("datos_originales/2021_12_Presidencial_2V_Datos_Eleccion.xlsx")


# limpieza inicial ----

# limpieza inicial, crear nombres de candidatos
primera_vuelta_1 <- primera_vuelta_0 |> 
  row_to_names(4) |> 
  clean_names()

segunda_vuelta_1 <- segunda_vuelta_0 |> 
  row_to_names(4) |> 
  clean_names() |>
  rename(nominado = cargo)

#unir primera y segunda vuelta  ----
elecciones_0 <- bind_rows(
  primera_vuelta_1 |> mutate(eleccion = "Primera vuelta"),
  segunda_vuelta_1 |> mutate(eleccion = "Segunda vuelta"))


# limpieza ----
eleciones_1 <- elecciones_0 |> 
  mutate(nominado = replace_na(nominado, "No"),
         nominado = str_to_title(nominado)) |> 
  mutate(primer_apellido = replace_na(primer_apellido, ""),
         candidatura = paste(nombres, primer_apellido),
         candidatura = str_squish(candidatura)) |> 
  mutate(local = str_to_title(local)) |> 
  select(comuna, local, mesa, candidatura, votos, eleccion, nominado)


# modificar mayúsculas 
eleciones_2 <- eleciones_1 |> 
  mutate(comuna = tolower(comuna),
         candidatura = str_to_title(candidatura))

# código de comunas ----
comunas <- eleciones_2 |>
  distinct(comuna)

# comunas con match inmediato
comunas_a <- comunas |> 
  left_join(cut_comunas |>
              mutate(comuna = tolower(comuna)),
            by = "comuna") |>
  filter(!is.na(cut_comuna))

# comunas sin match
comunas |> 
  filter(!comuna %in% comunas_a$comuna)

# hacer match sin tildes (sacar texto ascii)
comunas_b <- comunas |> 
  filter(!comuna %in% comunas_a$comuna) |> 
  left_join(cut_comunas |>
              mutate(comuna = tolower(comuna),
                     comuna = textclean::replace_non_ascii(comuna),
              ),
            by = "comuna") |> 
  filter(!is.na(cut_comuna))

# nuevos matches
comunas_b

# comunas aún sin match
comunas |> 
  filter(!comuna %in% comunas_a$comuna,
         !comuna %in% comunas_b$comuna)

# modificar nombres manualmente
comunas_c <- comunas |> 
  filter(!comuna %in% comunas_a$comuna,
         !comuna %in% comunas_b$comuna) |> 
  left_join(cut_comunas |>
              add_row(cut_comuna = 12202,
                      cut_region = 12,
                      comuna = "Antártica",
                      region = "Magallanes y de la Antártica Chilena") |> 
              mutate(comuna = recode(comuna,
                                     "Paiguano" = "Paihuano",
                                     "Marchihue" = "Marchigue",
                                     "Hualañé" = "Hualañe",
                                     "Aisén" = "Aysen",
                                     "Coihaique" = "Coyhaique",
                                     "Río Ibáñez" = "Rio Ibañez",
                                     "Cabo de Hornos" = "Cabo de Hornos(ex-navarino)",
                                     "Peñalolén" = "Peñalolen",
                                     "Ñiquén" = "Ñiquen",
                                     "Treguaco" = "Trehuaco",
                                     "Antártica" = "Antartica"
              ),
              comuna = tolower(comuna)),
            by = "comuna") |> 
  filter(!is.na(cut_comuna))

# comunas aún sin match
comunas |> 
  filter(!comuna %in% comunas_a$comuna,
         !comuna %in% comunas_b$comuna,
         !comuna %in% comunas_c$comuna)

# unir comunas modificadas
cut_comunas_2 <- bind_rows(comunas_a,
                           comunas_b,
                           comunas_c)

# anexar comunas a datos
eleciones_3 <- eleciones_2 |> 
  left_join(cut_comunas_2, by = "comuna")

# volver a poner comunas correctas y limpiar
eleciones_4 <- eleciones_3 |> 
  select(-comuna) |> 
  left_join(cut_comunas |> select(cut_comuna, comuna), by = "cut_comuna") |> 
  select(eleccion, candidatura, region, comuna, local, mesa, votos, nominado, everything()) |> 
  mutate(votos = as.numeric(votos)) |> 
  arrange(eleccion, cut_region, cut_comuna, local, mesa)


# procesamiento ----

## sumar votos por mesa ----
eleciones_mesa <- eleciones_4 |> 
  group_by(eleccion, mesa, local, comuna, cut_comuna, region, cut_region,
           candidatura) |> 
  summarize(votos = sum(votos, na.rm = T)) |> 
  ungroup() |> 
  # calcular variables mesa
  group_by(eleccion, mesa, local, comuna, cut_comuna, region, cut_region) |> 
  mutate(votos_mesa = sum(votos),
         votos_mesa_mayoria = max(votos)) |>
  mutate(ganador_mesa = ifelse(votos == votos_mesa_mayoria, candidatura, NA)) |>
  fill(ganador_mesa, .direction = "updown") |>
  ungroup()


## sumar votos por local ----
eleciones_local <- eleciones_4 |> 
  group_by(eleccion, local, comuna, cut_comuna, region, cut_region,
           candidatura) |> 
  summarize(votos = sum(votos, na.rm = T)) |> 
  ungroup() |> 
  # calcular variables local
  group_by(eleccion, local, comuna, cut_comuna, region, cut_region) |> 
  mutate(votos_local = sum(votos),
         votos_local_mayoria = max(votos)) |>
  mutate(ganador_local = ifelse(votos == votos_local_mayoria, candidatura, NA)) |>
  fill(ganador_local, .direction = "updown") |>
  ungroup()


## sumar votos por comuna ----
eleciones_comuna <- eleciones_4 |> 
  group_by(eleccion, comuna, cut_comuna, region, cut_region,
           candidatura) |> 
  summarize(votos = sum(votos, na.rm = T)) |> 
  ungroup() |> 
  # calcular variables comuna
  group_by(eleccion, comuna, cut_comuna, region, cut_region) |> 
  mutate(votos_comuna = sum(votos),
         votos_comuna_mayoria = max(votos)) |>
  mutate(ganador_comuna = ifelse(votos == votos_comuna_mayoria, candidatura, NA)) |>
  fill(ganador_comuna, .direction = "updown") |>
  ungroup()


eleciones_comuna |> 
  filter(comuna == "Ñuñoa")

## sumar votos por región ----
eleciones_region <- eleciones_4 |> 
  group_by(eleccion, region, cut_region,
           candidatura) |> 
  summarize(votos = sum(votos, na.rm = T)) |> 
  ungroup() |> 
  # calcular variables región
  group_by(eleccion, region, cut_region) |> 
  mutate(votos_region = sum(votos),
         votos_region_mayoria = max(votos)) |>
  mutate(ganador_region = ifelse(votos == votos_region_mayoria, candidatura, NA)) |>
  fill(ganador_region, .direction = "updown") |>
  ungroup()


## resultados nacionales ----
eleciones_nacional <- eleciones_4 |> 
  select(eleccion, candidatura, votos) |> 
  group_by(candidatura, eleccion) |> 
  summarize(votos = sum(votos)) |> 
  arrange(eleccion, candidatura) |> 
  # calcular variables nacional
  group_by(eleccion) |> 
  mutate(votos_pais = sum(votos),
         votos_pais_mayoria = max(votos)) |>
  mutate(ganador_pais = ifelse(votos == votos_pais_mayoria, candidatura, NA)) |>
  fill(ganador_pais, .direction = "updown") |>
  ungroup()
