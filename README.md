# RYSE-TFM-
# TFM - Análisis de Rendimiento de Jugadores Profesionales de League of Legends

Este proyecto forma parte del Trabajo de Fin de Máster (TFM) y tiene como objetivo analizar el rendimiento y comportamiento de jugadores profesionales de **League of Legends (LoL)** utilizando datos obtenidos mediante la **API oficial de Riot Games**.

El análisis se realiza a través de R y R Markdown, recopilando partidas clasificadas recientes de jugadores seleccionados, extrayendo estadísticas clave y generando un dataset unificado para su posterior análisis.

---

## Tecnologías utilizadas
- Lenguaje: **R**
- Entorno: **RStudio**
- Formato: **R Markdown (.Rmd)**
- Fuentes de datos: [Riot Games Developer API](https://developer.riotgames.com/)

---

## Estructura del proyecto

- `TFM prueba.Rmd`: script principal que gestiona:
  - Recolección de datos (PUUID, partidas recientes)
  - Extracción de estadísticas relevantes de partidas clasificadas (soloQ con un duración superior a 15 min)
  - Integración de datos en un único DataFrame
  - Creación de un dataset para cada jugador
- Otros archivos esperados (no incluidos):
  - `.Renviron`: archivo con la clave `api_key` de Riot Games
  - `datos/*.csv`: exportación o almacenamiento de resultados si aplica

---

## Cómo ejecutar el proyecto

1. Clona el repositorio:
   ```bash
   git clone https://github.com/tuusuario/nombre-repo.git
