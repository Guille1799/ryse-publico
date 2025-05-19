# RYSE-TFM-
# TFM - Análisis de Rendimiento de Jugadores Profesionales de League of Legends

Este proyecto forma parte del Trabajo de Fin de Máster (TFM) y tiene como objetivo analizar el rendimiento y comportamiento de jugadores profesionales de **League of Legends (LoL)** utilizando datos obtenidos mediante la **API oficial de Riot Games**.

Enlace al documento del proyecto: [Documento de Google](https://docs.google.com/document/d/1eidNoR7E6L5WWEgmf1BtzN6xazL0aB-b/edit?usp=sharing&ouid=105307291187483947765&rtpof=true&sd=true)

Enlace a Outline y Planificación de las Tareas: [Outline Metedología y Desarrollo](https://docs.google.com/document/d/1-xVnkuvqqZI0_UUrvR2LCbB2vsJqk2nCJonvTQYsBsA/edit?usp=sharing) y [Planificación de Tareas](https://docs.google.com/spreadsheets/d/1CuY2KTSSd9vutyjWNhCmVz7AMuAKnRWjWEdgWc6cSqU/edit?usp=sharing)

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
