# Trabajo Final

Repo con el enunciado y los materiales para la entrega final en **Análisis Inteligente de Datos**.

# Sistema de Carga y Análisis de Horas (Shiny)

Aplicación Shiny para **registrar, administrar y analizar horas de trabajo** del área de I+D.  
Permite cargar fragmentos diarios por persona y tipo de tarea, visualizar indicadores y mantener actualizados los catálogos base.

## Características principales
- **Carga de horas** por persona, fecha, tipo de tarea (NP, PE, SM, Rutinas) y detalle.
- **Cierre de día** que guarda las imputaciones en `data/imputaciones.csv`.
- **Tablero interactivo** con KPIs, gráficos (torta, acumulados, comparativos) y drill-down.
- **Administración de maestros**: personal, sectores, tipos, proyectos, PE, SM y rutinas.
- **Persistencia automática** en archivos CSV dentro de la carpeta `data/`.

# Fuente de informacion y limpieza de datos
El área cuenta con tres fuentes distintas de proyectos y además realiza tareas de rutina o presta servicios a otras áreas. 
El registro de estos proyectos se mantiene en 3 archivos en formato excel de donde se debe extraer la informacion para la carga de horas:
- SM: Solicitudes de modificación
- PE: Pedidos especiales
- NP: Nuevos proyectos

## SM — Solicitudes de modificación
Proceso aplicado a cada hoja del Excel:
- Se leen las columnas correspondientes al **número de SM** y **fecha de cierre**.
- Se filtran únicamente filas con número válido y **sin fecha de cierre asignada**.
- Se limpia el número (se remueven espacios y caracteres no numéricos).
- Se unifican todas las hojas, se eliminan duplicados y se generan:
  - `sm_id`
  - `sm_desc`

## PE — Pedidos Especiales
A partir de la hoja 4 del Excel:
- Se leen columnas con **número**, **descripción** y **estado**.
- Se aplican limpiezas de texto (espacios múltiples, caracteres especiales).
- Se filtran únicamente los PE **activos** (estado vacío).
- Se descartan filas sin número.
- Se genera `pe_desc` combinando *número – descripción*.
- Se eliminan duplicados y se construye la tabla final con:
  - `pe_id`
  - `pe_desc`

## NP — Nuevos Proyectos
Desde la hoja principal del Excel:
- Se leen número, nombre y **fecha de homologación**.
- Se limpian y normalizan textos.
- Se filtran únicamente los proyectos **sin fecha de cierre**.
- Se genera el campo `proyecto` combinando código + nombre.
- Se eliminan duplicados y se crea la tabla final con:
  - `proyecto_id`
  - `proyecto`
  - `activo = TRUE`

##  Estructura del proyecto
- `app.R`: interfaz y lógica del servidor.
- `global.R`: configuración, ayudas, carga de catálogos y funciones.
- `data/`: almacenamiento local de datos.
- `datos_crudos/`: archivos Excel usados para cargar NP, PE y SM.
