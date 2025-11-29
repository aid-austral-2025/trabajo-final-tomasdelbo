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

##  Estructura del proyecto
- `app.R`: interfaz y lógica del servidor.
- `global.R`: configuración, ayudas, carga de catálogos y funciones.
- `data/`: almacenamiento local de datos.
- `datos_crudos/`: archivos Excel usados para cargar NP, PE y SM.
