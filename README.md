# R-data-viz-contest-2025

Data visualization contest with R: fourth edition

Esta es mi entrada para el concurso de visualización de datos con R en Asturias 2025.

Dado el gran número de variables disponibles, he preferido en lugar de generar una única gráfica crear una *shiny app* que permita visualizar los datos por métrica y región de interés.

## Requermientos

Se emplearon las siguientes librerías:

```         
library(shiny)
library(shinyjs)
library(dplyr)
library(plotly)
library(ggplot2)
library(ComplexUpset)
library(viridis)
library(usmap)
library(sf)
library(collegeScorecard)
```

## **Resumen de Funcionalidades**

La aplicación ofrece dos funciones principales:

1.  **Visualización en Mapa:**
    -   **Vista Nacional:** Muestra un mapa de Estados Unidos donde cada estado se colorea según el valor medio de una métrica seleccionada (por ejemplo, tipo de institución, grado predominante, afiliación religiosa, etc.). La información emergente muestra el desglose porcentual de cada categoría dentro del estado.\
    -   **Vista Estatal:** Al seleccionar un estado específico, el mapa se enfoca en dicho estado y muestra puntos interactivos que representan universidades, con detalles como nombre, ciudad y categoría seleccionada. Al hacer clic en una universidad, se abre su sitio web en una nueva pestaña.
2.  **Gráfico Comparativo:**
    -   Genera un gráfico de contingencia (heatmap) que compara dos métricas seleccionadas. Cada cuadro del mapa de calor muestra el número de universidades correspondientes a cada combinación de categorías. El número de cada combinación se muestra en el centro de cada casilla para mayor claridad.

------------------------------------------------------------------------

## **¿Cómo Funciona la Aplicación?**

### **Preparación de Datos**

La aplicación transforma los datos de las universidades utilizando los paquetes **usmap** y **sf**: transforma las coordenadas geográficas para garantizar la compatibilidad con los mapas de EE. UU.

------------------------------------------------------------------------

### **Gestión de Métricas**

    Definí tres tipos de métricas:
    -   **Métricas Categóricas:** i.e. el grado predominante o el tipo de institución, con niveles predefinidos.
    -   **Métricas Booleanas:** i.e. si una universidad es HBCU o no (valores TRUE/FALSE).
    -   **Afiliación Religiosa:** es un caso especial y se trata como valor booleano (1 si existe afiliación, 0 si no).

------------------------------------------------------------------------

### **Interfaz de Usuario (UI)**

-   **Panel Lateral (`sidebarPanel`):**\
    El usuario puede seleccionar:
    -   Un estado (o "All" para ver a nivel nacional).\
    -   Una métrica principal para la visualización.\
    -   Una métrica secundaria para comparativa.\
    -   Una paleta de colores (de las paletas de **Viridis**).
-   **Panel Principal (`mainPanel`):**\
    Contiene dos salidas de Plotly:
    -   **Mapa Interactivo (`mapPlot`):** Visualiza la vista nacional o estatal según la selección del usuario.\
    -   **Gráfico Comparativo (`comparisonPlot`):** Muestra un mapa de calor con los valores de conteo de cada combinación de categorías.

------------------------------------------------------------------------

### **Lógica del Servidor**

-   **Filtrado de Datos Reactivo:**\
    El servidor filtra los datos según el estado seleccionado y lo usa para todas las visualizaciones.

-   **Renderización del Mapa (`mapPlot`):**

    -   **Vista Nacional:**
        -   Convierte la métrica seleccionada a valores numéricos.\
        -   Agrega valores a nivel estatal.\
        -   Genera texto emergente con el desglose de categorías por estado.\
        -   El objeto ggplot se convierte a un mapa interactivo con **Plotly**.\
    -   **Vista Estatal:**
        -   Convierte la métrica seleccionada a valores categóricos.\
        -   Muestra puntos de universidades con información interactiva (nombre, ciudad y categoría).\
        -   Al hacer clic en un punto, se abre el sitio web de la universidad.

-   **Renderización del Gráfico Comparativo (`comparisonPlot`):**\
    Si se selecciona una segunda métrica, se genera un gráfico de contingencia que cruza las categorías de las dos métricas seleccionadas. El número de cada combinación se muestra en el centro de cada casilla con `geom_text()` de ggplot2.

-   **Interactividad con Eventos de Plotly:**\
    La aplicación escucha eventos de clic en el mapa de Plotly:

    -   En la vista nacional, un clic en un estado actualiza el `selectInput` del estado.\
    -   En la vista estatal, un clic en un punto de universidad abre el sitio web usando un manejador de JavaScript personalizado.

------------------------------------------------------------------------

## **Motivos de la personalización**

-   **Paleta de Colores:**\
    El usuario puede elegir entre varias paletas de colores de **Viridis** para la escala de colores. Se consideró usar este paquete para facilitar la visualización a personas con daltonismo. Las distintas paletas deberían ser suficientes para cualquier uso.

-   **Comparación de Métricas:**\
    El gráfico de contingencia facilita la comparación entre cualquier combinación de métricas seleccionadas.
