library(shiny)
library(ggplot2)
library(latex2exp)
library(RColorBrewer)
library(shinyWidgets)
library(dplyr)
library(plotly)

source("macros.R")
source("calcular_cals.R")
source("calcular_semanas_necesarias.R")
source("creditos.R")

ui <- fluidPage(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Fredoka:wght@400;700&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
    body {
      background-color: #DAF5DC;
      color: #2A2A2A;
      font-family: 'Fredoka', sans-serif !important;
    }
    h1, h2, h3, h4, h5 {
      color: #1E1E1E;
      font-family: 'Fredoka', sans-serif !important;
    }
  ")),
  tags$title("Kcalsculadora")
  ),
  
  titlePanel(
    tags$div(
      style = "font-size: 36px; font-type: bold; text-align: center; font-family: 'Fredoka', sans-serif !important; color: #2C3E50;",
      HTML("🏋️🔥 <strong>Calculadora fitness de calorías</strong>🥩💪")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("peso", "⚖️ Peso (kg):", value = 70, min = 30, max = 200, step = 0.1),
      numericInput("altura", "📏 Altura (cm):", value = 170, min = 100, max = 250, step = 1),
      numericInput("edad", "🎂 Edad:", value = 20, min = 10, max = 100, step = 1),
      selectInput("sexo", "🚻 Sexo:", choices = c("♂️ Masculino" = "M", "♀️ Femenino" = "F")),
      #numericInput("grasa", "🍏 % Grasa corporal:", value = 15, min = 5, max = 50, step = 0.1),
      
      # Switch para saber si conoce el % de grasa corporal con botón de información
      tags$div(
        style = "display: flex; align-items: center;",  # Alinear elementos en la misma línea
        tags$div(
          style = "flex: 1;",  # Ocupar el espacio disponible
          switchInput("conoce_grasa", 
                      label = "🍏 ¿Conoces tu % de grasa corporal?", 
                      onLabel = "Sí", 
                      offLabel = "No", 
                      value = FALSE  # Por defecto está en "No"
          )
        ),
        actionLink("info_grasa", label = NULL, icon = icon("info-circle"), style = "margin-left: 5px;")
      ),
      
      # Mostrar input numérico solo si el usuario conoce su % de grasa corporal
      conditionalPanel(
        condition = "input.conoce_grasa == true",
        numericInput("grasa", "Introduce tu % de grasa corporal:", value = 15, min = 5, max = 50, step = 0.1)
      ),
      
      
      # **Nuevo selector para elegir nivel de entrenamiento**
      selectInput("nivel", "🏋️️ Nivel de entrenamiento:", 
                  choices = c("🔰 Principiante (menos de 6 meses)" = "principiante", 
                              "🎯 Intermedio (6 meses - 2 años)" = "intermedio",
                              "🏆 Avanzado (más de 2 años)" = "avanzado")),
      
      tags$div(
        style = "display: flex; align-items: center;",  # Alinear elementos en la misma línea
        tags$div(
          style = "flex: 1;",  # Ocupar el espacio disponible
          checkboxInput("incluir_tef", "🔥 Incluir termogénesis inducida por la dieta", value = FALSE)
        ),
        actionLink("info_tef", label = NULL, icon = icon("info-circle"), style = "margin-left: 5px;")
      ),
      
      # **Selector dinámico de actividad (cambiará según el nivel)**
      uiOutput("actividad_ui"), 
      
      tags$div(
        style = "display: flex; align-items: center;",  # Alinear elementos en la misma línea
        tags$div(
          style = "flex: 1;",  # Ocupar el espacio disponible
          selectInput("objetivo", "🎯 Objetivo:", 
                      choices = c("⚖️ Mantenimiento" = "mant", 
                                  "🏋️️ Definición" = "def", 
                                  "📈 Volumen" = "vol"))
        ),
        actionLink("info_objetivo", label = NULL, icon = icon("info-circle"), style = "margin-left: 5px;")
      ),
      
      # 🔹 Mostrar la recomendación según la selección del usuario
      uiOutput("recomendacion_objetivo"),
      
      # **Nuevo slider para elegir agresividad (solo si no es mantenimiento)**
      uiOutput("agresividad_ui"),
      
      actionButton("calcular", "📊 Calcular"),
      actionButton("reiniciar", "🔄 Reiniciar valores")
    ),
    
    mainPanel(
      htmlOutput("resultado"),
      plotOutput("graficoIMC"),
      htmlOutput("explicacion"),
      htmlOutput("creditos")
    )
  )
)

server <- function(input, output, session) {
  
  # grasa maxima en volumen para hombre
  por_grasa_vol <- 19
  
  # Cálculo del % de grasa corporal si el usuario no lo conoce
  grasa_usuario <- reactive({
    if (input$conoce_grasa) {
      return(input$grasa)  # Si el usuario conoce su grasa corporal
    } else {
      # Calcular el IMC
      IMC <- input$peso / ((input$altura / 100)^2)
      
      # Asignar valor a sexo (1 = Hombre, 0 = Mujer)
      sexo_valor <- ifelse(input$sexo == "M", 1, 0)
      
      # Aplicar la fórmula adecuada según la edad
      grasa_calculada <- ifelse(
        input$edad > 36,
        (1.20 * IMC) - ifelse(input$sexo == "M", 13, 2),  # Fórmula ajustada para mayores de 40
        (1.20 * IMC) + (0.23 * input$edad) - (10.8 * sexo_valor) - 5.4      # Fórmula original para menores o igual a 40
      )
      
      return(round(grasa_calculada, 1))  # Retorna el % de grasa estimado
    }
  })
  
  # Ventana emergente para explicar el % de grasa corporal
  observeEvent(input$info_grasa, {
    
    if (input$edad > 36) {
      showModal(modalDialog(
        title = "🍏 ¿Qué es el % de grasa corporal?",
        tags$p("El porcentaje de grasa corporal es la proporción de tu peso total que está compuesto por grasa. Es un indicador importante de tu salud general y composición corporal."),
        
        tags$h4("📏 ¿Cómo se calcula si no lo conoces?"),
        tags$p("Dado que tienes ", input$edad, " años, la aplicación utiliza una fórmula ajustada que no tiene en cuenta la edad para evitar sobreestimaciones en personas mayores:"),
        
        tags$blockquote(" % Grasa corporal = (1.20 × IMC) - (13 para hombres, 2 para mujeres) "),
        
        tags$ul(
          tags$li("💡 ", tags$strong("IMC"), ": Índice de Masa Corporal (peso en kg dividido por la altura en metros al cuadrado)."),
          tags$li("💡 ", tags$strong("Sexo"), ": Se resta 13 para hombres y 2 para mujeres.")
        ),
        
        tags$p("⚠️ Esta fórmula ajustada proporciona una estimación más precisa para personas mayores, ya que la fórmula tradicional tiende a sobreestimar la grasa corporal con la edad."),
        
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
    
      showModal(modalDialog(
        title = "🍏 ¿Qué es el % de grasa corporal?",
        tags$p("El porcentaje de grasa corporal es la proporción de tu peso total que está compuesto por grasa. Es un indicador importante de tu salud general y composición corporal."),
        
        tags$h4("📏 ¿Cómo se calcula si no lo conoces?"),
        tags$p("Si no conoces tu % de grasa corporal, la aplicación lo estima utilizando la fórmula de Deurenberg:"),
        
        tags$blockquote(" % Grasa corporal = (1.20 × IMC) + (0.23 × Edad) - (10.8 × Sexo) - 5.4 "),
        
        tags$ul(
          tags$li("💡 ", tags$strong("IMC"), ": Índice de Masa Corporal (peso en kg dividido por la altura en metros al cuadrado)."),
          tags$li("💡 ", tags$strong("Edad"), ": Tu edad en años."),
          tags$li("💡 ", tags$strong("Sexo"), ": Se usa 1 para hombres y 0 para mujeres.")
        ),
        
        tags$p("⚠️ Esta es solo una estimación y puede no ser completamente precisa, ya que factores como la masa muscular o la distribución de grasa pueden afectar el resultado."),
        
        easyClose = TRUE,  # Permite cerrar la ventana haciendo clic fuera de ella
        footer = NULL
      ))
    }
  })
  
  # **Actualizar dinámicamente el selectInput de actividad**
  
  # Ventana emergente para la termogénesis
  observeEvent(input$info_tef, {
    showModal(modalDialog(
      title = "¿Qué es la termogénesis inducida por la dieta (TEF)?",
      tags$p("La termogénesis inducida por la dieta (TEF, por sus siglas en inglés) es la energía que tu cuerpo utiliza para digerir, absorber y metabolizar los alimentos."),
      tags$p("Se estima que el TEF representa aproximadamente un 10% del gasto energético total (GET). Por eso, al incluir el TEF, se multiplica el GET por 1.1."),
      tags$p("Si desactivas esta opción, el cálculo del GET no incluirá este efecto."),
      easyClose = TRUE,  # Permite cerrar la ventana haciendo clic fuera de ella
      footer = NULL
    ))
  })
  
  observeEvent(input$info_objetivo, {
    showModal(modalDialog(
      title = "¿Qué significan las opciones de Objetivo?",
      tags$p("Las opciones de Objetivo te permiten elegir entre tres estrategias principales:"),
      tags$ul(
        tags$li(tags$strong("⚖️ Mantenimiento:"), " Mantener tu peso actual. Las calorías se ajustan para que no pierdas ni ganes peso."),
        tags$li(tags$strong("🏋️️ Definición:"), " Reducir tu porcentaje de grasa corporal mientras mantienes la mayor cantidad posible de masa muscular. Se recomienda un déficit calórico controlado."),
        tags$li(tags$strong("📈 Volumen:"), " Aumentar tu masa muscular mientras minimizas la ganancia de grasa. Se recomienda un superávit calórico moderado.")
      ),
      easyClose = TRUE,  # Permite cerrar la ventana haciendo clic fuera de ella
      footer = NULL
    ))
  })
  
  output$actividad_ui <- renderUI({
    if (input$nivel == "principiante" || input$nivel == "intermedio") {
      selectInput("actividad", "🔱 Nivel de actividad (Mifflin-St Jeor):", 
                  choices = c("🏡 Sedentario (poco o nada de ejercicio)" = 1.2, 
                              "🚶 Ligera actividad (1-3 días/semana)" = 1.375, 
                              "🏃 Moderada actividad (3-5 días/semana)" = 1.55, 
                              "💪 Alta actividad (6-7 días/semana)" = 1.725, 
                              "⚡ Muy alta actividad (entrenamientos dobles o trabajo físico duro)" = 1.9))
    } else {
      selectInput("actividad", "🔱 Nivel de actividad (Eric Helms):", 
                  choices = c("🏡 Sedentario + 3-6 días de entrenamiento" = 1.35,  
                              "🚶 Ligeramente activo + 3-6 días de entrenamiento" = 1.55,  
                              "💪 Activo + 3-6 días de entrenamiento" = 1.75,  
                              "⚡ Muy activo + 3-6 días de entrenamiento" = 1.95))
    }
  })
  
  output$agresividad_ui <- renderUI({
    if (input$objetivo != "mant") {
      tagList(
        sliderTextInput("agresividad", 
                        label = "📏 Agresividad del cambio:",
                        choices = c("🔵 Muy baja", "🟢 Baja", "🟡 Moderada", "🟠 Alta", "🔴 Muy alta"),
                        selected = "🟡 Moderada", 
                        grid = FALSE),
        htmlOutput("agresividad_seleccionada") # Espacio para mostrar el valor seleccionado
      )
    }
  })
  
  # 🔹 Mostrar el valor seleccionado dinámicamente
  output$agresividad_seleccionada <- renderUI({
    if (input$objetivo != "mant") {
      agresividad_texto <- switch(input$agresividad,
                                  "🔵 Muy baja" = "Cambio mínimo, sostenible a largo plazo (~250g/semana)",
                                  "🟢 Baja" = "Pérdida/ganancia lenta pero segura (~400g/semana)",
                                  "🟡 Moderada" = "Balance entre rapidez y sostenibilidad (~500g/semana)",
                                  "🟠 Alta" = "Cambio rápido, mayor riesgo de pérdida de músculo/ganancia de grasa (~600g/semana)",
                                  "🔴 Muy alta" = "Intenso, difícil de mantener (~650kg/semana)",
                                  "Desconocido")  # Valor por defecto si no coincide
      
      HTML(paste0("<p style='font-weight:bold; color: #007acc; text-align: center;'>", agresividad_texto, "</p>"))
    }
  })
  
  output$recomendacion_objetivo <- renderUI({
    # Verificar si el input de grasa es válido (no vacío y no NA)
    if (is.null(grasa_usuario()) || is.na(grasa_usuario()) || grasa_usuario() == "") {
      return(NULL)  # No mostrar nada si el campo está vacío o es inválido
    }
    
    if (input$objetivo == "vol" && ((input$sexo == "M" && grasa_usuario() > por_grasa_vol) || (input$sexo == "F" && grasa_usuario() > 25))) {
      HTML("<p style='color: orange; font-weight: bold;'>⚠️ Tu porcentaje de grasa es alto para un volumen. Con estos datos obtendras un peso objetivo menor al actual. <strong>Considera mejor una fase de definición.</strong> </p>")
    } else if (input$objetivo == "def" && ((input$sexo == "M" && grasa_usuario() < 10) || (input$sexo == "F" && grasa_usuario() < 16))) {
      HTML("<p style='color: red; font-weight: bold;'>⚠️ Definir con un % de grasa tan bajo puede ser riesgoso. Con estos datos obtendras un peso objetivo mayor al actual. <strong>¿Has pensado en hacer volumen en su lugar?</strong></p>")
    } else {
      NULL  # No mostrar nada si la condición no se cumple
    }
  })
  
  
  datos <- eventReactive(input$calcular, {
    
    peso <- input$peso
    altura <- input$altura / 100
    edad <- input$edad
    sexo <- input$sexo
    grasa <- grasa_usuario()
    actividad <- as.numeric(input$actividad)
    objetivo <- input$objetivo
    incluir_tef <- input$incluir_tef
    nivel <- input$nivel
    
    masa_magra <- peso * (1 - grasa / 100)
    
    TMB <- ifelse(sexo == "M", 
                  (10 * peso) + (6.25 * altura * 100) - (5 * edad) + 5, 
                  (10 * peso) + (6.25 * altura * 100) - (5 * edad) - 161)
    
    GET <- TMB * as.numeric(actividad) * ifelse(incluir_tef, 1.1, 1)
    
    # **Mapear texto del slider a valores numéricos**
    niveles_agresividad <- c("🔵 Muy baja" = 0.25, 
                             "🟢 Baja" = 0.4,      
                             "🟡 Moderada" = 0.6,  
                             "🟠 Alta" = 0.8,      
                             "🔴 Muy alta" = 1)    
    agresividad <- niveles_agresividad[input$agresividad]
    
    IMC <- peso / (altura^2)
    categoriaIMC <- ifelse(IMC < 18.5, "Bajo peso", 
                           ifelse(IMC < 24.9, "Peso normal", 
                                  ifelse(IMC < 29.9, "Sobrepeso", 
                                         ifelse(IMC < 34.9, "Obesidad I", "Obesidad II o mayor"))))
    
    # CALCULO PESO OBJETIVO
    
    # Factores de ajuste según agresividad
    ajuste_agresividad_vol <- case_when(
      agresividad <= 0.25 ~ 0.8,  # Muy baja agresividad reduce la ganancia
      agresividad <= 0.4  ~ 0.9,  
      agresividad <= 0.6  ~ 1.0,  # Moderada agresividad es la base
      agresividad <= 0.8  ~ 1.1,  
      TRUE               ~ 1.2   # Muy alta agresividad incrementa más la ganancia
    )
    
    # Factores de ajuste según agresividad (mayor agresividad en definición reduce la posibilidad de ganancia)
    ajuste_agresividad_def <- case_when(
      agresividad <= 0.25 ~ 1.0,   # Muy baja agresividad favorece recomposición
      agresividad <= 0.4  ~ 0.9,   
      agresividad <= 0.6  ~ 0.8,   # Moderada agresividad reduce más la ganancia
      agresividad <= 0.8  ~ 0.6,   
      TRUE               ~ 0.4    # Muy alta agresividad prácticamente elimina ganancia muscular
    )
    
    # Factores de ajuste según nivel de actividad en volumen
    ajuste_actividad_vol <- case_when(
      actividad <= 1.35 ~ 0.85,   # Actividad baja reduce la ganancia muscular
      actividad <= 1.55 ~ 1.0,   # Actividad moderada es la base
      actividad <= 1.75 ~ 1.1,  # Alta actividad favorece un poco más
      TRUE             ~ 1.15     # Muy alta actividad favorece más aún (dentro de lo razonable)
    )
    
    # Ajuste por actividad definición (mayor actividad favorece recomposición)
    ajuste_actividad_def <- case_when(
      actividad <= 1.35 ~ 0.9,   
      actividad <= 1.55 ~ 1.0,   
      actividad <= 1.75 ~ 1.05,  
      TRUE             ~ 1.1     
    )
    
    # Ganancia muscular en volumen
    ganancia_mensual_base_vol <- case_when(
      nivel == "principiante" ~ 1.0,  # Valor promedio entre 0.8 y 1.2
      nivel == "intermedio"   ~ 0.6,  # Promedio entre 0.4 y 0.8
      nivel == "avanzado"     ~ 0.3   # Promedio entre 0.2 y 0.4
    )
    
    # Ganancia muscular en definición (mucho menor)
    ganancia_mensual_base_def <- case_when(
      nivel == "principiante" ~ 0.35,   # Promedio entre 0.2 y 0.5 kg/mes
      nivel == "intermedio"   ~ 0.15,   # Promedio entre 0.1 y 0.2 kg/mes
      nivel == "avanzado"     ~ 0.05    # Promedio entre 0 y 0.1 kg/mes
    )
    
    # Ganancia muscular semanal ajustada (para volumen y definición en kg por semana)
    ganancia_muscular_semanal <- ifelse(objetivo == "vol",
                                        (ganancia_mensual_base_vol / 4) * ajuste_agresividad_vol * ajuste_actividad_vol,
                                        (ganancia_mensual_base_def / 4) * ajuste_agresividad_def * ajuste_actividad_def)
    
    # Función para calcular el multiplicador de ganancia muscular según la edad

    # Calcular el multiplicador de ganancia muscular basado en la edad usando ifelse
    multiplicador_ganancia_muscular <- ifelse(edad >= 18 & edad <= 30, 1.0,
                            ifelse(edad > 30 & edad <= 40, 1.0 - ((edad - 30) * 0.02),
                                   ifelse(edad > 40 & edad <= 50, 0.9 - ((edad - 40) * 0.02),
                                          ifelse(edad > 50 & edad <= 60, 0.7 - ((edad - 50) * 0.02),
                                                 ifelse(edad > 60, pmax(0.5 - ((edad - 60) * 0.01), 0.3), 
                                                        0.8)))))  # 0.8 para menores de 18 años
    
    ganancia_muscular_semanal <- ganancia_muscular_semanal * ifelse(input$sexo == "M", 1, 0.8) * multiplicador_ganancia_muscular
    
    # Duración base según nivel y objetivo
    duracion_base <- case_when(
      objetivo == "vol" & nivel == "principiante" ~ 16,  # 4 meses para principiantes en volumen
      objetivo == "vol" & nivel == "intermedio"   ~ 14,  # 3.5 meses
      objetivo == "vol" & nivel == "avanzado"     ~ 12,  # 3 meses
      
      objetivo == "def" & nivel == "principiante" ~ 10,  # 10 semanas para definición principiantes
      objetivo == "def" & nivel == "intermedio"   ~ 9,   # 9 semanas
      objetivo == "def" & nivel == "avanzado"     ~ 8    # 8 semanas
    )
    
    # Extensión de la duración del volumen si el % de grasa es bajo
    if (objetivo == "vol" && ((sexo == "M" && grasa < 12) || (sexo == "F" && grasa < 20))) {
      duracion_base <- duracion_base + 2  # Aumentar en 2 semanas si la grasa es baja
    }
    
    # Ajuste final de la duración del volumen
    
    # Asignar la duración mínima de semanas según el nivel y el objetivo
    semanas_necesarias_min <- ifelse(
      objetivo == "mant", 0,  # Mantenimiento no requiere ajuste, por lo tanto 0 semanas
      ifelse(objetivo == "def", 
             ifelse(nivel == "principiante", 10,  # Definición para principiantes: 10 semanas
                    ifelse(nivel == "intermedio", 9,  # Definición para intermedios: 9 semanas
                           8)),  # Definición para avanzados: 8 semanas
             ifelse(nivel == "principiante", 16,  # Volumen para principiantes: 16 semanas
                    ifelse(nivel == "intermedio", 14,  # Volumen para intermedios: 14 semanas
                           12))  # Volumen para avanzados: 12 semanas
      )
    )
    
    
    # Ganancia total de masa magra esperada durante el proceso
    incremento_masa_magra <- ganancia_muscular_semanal * semanas_necesarias_min
    
    # Cálculo del peso objetivo ajustado
    peso_objetivo <- ifelse(
      (objetivo == "def" && ((sexo == "M" && grasa < 10) || (sexo == "F" && grasa < 16))) ||
        (objetivo == "vol" && ((sexo == "M" && grasa > por_grasa_vol) || (sexo == "F" && grasa > 25))),
      peso,  # Si el % graso está fuera de rango para el objetivo, el peso objetivo será el peso actual
      ifelse(objetivo == "def", 
             ifelse(sexo == "M", (masa_magra + incremento_masa_magra) / (1 - 0.1),  
                    (masa_magra + incremento_masa_magra) / (1 - 0.16)),  
             ifelse(objetivo == "vol", 
                    ifelse(sexo == "M", (masa_magra + incremento_masa_magra) / (1 - 0.01*por_grasa_vol),  
                           (masa_magra + incremento_masa_magra) / (1 - 0.25)),  
                    ifelse(sexo == "M", masa_magra / (1 - 0.14),  
                           masa_magra / (1 - 0.22))))
    )
    
    # CÁLCULO DE LAS CALORÍAS
    
    calorias_recomendadas <- calcular_calorias(objetivo, nivel, agresividad, GET)
    
    # CÁLCULO DE LAS SEMANAS NECESARIAS
    
    
      semanas_necesarias <- ifelse(objetivo != "mant", calcular_semanas_necesarias(
        objetivo = objetivo,
        nivel = nivel,
        agresividad = agresividad,
        peso = peso,
        peso_objetivo = peso_objetivo,
        calorias_recomendadas = calorias_recomendadas,
        grasa = grasa,
        sexo = sexo,
        semanas_necesarias_min = semanas_necesarias_min,
        GET = GET  # Asegúrate de pasar el valor de GET aquí
      ), semanas_necesarias_min)
    
    # CÁLCULO MACRONUTRIENTES
    
    # Llamar a la función para obtener los macronutrientes
    resultados_macros <- macros(peso = peso, 
                                peso_objetivo = peso_objetivo, 
                                calorias_recomendadas = calorias_recomendadas, 
                                objetivo = objetivo)
    
    # Asignar los resultados a variables individuales si lo necesitas
    proteinas <- resultados_macros$proteinas
    grasas <- resultados_macros$grasas
    carbohidratos <- resultados_macros$carbohidratos
    
    # MENSAJE RESUMEN
    
    # Inicializar advertencia y ajuste de calorías
    advertencia_semanas <- ""
    nuevas_calorias <- calorias_recomendadas
    
    # Verificación si las semanas calculadas son menores que el mínimo necesario
    if (semanas_necesarias < semanas_necesarias_min && objetivo != "mant") {
      
      # Calcular la diferencia de semanas
      diferencia_semanas <- semanas_necesarias_min - semanas_necesarias
      
      # Ajuste calórico por semana basado en la diferencia, con un máximo de 6 semanas
      ajuste_agresividad <- case_when(
        agresividad == 0.25 ~ 30,   # 🔵 Muy baja agresividad (más ajuste: añadir más calorías en definición, quitar más en volumen)
        agresividad == 0.4  ~ 15,   # 🟢 Baja agresividad
        agresividad == 0.6  ~ 0,    # 🟡 Moderada agresividad (base)
        agresividad == 0.8  ~ -15,  # 🟠 Alta agresividad (menos ajuste)
        agresividad == 1    ~ -30   # 🔴 Muy alta agresividad (mínimo ajuste)
      )
      
      # Ajuste final por semana
      ajuste_por_dia <- 50 + (pmin(pmax(diferencia_semanas - 1, 0), 5) * 30) + ajuste_agresividad
      
      # Ajuste calórico total basado en la diferencia de semanas
      # ajuste_total_calorias <- diferencia_semanas * ajuste_por_semana
      
      # Elegir el adjetivo en función de la magnitud de la diferencia
      adjetivo <- ifelse(abs(semanas_necesarias_min - semanas_necesarias) >= 3, "bastante", "algo")
      
      # Actualizar las calorías recomendadas según el objetivo
      if (objetivo == "def") {
        # REDUCIR EL DÉFICIT para alargar el tiempo de definición
        nuevas_calorias <- calorias_recomendadas + ajuste_por_dia
        
        # Recalcular los macros usando las nuevas calorías para definición
        nuevos_macros_def <- macros(peso, peso_objetivo, nuevas_calorias, objetivo)
        
        # Añadir advertencia al final del resumen
        advertencia_semanas <- paste0(
          "<p style='color: #FF9900; font-weight: bold;'>⚠️ Esta definición no es muy recomendable, ya que es ", adjetivo, " agresiva. ",
          "Se recomienda una etapa de definición de mínimo <strong>", semanas_necesarias_min, 
          " semanas</strong> para un nivel <strong>", 
          ifelse(nivel == "principiante", "principiante",
                 ifelse(nivel == "intermedio", "intermedio", "avanzado")), 
          "</strong>, donde se reduzcan <strong>", round(abs(nuevas_calorias - GET), 0), 
          " kcal/día</strong>, resultando en un consumo total de <strong>", 
          round(nuevas_calorias, 0), " kcal/día</strong>. ",
          "Esto supondrá una pérdida de aproximadamente <strong>", round(abs(peso - peso_objetivo) / semanas_necesarias_min * 1000, 0), 
          " gramos por semana</strong>.</p>",
          
          "<p><strong>Distribución de macronutrientes recalculada:</strong></p>",
          "<ul>",
          "<li>💪 <strong>Proteínas:</strong> ", round(nuevos_macros_def$proteinas, 1), " g/día</li>",
          "<li>🥑 <strong>Grasas:</strong> ", round(nuevos_macros_def$grasas, 1), " g/día</li>",
          "<li>🍞 <strong>Carbohidratos:</strong> ", round(nuevos_macros_def$carbohidratos, 1), " g/día</li>",
          "</ul>"
        )
        
      } else if (objetivo == "vol") {
        # REDUCIR EL SUPERÁVIT para alargar el tiempo de volumen
        nuevas_calorias <- calorias_recomendadas - ajuste_por_dia
        
        # Recalcular los macros usando las nuevas calorías para volumen
        nuevos_macros_vol <- macros(peso, peso_objetivo, nuevas_calorias, objetivo)
        
        # Añadir advertencia al final del resumen
        advertencia_semanas <- paste0(
          "<p style='color: #FF9900; font-weight: bold;'>⚠️ Este volumen no es muy recomendable, ya que es ", adjetivo, " agresivo. ",
          "Se recomienda una etapa de volumen de mínimo <strong>", semanas_necesarias_min, 
          " semanas</strong> para un nivel <strong>", 
          ifelse(nivel == "principiante", "principiante",
                 ifelse(nivel == "intermedio", "intermedio", "avanzado")), 
          "</strong>, donde se aumenten <strong>", round(abs(nuevas_calorias - GET), 0), 
          " kcal/día</strong>, resultando en un consumo total de <strong>", 
          round(nuevas_calorias, 0), " kcal/día</strong>. ",
          "Esto supondrá una ganancia de aproximadamente <strong>", round(abs(peso - peso_objetivo) / semanas_necesarias_min * 1000, 0), 
          " gramos por semana</strong>.</p>",
          
          "<p><strong>Distribución de macronutrientes recalculada:</strong></p>",
          "<ul>",
          "<li>💪 <strong>Proteínas:</strong> ", round(nuevos_macros_vol$proteinas, 1), " g/día</li>",
          "<li>🥑 <strong>Grasas:</strong> ", round(nuevos_macros_vol$grasas, 1), " g/día</li>",
          "<li>🍞 <strong>Carbohidratos:</strong> ", round(nuevos_macros_vol$carbohidratos, 1), " g/día</li>",
          "</ul>"
        )
      }
    }
    
    # Determinar si se reducen o aumentan calorías
    ajuste_calorico <- round(abs(calorias_recomendadas - GET), 0)
    tipo_ajuste_calorico <- ifelse(objetivo == "def", "reducir", "aumentar")
    
    # Convertir agresividad en un nombre legible
    agresividad_limpia <- ifelse(objetivo == "mant", "No aplica",
                                 switch(
                                   as.character(agresividad),
                                   "0.25" = "muy baja",
                                   "0.4"  = "baja",
                                   "0.6"  = "moderada",
                                   "0.8"  = "alta",
                                   "1"    = "muy alta",
                                   "Desconocida" # Valor por defecto si no hay coincidencia
                                 )
    )
    
    # Si sigue siendo NA, asignar un valor por defecto
    if (is.na(agresividad_limpia)) {
      agresividad_limpia <- "desconocida"
    }
    
    
    # Evaluar si se recomienda volumen o definición
    recomendacion <- if ((sexo == "M" && grasa >= 16.5) || (sexo == "F" && grasa >= 23.5)) {
      "<p>📉 <strong>Recomendación:</strong> dado tu porcentaje de grasa actual, podrías considerar una fase de <strong>definición</strong> para reducir grasa corporal antes de continuar en mantenimiento o iniciar un volumen.</p>"
    } else if ((sexo == "M" && grasa <= 12) || (sexo == "F" && grasa <= 20)) {
      "<p>📈 <strong>Recomendación:</strong> tu porcentaje de grasa es bajo. Podrías considerar una fase de <strong>volumen</strong> para ganar masa muscular antes de mantener.</p>"
    } else {
      "<p>✅ <strong>Recomendación:</strong> estás en un buen rango de grasa corporal. El mantenimiento es una excelente opción para mejorar composición corporal y rendimiento. 
  Sin embargo, si tu objetivo principal es ganar masa muscular, también puedes optar por iniciar una fase de <strong>volumen</strong> con un superávit calórico controlado.</p>"
    }
    
    
    # Construcción del resumen según objetivo
    
    # Calcular el incremento base
    incremento_masa_magra_base <- ganancia_muscular_semanal * semanas_necesarias
    
    # Diferencia de semanas respecto al mínimo recomendado
    diferencia_semanas <- semanas_necesarias - semanas_necesarias_min
    
    # Ponderación basada en la diferencia de semanas
    incremento_masa_magra_total <- ifelse(
      diferencia_semanas > 0,
      incremento_masa_magra_base + (ganancia_muscular_semanal * diferencia_semanas * 0.3),  # 30% extra por semana adicional
      incremento_masa_magra_base * (1 + diferencia_semanas / semanas_necesarias_min * 0.5)  # Reducción proporcional hasta 50%
    )
    
    # Asegurarse de que el incremento no sea negativo
    incremento_masa_magra_total <- pmax(0, incremento_masa_magra_total)
    
    if (objetivo == "mant") {
      resumen <- paste0(
        "<h3 style='color: #007acc; font-weight: bold;'>📋 Resumen del plan</h3>",
        # Mostrar estimación de % grasa solo si el usuario no la conoce
        if (!input$conoce_grasa) {
          paste0("<p>Tu <strong>% de grasa corporal estimado</strong> es de ", round(grasa_usuario(), 1), "%. </p>")
        } else {
          ""  # No mostrar nada si el usuario conoce su % de grasa
        },
        "<p>Tu objetivo es <strong>mantenimiento</strong>, es decir, conservar tu peso actual y composición corporal.</p>",
        "<p>Para lograrlo, debes consumir aproximadamente <strong>", round(calorias_recomendadas, 0),
        " kcal/día</strong>, ajustando según tu actividad y sensaciones.</p>",
        "<p>📌 <strong>Consejos:</strong> si notas cambios en el peso o energía, ajusta ligeramente las calorías (+/- 100-200 kcal/día).</p>",
        recomendacion  # Se agrega la recomendación personalizada
      )
    } else {
      
      # Definir grasa objetivo
      
        grasa_objetivo <- ifelse(
        objetivo == "def", 
        ifelse(sexo == "M", 10, 16),  
        ifelse(objetivo == "vol", 
               ifelse(sexo == "M", por_grasa_vol, 25),  
               ifelse(sexo == "M", 14, 22))
      )
      
      # Construcción del resumen según el objetivo
      resumen <- paste0(
        "<h3 style='color: #007acc; font-weight: bold;'>📋 Resumen del plan</h3>",
        # Mostrar estimación de % grasa solo si el usuario no la conoce
        if (!input$conoce_grasa) {
          paste0("<p>Tu <strong>% de grasa corporal estimado</strong> es de ", round(grasa_usuario(), 1), "%. </p>")
        } else {
          ""  # No mostrar nada si el usuario conoce su % de grasa
        },
        # Descripción general del objetivo
        "<p>Con una fase de <strong>", 
        ifelse(objetivo == "def", "definición", ifelse(objetivo == "vol", "volumen", "mantenimiento")),
        "</strong> y una agresividad <strong>", agresividad_limpia, "</strong>, buscaremos ",
        
        # Cambiar el orden: primero el ajuste de peso, luego el % de grasa
        tipo_ajuste_calorico, " un total de <strong>", round(abs(peso - peso_objetivo), 1), 
        " kg</strong> en aproximadamente <strong>", 
        ifelse(is.na(semanas_necesarias), "un tiempo indeterminado", paste0(round(semanas_necesarias, 1), " semanas")), 
        "</strong> ",
        # Añadir los gramos por semana entre paréntesis
        ifelse(!is.na(semanas_necesarias) & semanas_necesarias > 0, 
               paste0("(<strong>", round((abs(peso - peso_objetivo) * 1000) / semanas_necesarias, 0), " g/semana</strong>)"), 
               ""
        ),
        ", sin sobrepasar el <strong>", grasa_objetivo, "% de grasa corporal</strong>.",
        
        "<br><br>",
        
        # Calorías recomendadas y ajuste calórico
        "Para ello, se ajustarán las calorías <strong>", tipo_ajuste_calorico, " ", ajuste_calorico, 
        " kcal/día</strong>, resultando en un consumo total de <strong>",
        round(calorias_recomendadas, 0), " kcal/día</strong>.",
        
        "<br><br>",
        
        # Inclusión de la ganancia de masa muscular
        if (objetivo %in% c("def", "vol")) {
          paste0(
            "<p>💪 Se espera una ganancia de <strong>", round(incremento_masa_magra_total, 2), 
            " kg de masa muscular</strong> durante el proceso.</p>",
            
            # Explicación adicional si el objetivo es definición
            if (objetivo == "def") {
              "<p>📉 Dado que estás en una fase de definición, la ganancia de masa muscular será limitada debido al déficit calórico. Sin embargo, un enfoque adecuado en el entrenamiento de fuerza y una nutrición precisa pueden ayudar a preservar y, en algunos casos, incluso aumentar ligeramente la masa magra.</p>"
            } else {
              "<p>📈 En esta fase de volumen, el superávit calórico junto con un entrenamiento de fuerza adecuado permitirá una ganancia significativa de masa muscular.</p>"
            }
          )
        } else {
          "<p>🔄 Como tu objetivo es mantenimiento, no se espera un cambio significativo en la masa muscular.</p>"
        },
        
        advertencia_semanas  # Se agrega cualquier advertencia sobre la duración del proceso
      )
    }
    
    # Analisis final
    
    # Generar contenido dinámico según el objetivo
    analisis_plan <- if (input$objetivo == "def") {
      "<h4>📈 Análisis del plan nutricional (Definición):</h4>
    <ul>
      <li>✅ <strong>Proteínas:</strong> se ha aumentado el consumo de proteínas para preservar la masa muscular y optimizar la recuperación.</li>
      <li>✅ <strong>Grasas:</strong> se han ajustado para garantizar un adecuado funcionamiento hormonal sin comprometer el déficit calórico.</li>
      <li>✅ <strong>Carbohidratos:</strong> se han reducido estratégicamente para facilitar la pérdida de grasa, priorizando fuentes de calidad y fibra.</li>
    </ul>
    
    <h4>⚠️ Consideraciones finales:</h4>
    <ul>
      <li>📌 <strong>Prioriza el entrenamiento de fuerza:</strong> esto ayudará a minimizar la pérdida de músculo durante el déficit calórico.</li>
      <li>📌 <strong>Monitorea tu progreso:</strong> un ritmo de pérdida de 0.5-1% de tu peso corporal por semana es lo ideal para minimizar la pérdida de masa muscular.</li>
      <li>📌 <strong>No recortes demasiado las grasas:</strong> un nivel mínimo de grasas es crucial para la producción hormonal y el bienestar general.</li>
    </ul>"
    } else if (input$objetivo == "vol") {
      "<h4>📈 Análisis del Plan Nutricional (Volumen):</h4>
    <ul>
      <li>✅ <strong>Proteínas:</strong> se han establecido en un nivel óptimo para apoyar la síntesis muscular sin sobrecargar el metabolismo.</li>
      <li>✅ <strong>Grasas:</strong> se han mantenido en un rango moderado para optimizar la producción hormonal y evitar un exceso calórico.</li>
      <li>✅ <strong>Carbohidratos:</strong> se ha priorizado su consumo para maximizar el rendimiento y la recuperación en entrenamientos intensos.</li>
    </ul>
    
    <h4>⚠️ Consideraciones finales:</h4>
    <ul>
      <li>📌 <strong>Evita ganar peso demasiado rápido:</strong> un superávit calórico moderado (0.25-0.5% del peso corporal por semana) minimizará la ganancia de grasa.</li>
      <li>📌 <strong>Prioriza el entrenamiento de fuerza:</strong> asegúrate de estar progresando en cargas o repeticiones para optimizar la ganancia muscular.</li>
      <li>📌 <strong>No descuides la proteína:</strong> aunque los carbohidratos son clave en volumen, un adecuado consumo de proteínas sigue siendo esencial.</li>
    </ul>"
    } else {
      "<h4>📈 Análisis del plan nutricional (Mantenimiento):</h4>
    <ul>
      <li>✅ <strong>Proteínas:</strong> se han mantenido en un nivel adecuado para conservar la masa muscular sin necesidad de un exceso.</li>
      <li>✅ <strong>Grasas:</strong> se ha asegurado una ingesta óptima para la salud hormonal y el bienestar general.</li>
      <li>✅ <strong>Carbohidratos:</strong> se han distribuido para garantizar un nivel estable de energía y buen rendimiento físico.</li>
    </ul>
    
    <h4>⚠️ Consideraciones finales:</h4>
    <ul>
      <li>📌 <strong>Monitorea tu peso y medidas:</strong> pequeños ajustes de 100-200 kcal pueden ser necesarios para estabilizar el peso a largo plazo.</li>
      <li>📌 <strong>Escucha a tu cuerpo:</strong> el mantenimiento es el momento ideal para encontrar un equilibrio entre rendimiento, saciedad y bienestar.</li>
      <li>📌 <strong>Optimiza la calidad de los alimentos:</strong> sin necesidad de restricciones, priorizar alimentos densos en nutrientes mejorará la salud y composición corporal.</li>
    </ul>"
    }
    
    
    list(
      IMC = round(IMC, 2),
      categoriaIMC = categoriaIMC,
      TMB = round(TMB, 2),
      GET = round(GET, 2),
      peso_objetivo = round(peso_objetivo, 2),
      masa_magra = round(masa_magra, 2),
      
      # Calorías recomendadas con ajuste
      calorias_recomendadas = round(calorias_recomendadas, 2),
      
      # Macronutrientes
      proteinas = round(proteinas, 2),
      grasas = round(grasas, 2),
      carbohidratos = round(carbohidratos, 2),
      
      # Datos sobre la estrategia de cambio de peso
      semanas_necesarias = ifelse(is.na(semanas_necesarias), "Indeterminado", round(semanas_necesarias, 1)),
      ajuste_calorico = round(abs(calorias_recomendadas - GET), 0),
      tipo_ajuste_calorico = ifelse(objetivo == "def", "reduciendo", "aumentando"),
      # agresividad_limpia = names(niveles_agresividad)[which(niveles_agresividad == cambio_peso_por_semana)],
      
      # Mensajes de salida
      recomendacion = recomendacion,
      resumen = resumen,
      analisis_plan = analisis_plan
    )
  })
  observeEvent(input$reiniciar, {
    updateNumericInput(session, "peso", value = 70)  # Reinicia el peso
    updateNumericInput(session, "altura", value = 170)  # Reinicia la altura
    updateNumericInput(session, "edad", value = 20)  # Reinicia la edad
    
    updateSelectInput(session, "sexo", selected = "M")  # Reinicia el sexo
    
    # Reiniciar el switch para % de grasa corporal a "No"
    updateSwitchInput(session, "conoce_grasa", value = FALSE)
    
    # Reinicia el % de grasa corporal (solo si está visible)
    updateNumericInput(session, "grasa", value = 15)  
    
    # Reinicia el nivel de entrenamiento
    updateSelectInput(session, "nivel", selected = "principiante")
    
    # Reinicia el nivel de actividad (este depende del nivel seleccionado)
    updateSelectInput(session, "actividad", selected = if (input$nivel == "avanzado") 1.35 else 1.2)
    
    # Reinicia el objetivo a mantenimiento
    updateSelectInput(session, "objetivo", selected = "mant")
    
    # Reinicia la agresividad si no es mantenimiento
    updateSliderTextInput(session, "agresividad", selected = "🟡 Moderada")
    
    # Reinicia la opción de incluir termogénesis inducida por la dieta
    updateCheckboxInput(session, "incluir_tef", value = TRUE)
  })
  
  # Variable reactiva para controlar si se permite el cálculo
  calculo_permitido <- reactiveVal(FALSE)
  
  # Variable reactiva para almacenar advertencias
  advertencia_mostrada <- reactiveVal(NULL)
  
  observeEvent(input$calcular, {
    # Resetear la bandera antes de validar
    calculo_permitido(FALSE)
    
    # Lista para almacenar mensajes de error
    mensajes_error <- c()
    
    # Validar si algún campo está vacío o es NULL
    if (is.null(input$peso) | is.na(input$peso)) {
      mensajes_error <- c(mensajes_error, "<p style='color: red; font-weight: bold;'>⚠️ Error: debes ingresar un peso válido.</p>")
    }
    
    if (is.null(input$altura) | is.na(input$altura)) {
      mensajes_error <- c(mensajes_error, "<p style='color: red; font-weight: bold;'>⚠️ Error: debes ingresar una altura válida.</p>")
    }
    
    if (is.null(input$edad) | is.na(input$edad)) {
      mensajes_error <- c(mensajes_error, "<p style='color: red; font-weight: bold;'>⚠️ Error: debes ingresar una edad válida.</p>")
    }
    
    if (is.null(grasa_usuario()) | is.na(grasa_usuario())) {
      mensajes_error <- c(mensajes_error, "<p style='color: red; font-weight: bold;'>⚠️ Error: debes ingresar un porcentaje de grasa corporal válido.</p>")
    }
    
    # 🔹 Validaciones de rango de valores
    if (!is.null(grasa_usuario()) & !is.na(grasa_usuario()) & (grasa_usuario() < 5 | grasa_usuario() > 50)) {
      mensajes_error <- c(mensajes_error, "<p style='color: red; font-weight: bold;'>⚠️ Error: el porcentaje de grasa corporal debe estar entre 5% y 50%.</p>")
    }
    
    if (!is.null(input$peso) & !is.na(input$peso) & (input$peso < 30 | input$peso > 200)) {
      mensajes_error <- c(mensajes_error, "<p style='color: red; font-weight: bold;'>⚠️ Error: el peso debe estar entre 30 y 200 kg.</p>")
    }
    
    if (!is.null(input$altura) & !is.na(input$altura) & (input$altura < 100 | input$altura > 250)) {
      mensajes_error <- c(mensajes_error, "<p style='color: red; font-weight: bold;'>⚠️ Error: la altura debe estar entre 100 y 250 cm.</p>")
    }
    
    if (!is.null(input$edad) & !is.na(input$edad) & (input$edad < 14 | input$edad > 100)) {
      mensajes_error <- c(mensajes_error, "<p style='color: red; font-weight: bold;'>⚠️ Error: la edad debe estar entre 14 y 100 años.</p>")
    }
    
    # Si hay errores, mostrarlos y detener la ejecución
    if (length(mensajes_error) > 0) {
      showModal(modalDialog(
        title = "❌ Error en la entrada",
        HTML(paste(mensajes_error, collapse = "")),
        easyClose = TRUE,
        footer = NULL
      ))
      return()  # Detener ejecución y NO permitir cálculo
    }
    
    # Si no hay errores, permitir el cálculo
    calculo_permitido(TRUE)
    
    # ACTUALIZAR LA ADVERTENCIA SOLO CUANDO SE PRESIONE "CALCULAR"
    mensaje_advertencia <- NULL
    
    if (input$objetivo == "vol") {
      if ((input$sexo == "M" && grasa_usuario() > 16) || (input$sexo == "F" && grasa_usuario() > 25)) {
        mensaje_advertencia <- "<p style='color: orange; font-weight: bold;'>⚠️ Advertencia: NO se recomienda hacer volumen con un % de grasa alto. Considera una fase de definición antes.</p>"
      }
    }
    
    if (input$objetivo == "def") {
      if ((input$sexo == "M" && grasa_usuario() < 10) || (input$sexo == "F" && grasa_usuario() < 16)) {
        mensaje_advertencia <- "<p style='color: red; font-weight: bold;'>⚠️ Advertencia: NO es recomendable definir con un % de grasa demasiado bajo. Un volumen controlado podría ser mejor opción.</p>"
      }
    }
    
    # Guardamos la advertencia para que no desaparezca al cambiar valores
    advertencia_mostrada(mensaje_advertencia)
  })
  
  output$resultado <- renderUI({
    
    req(calculo_permitido())
    
    datos_res <- datos()
    
    # Mostrar advertencia antes de los resultados si existe
    HTML(paste(
      if (!is.null(advertencia_mostrada())) advertencia_mostrada() else "",
      
      "<h3 style='color: #007acc; font-weight: bold;'>📊 Resultados</h3>",
      
      # Tabla con estilo
      "<table style='width:100%; border-collapse: collapse;'>",
      
      # Fila 1: TMB y GET
      "<tr style='background-color: #E8F8E0;'>",
      "<td style='padding: 10px;'><strong>🔥 Tasa Metabólica Basal:</strong></td>",
      "<td style='padding: 10px;'>", datos_res$TMB, " kcal/día</td>",
      "<td style='padding: 10px;'><strong>⚡ Gasto Energético Total</strong></td>",
      "<td style='padding: 10px;'>", datos_res$GET, " kcal/día</td>",
      "</tr>",
      
      # Fila 2: Peso Objetivo e IMC
      "<tr style='background-color: #C2EABA;'>",
      "<td style='padding: 10px;'><strong>⚖️ Peso objetivo:</strong></td>",
      "<td style='padding: 10px;'>", datos_res$peso_objetivo, " kg</td>",
      "<td style='padding: 10px;'><strong>📏 IMC:</strong></td>",
      "<td style='padding: 10px;'>", datos_res$IMC, " - ", datos_res$categoriaIMC, "</td>",
      "</tr>",
      
      # Fila 3: Calorías diarias recomendadas
      "<tr style='background-color: #E8F8E0;'>",
      "<td style='padding: 10px;'><strong>🍽️ Calorías recomendadas:</strong></td>",
      "<td colspan='3' style='padding: 10px; text-align: center; font-weight: bold; color: #ff5733;'>",
      datos_res$calorias_recomendadas, " kcal/día</td>",
      "</tr>",
      
      "</table>",  # Cierre de la tabla
      
      "<h3 style='color: #007acc; font-weight: bold;'>🍎 Distribución de macronutrientes</h3>",
      
      # Tabla de macronutrientes
      "<table style='width:100%; border-collapse: collapse;'>",
      
      "<tr style='background-color: #ffcccb;'>",
      "<td style='padding: 10px;'><strong>💪 Proteínas:</strong></td>",
      "<td style='padding: 10px;'>", datos_res$proteinas, " g/día</td>",
      "</tr>",
      
      "<tr style='background-color: #ffe4b5;'>",
      "<td style='padding: 10px;'><strong>🥑 Grasas:</strong></td>",
      "<td style='padding: 10px;'>", datos_res$grasas, " g/día</td>",
      "</tr>",
      
      "<tr style='background-color: #d1ffb3;'>",
      "<td style='padding: 10px;'><strong>🍞 Carbohidratos:</strong></td>",
      "<td style='padding: 10px;'>", datos_res$carbohidratos, " g/día</td>",
      "</tr>",
      
      "</table>",  # Cierre de la tabla
      
      datos_res$resumen  # Sección de Resumen
    ))
    
  })
  
  
  output$graficoIMC <- renderPlot({
    
    req(calculo_permitido())
    
    IMC <- datos()$IMC
    
    df <- data.frame(
      IMC_range = factor(c("Bajo\npeso", "Peso\nnormal", "Exceso\nde peso", 
                           "Obesidad\ngrado I", "Obesidad\ngrado II", "Obesidad\ngrado III"), 
                         levels = c("Bajo\npeso", "Peso\nnormal", "Exceso\nde peso", 
                                    "Obesidad\ngrado I", "Obesidad\ngrado II", "Obesidad\ngrado III")),
      min_IMC = c(0, 18.5, 25, 30, 35, 40),  # Rango real de IMC
      max_IMC = c(18.5, 25, 30, 35, 40, 45), # Rango real de IMC
      x_position = c(1, 2, 3, 4, 5, 6),  # Posiciones fijas para que todas las barras tengan el mismo ancho
      color = c("#D6E4FF", "#A3DA88", "#FFD580", "#FFA500", "#FF8C00", "#DC143C"), # Colores correctos
      label_range = c("< 18", "18 - 25", "25 - 30", "30 - 35", "35 - 40", "40 - 45")
    )
    
    # Función para convertir el IMC real a la escala de x_position
    convertir_IMC_a_x <- function(IMC, df) {
      for (i in 1:nrow(df)) {
        if (IMC >= df$min_IMC[i] && IMC <= df$max_IMC[i]) {
          # Escalar proporcionalmente dentro del segmento correspondiente
          return(df$x_position[i] + ((IMC - df$min_IMC[i]) / (df$max_IMC[i] - df$min_IMC[i])) - 0.5)
        }
      }
      return(NA)  # Si el IMC está fuera del rango, devuelve NA
    }
    
    x_intercept_IMC <- convertir_IMC_a_x(IMC, df)
    
    ggplot(df, aes(xmin = x_position - 0.5, xmax = x_position + 0.5, ymin = 0, ymax = 1, fill = IMC_range)) +
      geom_rect(color = "black") +
      geom_text(aes(x = x_position, y = 0.6, label = IMC_range), size = 5, fontface = "bold", color = "black") +
      geom_text(aes(x = x_position, y = 0.2, label = label_range), size = 4.5, fontface = "bold", color = "black") +
      geom_vline(xintercept = x_intercept_IMC, color = "blue", size = 1.5) +  
      scale_fill_manual(values = df$color) +
      theme_minimal() +
      ggtitle(paste0("Tu clasificación de IMC: ", round(IMC, 2))) +  # Negrita en el título
      theme(
        plot.title = element_text(face = "bold", size = 14),  # Negrita en el título
        axis.title = element_blank(),  # **Elimina nombres de los ejes**
        axis.text.x = element_blank(),  # **Elimina los números fuera del gráfico**
        axis.ticks.x = element_blank(),  
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        legend.position = "none",
        plot.margin = margin(10, 10, 20, 10)
      )
  }, width = 500, bg = NA)
  
  
  output$explicacion <- renderUI({
    
    req(calculo_permitido())
    
    datos_res <- datos()  # Obtener los resultados actuales
    
    withMathJax(HTML(paste0("
    <h3 style='color: #007acc; font-weight: bold;'>📖 Explicación de los cálculos</h3>

    <h4>1️⃣💪 Masa magra</h4>
    <p>La masa magra se obtiene eliminando el porcentaje de grasa corporal del peso total:</p>
    $$ Masa\\ magra = Peso \\times \\left(1 - \\frac{Grasa}{100} \\right) $$
     <p>🧮 <strong>Cálculo con tus datos:</strong></p>
    $$ ", input$peso, " \\times \\left(1 - \\frac{", grasa_usuario(), "}{100} \\right) = ", datos_res$masa_magra, " \\text{ kg} $$

    <h4>2️⃣🔥 Tasa Metabólica Basal (TMB)</h4>
    <p>Se calcula usando la fórmula de Mifflin-St Jeor:</p>
    <table style='width:100%; border-collapse: collapse;'>
      <tr style='background-color: #52A668;'>
        <th>Género</th>
        <th style='text-align: center;'>Fórmula</th>
      </tr>
      <tr style='background-color: #C2EABA;'>
        <td style='text-align: center;'>Hombres</td>
        <td style='text-align: center;'>$$TMB = (10 \\times Peso) + (6.25 \\times Altura) - (5 \\times Edad) + 5$$</td>
      </tr>
      <tr style='background-color: #E8F8E0;'>
        <td style='text-align: center;'>Mujeres</td>
        <td style='text-align: center;'>$$TMB = (10 \\times Peso) + (6.25 \\times Altura) - (5 \\times Edad) - 161$$</td>
      </tr>
    </table>
    <p>🧮 <strong>Cálculo con tus datos:</strong></p>
    $$ TMB = (10 \\times ", input$peso, ") + (6.25 \\times ", input$altura, ") - (5 \\times ", input$edad, ") + ", ifelse(input$sexo == "M", "5", "-161"), " = ", datos_res$TMB, " \\text{ kcal/día} $$

    <h4>3️⃣⚡ Gasto Energético Total (GET)</h4>
    <p>El <strong>Gasto Energético Total (GET)</strong> representa la cantidad total de energía que tu cuerpo necesita diariamente. Incluye tres componentes clave:</p>
    <ul>
      <li>🔹 <strong>Tasa Metabólica Basal (TMB):</strong> es la energía mínima que tu cuerpo necesita para mantener funciones vitales en reposo, como respirar, bombear sangre y mantener la temperatura corporal.</li>
      <li>🔹 <strong>Actividad física:</strong> es la energía adicional que gastas realizando cualquier tipo de movimiento, desde caminar hasta entrenar intensamente. Se ajusta mediante un <strong>factor de actividad</strong> que depende de tu nivel de ejercicio.</li>
      <li>🔹 <strong>Termogénesis (TEF):</strong> es la energía que tu cuerpo gasta en la digestión, absorción y metabolismo de los alimentos. Este efecto suele representar un <strong>10% del GET</strong> y se incluye opcionalmente en el cálculo (solo se aplica si se ha seleccionado la opción).</li>
    </ul>
    <p>La fórmula para calcular el GET es:</p>
    
    $$ GET = TMB \\times Actividad \\times TEF $$

    <table style='width:100%; border-collapse: collapse;'>
      <tr style='background-color: #52A668; color: black;'>
        <th style='text-align: center;'>Nivel de actividad</th>
        <th style='text-align: center;'>Factor</th>
      </tr>
      <tr style='background-color: #C2EABA;'><td>🏡 Sedentario (poco o nada de ejercicio)</td><td>1.2</td></tr>
      <tr style='background-color: #E8F8E0;'><td>🚶 Ligera actividad (1-3 días/semana)</td><td>1.375</td></tr>
      <tr style='background-color: #C2EABA;'><td>🏃 Moderada actividad (3-5 días/semana)</td><td>1.55</td></tr>
      <tr style='background-color: #E8F8E0;'><td>💪 Alta actividad (6-7 días/semana)</td><td>1.725</td></tr>
      <tr style='background-color: #C2EABA;'><td>⚡ Muy alta actividad (entrenamientos dobles o trabajo físico duro)</td><td>1.9</td></tr>
    </table>
    <p>🧮 <strong>Cálculo con tus datos:</strong></p>
    $$ GET = ", datos_res$TMB, " \\times ", input$actividad, " \\times ", ifelse(input$incluir_tef, "1.1", "1"), " = ", datos_res$GET, " \\text{ kcal/día} $$

    <h4>4️⃣📏 Índice de Masa Corporal (IMC)</h4>
    <p> El <strong>Índice de Masa Corporal (IMC)</strong> es una medida que relaciona tu peso y altura para evaluar si estás en un rango saludable mediante la formula siguiente:</p>
    $$ IMC = \\frac{Peso}{Altura^2} $$
    <p>🧮 <strong>Cálculo con tus datos:</strong></p>
    $$ IMC = \\frac{", input$peso, "}{", (input$altura / 100)^2, "} = ", datos_res$IMC, " $$

    <h4>5️⃣🍎 Distribución de macronutrientes</h4>
    <p>La distribución de macronutrientes es esencial para alcanzar tus objetivos, ya que cada uno cumple funciones específicas en el cuerpo:</p>
    <ul>
      <li>💪 <strong>Proteínas:</strong> fundamentales para la reparación y crecimiento muscular, especialmente importantes durante fases de definición para preservar la masa magra.</li>
      <li>🥑 <strong>Grasas:</strong> cruciales para la salud hormonal y la absorción de vitaminas, además de proporcionar energía sostenida.</li>
      <li>🍞 <strong>Carbohidratos:</strong> principal fuente de energía, esenciales para mantener el rendimiento físico y la recuperación.</li>
    </ul>
    
    <table style='width:100%; border-collapse: collapse;'>
      <tr style='background-color: #52A668; color: black;'>
        <th>Macronutriente</th>
        <th>Definición</th>
        <th>Mantenimiento</th>
        <th>Volumen</th>
      </tr>
      <tr style='background-color: #C2EABA;'>
        <td>Proteínas</td>
        <td>2.2 g/kg de peso</td>
        <td>1.8 g/kg de peso</td>
        <td>1.8 g/kg de peso</td>
      </tr>
      <tr style='background-color: #E8F8E0;'>
        <td>Grasas</td>
        <td>0.9 g/kg de peso</td>
        <td>1.0 g/kg de peso</td>
        <td>1.15 g/kg de peso</td>
      </tr>
    </table>

    
    <h5>🔹💪 Cálculo de proteínas</h5>
    <p>Las proteínas se ajustan según el objetivo para asegurar la preservación o el desarrollo muscular:</p>
    $$ Proteínas = Peso \\times Factor $$
    <p>🧮 <strong>Cálculo con tus datos:</strong></p>
    $$ Proteínas = ", input$peso, " \\times ", ifelse(input$objetivo == "def", "2.2", "1.8"), " = ", datos_res$proteinas, " \\text{ g/día} $$
    
    <h5>🔹🥑 Cálculo de grasas</h5>
    <p>Las grasas se adaptan para mantener el equilibrio hormonal y apoyar funciones vitales:</p>
    $$ Grasas = Peso \\times Factor $$
    <p>🧮 <strong>Cálculo con tus datos:</strong></p>
    $$ Grasas = ", input$peso, " \\times ", ifelse(input$objetivo == "def", "0.9", ifelse(input$objetivo == "vol", "1.15", 1.0)), " = ", datos_res$grasas, " \\text{ g/día} $$
    
    <h5>🔹🍞 Cálculo de carbohidratos</h5>
    <p>Después de asignar las calorías a las <strong>proteínas</strong> (4 kcal/g) y a las <strong>grasas</strong> (9 kcal/g), los <strong>carbohidratos</strong> se calculan utilizando las calorías restantes, ya que aportan <strong>4 kcal/g</strong>.</p>
    
    <p>La fórmula completa para calcular los carbohidratos es:</p>
    $$ Carbohidratos = \\frac{Calorías\\ totales - (Proteínas \\times 4 + Grasas \\times 9)}{4} $$
    
    <h6>🧮 <strong>Cálculo con tus datos:</strong></h6>

    $$ 
    Carbohidratos = \\frac{2201.1 - (", datos_res$proteinas, " \\times 4 + ", datos_res$grasas, " \\times 9)}{4} = ", datos_res$carbohidratos, " \\text{ g/día}
    $$

    <h3>📌 Conclusión</h3>

    <p>Según los datos ingresados y el objetivo seleccionado (<strong>", 
                            ifelse(input$objetivo == "def", "Definición", 
                                   ifelse(input$objetivo == "vol", "Volumen", "Mantenimiento")), 
                            "</strong>), la distribución final de macronutrientes se ha calculado para optimizar tu rendimiento y composición corporal.</p>
    
    <h4>💪 Resumen de macronutrientes:</h4>
    <ul>
      <li>🔹 <strong>Proteínas:</strong> ", datos_res$proteinas, " g/día</li>
      <li>🔹 <strong>Grasas:</strong> ", datos_res$grasas, " g/día</li>
      <li>🔹 <strong>Carbohidratos:</strong> ", datos_res$carbohidratos, " g/día</li>
    </ul>", datos_res$analisis_plan, "<p>🎯 <strong>Revisión recomendada:</strong> ajusta la dieta según tu progreso cada <strong>2-4 semanas</strong> para optimizar resultados.</p>
    </ul>
  ")))
  })
  
  output$creditos <- renderUI({
    creditos_ui()
  })
  
}

shinyApp(ui, server)