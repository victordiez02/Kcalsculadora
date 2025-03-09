
# Función para calcular las calorías recomendadas según el objetivo, nivel y agresividad
calcular_calorias <- function(objetivo, nivel, agresividad, GET) {
  
  # Si el objetivo es definición (déficit calórico)
  if (objetivo == "def") {
    # Déficit calórico recomendado según el nivel de experiencia
    deficit_min <- ifelse(nivel == "principiante", 250,
                          ifelse(nivel == "intermedio", 200, 100))  # Avanzado
    
    deficit_max <- ifelse(nivel == "principiante", 500,
                          ifelse(nivel == "intermedio", 400, 300))
    
    # Ajuste basado en la agresividad del usuario
    deficit_calorico <- deficit_min + (deficit_max - deficit_min) * (agresividad - 0.25) / (1 - 0.25)
    calorias_recomendadas <- GET - deficit_calorico
    
  } else if (objetivo == "vol") {
    # Superávit calórico recomendado según el nivel de experiencia
    superavit_min <- ifelse(nivel == "principiante", 250,
                            ifelse(nivel == "intermedio", 200, 100))  # Avanzado
    
    superavit_max <- ifelse(nivel == "principiante", 500,
                            ifelse(nivel == "intermedio", 400, 300))
    
    ajuste_agresividad <- case_when(
      agresividad <= 0.25 ~ 0,  # Muy baja agresividad reduce la ganancia
      agresividad <= 0.4  ~ 0.25,  
      agresividad <= 0.6  ~ 0.5,  # Moderada agresividad es la base
      agresividad <= 0.8  ~ 0.75,  
      TRUE               ~ 1   # Muy alta agresividad incrementa más la ganancia
    )
    
    # Ajuste basado en la agresividad del usuario
    superavit_calorico <- superavit_min + (superavit_max - superavit_min) * ajuste_agresividad
    calorias_recomendadas <- GET + superavit_calorico
    
  } else {
    # En caso de mantenimiento, no hay ajuste calórico
    calorias_recomendadas <- GET
  }
  
  return(calorias_recomendadas)
}
