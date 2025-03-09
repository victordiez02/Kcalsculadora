calcular_semanas_necesarias <- function(objetivo, nivel, agresividad, peso, peso_objetivo, 
                                        calorias_recomendadas, grasa, sexo, semanas_necesarias_min, GET) {
  
  # Calorías necesarias para cambiar 1 kg de peso corporal (~7700 kcal)
  kcal_por_kg <- 7700
  
  # Calcular el cambio de peso total (en kg)
  cambio_peso_total <- abs(peso_objetivo - peso)
  
  # Ajuste calórico diario (diferencia entre GET y calorías recomendadas)
  ajuste_diario_calorias <- abs(GET - calorias_recomendadas)
  
  # Ajuste de duración según la agresividad
  ajuste_duracion_agresividad <- ifelse(
    objetivo == "def",  # Para definición
    case_when(
      agresividad <= 0.25 ~ 1,  # Más conservador, ligeramente más tiempo
      agresividad <= 0.4  ~ 0.97,  # Neutro
      agresividad <= 0.6  ~ 0.95,  # Moderadamente agresivo
      agresividad <= 0.8  ~ 0.93,  # Más agresivo, menos tiempo
      TRUE               ~ 0.90   # Muy agresivo, reducción moderada del tiempo
    ),
    case_when(  # Para volumen
      agresividad <= 0.25 ~ 1.08,  # Conservador para evitar ganancia de grasa excesiva
      agresividad <= 0.4  ~ 1.05,
      agresividad <= 0.6  ~ 1.02,  # Neutro
      agresividad <= 0.8  ~ 0.98,  # Aumento más rápido
      TRUE               ~ 0.95   # Muy agresivo, ganancia rápida
    )
  )
  
  # Calorías totales necesarias para alcanzar el objetivo
  calorias_totales_necesarias <- cambio_peso_total * kcal_por_kg
  
  # Calcular semanas necesarias
  
  suavizar_semanas <- 1.5
  
  semanas_necesarias <- ifelse(
    ajuste_diario_calorias == 0,
    NA,  # Mantenimiento: no se necesita cambiar semanas
    (calorias_totales_necesarias / ajuste_diario_calorias / (7*suavizar_semanas)) * ajuste_duracion_agresividad
  )
  
  return(semanas_necesarias)
}
