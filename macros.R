
# macros.R

# Función para calcular macronutrientes
macros <- function(peso, peso_objetivo, calorias_recomendadas, objetivo) {
  
  # Cálculo de proteínas según el objetivo
  proteinas <- ifelse(objetivo == "def", 
                      peso * 2.2,  # Definición
                      ifelse(objetivo == "vol", 
                             peso * 1.8,  # Volumen
                             peso_objetivo * 1.8))  # Mantenimiento
  calorias_proteinas <- proteinas * 4  # Calorías de proteínas
  
  # Cálculo de grasas según el objetivo
  grasas <- ifelse(objetivo == "def", 
                   peso * 0.9,  # Definición
                   ifelse(objetivo == "vol", 
                          peso * 1.15,  # Volumen
                          peso_objetivo * 1))  # Mantenimiento
  calorias_grasas <- grasas * 9  # Calorías de grasas
  
  # Cálculo de carbohidratos (resto de calorías)
  calorias_carbohidratos <- calorias_recomendadas - (calorias_proteinas + calorias_grasas)
  carbohidratos <- calorias_carbohidratos / 4  # Carbohidratos (4 kcal por gramo)
  
  # Devolver los resultados en una lista
  return(list(proteinas = proteinas,
              grasas = grasas,
              carbohidratos = carbohidratos))
}