### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/paquetes_setup_tema.R")

### Importar datos ----
art_ref <- 
  read_excel("01_datos/articulos_constitucion_reformados.xlsx") %>% 
  mutate(fecha_publicacion = as_date(fecha_publicacion))
