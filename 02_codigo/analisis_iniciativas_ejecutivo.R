### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/paquetes_setup_tema.R")

### Importar datos ----
bd_iniciativas <- 
  read_excel("01_datos/bd_iniciativas_primeros_14_meses.xlsx",
             col_types = c("text", "text", "text", 
                           "text", "text", "text", "text", "text", 
                           "date", "text", "text", "text", "text", 
                           "text")) %>% 
  clean_names()


# Base de datos construida con información de http://sil.gobernacion.gob.mx/portal

### Renombrar variables ----
bd_iniciativas <- 
  bd_iniciativas %>% 
  rename(estatus_primer_año = estatus_al_final_del_primer_ano_de_la_administracion,
         estatus_mes_14 = estatus_al_final_del_mes_14_de_la_administracion)

### Transformar el tipo de la variable fecha_de_presentacion para que sea date ----
bd_iniciativas <- 
  bd_iniciativas %>% 
  mutate(fecha_de_presentacion = dmy(fecha_de_presentacion)) 

### Generar variables para definir estatus simplificado de las iniciativas al (i) año y (ii) mes 14 ----
bd_iniciativas <- 
  bd_iniciativas %>% 
  mutate(estatus_al_año = case_when(str_detect(estatus_primer_año, "Publicado") ~ "Publicado en el DOF",
                                    str_detect(estatus_primer_año, "Pendiente") ~ "Pendiente",
                                    str_detect(estatus_primer_año, "Devuelto") ~ "Pendiente",
                                    TRUE ~ estatus_primer_año),
         estatus_al_año = fct_relevel(estatus_al_año, "Publicado en el DOF", "Aprobado, por publicar en el DOF", "Pendiente", "Desechado"),
         estatus_al_mes_14 = case_when(str_detect(estatus_mes_14, "Publicado") ~ "Publicado en el DOF",
                                       str_detect(estatus_mes_14, "Pendiente") ~ "Pendiente",
                                       str_detect(estatus_mes_14, "Devuelto") ~ "Pendiente",
                                       TRUE ~ estatus_mes_14),
         estatus_al_mes_14 = fct_relevel(estatus_al_mes_14, "Publicado en el DOF", "Aprobado, por publicar en el DOF", "Pendiente", "Desechado"))


### Generar versión corta del nombre de cada presidente y reordenar niveles tanto de presidente_corto como de presidente ----
bd_iniciativas <- 
  bd_iniciativas %>% 
  mutate(presidente_corto = case_when(presidente == "Vicente Fox Quezada" ~ "Fox",
                                      presidente == "Felipe Calderón Hinojosa" ~ "Calderón", 
                                      presidente == "Enrique Peña Nieto" ~ "Peña Nieto",
                                      presidente == "Andrés Manuel López Obrador" ~ "López Obrador"), 
         presidente_corto = fct_relevel(presidente_corto, "Fox", "Calderón", "Peña Nieto", "López Obrador"),
         presidente = fct_relevel(presidente, "Vicente Fox Quezada", "Felipe Calderón Hinojosa", "Enrique Peña Nieto", "Andrés Manuel López Obrador"))

### Reordenar niveles de subclasificacion ----
bd_iniciativas <- 
  bd_iniciativas %>% 
  mutate(subclasificacion = fct_relevel(subclasificacion, "Reforma Constitucional", "Ley Secundaria"))


### Gráfica del número de iniciativas presentadas por cada presidente en los primeros 14 meses de gobierno ----

# Datos
bd_iniciativas %>% 
  group_by(presidente_corto, subclasificacion) %>% 
  summarise(numero = n()) %>% 
  ungroup() %>% 
  group_by(presidente_corto) %>% 
  mutate(total = sum(numero)) %>% 
  ungroup()

# Gráfica
bd_iniciativas %>% 
  group_by(presidente_corto, subclasificacion) %>% 
  summarise(numero = n()) %>% 
  ungroup() %>% 
  group_by(presidente_corto) %>% 
  mutate(total = sum(numero),
         presidente_corto_grafica = str_c(presidente_corto, "\n (", total, ")")) %>% 
  ungroup() %>%
  mutate(presidente_corto_grafica = fct_relevel(presidente_corto_grafica, "Fox\n (46)", "Calderón\n (30)", "Peña Nieto\n (41)", "López Obrador\n (16)")) %>% 
  ggplot(aes(x = presidente_corto_grafica, y = numero, fill = subclasificacion)) +
  geom_col() +
  geom_hline(yintercept = seq(5, 40, 5), color = "white", linetype = 3) +
  
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 45, 5), limits = c(-1, 48), expand = c(0, 0)) +
  scale_fill_manual(values = c("salmon", "steelblue")) +
  labs(title = str_wrap("Número de iniciativas de reformas constitucionales y a leyes secundarias presentadas por los últimos cuatro presidentes de México en los primeros 14 meses de su gobierno", width = 73),
       x = NULL,
       y = "Número\n",
       caption = "\n   @segasi / Fuente: Sistema de Información Legislativa de SEGOB.\n\n   Datos al 15 de febrero de 2002, 2008, 2014 y 2020, respectivamente.",
       fill = NULL) +
  tema +
  theme(panel.grid = element_blank(),
        legend.position = c(0.885, 0.9), 
        legend.text = element_text(size = 16)) +
  ggsave("03_graficas/numero_iniciativas_primeros_14_meses_por_tipo.png", width = 14.5, height = 9, dpi = 200)

### Gráfica del estatus de las iniciativas de reforma presentadas por los últimos cuatro presidentes al final de los primeros 14 meses de su gobierno ----

# Datos de iniciativas de leyes secundarias
bd_iniciativas %>% 
  count(presidente_corto, subclasificacion, estatus_al_mes_14) %>% 
  group_by(presidente_corto, subclasificacion) %>% 
  mutate(total = sum(n),
         porcentaje = (n/total)*100) %>% 
  ungroup() %>% 
  filter(subclasificacion  == "Ley Secundaria")

# Datos de iniciativas de reforma constitucional
bd_iniciativas %>% 
  count(presidente_corto, subclasificacion, estatus_al_mes_14) %>% 
  group_by(presidente_corto, subclasificacion) %>% 
  mutate(total = sum(n),
         porcentaje = (n/total)*100) %>% 
  ungroup() %>% 
  filter(subclasificacion  == "Reforma Constitucional")


# Gráfica
g_numero <- 
  bd_iniciativas %>% 
  count(presidente_corto, estatus_al_mes_14, subclasificacion) %>% 
  group_by(presidente_corto, subclasificacion) %>% 
  mutate(total = sum(n),
         porcentaje = (n/total)*100) %>% 
  ungroup() %>% 
  ggplot(aes(x= presidente_corto, y = n, fill = estatus_al_mes_14)) +
  geom_col() +
  geom_hline(yintercept = seq(10, 40, 10), linetype = 3, color = "white") +
  facet_wrap(~ subclasificacion) +
  # scale_y_continuous(expand = c(0, 0), limits = c(-1, 105), breaks = seq(0, 100, 10)) +
  scale_fill_manual(values = c("#1a9850", "#d9ef8b", "#d73027", "grey90")) +
  labs(title = str_wrap("Estatus de las iniciativas de reformas constitucionales y a leyes secundarias presentadas por los últimos cuatro presidentes de México, al final de los primeros 14 meses de su gobierno", width = 77),
       x = NULL,
       y = "Número\n",
       caption = NULL,
       fill = NULL) +
  tema +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(angle = 90),
        strip.background = element_rect(fill = "grey60", color = "grey60"),
        strip.text = element_text(size = 20, color = "white"), 
        legend.position = "none",
        # legend.position = c(0.20, 0.85)
  ) 


g_porcentaje <- 
  bd_iniciativas %>% 
  count(presidente_corto, estatus_al_mes_14, subclasificacion) %>% 
  group_by(presidente_corto, subclasificacion) %>% 
  mutate(total = sum(n),
         porcentaje = (n/total)*100) %>% 
  ungroup() %>% 
  ggplot(aes(x= presidente_corto, y = porcentaje, fill = estatus_al_mes_14)) +
  geom_col() +
  geom_hline(yintercept = seq(20, 80, 20), linetype = 3, color = "white") +
  facet_wrap(~ subclasificacion) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1, 105), breaks = seq(0, 100, 20)) +
  scale_fill_manual(values = c("#1a9850", "#d9ef8b", "#d73027", "grey90")) +
  labs(title = NULL,
       x = NULL,
       y = "Porcentaje\n",
       caption = "\n   @segasi / Fuente: Sistema de Información Legislativa de SEGOB.\n\n   Datos al 15 de febrero de 2002, 2008, 2014 y 2020, respectivamente.",
       fill = NULL) +
  tema +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(angle = 90),
        strip.background = element_rect(fill = "grey60", color = "grey60"),
        strip.text = element_text(size = 20, color = "white"), 
        legend.position = "bottom",
        # legend.position = c(0.20, 0.85)
  ) 


plot_grid(g_numero, g_porcentaje, ncol = 1) + 
  ggsave("03_graficas/numero_y_porcentaje_iniciativas_mes_14_por_estatus.png", width = 15, height = 12, dpi = 200)


