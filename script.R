library(gt)
library(htmltools)
library(ggplot2)
library(gapminder)
library(dplyr)
library(ggplot2)
library(gifski)
library(Rcpp)
library(dplyr)
library(gganimate)
library(remotes)
library(gtExtras)
library(googleVis)
library(readxl)
library(plotly)
library(xkcd)

setwd("C:/Users/Usuario/Downloads")
cor1<-read_excel("corrupcion.xlsx")
cor<-cor1[-c(11:14),]
cor<-cor[,-c(8)]
cor<-cor[,-c(1:2)]
names(cor) <- c("Manejo_irregular","corrup_pred&mil","Soborno","perd_indirecta","inv_directa")
fechas<-c("00s","10s","20s","30s","40s","50s","60s","70s","80s","90s")
fechas<-cbind(fechas)
cor2<-data.frame(fechas, cor)

## Limpieza de datos
cor$Manejo_irregular= as.numeric(cor$Manejo_irregular)
cor$`corrup_pred&mil`= as.numeric(cor$`corrup_pred&mil`)
cor$Soborno= as.numeric(cor$Soborno)
cor$perd_indirecta= as.numeric(cor$perd_indirecta)
cor$inv_directa= as.numeric(cor$inv_directa)


st <- as.Date("1990-1-1")
en <- as.Date("1900-1-1")
fech_o <- seq(en, st, by = "+10 year")


st1 <- as.Date("1999-12-31")
en1 <- as.Date("1909-12-31")
fech_f <- seq(en1, st1, by = "+10 year")

cor = data.frame(fech_o, fech_f, cor)

##Mostrar la tabla
cor_gt <- gt(cor)
cor_gt

#Poner porcentajes
plus_percent <- function(.x) {
  glue::glue("{.x}%")
}

cor_gt <- cor_gt %>%
  fmt("Manejo_irregular", fns = plus_percent) %>%
  fmt("corrup_pred.mil", fns = plus_percent) %>%
  fmt("Soborno", fns = plus_percent)%>%
  fmt("perd_indirecta", fns = plus_percent)%>%
  fmt("inv_directa", fns = plus_percent)
cor_gt

#Resaltar datos
cor_gt <- cor_gt %>%
  tab_style(
    style = cell_fill(color = "#F7EFB2"),
    locations = cells_body(
      rows = fech_o == "1990-01-01")
  )

cor_gt

#Sombrear
cor_gt <- cor_gt %>%
  tab_style(
    style = cell_text(size = px(15), weight = "bold", font = "arial"),
    locations = cells_body(vars(fech_o,`fech_f`, `Manejo_irregular`, `corrup_pred.mil`, `Soborno`, `perd_indirecta`, `inv_directa`))
  )

cor_gt

#Estilizar
cor_gt <- cor_gt %>%
  tab_options(
    column_labels.border.top.style = "none",
    table.border.top.style = "none",
    column_labels.border.bottom.style = "none",
    column_labels.border.bottom.width = 1,
    column_labels.border.bottom.color = "#334422",
    table_body.border.top.style = "none",
    table_body.border.bottom.color = "#0000001A",
    data_row.padding = px(7)
  )
cor_gt

#Cabeceta en mayuscula
cor_gt <- cor_gt %>%
  tab_style(
    style = cell_text(
      size = px(11),
      color = "#a6a6a6",
      font = "arial",
      transform = "uppercase"
    ),
    locations = cells_column_labels(everything())
  ) 

cor_gt

# Usando el semaforo Hulk 
head(cor) %>%
  gt::gt() %>%
  gt_hulk_col_numeric(	Manejo_irregular:inv_directa, trim = TRUE)


##Grafico dinamico de "manejo irregular"
dinamico<-cor %>%
  ggplot(aes(x= fech_f,
             y= Manejo_irregular, 
             color = Manejo_irregular)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  geom_line(size=2) +
  labs(title = 'Manejo_Irregular en {frame_along}',
       x = 'fecha_f',
       y = 'Manejo_irregular') +
  theme_minimal() +
  transition_reveal(fech_f)
dinamico

##Grafico de Barras
Column <- gvisColumnChart(cor2)
plot(Column)

##
Dashed <-  gvisLineChart(cor2, xvar="fechas", yvar=c("Soborno","corrup_pred.mil"),
                         options=list(
                           series="[{color:'green', targetAxisIndex: 0, 
                          lineWidth: 1, lineDashStyle: [2, 2, 20, 2, 20, 2]}, 
                          {color: 'blue',targetAxisIndex: 1, 
                          lineWidth: 2, lineDashStyle: [4, 1]}]",
                           vAxes="[{title:'Soborno'}, {title:'corrup_pred.mil'}]"
                         ))
plot(Dashed)

dinamico1<-cor %>%
  ggplot(aes(x= fech_f,
             y= inv_directa, 
             color = Manejo_irregular)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 5, stroke = 12) +
  labs(title = 'Inversion directa perdida {frame_along}',
       x = 'fecha_f',
       y = 'Ind_directa') +
  theme_minimal() +
  transition_reveal(fech_f)
dinamico1


 
 