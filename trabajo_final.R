library(tidyverse)
library(car)

#Grafico de dispersión
plot(peso_g~largo_mm,juveniles)

# Datos sin transformar
m_1<-lm(peso_g~largo_mm,juveniles)
abline(m_1)

# Gráfico de residuos vs. valores predichos:

plot(m_1, which = 1, add.smooth = FALSE, pch = 16)
# Supuestos: homocedasticidad? linealidad?

# QQplot del modelo preliminar:
plot(m_1, which = 2, pch = 16)

## Exploratorio: ¿transformación de Y? #######################
# Construyo "num_grupos" de intervalos

num_grupos <- 6
juveniles$grupo <- cut(juveniles$largo_mm,
                   quantile(juveniles$largo_mm, 
                            seq(from = 0, to = 1, by = 1/num_grupos)),
                   include.lowest = TRUE)

# Visualizo los grupos
plot(peso_g~largo_mm,juveniles, pch=16,
     col = juveniles$grupo)

summary(powerTransform(peso_g ~ grupo,juveniles)) #sugiere lambda=0

plot(log(peso_g)~largo_mm,juveniles)

m_2<-lm(log(peso_g)~largo_mm,juveniles)
abline(m_2)

plot(m_2, which = 1, add.smooth = FALSE, pch = 16)# Res. vs. Pred.
plot(m_2, which = 2, pch = 16)# QQplot

invTranEstimate(juveniles$largo_mm,log(juveniles$peso_g))
invTranPlot(juveniles$largo_mm,log(juveniles$peso_g)) #tambien sugiere log

#Posible modelo final: log(Y) vs log(X)

plot(log(peso_g)~log(largo_mm),juveniles)

# Ajuste modelo log(Y) vs. log(X)

m_3<-lm(log(peso_g)~log(largo_mm),juveniles)
abline(m_3)
summary(m_3)


## Gráfico con bandas (intervalos) de predicción del 95% #########

# Construyo tabla de datos auxiliar

grilla <- tibble(
  largo_mm = seq(from = min(juveniles$largo_mm), 
                   to = max(juveniles$largo_mm),
                   length = 200)  )
grilla

# Predicciones con intervalo de predicción (95%)

pred <- m_3 |>
  predict(newdata = grilla, interval = "prediction",
          level = 0.95) |>
  as_tibble()

#  Combino datos

datos_pred <- bind_cols(grilla, pred)
datos_pred

# Grafico con bandas de predicción del 95%

plot(log(peso_g) ~ log(largo_mm), juveniles, pch=16)
lines(fit ~ log(largo_mm), datos_pred)
lines(lwr ~ log(largo_mm), datos_pred, lty=3)
lines(upr ~ log(largo_mm), datos_pred, lty=3)


# Gráfico del modelo, retransformando a unidades originales #######

plot(peso_g ~ largo_mm, juveniles, pch=16)
lines(exp(fit) ~ largo_mm, datos_pred)
lines(exp(lwr) ~ largo_mm, datos_pred, lty=3)
lines(exp(upr) ~ largo_mm, datos_pred, lty=3)


## Presentación gráfica de modelo final usando ggpplot() ###################

## Preliminar: agrego residuos y predichos a los datos

juveniles$predicho <- fitted(m_3) # Valores predichos (ajustados, y_hat)
juveniles$residuo <- residuals(m_3) # Residuos "crudos" (y - y_hat)
juveniles$restandar <- rstandard(m_3) # Residuos estandarizados (para QQplot)

# Q-Q plot de los residuos
ggplot(juveniles) + aes(sample = restandar) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q plot de los residuos",
       x = "Cuantiles teóricos",
       y = "Cuantiles de los residuos") +
  theme_bw()


# Residuos vs. predichos
ggplot(juveniles) + 
  aes(predicho, residuo) +
  geom_hline(yintercept = 0, color = "grey30", linetype=3) +
  geom_point() +
  labs(x = "Predichos",
       y = "Residuos") +
  theme_bw()

# Diagrama de dispersión + ajuste lineal + bandas de pred. 95%
ggplot(juveniles) +
  aes(log(largo_mm), log(peso_g)) +
  geom_ribbon(aes(x=log(largo_mm), 
                  y = NULL,
                  ymin = lwr,
                  ymax = upr), 
              data = datos_pred, alpha = 0.5, fill = "lightblue") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  theme_bw() +
  labs(x = "ln ( Peso total (g) )",
       y = "ln ( Peso del hígado (g) )")

# Gráfico retransformado a unidades originales

ggplot(datos_pred) +
  geom_ribbon(aes(x=largo_mm,
                  ymin= exp(lwr),
                  ymax= exp(upr)),
              alpha = 0.5, fill = "lightblue") +
  geom_line(aes(largo_mm, exp(fit)),
            color = "blue", linewidth=1) +
  geom_point(aes(largo_mm, peso_g), data = juveniles) +
  theme_bw() +
  labs(x = "Largo (mm)",
       y = "Peso  (g)")
