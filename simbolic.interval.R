library(spatgeom)
library(RSDA)
library(dplyr)
library(sf)
library(ggplot2)
library(readr)
library(plotly)
library(pracma)

#Simulación 1. Linear. 

set.seed(123)
x1.centers <- runif(1000, -2, 2)
x2.centers <- runif(1000,-2, 2)
x3.centers <- runif(1000, -2, 2)
y.centers <- 0.6*x1.centers + 0.3*x2.centers + 0.1*x3.centers 

x1.ranges <- runif(1000, -2, 2)
x2.ranges <- runif(1000, -2, 2)
x3.ranges <- runif(1000, -2, 2)
y.ranges <- 0.6*x1.ranges + 0.3*x2.ranges + 0.1*x3.ranges 

x1.lower <- x1.centers - x1.ranges
x1.upper <- x1.centers + x1.ranges

x2.lower <- x2.centers - x2.ranges
x2.upper <- x2.centers + x2.ranges

x3.lower <- x3.centers - x3.ranges
x3.upper <- x3.centers + x3.ranges

y.lower <- y.centers - y.ranges #regresión de centros + regresión de rangos
y.upper <- y.centers + y.ranges #regresión de centros - regresión de rangos

interval.data.test <- data.frame(x1.lower, x1.upper, x2.lower, x2.upper, x3.lower, x3.upper, y.lower, y.upper)

ggplot(interval.data.test, aes(x = x1.centers, y = y.centers ))+geom_point()+labs(title = "X1 vs. Y") 
ggplot(interval.data.test, aes(x = x1.ranges, y = y.ranges ))+geom_point()+labs(title = "X1 vs. Y") 

ggplot(interval.data.test, aes(x = x2.centers, y = y.centers ))+geom_point()+labs(title = "X2 vs. Y") 
ggplot(interval.data.test, aes(x = x2.ranges, y = y.ranges ))+geom_point()+labs(title = "X2 vs. Y") 

ggplot(interval.data.test, aes(x = x3.centers, y = y.centers ))+geom_point()+labs(title = "X3 vs. Y") 
ggplot(interval.data.test, aes(x = x3.ranges, y = y.ranges ))+geom_point()+labs(title = "X3 vs. Y") 

indice1 <- spatgeom(y=y.centers, x=data.frame(x1.centers, x2.centers, x3.centers)) 
plot_curve(indice1, type = "curve")
plot_curve(indice1, type = "deriv")

indice2 <- spatgeom(y=interval.data.test$y.lower, x=data.frame(x1.lower, x2.lower, x3.lower)) 
plot_curve(indice2, type = "curve")
plot_curve(indice2, type = "deriv")

indice3 <- spatgeom(y=interval.data.test$y.upper, x=data.frame(x1.upper, x2.upper, x3.upper)) 
plot_curve(indice3, type = "curve")
plot_curve(indice3, type = "deriv")

# Para la variable $x_{1}$.

geom_indicesc <- indice1$results[[1]]$geom_indices 
geom_indicesr <- indice2$results[[1]]$geom_indices 

geom_indicescmenosr <- geom_indicesc - geom_indicesr 
geom_indicescmasr <- geom_indicesc + geom_indicesr 

geom_indicescmasr1 <- geom_indicescmasr %>% group_by(alpha) %>% filter(geom_corr==max(geom_corr)) %>% ungroup() 
geom_indicescmenosr1 <- geom_indicescmenosr %>% group_by(alpha) %>% filter(geom_corr==max(geom_corr)) %>% ungroup() 

geom_indicesc$label <- "centros"
geom_indicescmenosr$label <- "centros menos rangos"
geom_indicescmasr$label <- "centros más rangos"

data_combined <- rbind(geom_indicesc,geom_indicescmenosr,geom_indicescmasr)
data_combined1 <- rbind(geom_indicescmasr1, geom_indicescmenosr1)
data_combined1 <- rbind(c(0,1),data_combined1)

ggplotly(ggplot(data_combined, aes(x = alpha, y = geom_corr, color = label)) +
           geom_step(size = 1) +
           labs(
             title = "Correlación geométrica con datos uniformes",
             x = "Alpha",
             y = "Geom_corr",
             color = "Curvas"
           ) +
           theme_minimal() +
           theme(legend.position = "bottom"))

#Área acumulada entre las curvas: centros menos rangos y centros más rangos.
library(pracma)

alpha.vals <- sort(unique(c(geom_indicescmenosr1$alpha, geom_indicescmasr1$alpha)))
geom_corr.cmasr <- approx(geom_indicescmenosr1$alpha, geom_indicescmasr1$geom_corr, xout = alpha.vals, rule = 2)$y
geom_corr.cmenosr <- approx(geom_indicescmenosr1$alpha, geom_indicescmenosr1$geom_corr, xout = alpha.vals, rule = 2)$y

diff.geom_corr <- geom_corr.cmasr - geom_corr.cmenosr

table.cmasr <- cbind(alpha.vals, geom_corr.cmasr)
table.cmenosr <- cbind(alpha.vals, geom_corr.cmenosr)
table.diff <- cbind(alpha.vals, diff.geom_corr)
#table.diff

ggplot(table.diff, aes(x=alpha.vals, diff.geom_corr))+geom_point()

area.acumulada <- cumsum(c(0, diff.geom_corr[-1] * diff(alpha.vals))) #analizar esto
#area.acumulada

data.acumulada <- data.frame(alpha = alpha.vals, area = area.acumulada)
head(data.acumulada)

p <- ggplot(data.acumulada, aes(x = alpha, y = area.acumulada)) +
  geom_line(size = 1, color = "blue") +
  labs(
    title = "Área Acumulada entre las Curvas",
    x = "Alpha",
    y = "Área Acumulada"
  ) +
  theme_minimal()

ggplotly(p)

#Derivada.

tasa.cambio <- diff(area.acumulada) / diff(alpha.vals)
alpha.medios <- (alpha.vals[-1] + alpha.vals[-length(alpha.vals)])/2
data.derivada <- data.frame(alpha = alpha.medios, tasadecambio = tasa.cambio)

p.derivada <- ggplot(data.derivada, aes(x = alpha, y = tasa.cambio)) +
  geom_line(size = 1, color = "red") +
  labs(
    title = "Tasa de Cambio de la Función Acumulada",
    x = "Alpha",
    y = "Derivada (Tasa de Cambio)"
  ) +
  theme_minimal()

ggplotly(p.derivada)







# Simulación 2. Circle with hole.

set.seed(123)
cita <- runif(1000, 0, 2*pi)
r <- runif(1000, 0.5, 1)

cita1 <- runif(1000, 0, 2*pi)
r1 <- runif(1000, 0.5, 1)

x.centers <- r*cos(cita)
y.centers <- r*sin(cita)

x.ranges <- r1*cos(cita)
y.ranges <- r1*sin(cita)

x.lower <- x.centers - x.ranges
x.upper <- x.centers + x.ranges

y.lower <- y.centers - y.ranges #regresión de centros + regresión de rangos
y.upper <- y.centers + y.ranges #regresión de centros - regresión de rangos

interval.data.test <- data.frame(x.lower, x.upper, y.lower, y.upper)

ggplot(interval.data.test, aes(x = x.centers, y = y.centers ))+geom_point()+labs(title = "Xc vs. Y") 
ggplot(interval.data.test, aes(x = x.ranges, y = y.ranges ))+geom_point()+labs(title = "Xr vs. Y") 






# Simulación 3. Multiples circles with holes.

cita <- runif(1000, 0, 2*pi)
cita1 <- runif(1000, 0, 2*pi)

# Círculo centrado en (0,0) de radio entre 1.5 y 2.5
r.1 <- runif(1000, 1.5, 2.5)
r.11 <- runif(1000, 0.5, 1)

x1.center <- r.1*cos(cita)
y1.center <- r.1*sin(cita)

x1.ranges <- r.11*cos(cita1)
y1.ranges <- r.11*sin(cita1)

# Círculo centrado en (3.5, 3.5) de radio entre 0.5 y 1.
r.2 <- runif(1000, 0.5, 1)
r.22 <- runif(1000, 0.5, 1)

x2.center <- r.2*cos(cita) + 3.5
y2.center <- r.2*sin(cita) + 3.5

x2.ranges <- r.22*cos(cita1) + 3.5
y2.ranges <- r.22*sin(cita1) + 3.5

# Círculo centrado en (-4,4) de radio entre 1 y 2
r.3 <- runif(1000, 1, 2)
r.33 <- runif(1000, 1, 2)

x3.center <- r.2*cos(cita) - 4
y3.center <- r.2*sin(cita) +4

x3.ranges <- r.33*cos(cita1) - 4
y3.ranges <- r.33*sin(cita1) + 4

x1.lower <- x1.center - x1.ranges
x1.upper <- x1.center + x1.ranges

x2.lower <- x2.center - x2.ranges
x2.upper <- x2.center + x2.ranges

x3.lower <- x3.center - x3.ranges
x3.upper <- x3.center + x3.ranges

y1.lower <- y1.center - y1.ranges 
y1.upper <- y1.center + y1.ranges 

y2.lower <- y2.center - y2.ranges 
y2.upper <- y2.center + y2.ranges 

y3.lower <- y3.center - y3.ranges 
y3.upper <- y3.center + y3.ranges 

interval.data.test1 <- data.frame(x1.lower, x1.upper, x2.lower, x2.upper, 
                                 x3.lower, x3.upper, y1.lower, y1.upper, y2.lower, y2.upper, y3.lower, y3.upper)


ggplot(interval.data.test1, aes(x = x1.center, y = y1.center ))+geom_point()+labs(title = "X1 vs. Y") 
ggplot(interval.data.test1, aes(x = x1.ranges, y = y1.ranges ))+geom_point()+labs(title = "X1 vs. Y") 

ggplot(interval.data.test, aes(x = x2.center, y = y2.center ))+geom_point()+labs(title = "X2 vs. Y") 
ggplot(interval.data.test, aes(x = x2.ranges, y = y2.ranges ))+geom_point()+labs(title = "X2 vs. Y") 

ggplot(interval.data.test, aes(x = x3.center, y = y3.center ))+geom_point()+labs(title = "X3 vs. Y") 
ggplot(interval.data.test, aes(x = x3.ranges, y = y3.ranges ))+geom_point()+labs(title = "X3 vs. Y") 

# Simulación 4. Ishigami.

#x1.center <- runi
