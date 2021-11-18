library(tidyverse)
library(mosaic)
library(car)
library(plotly)
library(reshape2)


pairs(Utilities, panel = panel.smooth, pch=16, cex=0.7, col=rgb(.2,.2,.2,.5))

mylm <- lm(elecbill~kwh,data=Utilities)
summary(mylm)

pairs(cbind(R=mylm$resid, Fit=mylm$fitted.values ,Utilities), panel = panel.smooth, pch=16, cex=0.7, col=rgb(.2,.2,.2,.5))

lm2 <- lm(elecbill~kwh+month+I(month^2),data=Utilities)
summary(lm2)



u_lm <- lm(Ozone ~ Temp + Month, data= Utilities)

#Graph Resolution (more important for more complex shapes)
graph_reso <- 0.5

#Setup Axis
axis_x <- seq(min(Utilities$kwh), max(Utilities$kwh), by = 50)
axis_y <- seq(min(Utilities$month), max(Utilities$month), by = graph_reso)

#Sample points
air_surface <- expand.grid(kwh = axis_x, month = axis_y, KEEP.OUT.ATTRS=F)
air_surface$Z <- predict.lm(u_lm, newdata = air_surface)
air_surface <- acast(air_surface, month ~ kwh, value.var = "Z") #y ~ x

#Create scatterplot
plot_ly(Utilities, 
        x = ~kwh, 
        y = ~month, 
        z = ~elecbill,
        text = rownames(Utilities), 
        type = "scatter3d", 
        mode = "markers") %>%
  add_trace(z = air_surface,
            x = axis_x,
            y = axis_y,
            type = "surface")

b <- coef(u_lm)
b
plot(elecbill~kwh,data=Utilities,pch=16,col="skyblue")
month = 2.5
curve(b[1]+b[2]*kwh+b[3]*month+b[4]*month^2,xname="kwh", add = TRUE,
      col="black")

plot(elecbill~month,data=Utilities,pch=16,col="skyblue")
kwh =1000
curve(b[1]+b[2]*kwh+b[3]*month+b[4]*month^2,xname="month", add = TRUE,
      col="black")
