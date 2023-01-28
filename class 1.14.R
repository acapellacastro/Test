install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
ggplot::mpg
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()


ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~ class, nrow = 2)
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point()+
  geom_smooth()
ggplot(mpg, aesgeo(x = displ, y = hwy, color = class)) +
  geom_point(color = class))+
  geom_smooth(FALSE)
  
ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()

ggplot(mpg, mapping = aes (x = cty, y = hwy))+
  geom_point()                         

ggplot(mpg, mapping = aes (x = cty, y = hwy))+
  geom_point() + 
  scale_x_reverse() +
  scale_y_reverse()

ggplot(mpg, mapping = aes (x = cty, y = hwy, color =class))+
  geom_point() +
  scale_color_brewer()

ggplot(mpg, aes(x = class)) +
  geom_bar()

ggplot(mpg, aes(x = hwy)) +
  geom_histogram()

ggplot(mpg, aes(x= class, fill = drv)) +
  geom_bar()

ggplot(mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() +
  coord_cartesian(xlim=c(0,5))
ggplot(mpg, aes(x= class)) +
  geom_bar() + 
  coord_flip()
