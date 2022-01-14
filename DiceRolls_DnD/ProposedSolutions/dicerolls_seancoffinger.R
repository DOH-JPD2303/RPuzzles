# Three dimensions! Let's practice some geometry in R by solving a simple probability question using volume of an irregular polygon!
library(tidyverse)

df12 = expand.grid(c(1:4),c(1:6))
df23 = expand.grid(c(1:6),c(1:8))
df13 = expand.grid(c(1:4),c(1:8))


# Three dimension means we have a visible polygon! Let's build the exterior using the six extremes!
df12.pwin = df12 %>% mutate(win = ifelse(Var2 > Var1, 1, 0)) %>% spread(Var2, win) # Think dice 3 is 8
                                                                                   # Think dice 3 is 1, no wins
df23.pwin = df23 %>% mutate(win = ifelse(Var2 > Var1 & Var1 > 1, 1, 0)) %>% spread(Var2, win) # Think dice 1 is 1
                                                                                   # Think dice 1 is 4, only one win (4,5,6)
df13.pwin = df13 %>% mutate(win = ifelse(Var2 > Var1 + 1 & Var2 > 6, 1, 0)) %>% spread(Var2, win) # Think dice 2 is 6
                                                                                                  # Think dice 2 is 1, no wins
# Graph it
df12.pwin.side = df12 %>%
  mutate(win = ifelse(Var2 > Var1, 1, 0),
                            z = 8) %>%
  rename('x' = Var1,
         'y' = Var2)

df23.pwin.side = df23 %>% 
  mutate(win = ifelse(Var2 > Var1 & Var1 > 1, 1, 0),
         x = 1) %>%
  rename('y' = Var1,
         'z' = Var2)

df23.pwin.side2 = data.frame(x = 4, y = 5, z = 6, win = 1)

df13.pwin.side = df13 %>%
  mutate(win = ifelse(Var2 > Var1 + 1 & Var2 > 6, 1, 0),
         y = 6) %>%
  rename('x' = Var1,
         'z' = Var2)

polygonFrame = rbind(df12.pwin.side,
                     df23.pwin.side,
                     df23.pwin.side2,
                     df13.pwin.side)

library(plotly)
plot_ly(data = polygonFrame, x = ~x, y = ~y, z = ~z, color = ~win)

# Verticies!
df12.pwin # (1,1.5,8), (1,6,8), (4,4.5,8), (4,6,8)
df23.pwin # (1,1.5,2)**, (1,1.5,8), (1,6,6.5), (1,6,8)
#(4,5,6) = (4,4.5,5.5)
df13.pwin # (1,6,6.5), (1,6,8), (4,6,6.5), (4,6,8)


library(rgl)
library(alphashape3d)

p = data.frame(x = c(1, 1,   1,   1,   4,   4,     4,   4),
               y = c(6, 6,   1.5, 1.5, 4.5, 6,     6,   4.5),
               z = c(8, 6.5, 8,   2, 8,   6.5,   8,   5.5))

p3d = p %>% mutate(win = 2) %>% rbind(polygonFrame)
plot_ly(data = p3d, x = ~x, y = ~y, z = ~z, color = ~win)

pmat = as.matrix(p)

a = ashape3d(pmat, alpha = 10)
plot(a)

round((volume_ashape3d(a)/105) *100)


