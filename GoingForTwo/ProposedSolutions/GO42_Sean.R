library(tidyverse)

randomProbs = sample(seq(0,1,.01), 10000, replace = T)

points = function(p){
  try1 = rbinom(1,1,prob = p)
  try2 = rbinom(1,1,prob = p)
  
  winTable = data.frame('Try1' = try1, 
                        'Try2' = try2, 
                        'prob' = p) %>%
    mutate(win = ifelse(try1 == 1, 1,
                        ifelse(try1 == 0 & try2 == 1, .5, 0)))
  return(winTable)
}

pointsOut = do.call('rbind', lapply(randomProbs, points))

WLmod = glm(data = pointsOut %>% filter(win != .5),
            as.factor(win) ~ prob,
            family = "binomial")

summary(WLmod)

find50 = function(logreg, y){
  function(x){
    predict(logreg, data.frame(prob = x), type = 'response') - y
  }
}
cross = uniroot(find50(WLmod, .500), range(seq(0,1,.01)))$root

ggplot(pointsOut %>% filter(win != .5), aes(x = prob, y = win))+
  theme_classic() +
  geom_jitter(height = .01) +
  geom_smooth(method = "glm", method.args = list (family = "binomial")) +
  geom_vline(xintercept = cross) +
  geom_hline(yintercept = .5, color = 'red') +
  annotate('text', x = .3, y = .55, label = paste0(cross,', 0.5'))
