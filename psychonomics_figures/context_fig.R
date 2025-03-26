rm(list=ls())
library(tidyverse)
library(here)
library(latex2exp)
library(patchwork)
dat <- tribble(
  ~opt, ~x, ~y,~col,
  "$T$", 5, 10,"black",
  "$C$", 10, 5,"black",
  "$D$",2.5,10,"#9999CC",
# "$S$, 3,12, "#66CC99"
) 
# base_plot <- dat %>% 
#   filter(opt %in% c("$T$","$C$")) %>%
#   ggplot()+
#   geom_text(aes(x,y,label=TeX(opt,output="character"),
#                 col=col),parse=T,size=6)+
#   geom_text(aes(7.5,12),
#             label=TeX("$P(T) = P(C)$",
#                 output="character"),parse=T,
#             inherit.aes = F,size=6)+
#   scale_color_identity()+
#   labs(x="quality",y="affordability",
#        title="Binary Choice",
#        subtitle=TeX("Choice Set: $T$, $C$"),
#        parse=T)+
#   coord_fixed(xlim=c(0,15),ylim=c(0,15))+
#   theme_classic()+
#   theme(plot.title = element_text(hjust=0.5,size=18),
#         axis.title = element_text(size=14),
#         plot.subtitle=element_text(hjust=0.5),
#         axis.ticks = element_blank(),
#         axis.text = element_blank())
# base_plot

attraction_plot <- dat %>% 
  ggplot()+
  geom_text(aes(x,y,label=TeX(opt,output="character"),
                col=col),parse=T,size=14)+
  geom_text(aes(7,1),
            label=TeX("$P(T) > $P(C)$",
                      output="character"),parse=T,
            inherit.aes = F,size=14)+
  scale_color_identity()+
  labs(title="Attraction Effect",
       #subtitle=TeX("Choice Set: $T$, $C$, $D$"),
       parse=T)+
  coord_fixed(xlim=c(0,15),ylim=c(0,15))+
  theme_classic()+
  theme(plot.title = element_text(hjust=0.5,colour = "#9999CC"),
        plot.subtitle = element_text(hjust=0.5),
        axis.ticks = element_blank(),
        axis.title=element_blank(),
        axis.text = element_blank(),
        text=element_text(size=32))
attraction_plot

# similarity_plot <- dat %>% 
#   filter(opt %in% c("$T$","$C$","$S$")) %>%
#   ggplot()+
#   geom_text(aes(x,y,label=TeX(opt,output="character"),
#                 col=col),parse=T,size=5)+
#   geom_text(aes(7.5,14),
#             label=TeX("$P(T) < $P(C)$",
#                       output="character"),parse=T,
#             inherit.aes = F,size=6)+
#   scale_color_identity()+
#   coord_fixed(xlim=c(0,15),ylim=c(0,15))+
#   labs(x="quality",y="affordability",
#        title="Similarity Effect",
#        subtitle=TeX("Choice Set: $T$, $C$, $S$"),
#        parse=T)+
#   theme_classic()+
#   theme(plot.title = element_text(hjust=0.5,size=18,colour = "#66CC99"),
#         axis.title = element_text(size=14),
#         plot.subtitle=element_text(hjust=0.5),
#         axis.ticks = element_blank(),
#         axis.text = element_blank())
# similarity_plot

# (base_plot|(attraction_plot/similarity_plot))+
#   plot_annotation(title="Context Effects in Choice",
#                   theme =theme(plot.title = element_text(hjust=0.5,size=24,face="bold")))
ggsave(here("psychonomics_figures","plots","context_fig.png"),
       width=5,height=4)
            
