## Load packages
library(nlrx)
library(tidyverse)
library(gganimate)
library(cartography) 
library(rcartocolor)
library(ggthemes) 


# Filter out unneeded variables and objects
# BAU scenario
turtles <- results_unnest %>% select(`[step]`, Scenario, pxcor, pycor, age, agent, health) %>% 
  filter(agent == "turtles", Scenario == "BAU", pycor < 324 & pxcor < 294 & pxcor > 0, health <= 100) %>% 
  dplyr::filter(`[step]` %in% seq(5181,8701,10))

patches <- results_unnest %>% select(`[step]`, Scenario, pxcor, pycor, pcolor) %>% 
            filter(Scenario == "BAU", pycor < 324 , `[step]` %in% seq(5181,8701,10))

# Patches
#--> steps to 

# Create facet plot:
ggplot() +
  facet_wrap(~`[step]`, ncol= 10) +
  coord_equal() +
  geom_tile(data=patches, aes(x=pxcor, y=pycor, fill=pcolor), alpha = .2) +
  geom_point(data=turtles, aes(x = pxcor, y = pycor, color = age), size=1, show.legend = FALSE) +
  scale_fill_gradient(low = "white", high = "grey20") +
  scale_color_manual(breaks=c("young", "active", "old"), 
                     values = c("young" = "#56B4E9", "active" = "#E69F00", "old" = "#999999")) +
  guides(fill=guide_legend(title="PM10")) +
  ggtitle("Unhealthly Population after a long-term exposure") +
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        #axis.title.x=element_blank(),
        #axis.title.y=element_blank(),legend.position="none",
        #panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),plot.background=element_blank()
        )
    
turtles %>% 
  group_by(`[step]`, age) %>% 
  tally() %>% 
  print(n = length(turtles$age)) %>% 
  reshape2::dcast(`[step]` ~ age) -> turtle.stat

turtle.stat$total <- rowSums(turtle.stat[,c(2:4)], na.rm = T)

##########


p1 <- ggplot() +
  geom_tile(data=patches, aes(x=pxcor, y=pycor, fill=factor(pcolor))) +
  geom_point(data=turtles, aes(x = pxcor, y = pycor, group=who, color = breed), size=2) +
  scale_fill_gradient(low = "white", high = "grey20") +
  scale_color_manual(breaks=c("young", "active", "old"), 
                     values = c("young" = "#56B4E9", "active" = "#E69F00", "old" = "#999999")) +
  guides(fill=guide_legend(title="PM10")) +
  transition_time(`[step]`) +
  coord_equal() +
  labs(title = 'Step: {frame_time}') +
  theme_void()

# Animate the plot and use 1 frame for each step of the model simulations
gganimate::animate(p1, nframes = length(unique(patches$`[step]`)), width=400, height=400, fps=4)
anim_save("seoul.gif")