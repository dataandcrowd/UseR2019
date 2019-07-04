#Sys.setenv(JAVA_HOME='/usr/local/software/spack/spack-0.11.2/opt/spack/linux-rhel7-x86_64/gcc-5.4.0/jdk-8u141-b15-p4aaoptkqukgdix6dh5ey236kllhluvr/jre') #Ubuntu cluster
Sys.setenv(JAVA_HOME= "/usr/lib/jvm/java-11-openjdk-amd64")


## Load packages
library(nlrx)
library(tidyverse) 
library(rcartocolor)
library(ggthemes) 


# Office
netlogopath <- file.path("/home/hs621/NetLogo 6.0.4")
outpath <- file.path("/home/hs621/out")



## Step1: Create a nl obejct:
nl <- nl(nlversion = "6.0.4",
         nlpath = netlogopath,
         modelpath = file.path("/data/github/jasss/Gangnam_v6.nlogo"),
         jvmmem = 1024)

## Step2: Add Experiment
nl@experiment <- experiment(expname = "seoul",
                            outpath = outpath,
                            repetition = 1,   
                            tickmetrics = "true",
                            idsetup = "setup",  
                            idgo = "go",        
                            runtime = 8764,
                            evalticks=seq(1,8764, by = 100),
                            constants = list("PM10-parameters" = 100,
                                             "Scenario" = "\"BAU\"",
                                             "scenario-percent" = "\"inc-sce\"",
                                             "AC" = 100),
                            #variables = list('AC' = list(values=c(100,150,200))),
                            metrics.turtles =  list("people" = c("xcor", "ycor", "homename", "destinationName", "age", "health")),
                            metrics.patches = c("pxcor", "pycor", "pcolor")
)

# Evaluate if variables and constants are valid:
eval_variables_constants(nl)

#nl@simdesign <- simdesign_distinct(nl = nl, nseeds = 1)
nl@simdesign <- simdesign_simple(nl = nl, nseeds = 1)

# Step4: Run simulations:
init <- Sys.time()
results <- run_nl_all(nl = nl)
Sys.time() - init


# Attach results to nl object:
setsim(nl, "simoutput") <- results

# Report spatial data:
results_unnest <- unnest_simoutput(nl)


# Write output to outpath of experiment within nl
#write_simoutput(nl)


# Filter out unneeded variables and objects
# BAU scenario
turtles <- results_unnest %>% 
  select(`[step]`, Scenario, xcor, ycor, age, agent, health) %>% 
  filter(agent == "turtles", Scenario == "BAU", ycor < 324 & xcor < 294 & xcor > 0, health <= 100) %>% 
  filter(`[step]` %in% seq(5000,8764))

patches <- results_unnest %>% select(`[step]`, Scenario, pxcor, pycor, pcolor) %>% 
  filter(Scenario == "BAU", pycor < 324) %>% 
  filter(`[step]` %in% seq(5000,8764))


# Create facet plot:
ggplot() +
  facet_wrap(~`[step]`, ncol= 10) +
  coord_equal() +
  geom_tile(data=patches, aes(x=pxcor, y=pycor, fill=pcolor), alpha = .2) +
  geom_point(data=turtles, aes(x = xcor, y = ycor, color = age), size=1) +
  scale_fill_gradient(low = "white", high = "grey20") +
  scale_color_manual(breaks=c("young", "active", "old"), 
                     values = c("young" = "#56B4E9", "active" = "#E69F00", "old" = "#999999")) +
  guides(fill=guide_legend(title="PM10")) +
  ggtitle("Unhealthly Population after a long-term exposure") +
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank()
        #axis.title.x=element_blank(),
        #axis.title.y=element_blank(),legend.position="none",
        #panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),plot.background=element_blank()
  )


## number of turtles
turtles %>% 
  group_by(`[step]`, age) %>% 
  tally() %>% 
  print(n = length(turtles$age)) %>% 
  reshape2::dcast(`[step]` ~ age) -> turtle.stat

turtle.stat$total <- rowSums(turtle.stat[,c(2:4)], na.rm = T)


## Density plot
# health distribution: density plot!

turtles_density <- results_unnest %>% 
  select(`[step]`, Scenario, xcor, ycor, age, agent, health, homename, destinationName) %>% 
  filter(agent == "turtles", Scenario == "BAU", ycor < 324 & xcor < 294 & xcor > 0) %>% 
  filter(`[step]` %in% seq(1,8764))


turtles_density$health[turtles_density$health <= 0] <- 0

turtles_density %>% 
  ggplot(aes(health, fill = age)) + 
  geom_density(alpha = 0.4) +
  theme_bw() +
  theme(legend.title = element_text(size=20, face="bold"),
        legend.text = element_text(size=15),
        legend.position = c(0.2, 0.8),
        axis.text=element_text(size=20),
        axis.title=element_text(size=15,face="bold")
  )

