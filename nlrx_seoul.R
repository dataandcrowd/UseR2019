#Sys.setenv(JAVA_HOME='/usr/local/software/spack/spack-0.11.2/opt/spack/linux-rhel7-x86_64/gcc-5.4.0/jdk-8u141-b15-p4aaoptkqukgdix6dh5ey236kllhluvr/jre') #Ubuntu cluster
Sys.setenv(JAVA_HOME='')


## Load packages
library(nlrx)
library(tidyverse) 
library(rcartocolor)
library(ggthemes) 


# Windows 
netlogopath <- file.path("C:/Program Files/NetLogo 6.0.4") # Where your NetLogo execution file is
outpath <- file.path("d:/out") 

# Cluster
#netlogopath <- file.path("/usr/local/Cluster-Apps/netlogo/6.0.4")
#outpath <- file.path("/home/hs621/github/nlrx")

## Step1: Create a nl obejct:
nl <- nl(nlversion = "6.0.4",
         nlpath = netlogopath,
         modelpath = file.path("/home/hs621/github/jasss/Gangnam_v6.nlogo"),
         jvmmem = 1024)


## Step2: Add Experiment
nl@experiment <- experiment(expname = "seoul",
                            outpath = outpath,
                            repetition = 1,   
                            tickmetrics = "true",
                            idsetup = "setup",  
                            idgo = "go",        
                            runtime = 8763,
                            evalticks=seq(1,8763, by = 10),
                            constants = list("PM10-parameters" = 100,
                                             "Scenario" = "\"BAU\"",
                                             "scenario-percent" = "\"inc-sce\""),
                                             #"AC" = 100),
                            variables = list('AC' = list(values=c(100,150,200))),
                            metrics.turtles =  list("people" = c("pxcor", "pycor", "homename", "destinationName", 
                                                                 "age", "health"))
                            #metrics.patches = c("pxcor", "pycor", "pcolor")
                            )

# Evaluate if variables and constants are valid:
eval_variables_constants(nl)

nl@simdesign <- simdesign_distinct(nl = nl, nseeds = 1)
#nl@simdesign <- simdesign_simple(nl = nl, nseeds = 1) # Constants

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

# Do further analysis:
#analyze_nl(nl)

rm(nl)
save.image(paste("seoul_", results$`random-seed`[1], ".RData", sep = ""))
