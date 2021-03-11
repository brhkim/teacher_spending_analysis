################################################################################
#  
#   Name: expenditures_coefs_plot_rq3.R
#   
################################################################################


################################################################################
#
#   #setup - Setup file and check dependencies
#
################################################################################

# Clear things out to start
rm(list = ls())

# Load in libraries
libs <- c('tidyverse','ggplot2','haven','readxl', 'viridis', 'specr')
sapply(libs, require, character.only = TRUE)


################################################################################
#
#   #import - Load in a dataset and start taking a peek around
#
################################################################################

# Prepare loop variables for proper ingestion of many regression output tables
autovars <- c("auto_a", "auto_b", "auto_c")
outcomevars <- c("propspend_sch", "propspend_all", "propspend_base", "expenditures", "anyspend")
inttypes <- c("unc", "unint", "int")
bingroups <- c("4", "2")
experiences <- c("0", "1")
urbanicities <- c("0", "1")

# Set up an output dataframe for the ingestion loop
output <- data.frame(auto=as.character(),
                     category=as.character(), 
                     b=as.numeric(),
                     p=as.character(),
                     min95=as.numeric(),
                     max95=as.numeric(),
                     out=as.character(),
                     int=as.character(),
                     bin=as.character(),
                     exp=as.character(),
                     urb=as.character())

# Start ingestion loop
for (autovar in autovars) {
  for (outcome in outcomevars) {
    for (inttype in inttypes) {
      for (bingroup in bingroups) {
        for (experience in experiences) {
          for (urbanicity in urbanicities) {
            # Get the filename and read it in
            filename1 <- paste0("data/rq3/3_", autovar, "_", outcome, "_", inttype, "_", bingroup, "_exp", experience, "_urb", urbanicity, ".csv")
            tmp1 <- read_csv(filename1, skip=1)
            
            # Pull out the nsize
            nsize1 <- tmp1 %>%
              filter(X1=="N")
            nsize1 <- nsize1$b[1]
            
            # Clean up the dataset
            tmp1 <- tmp1 %>%
              rename(category=X1)%>%
              filter(str_detect(category, "auto") | str_detect(category, "race") | str_detect(category, "frpl")) %>%
              mutate(auto=autovar,
                     out=outcome,
                     int=inttype,
                     bin=bingroup,
                     exp=experience,
                     urb=urbanicity,
                     nsize=nsize1)
            
            # Add it to the output dataset
            output <- bind_rows(output, tmp1)
            
          }
        }
      }
    }
  }
}

# Clean up the output dataset
output <- output %>%
  separate(category, into=c("cat1", "cat2"), sep="#") %>%
  mutate(auto2=case_when(is.na(cat2) & str_detect(cat1, "auto") ~ substr(cat1, 1, 1),
                        TRUE ~ "9"),
         race=case_when(is.na(cat2) & str_detect(cat1, "race") ~ substr(cat1, 1, 1),
                        !is.na(cat2) ~ substr(cat2, 1, 1),
                        TRUE ~ "9"),
         frpl=case_when(!is.na(cat2) & str_detect(cat1, "frpl") ~ substr(cat1, 1, 1),
                        TRUE ~ "9")) %>%
  select(-cat1, -cat2)

# Save a checkpoint
base::save(output, file="rq3tmp.RData")

################################################################################
#
#   #visualize - Visualize the data using ggplot2
#
################################################################################

# Load the checkpoint
base::load("rq3tmp.RData")

# Prepare plot titles, labels, etc. as needed according to the loop
outcomelabs <- c("Teacher Spending as a Proportion of School-Related Income", "Teacher Spending as a Proportion of All Income", "Teacher Spending as a Proportion of Base Salary", "Teacher Spending in 2019Q4 Dollars", "Occurrence of Any Teacher Spending")
autolabs <- c("\nTeacher Autonomy (NCES-style Definition)", "\nTeacher Autonomy (Above/Below Median)", "Teacher Autonomy (Terciles)")
intlabs <- c("No Student Demographics", "Student Race/Ethnicity", "Student Race/Ethnicity Interacted \nwith Student FRPL")
binlabs <- c("Four Group Specification", "Two Group Specification")
explabs <- c("", "Teacher Experience")
urblabs <- c("", "School Urbanicity")

for (i_auto in 1:3) {
for (i_out in 1:5) {
for (i_int in 1:3) {
for (i_bin in 1:2) {
for (i_exp in 1:2) {
for (i_urb in 1:2) {

  # Skip plotting output if no other controls were added from the last one (i.e. duplicate plot)
  if (i_int==1 & (i_exp!=1 | i_urb!=1)) {
    next
  }
  
  # Set up autonomy variable labels
  if (i_auto==1) {
    autoplotlabs <- c("Low", "Moderate", "High")
  }
  if (i_auto==2) {
    autoplotlabs <- c("Below Median", "Above Median")
  }
  if (i_auto==3) {
    autoplotlabs <- c("Bottom Tercile", "Middle Tercile", "Top Tercile")
  }
  
  # Grab the proper data from the main dataset to plot based on loop parameters
  graphdata <- output %>%
    filter(auto==autovars[i_auto],
           auto2!="9",
           out==outcomevars[i_out],
           int==inttypes[i_int],
           bin==bingroups[i_bin],
           exp==experiences[i_exp],
           urb==urbanicities[i_urb]) %>%
    mutate(race=as.factor(race),
           frpl=as.factor(frpl))
  
  # Get the nsize and round it for reporting compliance
  ncheck <- graphdata$nsize %>% 
    mean() %>%
    round(digits=-2)
  
  # Get the proper bin label names for race/ethnicity/FRPL buckets
  if (bingroups[i_bin]=="4") {
    binplotlab <- c("0-24%", "25-49%", "50-74%", "75-100%")
    binplotnudge <- 0.15
  }
  if (bingroups[i_bin]=="2") {
    binplotlab <- c("0-49%", "50-100%")
    binplotnudge <-0.04
  }
  
  # Create the plot title and subtitle
  plottitle <- paste0(outcomelabs[i_out], " and ", autolabs[i_auto])
  
  subtitleprep <- intlabs[i_int]
  if (i_int!=1) {
    subtitleprep <- paste0(intlabs[i_int], " (", binlabs[i_bin], ")")
  }
  
  plotsubtitle <- paste0(subtitleprep, " (n = ", ncheck, ")")
  if (i_exp==2 | i_urb==2) {
    plotsubtitle <- paste0(subtitleprep, ", also accounting for ", explabs[i_exp], urblabs[i_urb], " (n = ", ncheck, ")")
  }
  if (i_exp==2 & i_urb==2) {
    plotsubtitle <- paste0(subtitleprep, ", also accounting for ", explabs[i_exp], " and ", urblabs[i_urb], " (n = ", ncheck, ")")
  }
  
  # Find the left-most and right-most points for labeling the numbers
  labeldata_top <- graphdata %>%
    filter(as.numeric(frpl)==max(as.numeric(graphdata$frpl)))
  labeldata_bot <- graphdata %>%
    filter(as.numeric(frpl)==min(as.numeric(graphdata$frpl)))
  
  # Empty out bottom dataset if the top and bottom points are the same to
  # prevent double-labeling
  if (identical(labeldata_top, labeldata_bot)) {
    labeldata_bot <- labeldata_bot[0,]
  }
  
  # Figure out the proper rounding for the variable at hand
  roundprep <- 0
  if (mean(abs(graphdata$b))<1) {
    roundnum <- 0
    roundcheck <- mean(abs(graphdata$b))
    while (roundcheck<1) {
      roundcheck <- mean(abs(graphdata$b))
      roundnum <- roundnum + 1
      roundcheck <- roundcheck * (10^roundnum)
    }
    roundprep <- roundnum + 2
  } 
  
  # Plot the autonomy results
  ggplot(graphdata, aes(x = auto2, y = b)) +
    theme_bw() +
    geom_hline(yintercept=0, lty=2, colour="grey") +
    geom_point(size=2) +
    geom_text(data=labeldata_top, aes(label=round(b, digits=roundprep)), hjust=0, vjust=0.5, nudge_x=0.13, show.legend=FALSE) +
    geom_text(data=labeldata_bot, aes(label=round(b, digits=roundprep)), hjust=1, vjust=0.5, nudge_x=-0.13, show.legend=FALSE) +
    geom_errorbar(aes(ymin=min95, ymax=max95),
                  width=0.1) +
    scale_x_discrete(labels=autoplotlabs) +
    theme(axis.text.x = element_text(size=12)) +
    theme(axis.text.y = element_text(size=12)) +
    theme(legend.text = element_text(size=12)) +
    theme(legend.title = element_text(size=12)) +
    labs(x="\nTeacher Autonomy Level", y="Coefficient\n",
         title=plottitle, subtitle=plotsubtitle)
  
  # Save the file
  filename <- paste0("output/rq3/rq3_", autovars[i_auto], "_", outcomevars[i_out], "_", inttypes[i_int], "_bin", bingroups[i_bin], "_exp", experiences[i_exp], "_urb", urbanicities[i_urb], ".jpg")
  ggsave(filename, width=10, height=6)
  
  # Only plot the race/ethnicity/FRPL variable results if those were included in the present regression
  if(i_int!=1){
    
    # Get the proper data from the main dataset based on the loop specifications
    graphdata <- output %>%
      filter(auto==autovars[i_auto],
             auto2=="9",
             out==outcomevars[i_out],
             int==inttypes[i_int],
             bin==bingroups[i_bin],
             exp==experiences[i_exp],
             urb==urbanicities[i_urb]) %>%
      mutate(race=as.factor(race),
             frpl=as.factor(frpl))
    
    # Get the data nsize and round it for reporting compliance
    ncheck <- graphdata$nsize %>% 
      mean() %>%
      round(digits=-2)  
    
    # Set up the plot title and subtitle
    plottitle <- paste0(outcomelabs[i_out], " and ", intlabs[i_int])

    subtitleprep <- intlabs[i_int]
    if (i_int!=1) {
      subtitleprep <- paste0(intlabs[i_int], " (", binlabs[i_bin], ")")
    }
    
    plotsubtitle <- paste0(binlabs[i_bin], ", also accounting for ", autolabs[i_auto], " (n = ", ncheck, ")")
    if (i_exp==2 | i_urb==2) {
      plotsubtitle <- paste0(binlabs[i_bin], ", also accounting for ", autolabs[i_auto], ", ", explabs[i_exp], urblabs[i_urb], " (n = ", ncheck, ")")
    }
    if (i_exp==2 & i_urb==2) {
      plotsubtitle <- paste0(binlabs[i_bin], ", also accounting for ", autolabs[i_auto], ", ", explabs[i_exp], ", and ", urblabs[i_urb], " (n = ", ncheck, ")")
    }
    
    # Find the left-most and right-most points for labeling
    labeldata_top <- graphdata %>%
      filter(as.numeric(frpl)==max(as.numeric(graphdata$frpl)))
    labeldata_bot <- graphdata %>%
      filter(as.numeric(frpl)==min(as.numeric(graphdata$frpl)))
    
    # Empty out bottom dataset if the top and bottom points are the same to
    # prevent double-labeling
    if (identical(labeldata_top$frpl, labeldata_bot$frpl)) {
      labeldata_bot <- labeldata_bot[0,]
    }
    
    # Figure out the proper rounding for each variable
    roundprep <- 0
    if (mean(abs(graphdata$b))<1) {
      roundnum <- 0
      roundcheck <- mean(abs(graphdata$b))
      while (roundcheck<1) {
        roundcheck <- mean(abs(graphdata$b))
        roundnum <- roundnum + 1
        roundcheck <- roundcheck * (10^roundnum)
      }
      roundprep <- roundnum + 2
    } 
    
    # Decide whether or not to include the legend if only race/ethnicity is being plotted
    guidescheck <- guide_legend()
    if (i_int==2) {
      guidescheck <- FALSE
    }
    
    # Plot the results for race/ethnicity/FRPL variables in autonomy regressions
    ggplot(graphdata, aes(x = race, y = b, color=frpl)) +
      theme_bw() +
      geom_hline(yintercept=0, lty=2, colour="grey") +
      scale_color_viridis_d(option="D", labels=binplotlab) +
      geom_point(size=2, position=position_dodge(width=binplotnudge)) +
      geom_text(data=labeldata_top, aes(label=round(b, digits=roundprep)), hjust=0, vjust=0.5, nudge_x=0.13, show.legend=FALSE) +
      geom_text(data=labeldata_bot, aes(label=round(b, digits=roundprep)), hjust=1, vjust=0.5, nudge_x=-0.13, show.legend=FALSE) +
      geom_errorbar(aes(ymin=min95, ymax=max95),
                    width=binplotnudge, position=position_dodge(width=binplotnudge)) +
      scale_x_discrete(labels=binplotlab) +
      theme(axis.text.x = element_text(size=12)) +
      theme(axis.text.y = element_text(size=12)) +
      theme(legend.text = element_text(size=12)) +
      theme(legend.title = element_text(size=12)) +
      guides(color=guidescheck) +
      labs(x="\nProportion of Racial/Ethnic Minority Students in School", y="Coefficient\n",
           color="Proportion of \nFRPL-Eligible \nStudents in School", title=plottitle, subtitle=plotsubtitle)
    
    # Save the file
    filename <- paste0("output/rq3/rq3b_", autovars[i_auto], "_", outcomevars[i_out], "_", inttypes[i_int], "_bin", bingroups[i_bin], "_exp", experiences[i_exp], "_urb", urbanicities[i_urb], ".jpg")
    ggsave(filename, width=10, height=6)
  }
  
}
}
}
}
}
}