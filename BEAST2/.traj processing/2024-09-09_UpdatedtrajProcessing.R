
require(dplyr)

require(tidyverse)

require("ggplot2")

#Two functions that change the .traj files into plotable outputs:
gridTrajectoriesByTime <- function(traj, times) {
  traj %>% filter(variable=="N") %>%
    group_by(Sample, type) %>%
    reframe(N=approx(t, value, times, method="constant", f=1, yleft=0)$y,
            t=times)
}

gridTrajectoriesByAge <- function(traj, ages) {
  traj %>% filter(variable=="N") %>%
    group_by(Sample, type) %>%
    reframe(N=approx(age, value, ages, method="constant", f=0, yright=0)$y,
            age=ages)
}

#Loading in all 4 .traj files
TrajConst= read_tsv("07-01-CRBDS-ULN-MkPG.New Pterosaur Matrix.traj", col_types="ic")
TrajBD= read_tsv("07-01-VBDCRS-ULN-MkPG.New Pterosaur Matrix.traj", col_types="ic")
TrajSam= read_tsv("07-01-CRBDVS-ULN-MkPG.New Pterosaur Matrix.traj", col_types="ic")
TrajBDSam= read_tsv("07-01-VBDS-ULN-MkPG.New Pterosaur Matrix.traj", col_types="ic")

#Setting the Age and Time series to grid by.
AgePtero=seq(0, 100, 0.5)
TimePtero=seq(0, 100, 0.5)
#Set amount of subsampling. For example 10 means every tenth value of the trajectory.
Subsample=1000

####Constant rates plot:####
{
  #Loads the wanted processed part of the .Traj.
  TrajTimeConst=gridTrajectoriesByTime(TrajConst,TimePtero)
  #Parsing the inputs to be readable
  TimeConst=TrajTimeConst$t[seq(1, length(TrajTimeConst$t), Subsample)]
  NConst=TrajTimeConst$N[seq(1, length(TrajTimeConst$N), Subsample)]
  df=data.frame(y=NConst,
                x=TimeConst)
  title="Constant Rates"
  #The plot
  plot1=ggplot(data = df, aes(x=x, y = y)) +
    geom_point() +
    ggtitle(title)+ #for the title
    ylab("N")+ # for the x axis label
    xlab("Time (Myr)")+ # for the y axis label
    theme_bw()+ #Makes the background white.
    theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98)) +
    theme(legend.position = c(0.3, 0.85)) +
    theme(legend.key.size = unit(0.3, "cm")) +
    theme(legend.title = element_blank()) + 
    scale_y_continuous(limits=c(-5,50)) 
  
  plot1
}

####Changing sampling plot:####
{
  TrajTimeSam=gridTrajectoriesByTime(TrajSam,TimePtero)
  #Parsing the inputs to be readable
  TimeSam=TrajTimeSam$t[seq(1, length(TrajTimeSam$t), Subsample)]
  NSam=TrajTimeSam$N[seq(1, length(TrajTimeSam$N), Subsample)]
  df=data.frame(y=NSam,
                x=TimeSam)
  title="Changing Sampling rate"
  #The plot
  plot2=ggplot(data = df, aes(x=x, y = y)) +
    geom_point() +
    ggtitle(title)+ #for the title
    ylab("N")+ # for the x axis label
    xlab("Time (Myr)")+ # for the y axis label
    theme_bw()+ #Makes the background white.
    theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98)) +
    theme(legend.position = c(0.3, 0.85)) +
    theme(legend.key.size = unit(0.3, "cm")) +
    theme(legend.title = element_blank())+ 
    scale_y_continuous(limits=c(-5,50))  
  
  plot2
}

####Changing all rates plot:####
{
  TrajTimeBDSam=gridTrajectoriesByTime(TrajBDSam,TimePtero)
  #Parsing the inputs to be readable
  TimeBDSam=TrajTimeBDSam$t[seq(1, length(TrajTimeBDSam$t), Subsample)]
  NBDSam=TrajTimeBDSam$N[seq(1, length(TrajTimeBDSam$N), Subsample)]
  df=data.frame(y=NBDSam,
                x=TimeBDSam)
  title="Changing BD+Sampling"
  #The plot
  plot3=ggplot(data = df, aes(x=x, y = y)) +
    geom_point() +
    ggtitle(title)+ #for the title
    ylab("N")+ # for the x axis label
    xlab("Time (Myr)")+ # for the y axis label
    theme_bw()+ #Makes the background white.
    theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98)) +
    theme(legend.position = c(0.3, 0.85)) +
    theme(legend.key.size = unit(0.3, "cm")) +
    theme(legend.title = element_blank())+ 
    scale_y_continuous(limits=c(-5,50))  
  
  plot3
}
####Changing birth-death rates plot:###
{
  TrajTimeBD=gridTrajectoriesByTime(TrajBD,TimePtero)
  #Parsing the inputs to be readable
  TimeBD=TrajTimeBD$t[seq(1, length(TrajTimeBD$t), Subsample)]
  NBD=TrajTimeBD$N[seq(1, length(TrajTimeBD$N), Subsample)]
  df=data.frame(y=NBD,
                x=TimeBD)
  title="Changing Birth-Death rates"
  #The plot
  plot4=ggplot(data = df, aes(x=x, y = y)) +
    geom_point() +
    ggtitle(title)+ #for the title
    ylab("N")+ # for the x axis label
    xlab("Time (Myr)")+ # for the y axis label
    theme_bw()+ #Makes the background white.
    theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98)) +
    theme(legend.position = c(0.3, 0.85)) +
    theme(legend.key.size = unit(0.3, "cm")) +
    theme(legend.title = element_blank())+ 
    scale_y_continuous(limits=c(-5,50))  
  
  plot4
}
#The multiplot
{
  default_width_fp_in = 170 / 25.4 # width for  full page fig in inch
  file_name = paste("figs/Trajectoryplots_raw.pdf")
  pdf(file = file_name , width=default_width_fp_in, height = 29.7/2.54)
  combined_plot=gridExtra::grid.arrange(plot3,plot2,plot4,plot1, ncol=1)
  dev.off()
}




####Changing all rates 95% HPD plot:####

#Set amount of subsampling. For example 10 means every tenth value of the trajectory.
Subsample=1000
{
  #Loads the wanted processed Traj.
  TrajTimeBDSam=gridTrajectoriesByTime(TrajBDSam,TimePtero)
  #Parsing the inputs to be readable and getting the density curve for N
  NHPDBDSam=TrajTimeBDSam$N[seq(1, length(TrajTimeBDSam$N), Subsample)]
  OmitNHPDBSam=na.omit(NHPDBDSam)
  NDensityHPDBDSam = density(OmitNHPDBSam)
  df=data.frame(y=NDensityHPDBDSam$y,
                x=NDensityHPDBDSam$x)
  title="Changing All Rates"
  
  # Numerically find HPDI
  const = sum(NDensityHPDBDSam$y)
  spxx = sort(NDensityHPDBDSam$y, decreasing = TRUE) / const
  crit = spxx[which(cumsum(spxx) >=0.95)[1]]*const
  
  #The plot
  plot1Dens=ggplot(data = df, aes(x=x, y = y)) +
    geom_line() +
    geom_hline(yintercept=crit, linetype="dashed", color = "red")+
    ggtitle(title)+ #for the title
    ylab("Density")+ # for the x axis label
    xlab("N value")+ # for the y axis label
    theme_bw()+ #Makes the background white.
    theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98)) +
    theme(legend.position = c(0.3, 0.85)) +
    theme(legend.key.size = unit(0.3, "cm")) +
    theme(legend.title = element_blank())+
    scale_x_continuous(limits=c(-10,150))
  
  plot1Dens
}
####Changing sampling plot:####
{  
  #Loads the wanted processed Traj.
  TrajTimeSam=gridTrajectoriesByTime(TrajSam,TimePtero)
  #Parsing the inputs to be readable and getting the density curve for N
  NHPDSam=TrajTimeSam$N[seq(1, length(TrajTimeSam$N), Subsample)]
  OmitNHPDSam=na.omit(NHPDSam)
  NDensityHPDSam = density(OmitNHPDSam)
  df=data.frame(y=NDensityHPDSam$y,
                x=NDensityHPDSam$x)
  title="Changing Sampling rate"
  
  # Numerically find HPDI
  const = sum(NDensityHPDSam$y)
  spxx = sort(NDensityHPDSam$y, decreasing = TRUE) / const
  crit = spxx[which(cumsum(spxx) >=0.95)[1]]*const
  
  #The plot
  plot2Dens=ggplot(data = df, aes(x=x, y = y)) +
    geom_line() +
    geom_hline(yintercept=crit, linetype="dashed", color = "red")+
    ggtitle(title)+ #for the title
    ylab("Density")+ # for the x axis label
    xlab("N value")+ # for the y axis label
    theme_bw()+ #Makes the background white.
    theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98)) +
    theme(legend.position = c(0.3, 0.85)) +
    theme(legend.key.size = unit(0.3, "cm")) +
    theme(legend.title = element_blank()) +
    scale_x_continuous(limits=c(-10,150))
  
  plot2Dens
}
####Constant rates plot:###
{  
  #Loads the wanted processed Traj.
  TrajTimeConst=gridTrajectoriesByTime(TrajConst,TimePtero)
  #Parsing the inputs to be readable and getting the density curve for N
  NHPDConst=TrajTimeConst$N[seq(1, length(TrajTimeConst$N), Subsample)]
  OmitNHPDConst=na.omit(NHPDConst)
  NDensityHPDConst = density(OmitNHPDConst)
  df=data.frame(y=NDensityHPDConst$y,
                x=NDensityHPDConst$x)
  title="Constant rates"
  # Numerically find HPDI
  const = sum(NDensityHPDConst$y)
  spxx = sort(NDensityHPDConst$y, decreasing = TRUE) / const
  crit = spxx[which(cumsum(spxx) >=0.95)[1]]*const
  
  #The plot
  plot3Dens=ggplot(data = df, aes(x=x, y = y)) +
    geom_line() +
    geom_hline(yintercept=crit, linetype="dashed", color = "red")+
    ggtitle(title)+ #for the title
    ylab("Density")+ # for the x axis label
    xlab("N value")+ # for the y axis label
    theme_bw()+ #Makes the background white.
    theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98)) +
    theme(legend.position = c(0.3, 0.85)) +
    theme(legend.key.size = unit(0.3, "cm")) +
    theme(legend.title = element_blank())+
    scale_x_continuous(limits=c(-10,150)) 
  
  plot3Dens
}
####Changing birth-death rates plot:###
{
  #Loads the wanted processed Traj.
  TrajTimeBD=gridTrajectoriesByTime(TrajBD,TimePtero)
  #Parsing the inputs to be readable and getting the density curve for N
  NHPDBD=TrajTimeBD$N[seq(1, length(TrajTimeBD$N), Subsample)]
  OmitNHPDBD=na.omit(NHPDBD)
  NDensityHPDBD = density(OmitNHPDBD)
  df=data.frame(y=NDensityHPDBD$y,
                x=NDensityHPDBD$x)
  title="Changing Birth-Death rates"
  # Numerically find HPDI
  const = sum(NDensityHPDBD$y)
  spxx = sort(NDensityHPDBD$y, decreasing = TRUE) / const
  crit = spxx[which(cumsum(spxx) >=0.95)[1]]*const
  
  #The plot
  plot4Dens=ggplot(data = df, aes(x=x, y = y)) +
    geom_line() +
    geom_hline(yintercept=crit, linetype="dashed", color = "red")+
    ggtitle(title)+ #for the title
    ylab("Density")+ # for the x axis label
    xlab("N value")+ # for the y axis label
    theme_bw()+ #Makes the background white.
    theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98)) +
    theme(legend.position = c(0.3, 0.85)) +
    theme(legend.key.size = unit(0.3, "cm")) +
    theme(legend.title = element_blank())+
    scale_x_continuous(limits=c(-10,150)) 
  
  plot4Dens
}

#The multiplot
{
  default_width_fp_in = 170 / 25.4 # width for  full page fig in inch
  file_name = paste("figs/Densityplots_raw.pdf")
  pdf(file = file_name , width=default_width_fp_in, height = 29.7/2.54)
  combined_plot=gridExtra::grid.arrange(plot3Dens,plot2Dens,plot4Dens,plot1Dens, ncol=1)
  dev.off()
}


####Version with distribution range after 95% HPD####
{
TrajTimeConst=gridTrajectoriesByTime(TrajConst,TimePtero)
TrajTimeSam=gridTrajectoriesByTime(TrajSam,TimePtero)
TrajTimeBDSam=gridTrajectoriesByTime(TrajBDSam,TimePtero)
TrajTimeBD=gridTrajectoriesByTime(TrajBD,TimePtero)
}
library(deeptime)
library(coda)

BoundariesUpLow = function(Trajectory){
  result_list=list()
  t=as.character(Trajectory$t)  
  N=Trajectory$N
  
  # Loop through N and t, an`6.5`# Loop through N and t, and append values of t to the corresponding variable in result_list
  for (i in 1:length(t)) {
    if (t[i] %in% names(result_list)) {
      result_list[[t[i]]] = c(result_list[[t[i]]], N[i])  # Append the value to the existing vector
    } else {
      result_list[[t[i]]] = N[i]  # Create a new vector for the variable
    }
  }
  {
  HPDBoundary$Lower=1:201
  HPDBoundary$Upper=1:201
  }
for (i in 1:length(AgePtero)) {
    LoopMCMC=as.mcmc(result_list[[i]])
    Interval=HPDinterval(LoopMCMC)
    HPDBoundary$Lower[i]= Interval[1]
    HPDBoundary$Upper[i]= Interval[2]
}
  return(HPDBoundary)
}
  
  
#Finding the mean and the sd:
summarySE_NA <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE, conf.interval=.95) {
  library(doBy)

  # New version of length which can handle NA's: if na.rm==T, don't count them
  
  length2 <- function (x, na.rm=TRUE) {
    length(x)
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N2"
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#Plotting graph for Const
{
MeanConst <- summarySE_NA(TrajTimeConst, measurevar="N", groupvars="t")
title="Constant Rates"
UpLow = BoundariesUpLow(TrajTimeConst)
df=data.frame(y=MeanConst$N,
              x=MeanConst$t+146.17,
              Sdtop=UpLow$Lower,
              Sdbot=UpLow$Upper)

Constplot=ggplot(data = df, aes(x=x, y = y)) +
  geom_ribbon(aes(ymin=Sdbot,ymax=Sdtop, fill=Sdbot<Sdtop),show.legend=FALSE)+
  geom_line() +
  ggtitle(title)+ #for the title
  ylab("N")+ # for the x axis label
  coord_geo(dat="stages")+
  scale_x_reverse("Age (Ma)")+
  theme_bw()+ #Makes the background white.
  theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98)) +
  theme(legend.position = c(0.3, 0.85)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  theme(legend.title = element_blank()) + 
  scale_y_continuous(limits=c(-5,70)) 

Constplot
}
#Plotting graph for BD
{
  MeanBD <- summarySE_NA(TrajTimeBD, measurevar="N", groupvars="t")
  title="Changing Birth-Death"
  UpLow = BoundariesUpLow(TrajTimeBD)
  df=data.frame(y=MeanBD$N,
                x=MeanBD$t+146.17,
                Sdtop=UpLow$Lower,
                Sdbot=UpLow$Upper)
  
  
  BDplot=ggplot(data = df, aes(x=x, y = y)) +
    geom_ribbon(aes(ymin=Sdbot,ymax=Sdtop, fill=Sdbot<Sdtop),show.legend=FALSE)+
    geom_line() +
    ggtitle(title)+ #for the title
    ylab("N")+ # for the x axis label
    coord_geo(dat="stages")+
    scale_x_reverse("Age (Ma)")+
    theme_bw()+ #Makes the background white.
    theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98)) +
    theme(legend.position = c(0.3, 0.85)) +
    theme(legend.key.size = unit(0.3, "cm")) +
    theme(legend.title = element_blank()) + 
    scale_y_continuous(limits=c(-5,70)) 
  
  BDplot
}

#Plotting graph for BD-Sam
{
  MeanBDSam <- summarySE_NA(TrajTimeBDSam, measurevar="N", groupvars="t")
  title="Changing Birth-Death & Sampling"
  UpLow = BoundariesUpLow(TrajTimeBDSam)
  df=data.frame(y=MeanBDSam$N,
                x=MeanBDSam$t+146.17,
                Sdtop=UpLow$Lower,
                Sdbot=UpLow$Upper)
  
  BDSamplot=ggplot(data = df, aes(x=x, y = y)) +
    geom_ribbon(aes(ymin=Sdbot,ymax=Sdtop, fill=Sdbot<Sdtop),show.legend=FALSE)+
    geom_line() +
    ggtitle(title)+ #for the title
    ylab("N")+ # for the x axis label
    coord_geo(dat="stages")+
    scale_x_reverse("Age (Ma)")+
    theme_bw()+ #Makes the background white.
    theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98)) +
    theme(legend.position = c(0.3, 0.85)) +
    theme(legend.key.size = unit(0.3, "cm")) +
    theme(legend.title = element_blank()) + 
    scale_y_continuous(limits=c(-5,70)) 
  
  BDSamplot
}
#Plotting graph for Sam
{
  MeanSam <- summarySE_NA(TrajTimeSam, measurevar="N", groupvars="t")
  title="Changing  Sampling"
  UpLow = BoundariesUpLow(TrajTimeSam)
  df=data.frame(y=MeanSam$N,
                x=MeanSam$t+146.17,
                Sdtop=UpLow$Lower,
                Sdbot=UpLow$Upper)
  
  Samplot=ggplot(data = df, aes(x=x, y = y)) +
    geom_ribbon(aes(ymin=Sdbot,ymax=Sdtop, fill=Sdbot<Sdtop),show.legend=FALSE)+
    geom_line() +
    ggtitle(title)+ #for the title
    ylab("N")+ # for the x axis label
    coord_geo(dat="stages")+
    scale_x_reverse("Age (Ma)")+
    theme_bw()+ #Makes the background white.
    theme(text = element_text(size = 6), plot.tag = element_text(face = "bold"), plot.tag.position = c(0.01, 0.98)) +
    theme(legend.position = c(0.3, 0.85)) +
    theme(legend.key.size = unit(0.3, "cm")) +
    theme(legend.title = element_blank()) + 
    scale_y_continuous(limits=c(-5,70)) 
  
  Samplot
}

{
  default_width_fp_in = 170 / 25.4 # width for  full page fig in inch
  file_name = paste("figs/Meanplots_raw.pdf")
  pdf(file = file_name , width=default_width_fp_in, height = 29.7/2.54)
  combined_plot=gridExtra::grid.arrange(Constplot,BDplot,Samplot,BDSamplot, ncol=1)
  dev.off()
}

