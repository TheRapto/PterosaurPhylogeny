## Scripts useful for manipulating trajectory data

require(tidyverse)

require("ggplot2")


parseTrajectory <- function(trajStr) {
  strValues <- str_split(str_split(trajStr, ",")[[1]], ":", simplify = TRUE)
  values <- apply(strValues[,-2], 2, as.numeric)
  time <- values[,1]
  src <- values[,2]
  dest <- values[,3]
  mult <- values[,4]
  N <- values[,-(1:4)]
  event<- strValues[,2]

  res <- list(time = time,
              N = N,
              event = event,
              src = src,
              dest = dest,
              mult = mult)

  return(res)
}

#Loading in the original trajectory file for processing:
Traj= read_tsv("constant rate FBD - Mkp - age uncertainty - linked occurences - Initial - 30-11.New Pterosaur Matrix.traj", col_types="ic")

#The function that processes the trajectroies, adapted to handle preloaded Traj's
loadTrajectories <- function(filename, burninFrac=0.1, subsample=NA) {
    states <- NULL
    events <- NULL

    message("Loading ", filename,"...", appendLF = FALSE)
    df_in <- Traj

    if (burninFrac>0) {
        n <- dim(df_in)[1]
        df_in <- df_in[-(1:ceiling(burninFrac*n)),]
    }

    if (!is.na(subsample)) {
        indices <- unique(round(seq(1, dim(df_in)[1], length.out=subsample)))
        df_in <- df_in[indices,]
    }
    
    for (row in 1:(dim(df_in)[1])) {
      cat(paste('\014',row,"/",dim(df_in)[1])) #Added to be able to monitor progress of processing
        trajStr <- df_in[row,2]
        trajStates <- parseTrajectory(trajStr)
        Ndim <- dim(trajStates$N)

        if (length(Ndim)==0) {
            ntypes <- 1
            states <- bind_rows(states,
                                tibble(traj=row,
                                       type=0,
                                       time=trajStates$time,
                                       N=trajStates$N))
        } else {
            ntypes <- dim(trajStates$N)[2]
            for (s in 1:ntypes) {
                
                states <- bind_rows(states,
                                    tibble(traj=row,
                                           type=s-1,
                                           time=trajStates$time,
                                           N=trajStates$N[,s]))
            }
        }

        events <- bind_rows(events,
                            tibble(traj=row,
                                   time=trajStates$time,
                                   event=trajStates$event,
                                   src=trajStates$src,
                                   dest=trajStates$dest,
                                   mult=trajStates$mult))
    }

    states <- states %>% group_by(traj) %>% mutate(age=max(time)-time)
    events <- events %>% group_by(traj) %>% mutate(age=max(time)-time)
    
    message("done.")
    
    return(list(states=states, events=events))
}

#Runs the traj processor for the loaded in Traj:
Processed_Traj=loadTrajectories("Ohno")
#Saves the processed traj to the wanted name:
save(Processed_Traj, file="Processed_Traj_Constant.Rdata")


####Changing all rates plot:####
{
#Loads the wanted processed Traj. Choose one:
load("Processed_Traj_AllRates.Rdata")
#Parsing the inputs to be readable
Time=Processed_Traj$states$time[seq(1, length(Processed_Traj$states$time), 10)]
N=Processed_Traj$states$N[seq(1, length(Processed_Traj$states$N), 10)]
df=data.frame(y=N,
              x=Time)
title="Changing All Rates"
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
  theme(legend.title = element_blank()) 

plot1
}
####Changing sampling plot:####
{
load("Processed_Traj_Sampling.Rdata")
#Parsing the inputs to be readable
Time=Processed_Traj$states$time[seq(1, length(Processed_Traj$states$time), 10)]
N=Processed_Traj$states$N[seq(1, length(Processed_Traj$states$N), 10)]
df=data.frame(y=N,
             x=Time)
title="Changing Sampling"
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
  theme(legend.title = element_blank()) 

plot2
}
####Constant rates plot:###
{
load("Processed_Traj_Constant.Rdata")
#Parsing the inputs to be readable
Time=Processed_Traj$states$time[seq(1, length(Processed_Traj$states$time), 10)]
N=Processed_Traj$states$N[seq(1, length(Processed_Traj$states$N), 10)]
df=data.frame(y=N,
              x=Time)
title="Constant rates"
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
  theme(legend.title = element_blank()) 

plot3
}

#The multiplot
{
default_width_fp_in = 170 / 25.4 # width for  full page fig in inch
file_name = paste("figs/Trajectoryplots_raw.pdf")
pdf(file = file_name , width=default_width_fp_in, height = 29.7/2.54)
combined_plot=gridExtra::grid.arrange(plot3,plot2,plot1, ncol=1)
dev.off()
}

