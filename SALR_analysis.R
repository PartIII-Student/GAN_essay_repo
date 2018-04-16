# Packages ################################################## 
rm(list = ls(all=T))
wd = dirname(rstudioapi::getActiveDocumentContext()$path)
suppressMessages(suppressWarnings(library(reshape2)))
library(plyr)
suppressMessages(suppressWarnings(library(ggplot2)))
library(scatterplot3d)
suppressMessages(suppressWarnings(library(data.table)))
suppressMessages(suppressWarnings(library(plotly)))
library(lattice)
library(FNN)
library(pbapply)
library('grDevices')
library(dplyr)
source('mulitplot.R')



# Which plots to show ################################################## 
phi_against_gamma = T
loess_plot_KL = T
plot_best_histogram_KL = T
qqplot = T
plot_med_histogram_KL = T
qqplot_median = T
qqplot_randoms = T

recalc_KL_main = F
recalc_KL_control = F

# Sub folder ################################################## 
main_folder = 'SALR_data'
sub_folder = 'gd'

save_picture_name = function(name){
  dir = paste(c(wd,'/plots_pictures/',main_folder,'_',sub_folder,'_',name,'.jpeg'),
              collapse = '')
  return(dir)
}


# Make colours and font size  ################################################## 

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols = gg_color_hue(2)

export_theme1 = theme(text=element_text(size=40),
                     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                     axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

export_theme = theme(text=element_text(size=20),
                     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                     axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

rstudio_theme = theme(text=element_text(size=8),
                      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

# Collect data ################################################## 

data_path = paste0(c(wd,'/',main_folder,'/',sub_folder), collapse='')
setwd(data_path)

collected_data = fread('output.csv', header = F, sep = ',')
collected_data = data.matrix(collected_data)
collected_data = na.omit(collected_data)
collected_data= data.frame(Gamma =collected_data[,1] ,
                           Phi= collected_data[,2] ,collected_data[,3:ncol(collected_data)])


standardized_data = collected_data
standardized_data[,3:ncol(standardized_data)] = standardized_data[,3:ncol(standardized_data)] -6

nrow(collected_data)


gamma_boundaries= c(min(collected_data$Gamma)+
                      (max(collected_data$Gamma)-min(collected_data$Gamma))/3 ,
                    min(collected_data$Gamma)+
                      2*(max(collected_data$Gamma)-min(collected_data$Gamma))/3)

phi_boundaries = c(min(collected_data$Phi),
                   min(collected_data$Phi)+
                     (max(collected_data$Phi)-min(collected_data$Phi))/2)






# Samples plot  ################################################## 

if(phi_against_gamma){
  cols = gg_color_hue(2)
  p1 = ggplot(collected_data,aes(x = Gamma, y = Phi))+geom_point(col=cols[1])+
    geom_hline(yintercept = phi_boundaries[1]+0.001, col=cols[2], lty = 2, lwd = 3) +
    geom_hline(yintercept = phi_boundaries[2], col=cols[2], lty = 2, lwd = 3)+
    geom_vline(xintercept = gamma_boundaries[1], col=cols[2], lty = 2, lwd = 3) +
    geom_vline(xintercept = gamma_boundaries[2], col=cols[2], lty = 2, lwd = 3)+
    xlab(bquote(gamma))+ylab(bquote(phi))+export_theme1
  print(p1)
  jpeg(save_picture_name('samples'), units="in", width=16, height=12, res=300)
  print(p1)
  dev.off()
}

# How many NaNs ----

means = rowMeans(collected_data[,3:ncol(collected_data)])
mean(means > 0 | means < 0) 


# Calc KL Divergence and save to file -------

if(recalc_KL_main){
  
  real_dist = rnorm(10000 , 6,1)
  
  kl_to_real = function(vec){
    vec_1 = as.numeric(vec)
    return(KL.divergence(real_dist,vec_1,k=10)[10])
  }
  
  kl_from_real = function(vec){
    vec_1 = as.numeric(vec)
    return(KL.divergence(vec_1,real_dist,k=10)[10])
  }
  
  KL_vec = pbapply(collected_data[,3:ncol(collected_data)],MARGIN = 1,kl_to_real)
  
  KL_vec2 = pbapply(collected_data[,3:ncol(collected_data)],MARGIN = 1,kl_from_real)
  
  KL_dataframe <<- data.frame(Gamma = collected_data[,1],
                              Phi = collected_data[,2],
                              KL_real_gen = KL_vec,
                              KL_gen_real = KL_vec2)
  
  write.csv(x = KL_dataframe, 'KL_divergence.csv',row.names = F)
} else{
  KL_dataframe <<- read.csv('KL_divergence.csv', header=T)
}


# Bring in control data ------

controls = c('adam','momentum')

control_data_sets = list()
control_standardized_sets = list()
kl_controls = list()

control_data_sets[[1]] = collected_data[collected_data$Phi == 0,]
control_standardized_sets[[1]] = standardized_data[collected_data$Phi == 0,]
kl_controls[[1]] = KL_dataframe[KL_dataframe$Phi == 0,]


for(i in 2:3){
  
  
  data_path = paste0(c(main_folder,'/',controls[i-1]), collapse='')
  
  setwd(data_path)
  
  collected_data1 = fread('output.csv', header = F, sep = ',')
  collected_data1 = data.matrix(collected_data1)
  collected_data1 = na.omit(collected_data1)
  collected_data1= data.frame(Gamma =collected_data1[,1] ,
                              collected_data1[,2:ncol(collected_data1)])
  
  
  standardized_data1 = collected_data1
  standardized_data1[,2:ncol(standardized_data1)] = standardized_data1[,2:ncol(standardized_data1)] -6
  
  print(nrow(collected_data1))
  
  control_data_sets[[i]] = collected_data1
  control_standardized_sets[[i]] = standardized_data1
  
  
  # remove rows that didnt work
  
  if(recalc_KL_control){
    
    real_dist = rnorm(10000 , 6,1)
    
    kl_to_real = function(vec){
      vec_1 = as.numeric(vec)
      return(KL.divergence(real_dist,vec_1,k=10)[10])
    }
    
    kl_from_real = function(vec){
      vec_1 = as.numeric(vec)
      return(KL.divergence(vec_1,real_dist,k=10)[10])
    }
    
    KL_vec = pbapply(collected_data1[,2:ncol(collected_data1)],MARGIN = 1,kl_to_real)
    
    KL_vec2 = pbapply(collected_data1[,2:ncol(collected_data1)],MARGIN = 1,kl_from_real)
    
    KL_dataframe1 = data.frame(Gamma = collected_data1[,1],
                               Phi = collected_data1[,2],
                               KL_real_gen = KL_vec,
                               KL_gen_real = KL_vec2)
    
    kl_controls[[i]]  = KL_dataframe1
    
    write.csv(x = KL_dataframe1, 'KL_divergence.csv',row.names = F)
  } else{
    KL_dataframe1 = read.csv(paste0(c(data_path,'/KL_divergence.csv'), collapse = ''), header=T)
    kl_controls[[i]] = KL_dataframe1
  }
  
}


for(i in 2:3){
  plot(1:nrow(kl_controls[[i]]),kl_controls[[i]][,3])
  
}



# LOESS KL plot ------


theme.novpadding <- list(
  layout.heights = list(
    top.padding = 0,
    main.key.padding = 0,
    key.axis.padding = 0,
    axis.xlab.padding = 0,
    xlab.key.padding = 0,
    key.sub.padding = 0,
    bottom.padding = 0
  ),
  layout.widths = list(
    left.padding = 0,
    key.ylab.padding = 0,
    ylab.axis.padding = 0,
    axis.key.padding = 0,
    right.padding = 0
  ),
  box.3d = list(col=c(1,NA,NA,1,1,NA,1,1,1))
)


if(loess_plot_KL){
  KL_dataframe[,3:4] = -KL_dataframe[,3:4]
  
  size_of_grid = 80
  span1 = 0.5
  
  fit_loess = loess(KL_real_gen~Gamma*Phi,data =KL_dataframe, span = span1)
  
  g_p_grid = expand.grid(list(Gamma = seq(min(KL_dataframe$Gamma), max(KL_dataframe$Phi),
                                      length.out = size_of_grid), 
                              Phi = seq(min(KL_dataframe$Gamma),  max(KL_dataframe$Phi), 
                                      length.out = size_of_grid)))
  predicted_KL = predict(fit_loess, newdata = g_p_grid)
  
  new_data = melt(predicted_KL)
  new_data$Gamma = sapply(new_data$Gamma,function(x) as.numeric(gsub("Gamma=", "", x)))
  new_data$Phi = sapply(new_data$Phi,function(x) as.numeric(gsub("Phi=", "", x)))
  trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1))
  p1 =wireframe(value ~ Gamma * Phi, data = new_data, xlab = expression(gamma), par.settings = theme.novpadding,
                ylab = expression(phi), zlab = list('-Estimated KL Divergence',rot = 90), col = 'black',
                shade = F,pretty=T,scales = list(x =list(arrows=F),
                                                 y=list(arrows=F), 
                                                 z = list(arrows = F)),
                screen = list(z = -50, x = -60))
  print(p1)
  jpeg(save_picture_name('loess'), units="in", width=7, height=7, res=300)
  #trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1))
  print(p1)
  dev.off()
  # other KL divegence
  
  fit_loess = loess(KL_gen_real~Gamma*Phi,data =KL_dataframe, span = span1)
  
  g_p_grid = expand.grid(list(Gamma = seq(min(KL_dataframe$Gamma), max(KL_dataframe$Phi),
                                          length.out = size_of_grid), 
                              Phi = seq(min(KL_dataframe$Gamma),  max(KL_dataframe$Phi), 
                                        length.out = size_of_grid)))
  predicted_KL = predict(fit_loess, newdata = g_p_grid)
  
  new_data = melt(predicted_KL)
  new_data$Gamma = sapply(new_data$Gamma,function(x) as.numeric(gsub("Gamma=", "", x)))
  new_data$Phi = sapply(new_data$Phi,function(x) as.numeric(gsub("Phi=", "", x)))
  trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1))
  p1 =wireframe(value ~ Gamma * Phi, data = new_data, xlab = expression(gamma), par.settings = theme.novpadding,
                ylab = expression(phi), zlab = list('-Estimated KL Divergence',rot = 90), col = 'black',
                shade = F,pretty=T,scales = list(x =list(arrows=F),
                                                 y=list(arrows=F), 
                                                 z = list(arrows=F)),
                screen = list(z = -50, x = -60))
  print(p1)
  jpeg(save_picture_name('loess_gen_to_real'), units="in", width=7, height=7, res=300)
  trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1))
  print(p1)
  dev.off()
  KL_dataframe[,3:4] = -KL_dataframe[,3:4]
}  




# Split dataframe function -------
split_data_by_boundaries = function(df){
  
  panel_1 = df[df$Gamma<= gamma_boundaries[1] & 
                df$Phi == phi_boundaries[1],]
  
  panel_2 = df[gamma_boundaries[1]< df$Gamma &  
                             df$Gamma <= gamma_boundaries[2] &
                             df$Phi == phi_boundaries[1],]
  
  panel_3 = df[gamma_boundaries[2] < df$Gamma & 
                             df$Phi == phi_boundaries[1],]
  
  panel_4 = df[df$Gamma <= gamma_boundaries[1] & 
                             phi_boundaries[1] < df$Phi &
                             df$Phi <= phi_boundaries[2],]
  
  panel_5 = df[gamma_boundaries[1]< df$Gamma &  
                             df$Gamma <= gamma_boundaries[2] &
                             phi_boundaries[1] < df$Phi &
                             df$Phi <= phi_boundaries[2],]
  
  panel_6 = df[gamma_boundaries[2] < df$Gamma & 
                             phi_boundaries[1] < df$Phi &
                             df$Phi <= phi_boundaries[2],]
  
  panel_7 = df[df$Gamma <= gamma_boundaries[1] & 
                             phi_boundaries[2]< df$Phi,]
  
  panel_8 = df[gamma_boundaries[1]< df$Gamma &  
                             df$Gamma <= gamma_boundaries[2] &
                             phi_boundaries[2]< df$Phi,]
  
  panel_9 = df[gamma_boundaries[2] < df$Gamma &  
                             phi_boundaries[2]< df$Phi,]
  
  return(list(panel_1,panel_2,panel_3,panel_4,panel_5,panel_6,panel_7,panel_8,panel_9))
}

collected_split = split_data_by_boundaries(collected_data)
standardized_split = split_data_by_boundaries(standardized_data)
KL_split = split_data_by_boundaries(KL_dataframe)


# Find best in each panel ----
which_best_kl = function(df){
  return(which.min(as.numeric(df[,3])))
}
  
best_row_indexes = lapply(KL_split,which_best_kl)

# Titles for each panel -------

titles = list(bquote(gamma~ '<' ~.(round(gamma_boundaries[1],5))~', '
                     ~phi~'='~.(round(phi_boundaries[1],5))),
              bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'
                     ~.(round(gamma_boundaries[2],5))~', '
                     ~phi~'='~.(round(phi_boundaries[1],5))),
              bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~', '~phi~'='~
                       .(round(phi_boundaries[1],5))),
              bquote(gamma~'<'~.(round(gamma_boundaries[1],5))~', '
                     ~.(round(phi_boundaries[1],5))~'<'
                     ~phi~'<'~.(round(phi_boundaries[2],5))),
              bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'
                     ~.(round(gamma_boundaries[2],5))~', '~
                       .(round(phi_boundaries[1],5))~'<'~phi~'<'
                     ~.(round(phi_boundaries[2],5))),
              bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~', '~
                       .(round(phi_boundaries[1],5))~'<'~phi~'<'
                     ~.(round(phi_boundaries[2],5))),
              bquote(gamma~'<'~.(round(gamma_boundaries[1],5))~', '
                     ~.(round(phi_boundaries[2],5))~'<'~phi),
              bquote(.(round(gamma_boundaries[1],5))~'<'~gamma~'<'
                     ~.(round(gamma_boundaries[2],5))~
                       ', '~.(round(phi_boundaries[2],5))~'<'~phi),
              bquote(.(round(gamma_boundaries[2],5))~'<'~gamma~
                       ', '~.(round(phi_boundaries[2],5))~'<'~phi))


# Best Histogram ----


plot_individual_hist = function(vec, title = '', max_density = 0.5, theme_choice){
  p = ggplot(data.frame(x=vec), aes(x)) + 
    geom_histogram(fill = cols[2],aes(y = ..density..),bins = 40)+
    stat_function(fun = dnorm, args = list(mean = 6, sd = 1), lwd = 1, col = cols[1])+ 
    xlab('')+ ylab('')+theme_choice+
    labs(title=title)+
    ylim(0,max_density)+xlim(2,10)
  return(p)
}


if(plot_best_histogram_KL){ 
  
  best_hist_data = list()
  for(i in 1:9){
    best_hist_data[[i]] = as.numeric(collected_split[[i]][best_row_indexes[[i]],3:ncol(collected_split[[i]])])
  }
  
  for(i in 1:9){
    name = paste('p',i,sep = '')
    assign(name,plot_individual_hist(best_hist_data[[i]], titles[[i]],theme_choice = export_theme))
  }
  
  for(i in 1:9){
    name = paste('q',i,sep = '')
    assign(name,plot_individual_hist(best_hist_data[[i]], titles[[i]],theme_choice = rstudio_theme))
  }

  jpeg(save_picture_name('best_histograms_KL'), units="in", width=16, height=12, res=300)
  multiplot(p1, p2, p3, p4, p5, p6, p7, p8,p9, cols=3)
  dev.off()
  # multiplot(q1, q2, q3, q4, q5, q6, q7, q8,q9, cols=3)
}

# Best QQ plot -------


qqplot_individual <- function(vec,title = '',xlab ='Theoretical Quantiles',
                           ylab = 'Actual Quantiles', theme_choice = rstudio_theme){ 
  qq = data.frame(name = replicate(length(vec),paste('row.',i,sep='')),resids = vec)
  line_colour = cols[2]
  ggplot(qq) +
    stat_qq(aes(sample = resids, color = factor(name)))+
    geom_abline(slope = 1, intercept = 0, col = line_colour, lty = 2, lwd = 2)+
    theme(legend.position="none")+labs(title=title)+
    xlab(xlab)+ylab(ylab)+
    ylim(-3,3)+
    xlim(-3,3)+
    theme_choice
}

if(qqplot){ 
  
  best_qq_data = list()
  for(i in 1:9){
    best_qq_data[[i]] = as.numeric(standardized_split[[i]][best_row_indexes[[i]],3:ncol(standardized_split[[i]])])
  }

  for(i in 1:9){
    name = paste('p',i,sep = '')
    assign(name,qqplot_individual(best_qq_data[[i]], titles[[i]],xlab ='',ylab = '',theme_choice = export_theme))
  }
  
  for(i in 1:9){
    name = paste('q',i,sep = '')
    assign(name,qqplot_individual(best_qq_data[[i]], titles[[i]],xlab ='',ylab = '',theme_choice = rstudio_theme))
  }
  
  
  jpeg(save_picture_name('QQplot'), units="in", width=16, height=12, res=300)
  multiplot_QQ(p1, p2, p3, p4, p5, p6, p7, p8,p9, cols=3)
  dev.off()
  # multiplot_QQ(q1, q2, q3, q4, q5, q6, q7, q8,q9, cols=3)
}

# Median Histogram ----

which.median = function(x) {
  if (length(x) %% 2 != 0) {
    which(x == median(x))
  } else if (length(x) %% 2 == 0) {
    a = sort(x)[c(length(x)/2, length(x)/2+1)]
    which(x == a[1])
  }
}

median_of_third_col = function(df){
  return(which.median(df[,3]))
}

median_row_indexes = lapply(KL_split,median_of_third_col)

if(plot_med_histogram_KL){ 
  
  med_hist_data = list()
  for(i in 1:9){
    med_hist_data[[i]] = as.numeric(collected_split[[i]][median_row_indexes[[i]],3:ncol(collected_split[[i]])])
  }
  
  for(i in 1:9){
    name = paste('p',i,sep = '')
    assign(name,plot_individual_hist(med_hist_data[[i]], titles[[i]],theme_choice = export_theme))
  }
  
  for(i in 1:9){
    name = paste('q',i,sep = '')
    assign(name,plot_individual_hist(med_hist_data[[i]], titles[[i]],theme_choice = rstudio_theme))
  }
  
  jpeg(save_picture_name('med_histograms_KL'), units="in", width=16, height=12, res=300)
  multiplot(p1, p2, p3, p4, p5, p6, p7, p8,p9, cols=3)
  dev.off()
  # multiplot(q1, q2, q3, q4, q5, q6, q7, q8,q9, cols=3)
}

# Median QQ plot -------


if(qqplot_median){ 
  
  med_qq_data = list()
  for(i in 1:9){
    med_qq_data[[i]] = as.numeric(standardized_split[[i]][median_row_indexes[[i]],3:ncol(standardized_split[[i]])])
  }
  
  for(i in 1:9){
    name = paste('p',i,sep = '')
    assign(name,qqplot_individual(med_qq_data[[i]], titles[[i]],xlab ='',ylab = '',theme_choice = export_theme))
  }
  
  for(i in 1:9){
    name = paste('q',i,sep = '')
    assign(name,qqplot_individual(med_qq_data[[i]], titles[[i]],xlab ='',ylab = '',theme_choice = rstudio_theme))
  }
  
  
  jpeg(save_picture_name('QQplot_med'), units="in", width=16, height=12, res=300)
  multiplot_QQ(p1, p2, p3, p4, p5, p6, p7, p8,p9, cols=3)
  dev.off()
  # multiplot_QQ(q1, q2, q3, q4, q5, q6, q7, q8,q9, cols=3)
}


# Overall median hist ----



median_row = as.numeric(collected_data[which.median(KL_dataframe[,3]),
                                       3:ncol(collected_data)])

jpeg(save_picture_name('overall_median_hist'), units="in", width=10, height=7, res=300)
plot_individual_hist(median_row, max_density = 0.5, theme_choice = export_theme)
dev.off()

# Just phi = 0 hist ----

which.percentile = function(x, perc) {
  if(perc>1){
    perc = perc/100
  }
  index = as.integer(length(x)*perc)
  percentile = sort(x)[index]
  return(which(percentile == x))
}

median_row_phi_0 = as.numeric(collected_data[which.percentile(KL_dataframe[KL_dataframe$Phi == 0,3], 0.5),
                                       3:ncol(collected_data)])

jpeg(save_picture_name('hist_phi_0'), units="in", width=10, height=7, res=300)
plot_individual_hist(median_row_phi_0, max_density = 0.5, theme_choice = export_theme)
dev.off()


# 9 random QQ plots ----



if(qqplot_randoms){ 
  
  random_data = standardized_data[sample(1:nrow(standardized_data),size = 9, replace = F),]
  
  titles = list()
  for(i in 1:9){
    titles[[i]] = bquote(gamma~'='~.(random_data$Gamma[i])~', '
                        ~phi~'='~.(random_data$Phi[i]))
  }
  
  for(i in 1:9){
    name = paste('p',i,sep = '')
    assign(name,qqplot_individual(as.numeric(random_data[i,3:ncol(random_data)])
                                  , titles[[i]],xlab ='',ylab = '',theme_choice = export_theme))
  }
  
  
  jpeg(save_picture_name('QQplot_random'), units="in", width=16, height=12, res=300)
  multiplot_QQ(p1, p2, p3, p4, p5, p6, p7, p8,p9, cols=3)
  dev.off()
  # multiplot_QQ(q1, q2, q3, q4, q5, q6, q7, q8,q9, cols=3)
}




# find high and low points of smoothed surface ----



size_of_grid = 80
span1 = 0.5

fit_loess = loess(KL_real_gen~Gamma*Phi,data =KL_dataframe, span = span1)

g_p_grid = expand.grid(list(Gamma = seq(min(KL_dataframe$Gamma), max(KL_dataframe$Phi),
                                        length.out = size_of_grid), 
                            Phi = seq(min(KL_dataframe$Gamma),  max(KL_dataframe$Phi), 
                                      length.out = size_of_grid)))
predicted_KL = predict(fit_loess, newdata = g_p_grid)

predicted_KL1 = melt(predicted_KL)
predicted_KL1$Gamma = sapply(predicted_KL1$Gamma,function(x) as.numeric(gsub("Gamma=", "", x)))
predicted_KL1$Phi = sapply(predicted_KL1$Phi,function(x) as.numeric(gsub("Phi=", "", x)))

predicted_KL1 = na.omit(predicted_KL1)

best_phi = predicted_KL1[which.min(predicted_KL1[,3]),2]
worst_phi = predicted_KL1[which.max(predicted_KL1[,3]),2]

band_half_size = 0.005

best_band = KL_dataframe[KL_dataframe$Phi > best_phi - band_half_size &
                           KL_dataframe$Phi < best_phi + band_half_size, ]
worst_band = KL_dataframe[KL_dataframe$Phi > worst_phi - 2*band_half_size & 
                            KL_dataframe$Phi < worst_phi + band_half_size, ]


all_data_to_plot  = data.frame(Gamma = best_band[,1], variable = replicate(nrow(best_band),'Best')
                               , value =best_band[,3])

all_data_to_plot = rbind(all_data_to_plot, data.frame(Gamma = worst_band[,1], 
                                                      variable = replicate(nrow(worst_band),'Worst')
                                                      , value =worst_band[,3]))     

names = c('SGD', 'Adam','Momentum')
for(i in 1:3){
  dat = kl_controls[[i]]
  all_data_to_plot = rbind(all_data_to_plot, data.frame(Gamma = dat[,1], 
                                                        variable = replicate(nrow(dat),
                                                                             names[i])
                                                        , value =dat[,3]))                               
  
}



best_title = bquote('Score Adaptive,'~phi %~~% .(round(best_phi,2)))
worst_title = bquote('Score Adaptive, '~phi %~~% .(round(worst_phi,2)))
gamma_title = bquote(gamma)




export_theme2 = theme(text=element_text(size=30),
                      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
                      legend.key.size = unit(5, 'lines'))

jpeg(save_picture_name('compare_algos'), units="in", width = 14.4, height=9, res=300)

ggplot(all_data_to_plot,aes(x = Gamma, y = value, colour =variable )) +
  geom_smooth(aes(colour = variable),method = 'loess',se = F, lwd = 2)+
  ylab('Estimated KL Divergence')+
  xlab(gamma_title)+ export_theme2+
  scale_colour_discrete(name = 'Method',
                      breaks = c('Best','Worst','SGD','Adam','Momentum'),
                      labels = c(best_title,worst_title,'SGD','Adam','Momentum')) 

dev.off()

ggplot(all_data_to_plot,aes(x = Gamma, y = value, colour =variable )) +
  geom_smooth(aes(colour = variable),method = 'loess',se = F)+
  ylab('Estimated KL Divergence')+
  xlab(gamma_title)+
  scale_colour_discrete(name = 'Algorithm',
                        breaks = c('Best','Worst','SGD','Adam','Momentum'),
                        labels = c(best_title,worst_title,'SGD','Adam','Momentum')) 


# other divergence ----





size_of_grid = 80
span1 = 0.5

fit_loess = loess(KL_gen_real~Gamma*Phi,data =KL_dataframe, span = span1)

g_p_grid = expand.grid(list(Gamma = seq(min(KL_dataframe$Gamma), max(KL_dataframe$Phi),
                                        length.out = size_of_grid), 
                            Phi = seq(min(KL_dataframe$Gamma),  max(KL_dataframe$Phi), 
                                      length.out = size_of_grid)))
predicted_KL = predict(fit_loess, newdata = g_p_grid)

predicted_KL1 = melt(predicted_KL)
predicted_KL1$Gamma = sapply(predicted_KL1$Gamma,function(x) as.numeric(gsub("Gamma=", "", x)))
predicted_KL1$Phi = sapply(predicted_KL1$Phi,function(x) as.numeric(gsub("Phi=", "", x)))

predicted_KL1 = na.omit(predicted_KL1)

best_phi = predicted_KL1[which.min(predicted_KL1[,3]),2]
worst_phi = predicted_KL1[which.max(predicted_KL1[,3]),2]

band_half_size = 0.005

best_band = KL_dataframe[KL_dataframe$Phi > best_phi - band_half_size &
                           KL_dataframe$Phi < best_phi + band_half_size, ]
worst_band = KL_dataframe[KL_dataframe$Phi > worst_phi - 2*band_half_size & 
                            KL_dataframe$Phi < worst_phi + band_half_size, ]


all_data_to_plot  = data.frame(Gamma = best_band[,1], variable = replicate(nrow(best_band),'Best')
                               , value =best_band[,3])

all_data_to_plot = rbind(all_data_to_plot, data.frame(Gamma = worst_band[,1], 
                                                      variable = replicate(nrow(worst_band),'Worst')
                                                      , value =worst_band[,3]))     


names = c('SGD', 'Adam','Momentum')
for(i in 1:3){
  dat = kl_controls[[i]]
  all_data_to_plot = rbind(all_data_to_plot, data.frame(Gamma = dat[,1], 
                                                        variable = replicate(nrow(dat),
                                                                             names[i])
                                                        , value =dat[,4]))                               
  
}



best_title = bquote('Score Adaptive,'~phi %~~% .(round(best_phi,2)))
worst_title = bquote('Score Adaptive, '~phi %~~% .(round(worst_phi,2)))
gamma_title = bquote(gamma)



ggplot(all_data_to_plot,aes(x = Gamma, y = value, colour =variable )) +
  geom_smooth(aes(colour = variable),method = 'loess',se = F)+
  ylab('Estimated KL Divergence')+
  xlab(gamma_title)+
  scale_colour_discrete(name = '',
                        breaks = c('Best','Worst','SGD','Adam','Momentum'),
                        labels = c(best_title,worst_title,'SGD','Adam','Momentum')) 




# plot random hist -----

median_row_phi_0 = as.numeric(collected_data[sample(1:nrow(collected_data),1),
                                             3:ncol(collected_data)])

jpeg(save_picture_name('random_hist'), units="in", width=10, height=7, res=300)
plot_individual_hist(median_row_phi_0, max_density = 0.5, theme_choice = export_theme)
dev.off()
