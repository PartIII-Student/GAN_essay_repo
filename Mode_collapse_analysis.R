# Import -----
rm(list = ls(all=T))
wd = dirname(rstudioapi::getActiveDocumentContext()$path)
suppressMessages(suppressWarnings(library(reshape2)))
library(plyr)
suppressMessages(suppressWarnings(library(ggplot2)))
library(scatterplot3d)
suppressMessages(suppressWarnings(library(data.table)))
suppressMessages(suppressWarnings(library(plotly)))
library(lattice)
source('mulitplot.R')
library(entropy)
library(FNN)
library(pbapply)


# Objective ----
# Objective 1 is min log(1-D(G(z)), Objective 2 is max log(D(G(z))

# Colours ------------


export_theme = theme(text=element_text(size=20),
                     axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                     axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols = gg_color_hue(2)




save_picture_name = function(name){
  dir = paste(c(wd,'/plots_pictures/mode','_',name,'.jpeg'),
              collapse = '')
  return(dir)
}



# Max density -------
bin_number = 30
pull_max_density =function(vec){
  min_break = round_any(min(vec), diff(range(vec))/bin_number, floor)
  max_break = round_any(max(vec), diff(range(vec))/bin_number, ceiling)
  breaks = seq(min_break, max_break, diff(range(vec/bin_number)))
  histo = hist(vec, breaks=breaks, plot=F)
  return(max(histo$density))
}


# Actual density ----

mixture_density = function(x){
  return(dnorm(x,10,1)/2 + dnorm(x,0,1)/2)
}

# Data -------------------
dataframes = list()
total = 0

item = 1
for(objective in 1:2){
  for(minibatch in c('minibatch','no_minibatch')){
    folder = paste0(c(wd,'/mode_data/'), collapse = '')
    sub_folder = paste(c('objective_',objective,'_',minibatch,'/'), collapse = '')
    file = paste(c(folder,sub_folder,'output.csv'),collapse = '')
    df = data.frame(as.matrix(fread(file, header = F)))
    df = na.omit(df)
    dataframes[[item]] = df
    item =item+1
    total = total+nrow(df)
    print(nrow(df))
    rm(df)
  }
}

total

# K nearest neighbours ---------


length_vec = 10000
which_mean = sample(c(0,10),size = length_vec, replace =T)
real_dist = rnorm(n = length_vec,mean = which_mean,sd =1 )

kl_to_real = function(vec){
  vec_1 = as.numeric(vec)
  return(KL.divergence(real_dist,vec_1, k=5)[5])
}

kl_by_row = function(df){
  return(as.numeric(pbapply(df[,3:ncol(df)],1, kl_to_real)))
}


kl_rows = lapply(X = dataframes,FUN = kl_by_row)

which_min_kl_div = lapply(kl_rows,FUN = function(vec){return(which.min(vec))})

collected_rows_for_hist = list()
for(item in 1:4){
  collected_rows_for_hist[[item]] = as.numeric(dataframes[[item]][which_min_kl_div[[item]],3:ncol(dataframes[[item]])])
}

which.median = function(x) {
  if (length(x) %% 2 != 0) {
    which(x == median(x))
  } else if (length(x) %% 2 == 0) {
    a = sort(x)[c(length(x)/2, length(x)/2+1)]
    which(x == a[1])
  }
}

which_med_kl_div = lapply(kl_rows,which.median)

# Compare two median side by side general function ----

plot_median_side_by_side = function(index1,index2,title1,title2, y_max=0.5){
  collected_rows_for_hist_med = list()
  indice = c(index1,index2)
  for(item in indice){
    collected_rows_for_hist_med[[item]] = as.numeric(dataframes[[item]][which_med_kl_div[[item]],3:ncol(dataframes[[item]])])
  }
  
  p1 = ggplot(data.frame(x=collected_rows_for_hist_med[[index1]]), aes(x)) + 
    geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = mixture_density, lwd = 1, col = cols[1])+
    xlim(-5,15) + ylim(0,y_max)+ export_theme+
    labs(title=title1)+ylab('Density')
  
  p2 = ggplot(data.frame(x=collected_rows_for_hist_med[[index2]]), aes(x)) + 
    geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = mixture_density, lwd = 1, col = cols[1])+
    xlim(-5,15) + ylim(0,y_max)+ export_theme+
    labs(title=title2)+ylab('Density')
  
  return(suppressWarnings(multiplot(p1, p2, cols=2)))
  
}

plot_median_side_by_side(1,2,'Minibatch Discrimination','No Minibatch Discrimination') # oringial objective




jpeg(save_picture_name('mini_vs_non'), units="in", width=12, height=7, res=300)
plot_median_side_by_side(3,4,'Minibatch Discrimination','No Minibatch Discrimination') # alternative
dev.off()


dataframes[[2]] = dataframes[[2]][-1,]
kl_rows[[2]] = dataframes[[2]][-1,]

jpeg(save_picture_name('objective'), units="in", width=12, height=7, res=300)
plot_median_side_by_side(2,4,'min log(1-D(G(x))','max log(D(G(x))') # non minibatch
dev.off()


# Compare two best side by side general function ----

plot_best_side_by_side = function(index1,index2,title1,title2, y_max=0.5){
  collected_rows_for_hist = list()
  indice = c(index1,index2)
  for(item in indice){
    collected_rows_for_hist[[item]] = as.numeric(dataframes[[item]][which_min_kl_div[[item]],3:ncol(dataframes[[item]])])
  }
  
  p1 = ggplot(data.frame(x=collected_rows_for_hist[[index1]]), aes(x)) + 
    geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = mixture_density, lwd = 1, col = cols[1])+
    xlim(-5,15) + ylim(0,y_max)+
    labs(title=title1)+ylab('Density')
  
  p2 = ggplot(data.frame(x=collected_rows_for_hist[[index2]]), aes(x)) + 
    geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = mixture_density, lwd = 1, col = cols[1])+
    xlim(-5,15) + ylim(0,y_max)+
    labs(title=title2)+ylab('Density')
  
  suppressWarnings(multiplot(p1, p2, cols=2))
  
}

plot_best_side_by_side(1,2,'Minibatch Discrimination','No Minibatch Discrimination') # oringial objective
plot_best_side_by_side(3,4,'Minibatch Discrimination','No Minibatch Discrimination') # alternative
plot_best_side_by_side(2,4,'min log(1-D(G(x))','max log(D(G(x))') # non minibatch





# Pick out 75% quantile and plot ------
which.perc = function(x, perc) {
  if(perc>1){perc1 = perc/100}
  else{perc1 = perc}
  return(which(x == sort(x)[round(length(x)*perc1)]))
}


plot_single = function(data_index, data_list,title1 = '',y_max=0.5){
  bin_number = 40
  data = as.numeric(dataframes[[data_index]][data_list[[data_index]],3:ncol(dataframes[[data_index]])])
  ggplot(data.frame(x=data), aes(x)) + 
    geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = mixture_density, lwd = 1, col = cols[1])+
    xlim(-5,15) + ylim(0,y_max)+
    labs(title=title1)+ylab('Density')
}

which_25_kl_div = lapply(kl_rows,which.perc,perc = 0.75)

plot_single(2, which_25_kl_div) # oringinal, no minibatch

plot_single(4, which_25_kl_div) # new, no minibatch


# pick out tenth percentile

which_10_kl_div = lapply(kl_rows,which.perc,perc = 0.9)

plot_single(2, which_10_kl_div) # oringinal, no minibatch

plot_single(4, which_10_kl_div) # new, no minibatch

# pick out median

which_10_kl_div = lapply(kl_rows,which.perc,perc = 0.5)

plot_single(2, which_10_kl_div) # oringinal, no minibatch

plot_single(4, which_10_kl_div) # new, no minibatch




# Find proportions 

found_both = function(vec){
  vec1 = as.numeric(vec)
  mode1 = mean(-2<vec1 & vec1<2 )
  mode2 = mean(8<vec1 & vec1<12 )
  return(min(c(mode1 , mode2)))
}


sorted_dataframe = function(dat){
  data1 = dat[,2:nrow(dat)]
  fb = apply(data1,MARGIN = 1,found_both)
  data2 = cbind(fb,data1)
  data2 = data2[order(data2$fb),]
  return(data2)
}

sorted = lapply(dataframes, sorted_dataframe)

View(sorted[[4]])

print_sorted_row_number = function(row_number, dataset_index){
  data3 = as.numeric(sorted[[dataset_index]][row_number,2:ncol(data2)])
  ggplot(data.frame(x=data3), aes(x)) + 
    geom_histogram(fill = cols[2],aes(y = ..density..),bins = bin_number)+
    stat_function(fun = mixture_density, lwd = 1, col = cols[1])+
    xlim(-5,15) 
}

print_sorted_row_number(250,3) # at least 75% found both for minibatch
print_sorted_row_number(900,4) # less that 10% of cases


print_sorted_row_number(850,2) # less that 10% of cases


# so for minibatch at least 700 out of 1000 found both








# Find average KL by group -----
means = as.numeric(lapply(kl_rows, mean))
means = data.frame(original_objective = means[1:2], changed_objective = means[3:4]) 
rownames(means) = c('Minibatch','No minibatch') 
means

means[1,1] / means[1,2]


