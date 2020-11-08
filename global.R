library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(magrittr)
library(markdown)
library(ggplot2)
library(plotly)
library(DT)
library(rlang)
library(RColorBrewer)
library(lattice)
library(scales)
library(plyr)
library(dplyr)
# new
library(feather)
library(heatmaply)
library(ggthemes)

source('./code/data_convert/function_calc_SNR.R')
source('./code/data_convert/pre_snr.R')
source('./code/data_convert/mendel_ratio_SV_SNV_plotly.R')
# new
source('./code/data_convert/plotly_heatmap.R')
source('./code/data_convert/SNV_SV_quality.R')
##########################function####################
make_subbar_plotly <- function(batch_name, df_detect_gene){
  dt_c = df_detect_gene[n_rep == 1][, N1:=df_detect_gene[n_rep == 1][['N']]][, N2:=df_detect_gene[n_rep == 2][['N']]][, N3:=df_detect_gene[n_rep == 3][['N']]]
  print(dt_c)
  dt_b = dt_c[group == batch_name]
  fig = plot_ly(dt_b, x = ~sample, y = ~N3, type = 'bar', name = '3', color = '#1f77b4', alpha = 1) %>% 
    add_trace(x = ~sample, y = ~N2, name = '2', color = '#17becf', alpha = 0.5) %>%
    add_trace(x = ~sample, y = ~N1, name = '1', color = 'blue', alpha = 0.1) %>%
    layout(yaxis = list(title = 'Count'), barmode = 'stack', xaxis = list(title = batch_name))
  return(fig)
}


############################data#######################
#sample fpkm correlation
df_cor_fpkm <- readRDS(file = './data/rnaseq/qc_type/cross_batch/cor-fpkm.rds')

#exp annot
print('annot')
print(Sys.time())
df_exp_annot <- fread(file = './data/rnaseq/qc_type/exprSet_RNA_anno.txt', sep = '\t')
print(Sys.time())
pca_group <- colnames(df_exp_annot)[-c(1:5)]

#meta data
dt_meta <-  readRDS(file = './data/rnaseq/qc_type/metadata_RNA.rds')
main_group <- c(colnames(df_cor_fpkm)[-c(1, 2, 17, 18)], 'compare_group')

###################### snv_distri_plotly #################

snv_distri_plotly <- function(all_df,xfactor="DP",Xlab='Depth',axisxysize=13,
                              Margin=100
){
  p <- ggplot(all_df, aes(x=.data[[xfactor]],  color=tag, fill=tag)) + 
    geom_histogram( aes(y=..density..), 
                    alpha=0.5, bins=50,
                    position="identity")+
    #  scale_color_manual(values=c("#00bfc4","#f8766d"))+
    #  scale_fill_manual(values=c("#00bfc4","#f8766d"))+
    theme_light()+
    ylab('Density') + 
    xlab(Xlab)+
    labs(fill = " ")+
    theme(axis.text.x = element_text(size = axisxysize),
          axis.text.y = element_text(size = axisxysize),
          axis.title.x = element_text(size = axisxysize),
          axis.title.y = element_text(size = axisxysize)) 
  p <- ggplotly(p) %>% layout(margin=list(l=Margin,t=Margin,r=Margin,b=Margin))
  p$x$layout$annotations[[1]]$text <- ""
  p
}









