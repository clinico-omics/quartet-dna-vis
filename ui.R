# ui.R

sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs", 
              #menuItem("Summary", tabName = "summary", icon = icon("table")), 
              #menuItem("Intra Batch", tabName = 'intra_batch', icon = icon("bar-chart-o"), selected = TRUE),
              #menuItem("Cross Batch", tabName = 'cross_batch', icon = icon("blackberry"), selected = TRUE),
              #menuItem("PCA", tabName = 'pca', icon = icon("grip-horizontal"), selected = TRUE),
              #menuItem("Reference Dataset", tabName = 'RD', icon = icon("table"), selected = TRUE),
              menuItem("Mendelian Violations", tabName = 'mendelian', icon = icon("angle-double-right"), selected = TRUE),
              menuItem("Variant Quality", tabName = 'variant_quality', icon = icon("angle-double-right"), selected = TRUE),
              menuItem("Difficult Genomic Regions", tabName = 'diff_region', icon = icon("angle-double-right"), selected = TRUE),
              menuItem("Variant Validation", tabName = 'variant_validation', icon = icon("angle-double-right"), selected = TRUE),
              menuItem("SV Reference", tabName = 'SV_Reference', icon = icon("angle-double-right"), selected = TRUE),
              menuItem("Performance Assessment", tabName = 'Performance_Assessment', icon = icon("angle-double-right"), selected = TRUE),
              menuItem("Variant Statistics", tabName = 'Variant_Statistics', icon = icon("angle-double-right"), selected = TRUE),
              menuItem("Jaccard Index", tabName = 'Jaccard_Index', icon = icon("angle-double-right"), selected = TRUE),
              menuItem("Quartet Advantage", tabName = 'Quartet_Advantage', icon = icon("angle-double-right"), selected = TRUE),
              menuItem("Small Variants Distribution", tabName = 'Small_Variants_Distribution', icon = icon("angle-double-right"), selected = TRUE),
              menuItem("Reference Datasets Summary", tabName = 'Reference_Datasets_Summary', icon = icon("angle-double-right"), selected = TRUE),
              menuItem("Large Deletion", tabName = 'Large_Deletion', icon = icon("angle-double-right"), selected = TRUE)
              
              )
  )
# icon:angle-double-right,pied-piper-square,lastfm-square,check-circle,resolving,thumbs-up,anchor,

contentHeight <- '600px'
body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    # Summary
    tabItem(tabName = "summary",
            box(width = NULL,
                br(), br(),
                DTOutput('summary'))),
    # Intra batch qc metrics
    tabItem(
      tabName = "intra_batch",
      fluidRow(
        column(width = 3,
               tabBox(width = NULL, 
                      tabPanel(h5("Parameters"),
                               selectInput("qc_type_intra",
                                           label = 'Intra QC Type',
                                           choices = c('Detected_Gene', 'CV', 'SNR'),
                                           selected = 'SNR'), 
                               selectInput("group_a_intra",
                                           label = 'Main Group',
                                           choices = c('batch', 'libraryPrep', 'kit', 'lab'),
                                           selected = 'batch'),
                               selectInput("barplot_r_legend_pos", 
                                           label = "Legend Position",
                                           choices = c('v', 'h'),
                                           selected = "v"),
                               selectInput("color_name_ji",
                                           label = "Color",
                                           choices = c('Set1', 'Set2', 'Set3',  'Paired', 'YlOrRd'),
                                           selected = "Set1"),
                               sliderInput("bar_plot_xy_tickfont", "X & Y Tick Size",
                                           min = 10, max = 30, value = 20, step = 1), 
                               sliderInput("bar_plot_titlefont", "X & Y Title Font Size",
                                           min = 10, max = 30, value = 20,  step = 1),
                               sliderInput("bar_plot_legend_labelsize", "Legend Labels Size",
                                           min = 10, max = 30, value = 20, step = 1),
                               sliderInput("bar_plot_margin", "Margin",
                                           min = 80, max = 200, value = 80, step = 5)))),
        column(width = 9,
               box(width = NULL, withSpinner(plotlyOutput("plot_intra_batch", height = contentHeight)))))),

    #cross batch qc metrics    
    tabItem(
      tabName = "cross_batch",
      fluidRow(
        column(width = 3,
               tabBox(width = NULL, 
                      tabPanel(h5("Parameters"),
                               selectInput("qc_type_cross",
                                           label = 'Cross Batch QC Type',
                                           choices = c('detect-JI', 'cor-fpkm', 'cor-fc', 'DEG-JI'),
                                           selected = 'cor-fpkm'),
                               selectInput("group_a_cross",
                                           label = 'Main Group',
                                           choices = main_group,
                                           selected = 'batch_type'),
                               selectInput("group_b",
                                           label = 'Sub Group',
                                           choices = main_group,
                                           selected = 'libraryPrepA'),
                               selectizeInput("group_b_unit",
                                              label = 'Element of Subgroup',
                                              multiple = T,
                                              choices = NULL),
                               sliderInput("cor_plot_xy_tickfont", "X & Y Tick Size",
                                           min = 10, max = 30, value = 20, step = 1), 
                               sliderInput("cor_plot_titlefont", "X & Y Title Font",
                                           min = 10, max = 30, value = 20,  step = 1),
                               sliderInput("cor_plot_legend_labelsize", "Legend Labels Size",
                                           min = 10, max = 30, value = 20, step = 1),
                               sliderInput("cor_plot_margin", "Margin",
                                           min = 0, max = 200, value = 0, step = 5)))),
        
        column(width = 9,
               box(width = NULL, withSpinner(plotlyOutput("plot_qc_metric", height = contentHeight)))))),
    
    
    
    #  Mendelian
    # tabItem(
    #   tabName = "mendelian",
    #   fluidRow(
    #     column(width = 3,
    #            tabBox(width = NULL, 
    #                   tabPanel(h5("Parameters"),
    #                            selectInput("Mendelian_Violations_Quality",
    #                                        label = 'Mendelian Violations Quality',
    #                                        choices = c('Low Quality', 'High Quality'),
    #                                        selected = 'Low Quality'),
    #                            
    #                            conditionalPanel("input.Mendelian_Violations_Quality == 'Low Quality'",
    #                                             selectInput("Mendelian_Violations_type1",
    #                                                         label = 'Mendelian Violations Type',
    #                                                         choices = c("All Quartet Sample","Spanning Deletions","Detectable Mendelian Violations"),
    #                                                         selected = "All Quartet Sample")
    #                            ),
    #                            conditionalPanel("input.Mendelian_Violations_Quality == 'Low Quality'",
    #                                             sliderInput("mendelian_plot_annotation_size", "Annotation Size",
    #                                                         min = 0, max = 10, value = 4,  step = 0.1)
    #                            ),
    #                            
    #                            conditionalPanel("input.Mendelian_Violations_Quality == 'High Quality'",
    #                                             selectInput("Mendelian_Violations_type2",
    #                                                         label = 'Mendelian Violations Type',
    #                                                         choices = c("Small Variants","Structural Variants"),
    #                                                         selected = "Small Variants")
    #                            ),
    #                            sliderInput("mendelian_plot_xy_size", "X & Y Tick Size",
    #                                        min = 1, max = 30, value = 11, step = 1), 
    #                            # sliderInput("mendelian_plot_annotation_size", "Annotation Size",
    #                            #             min = 0, max = 5, value = 2.5,  step = 0.1),
    #                            sliderInput("mendelian_plot_margin", "Margin",
    #                                        min = 0, max = 200, value = 30 , step = 5)))),
    #     
    #     column(width = 9,
    #            box(width = NULL, withSpinner(plotlyOutput("mendelian_plot", height = contentHeight)))))),
    # 
    
    #  Mendelian Change (add 4 figures)
    tabItem(
      tabName = "mendelian",
      fluidRow(
        column(width = 3,
               tabBox(width = NULL, 
                      tabPanel(h5("Parameters"),
                               
                               selectInput("Mendelian_Violations",
                                           label = 'Mendelian Violations',
                                           choices = c("Mendelian Violations Ratio", "Mendelian Violation Quality"),
                                           selected = "Mendelian Violations Ratio"),
                               conditionalPanel("input.Mendelian_Violations == 'Mendelian Violations Ratio'",
                                                selectInput("Mendelian_Violations_type3",
                                                            label = 'Mendelian Violations Type',
                                                            choices = c("SNV","Indel","Deletion","Insertion"),
                                                            selected = "SNV"),
                                                sliderInput("mendelian_title_size", "Title Size",
                                                            min = 0, max = 30, value = 15,  step = 1),
                                                sliderInput("mendelian_plot_figHeight", "Main Figure Height",
                                                            min = 0, max = 1, value = 0.5,  step = 0.1),
                                                sliderInput("mendelian_plot_figMargin", "Main Figure Margin",
                                                            min = 0, max = 1, value = 0.01,  step = 0.01),
                                                #mendelian_plot_Showlegend
                                                selectInput("mendelian_plot_Showlegend",
                                                            label = 'Show Legend',
                                                            choices=c("YES"=TRUE, "NO"=FALSE),
                                                            selected =TRUE) #,
                                     
                               ),
                               
                               conditionalPanel("input.Mendelian_Violations == 'Mendelian Violation Quality'",
                                                selectInput("Mendelian_Violations_Quality",
                                                            label = 'Mendelian Violations Quality',
                                                            choices = c('Low Quality', 'High Quality'),
                                                            selected = 'Low Quality'),
                                                conditionalPanel("input.Mendelian_Violations_Quality == 'Low Quality'",
                                                                 selectInput("Mendelian_Violations_type1",
                                                                             label = 'Mendelian Violations Type',
                                                                             choices = c("All Quartet Sample","Spanning Deletions","Detectable Mendelian Violations"),
                                                                             selected = "All Quartet Sample"),
                                                                 sliderInput("mendelian_plot_annotation_size", "Annotation Size",
                                                                             min = 0, max = 10, value = 4,  step = 0.1)
                                                ),
                                                conditionalPanel("input.Mendelian_Violations_Quality == 'High Quality'",
                                                                 selectInput("Mendelian_Violations_type2",
                                                                             label = 'Mendelian Violations Type',
                                                                             choices = c("Small Variants","Structural Variants"),
                                                                             selected = "Small Variants")
                                                ) #,
                                                
                               ),

                               sliderInput("mendelian_plot_xy_size", "X & Y Tick Size",
                                           min = 1, max = 30, value = 11, step = 1), 
                               # sliderInput("mendelian_plot_annotation_size", "Annotation Size",
                               #             min = 0, max = 5, value = 2.5,  step = 0.1),
                               sliderInput("mendelian_plot_margin", "Margin",
                                           min = 0, max = 200, value = 50 , step = 5)))),
        
        column(width = 9,
               box(width = NULL, withSpinner(plotlyOutput("mendelian_plot", height = contentHeight)))))),
    
    
    # Variant Quality
    tabItem(
      tabName = "variant_quality",
      fluidRow(
        column(width = 3,
               tabBox(width = NULL, 
                      tabPanel(h5("Parameters"),
                               selectInput("Variant_Quality_type",
                                           label = 'Variant Quality Type',
                                           choices = c('Small Variants', 'Structural Variants'),
                                           selected = 'Structural Variants'),
                               
                               conditionalPanel("input.Variant_Quality_type == 'Small Variants'",
                                                selectInput("Variant_Quality_distribution_type1",
                                                            label = 'Distribution Type',
                                                            choices = c("Depth","Allele Frequency","Genotype Quality","Mapping Quality"),
                                                            selected = "Depth")
                                                            ),
                               conditionalPanel("input.Variant_Quality_type == 'Structural Variants'",
                                                selectInput("Variant_Quality_distribution_type2",
                                                            label = 'Distribution Type',
                                                            choices = c("Allele Frequency","RE"),
                                                            selected = "Allele Frequency")
                               ),
                               selectInput("Variant_Quality_plot_his_type",
                                           label = 'Histogram Type',
                                           choices = c('identity','stack','dodge'),
                                           selected = 'identity'),
                               sliderInput("Variant_Quality_plot_his_height", "Histogram Height",
                                           min = 10, max = 200, value = 50, step = 10), 
                               sliderInput("Variant_Quality_plot_xy_size", "X & Y Tick Size",
                                           min = 1, max = 30, value = 13, step = 1), 
                               sliderInput("Variant_Quality_plot_margin", "Margin",
                                           min = 0, max = 200, value = 50 , step = 5)))),
        
        column(width = 9,
               box(width = NULL, withSpinner(plotlyOutput("Variant_Quality_plot", height = contentHeight)))))),
    
    
    # diff region
    tabItem(
      tabName = "diff_region",
      fluidRow(
        column(width = 3,
               tabBox(width = NULL, 
                      tabPanel(h5("Parameters"),
                               selectInput("diff_region_variant_type",
                                           label = 'Variant Type',
                                           choices = c('Small Variants', 'Structural Variants'),
                                           selected = 'Small Variants'),
                               # selectInput("Variant_Quality_plot_his_type",
                               #             label = 'Histogram type',
                               #             choices = c('identity','stack','dodge'),
                               #             selected = 'identity'),
                               sliderInput("diff_region_plot_subtitle_size", "Subtitle Size",
                                           min = 1, max = 30, value = 11, step = 1), 
                               sliderInput("diff_region_plot_xy_size", "X & Y Tick Size",
                                           min = 1, max = 30, value = 13, step = 1), 
                               sliderInput("diff_region_plot_margin", "Margin",
                                           min = 0, max = 200, value = 50 , step = 5)))),
        
        column(width = 9,
               box(width =NULL, withSpinner(plotlyOutput("diff_region_plot", width = "100%",height = contentHeight)))))),
      
   #variant_validation
    tabItem(
      tabName = "variant_validation",
      fluidRow(
        column(width = 3,
               tabBox(width = NULL, 
                      tabPanel(h5("Parameters"),
                               selectInput("variant_validation_type",
                                           label = 'Variant type',
                                           choices = c('Small Variant Validation', 'SV Validation'),
                                           selected = 'Small Variant Validation'),
                               sliderInput("variant_validation_subtitle_size", "Subtitle Size",
                                           min = 1, max = 30, value = 11, step = 1), 
                               sliderInput("variant_validation_xy_size", "X & Y Tick Size",
                                           min = 1, max = 30, value = 13, step = 1), 
                               sliderInput("variant_validation_margin", "Margin",
                                           min = 0, max = 200, value = 50 , step = 5)))),
        
        column(width = 9,
               box(width =NULL, withSpinner(plotlyOutput("variant_validation_plot", width = "100%",height = contentHeight)))))),
    
   # SV_Reference
   tabItem(
     tabName = "SV_Reference",
     fluidRow(
       column(width = 3,
              tabBox(width = NULL, 
                     tabPanel(h5("Parameters"),
                              selectInput("SV_Reference_type",
                                          label = 'SV Reference Type',
                                          choices = c('Summary', 'Detail'),
                                          selected = 'Summary'),
                              textInput("SV_Reference_legend_title", "Legend Title", value = "Type"),
                              sliderInput("SV_Reference_xy_size", "X & Y Tick Size",
                                          min = 1, max = 30, value = 13, step = 1), 
                              sliderInput("SV_Reference_margin", "Margin",
                                          min = 0, max = 200, value = 50 , step = 5)))),
       
       column(width = 9,
              box(width =NULL, withSpinner(plotlyOutput("SV_Reference_plot", width = "100%",height = contentHeight)))))),
    
    

   # Performance_Assessment
   tabItem(
     tabName = "Performance_Assessment",
     fluidRow(
       column(width = 3,
              tabBox(width = NULL, 
                     tabPanel(h5("Parameters"),
                              selectInput("PA_variant_type",
                                          label = 'Variant Type',
                                          choices = c('SNV', 'SV'),
                                          selected = 'SNV'),
                              selectInput("PA_Performance",
                                          label = 'Performance',
                                          choices = c('Precision', 'Reproducibility'),
                                          selected = 'Precision'),
                              #textInput("PA_Performance", "Legend_Title", value = "Type"),
                              sliderInput("PA_porportion", "Figure Height",
                                          min = 0, max = 1, value = 0.2, step = 0.1), 
                              sliderInput("PA_margin", "Margin",
                                          min = 0, max = 200, value = 50 , step = 5)))),
       
       column(width = 9,
              box(width =NULL, withSpinner(plotlyOutput("Performance_Assessment_plot", width = "100%",height = contentHeight)))))),
   
   #Variant_Statistics
   tabItem(
     tabName = "Variant_Statistics",
     fluidRow(
       column(width = 3,
              tabBox(width = NULL, 
                     tabPanel(h5("Parameters"),
                              selectInput("Variant_Statistics_type",
                                          label = 'Variant Type',
                                          choices = c('SNV', 'SV'),
                                          selected = 'SNV'),
                              selectInput("Variant_Statistics_showlegend",
                                          label = 'Show Legend',
                                          choices=c("YES"=TRUE, "NO"=FALSE),
                                          selected =FALSE),
                              sliderInput("Variant_Statistics_figH", "Figure Height",
                                          min = 0, max = 1, value = 0.8, step = 0.1), 
                              sliderInput("Variant_Statistics_yaxisfont", "Yaxis Font",
                                          min = 0, max = 30, value = 15, step = 1),
                              sliderInput("Variant_Statistics_fig_margin", "Figure Margin",
                                          min = 0, max = 0.1, value = 0, step = 0.01),
                              sliderInput("Variant_Statistics_margin", "Margin",
                                          min = 0, max = 200, value = 50 , step = 5)))),
       
       column(width = 9,
              box(width =NULL, withSpinner(plotlyOutput("Variant_Statistics_plot", width = "100%",height = contentHeight)))))),
   
   
   #Jaccard_Index
   tabItem(
     tabName = "Jaccard_Index",
     fluidRow(
       column(width = 3,
              tabBox(width = NULL, 
                     tabPanel(h5("Parameters"),
                              selectInput("Jaccard_Index_type",
                                          label = 'Variant Type',
                                          choices = c('SNV', 'INDEL',
                                                      'Insertion','Deletion','Duplication','Inversion','Breakend'),
                                          selected = 'SNV'),
                              textInput("Jaccard_Index_legend_title", "Legend Title", value = "Jaccard Index"),
                              sliderInput("Jaccard_Index_margin", "Margin",
                                          min = 0, max = 200, value = 100 , step = 5)))),
       
       column(width = 9,
              box(width =NULL, withSpinner(plotlyOutput("Jaccard_Index_plot",height = contentHeight)))))),
   
   #Quartet_Advantage
   tabItem(
     tabName = "Quartet_Advantage",
     fluidRow(
       column(width = 3,
              tabBox(width = NULL, 
                     tabPanel(h5("Parameters"),
                              selectInput("Quartet_Advantage_type",
                                          label = 'Variant Type',
                                          choices = c('Small variants', 'Structural variants'),
                                          selected = 'Small variants'),
                              textInput("Jaccard_Index_legend_title", "Legend Title", value = "Jaccard Index"),
                              sliderInput("Quartet_Advantage_xy_size", "X & Y Tick Size",
                                          min = 0, max = 20, value = 11 , step = 1),
                              sliderInput("Quartet_Advantage_title_size", "Title Size",
                                          min = 0, max = 20, value = 14 , step = 1),
                              sliderInput("Quartet_Advantage_margin", "Margin",
                                          min = 0, max = 200, value = 120 , step = 5)
                              ))),
       
       column(width = 9,
              box(width =NULL, withSpinner(plotlyOutput("Quartet_Advantage_plot",height = contentHeight)))))),
   
   #Small_Variants_Distribution
   tabItem(
     tabName = "Small_Variants_Distribution",
     fluidRow(
       column(width = 3,
              tabBox(width = NULL, 
                     tabPanel(h5("Parameters"),
                              selectInput("Small_Variants_Distribution_type",
                                          label = 'Distribution Type',
                                          choices = c("Depth","Allele Frequency","Genotype Quality","Mapping Quality"),
                                          selected = "Depth"),
                              sliderInput("Small_Variants_Distribution_xy_size", "X & Y Tick Size",
                                          min = 0, max = 20, value = 11 , step = 1),
                              sliderInput("Small_Variants_Distribution_margin", "Margin",
                                          min = 0, max = 300, value = 100 , step = 5)
                     ))),
       
       column(width = 9,
              box(width =NULL, withSpinner(plotlyOutput("Small_Variants_Distribution_plot",height = contentHeight)))))),
 
   
   #Reference_Datasets_Summary
   tabItem(
     tabName = "Reference_Datasets_Summary",
     fluidRow(
       column(width = 3,
              tabBox(width = NULL, 
                     tabPanel(h5("Parameters"),
                              selectInput("Reference_Datasets_Summary_type",
                                          label = 'Variant Type',
                                          choices = c('Small Variants', 'Structural Variants'),
                                          selected = 'Small Variants'),
                              selectInput("Reference_Datasets_Summary_showlegend",
                                          label = 'Show Legend',
                                          choices=c("YES"=TRUE, "NO"=FALSE),
                                          selected =FALSE),
                              sliderInput("Reference_Datasets_Summary_figH", "Figure Height",
                                          min = 0, max = 1, value = 0.8, step = 0.1), 
                              sliderInput("Reference_Datasets_Summary_yaxisfont", "Yaxis Font",
                                          min = 0, max = 30, value = 15, step = 1),
                              sliderInput("Reference_Datasets_Summary_fig_margin", "Figure Margin",
                                          min = 0, max = 0.1, value = 0, step = 0.01),
                              sliderInput("Reference_Datasets_Summary_margin", "Margin",
                                          min = 0, max = 200, value = 50 , step = 5)))),
       
       column(width = 9,
              box(width =NULL, withSpinner(plotlyOutput("Reference_Datasets_Summary_plot", width = "100%",height = contentHeight)))))),
   
   
   #Large_Deletion
   tabItem(
     tabName = "Large_Deletion",
     fluidRow(
       column(width = 3,
              tabBox(width = NULL, 
                     tabPanel(h5("Parameters"),
                              selectInput("Large_Deletion_Variant_type",
                                          label = 'Variant Type',
                                          choices = c('Insertion', 'Deletion'),
                                          selected = 'Insertion'),
                              selectInput("Large_Deletion_show_type",
                                          label = 'Show Type',
                                          choices=c("Variant Number", "Filtered Variant"),
                                          selected ="Filtered Variant"),
                              sliderInput("Large_Deletion_xy_size", "X & Y Tick Size",
                                          min = 0, max = 30, value = 15, step = 1),
                              sliderInput("Large_Deletion_margin", "Margin",
                                          min = 0, max = 200, value = 50 , step = 5)))),
       
       column(width = 9,
              box(width =NULL, withSpinner(plotlyOutput("Large_Deletion_plot", width = "100%",height = contentHeight)))))),
   

   ## add new

    # PCA SNR
    tabItem(
      tabName = "pca",
      fluidRow(
        column(width = 3,
               tabBox(width = NULL,
                      tabPanel(h5("Parameters"),
                               materialSwitch(inputId = "d2_d3_switch", label = '2D/3D', status = 'success'),
                               selectInput("pca_zscore",
                                           label = 'ZScore',
                                           choices = c('YES', 'NO'),
                                           selected = 'NO'),
                               selectInput("group_pca",
                                           label = 'Main Group',
                                           choices = pca_group,
                                           selected = 'batch'),
                               selectizeInput("group_pca_unit",
                                              label = 'Element of Main Group',
                                              multiple = T,
                                              choices = NULL),
                               selectInput("pca_r_legend_pos",
                                           label = "Legend Position",
                                           choices = c('v', 'h'),
                                           selected = "v"),
                               sliderInput("pca_plot_xy_tickfont", "X & Y Tick Size",
                                           min = 10, max = 30, value = 20, step = 1),
                               sliderInput("pca_plot_titlefont", "X & Y Title Font",
                                           min = 10, max = 30, value = 20,  step = 1),
                               sliderInput("pca_plot_legend_labelsize", "Legend Labels Size",
                                           min = 10, max = 30, value = 20, step = 1),
                               sliderInput("pca_plot_margin", "Margin",
                                           min = 0, max = 200, value = 80, step = 5)))),

        column(width = 9, box(width = NULL, withSpinner(plotlyOutput("plot_pca", height = contentHeight)))))), 
    
    # Reference dataset
    tabItem(
      tabName = "RD",
      fluidRow(
        column(width = 3,
               tabBox(width = NULL, 
                      tabPanel(h5("Parameters"),
                               selectInput("data_type",
                                           label = 'Data Type',
                                           choices = c('Detected_Gene_Distribution', 'Protocols_Consensus', 'Reference_Data_Gene_Type', 
                                                       'Reference_dataset_Number_DEG', 'Reference_Data_DEG_Distribution', 
                                                       'Performance_Evaluation_Gene_Detection', 'Performance_Evaluation_Reletive_Expression', 
                                                       'Performance_Evaluation_DEG', 'QC_Metrics_Correlation'),
                                           selected = 'QC_Metrics_Correlation'), 
                               selectInput("barplot_r_legend_pos", 
                                           label = "Legend Position",
                                           choices = c('v', 'h'),
                                           selected = "v"),
                               selectInput("color_name_ji",
                                           label = "Color",
                                           choices = c('Set1', 'Set2', 'Set3',  'Paired', 'YlOrRd'),
                                           selected = "Set1"),
                               sliderInput("bar_plot_xy_tickfont", "X & Y Tick Size",
                                           min = 10, max = 30, value = 20, step = 1), 
                               sliderInput("bar_plot_titlefont", "X & Y Title Font",
                                           min = 10, max = 30, value = 20,  step = 1),
                               sliderInput("bar_plot_legend_labelsize", "Legend Labels Size",
                                           min = 10, max = 30, value = 20, step = 1),
                               sliderInput("bar_plot_margin", "Margin",
                                           min = 80, max = 200, value = 80, step = 5)))),
        
        column(width = 9,
               box(width = NULL, withSpinner(plotlyOutput("plot_RD", height = contentHeight))))))
    
    
    ))








dashboardPage(
  dashboardHeader(title = "Quartet Vis"),
  sidebar,
  body
)
