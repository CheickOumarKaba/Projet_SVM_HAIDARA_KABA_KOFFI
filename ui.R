base <-readRDS("data\\newdata.rds")
library(rmarkdown)
library(knitr)
library(DT)
library(kableExtra)

navbarPage("BIENVENU A VOUS !",
           
           
           ###1ère  page de l'appli : rapport html
           
           tabPanel("Rapport",
                    includeCSS("README.html")
           ),
           
           
           ###2e  page de l'appli
           
                      
                      tabPanel("Visualiser les données",
                               dataTableOutput("viewdata")
                              ),
           
           ###3e page 
           
                      tabPanel("La variable d'interêt",
                                  selectInput("Y",label="Variable d'intéret 
                                             (selectionner uniquement la variable Class)",
                                             selected="Class",choices=colnames(base)
                                             ),
                                 
                                 strong("Type de la Variable : Binaire"),
                                 
                                 br(),
                                 br(),
                                 
                                 strong("Modalité 0 : Cas de non fraude"),
                                 br(),
                                 br(),
                                 strong("Modalité 1 : Cas de fraude"),
                               
                      plotOutput("Yplot"),
                               ),
           
           ## 4e page
           
           
                     tabPanel("Les variables explicatives",
                              selectInput("X",label="Variables explicatives",
                                              choices=colnames(base[,-31])
                                         ),
                     plotOutput("Xplot")
                             ),   
                      
        
           
           ### 5ème page 
           
           tabPanel("Implémentation des SVM sur l'échantillon d'apprentissage",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("typekernel",label= "Type de Kernel",choices=c("Linéaire",
                                    "Polynomial","Radial","Sigmoïde",selected="Linéaire")
                        ),
                        br(),
                        br(),
                        strong("Sélection des hyper-paramètres"),
                        br(),
                        br(),
                        numericInput("C", label=h5("Valeur du paramètre de cout C souhaitée (entre 0 et
                                     200)"),
                                     value=10, min =0, max = 200, step =1),
                        
                        numericInput("Deg", label=h5("Valeur du degré souhaitée (entre 0 et 20)"),
                                     value=2, min =0, max = 20, step =1),
                      
                        numericInput("Gam", label=h5("Valeur du paramètre Gamma souhaitée (entre 0 et 20)"),
                                     value=0.1, min =0, max = 20, step =0.01),
                      
                        numericInput("Co", label=h5("Valeur du paramètre Coef0 souhaitée (entre 0 et 20)"),
                                     value=0.1, min =0, max = 20, step =0.01),
                        br(),
                        br(),
                        sliderInput("ech", label = "taille de l'échantillon d'apprentissage 
                                     souhaitée (%)", min = 0, 
                                    max = 100, value = 70)
                      ),
                      mainPanel(
                        verbatimTextOutput("svmSummary")
                      )
                    )
           ),
           
           ### 6ème page
           
           tabPanel("Performance des SVM sur l'échantillon test",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("typekernel2",label= "Type de Kernel",choices=c("Linéaire",
                                      "Polynomial","Radial","Sigmoïde",selected="Linéaire")
                        ),
                        br(),
                        br(),
                        strong("Sélection des hyper-paramètres"),
                        br(),
                        br(),
                        numericInput("C2", label=h5("Valeur du paramètre de cout C souhaitée (entre 0 et
                                     200)"),
                                     value=10, min =0, max = 200, step =1),
                        
                        numericInput("Deg2", label=h5("Valeur du degré souhaitée (entre 0 et 20)"),
                                     value=2, min =0, max = 20, step =1),
                        
                        numericInput("Gam2", label=h5("Valeur du paramètre Gamma souhaitée (entre 0 et 20)"),
                                     value=0.1, min =0, max = 20, step =0.01),
                        
                        numericInput("Co2", label=h5("Valeur du paramètre Coef0 souhaitée (entre 0 et 20)"),
                                     value=0.1, min =0, max = 20, step =0.01),
                        br(),
                        br(),
                        sliderInput("ech2", label = "taille de l'échantillon test 
                                     souhaitée (%)", min = 0, 
                                    max = 100, value = 30)
                      ),
                      mainPanel(
                       plotOutput("ROCsvm"),
                       br(),
                       strong("Valeur de l'AUC correspondante"),
                       verbatimTextOutput("AUCsvm"),
                       br(),
                       strong("Valeur de l'indice de Gini correspondant"),
                       verbatimTextOutput("GINIsvm")
                      )
                    )
           ),
           
           ### 7è page 
           
           tabPanel("Comparaison des performances des SVM à la Regression logistique",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("typekernel3",label= "Type de Kernel",choices=c("Linéaire",
                                      "Polynomial","Radial","Sigmoïde",selected="Linéaire")
                        ),
                        br(),
                        br(),
                        strong("Sélection des hyper-paramètres"),
                        br(),
                        br(),
                        numericInput("C3", label=h5("Valeur du paramètre de cout C souhaitée (entre 0 et
                                     200)"),
                                     value=10, min =0, max = 200, step =1),
                        
                        numericInput("Deg3", label=h5("Valeur du degré souhaitée (entre 0 et 20)"),
                                     value=2, min =0, max = 20, step =1),
                        
                        numericInput("Gam3", label=h5("Valeur du paramètre Gamma souhaitée (entre 0 et 20)"),
                                     value=0.1, min =0, max = 20, step =0.01),
                        
                        numericInput("Co3", label=h5("Valeur du paramètre Coef0 souhaitée (entre 0 et 20)"),
                                     value=0.1, min =0, max = 20, step =0.01),
                        br(),
                        br(),
                        sliderInput("ech3", label = "taille de l'échantillon test 
                                     souhaitée (%)", min = 0, 
                                    max = 100, value = 30)
                      ),
                      mainPanel(
                    
                        plotOutput("ROCsvm2"),
                        br(),
                        strong("Performances du SVM"),
                        br(),
                        strong(h6("Valeur de l'AUC correspondante")),
                        verbatimTextOutput("AUCsvm2"),
                        br(),
                        strong(h6("Valeur de l'indice de Gini correspondant")),
                        verbatimTextOutput("GINIsvm2"),
                        br(),
                        strong("Performances de la regression logistique"),
                        br(),
                        strong(h6("Valeur de l'AUC correspondante")),
                        verbatimTextOutput("AUCglm"),
                        br(),
                        strong(h6("Valeur de l'indice de Gini correspondant")),
                        verbatimTextOutput("GINIglm")
                      )
                    )
           )
           
)