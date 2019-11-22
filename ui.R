base <-read.csv("data/newdata.csv")
library(rmarkdown)
library(knitr)
library(kableExtra)
library(ROCR)
library(rmarkdown)
library(e1071)
library(ISLR)
library(DMwR)
library(DT)
library(tinytex)
library(shinythemes)
library(shinyBS)

navbarPage("BIENVENUE A VOUS !", theme = shinytheme("cosmo"),
           
           
           ###1ère  page de l'appli : rapport html
           
           tabPanel("Rapport",
                    includeCSS("README.html")
           ),
           
           
           ###2e  page de l'appli
           
                      
                      tabPanel("Visualiser les données",
                               DT::dataTableOutput("viewdata")
                              ),
           
           ###3e page 
           
                      tabPanel("La variable d'interêt",
                                  selectInput("Y",label="Variable d'interêt Class",choices=colnames(base),
                                              selected = "Class"
                                             ),
                               bsTooltip("Y", "Selectionner uniquement a variable Class",
                                         "right", trigger = "hover"),
                                 
                                 strong("Type de la Variable : Binaire"),
                                 
                                 br(),
                                 br(),
                                 
                                 strong("Modalité 0 : Cas de non fraude"),
                                 br(),
                                 br(),
                                 strong("Modalité 1 : Cas de fraude"),
                               
                      plotOutput("Yplot"), 
                      textOutput("no")
                               ),
           
           ## 4e page
           
           
                     tabPanel("Les variables explicatives",
                              selectInput("X",label="Variables explicatives",
                                              choices=colnames(base[,-31])
                                         ),
                     plotOutput("Xplot")
                             ),   
                      
        
           
           ### 5ème page 
           
           tabPanel("Implémentation des SVM",
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
                        numericInput("C", label=h5("Valeur du paramètre de coût C souhaitée"),
                                     value=10, min =0, max = 20000, step =1),
                        bsTooltip("C", "Paramètre Pour tout type de Kernel. Valeur par défaut : 1",
                                  placement ="bottom", trigger = "hover"),
                        
                        
                        numericInput("Deg", label=h5("Valeur du degré souhaitée"),
                                     value=3, min =0, max = 20000, step =1),
                        bsTooltip("Deg", "Paramètre Pour le Kernel Polynomial. Valeur par défaut : 3",
                                  placement ="bottom", trigger = "hover"),
                        
                      
                        numericInput("Gam", label=h5("Valeur du paramètre Gamma souhaitée"),
                                     value=0.1, min =0, max =20000, step =0.01),
                        bsTooltip("Gam", "Paramètre pour tout type de Kernel, excepté le Kernel Linéaire. Valeur par défaut : 1/(nombre de colonne de la base)",
                                  placement = "bottom", trigger = "hover"),
                        
                        
                        numericInput("Co", label=h5("Valeur du paramètre Coef0 souhaitée"),
                                     value=0.1, min =0, max = 20000, step =0.01),
                        bsTooltip("Co", "Paramètre pour les Kernels Polynomial et Sigmoïde. Valeur par défaut : 0", placement = "bottom", trigger = "hover"),
                        br(),
                        br(),
                        sliderInput("ech", label = "taille de l'échantillon d'apprentissage 
                                     souhaitée (%)", min = 0, 
                                    max = 100, value = 70),
                        bsTooltip("ech", "En général, entre 60 et 90%", placement = "bottom", 
                                  trigger = "hover")
                      ),
                      mainPanel(
                        verbatimTextOutput("svmSummary")
                      )
                    )
           ),
           
           ### 6ème page
           
           tabPanel("Performance des SVM",
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
                        numericInput("C2", label=h5("Valeur du paramètre de coût C souhaitée"),
                                     value=10, min =0, max = 20000, step =1),
                        bsTooltip("C2", "Paramètre Pour tout type de Kernel. Valeur par défaut : 1",
                                  placement ="bottom", trigger = "hover"),
                        
                        
                        numericInput("Deg2", label=h5("Valeur du degré souhaitée"),
                                     value=2, min =0, max = 20000, step =1),
                        bsTooltip("Deg2", "Paramètre Pour le Kernel Polynomial. Valeur par défaut : 3",
                                  placement ="bottom", trigger = "hover"),
                        
                        
                        numericInput("Gam2", label=h5("Valeur du paramètre Gamma souhaitée"),
                                     value=0.1, min =0, max = 20000, step =0.01),
                        bsTooltip("Gam2", "Paramètre pour tout type de Kernel, excepté le Kernel Linéaire. Valeur par défaut : 1/(nombre de colonne de la base)",
                                  placement = "bottom", trigger = "hover"),
                        
                        
                        numericInput("Co2", label=h5("Valeur du paramètre Coef0 souhaitée"),
                                     value=0.1, min =0, max = 20000, step =0.01),
                        bsTooltip("Co2", "Paramètre pour les Kernels Polynomial et Sigmoïde. Valeur par défaut : 0", 
                                  placement = "bottom", trigger = "hover"),
                        br(),
                        br(),
                        sliderInput("ech2", label = "taille de l'échantillon d'apprentissage
                                    souhaitée (%)", min = 0, 
                                    max = 100, value = 70),
                        bsTooltip("ech2", "En général, entre 60 et 90%. Le pourcentage restant servira à évaluer les performances des SVM"
                                  , placement = "bottom", 
                                  trigger = "hover")
                      ),
                      mainPanel(
                       plotOutput("ROCsvm"),
                       br(),
                       strong("matrice de confusion"),
                       verbatimTextOutput("conf1"),
                       br(),
                       strong("Taux de mauvaise classification sur la modalité d'interêt"),
                       verbatimTextOutput("T1"),
                       br(),
                       strong("Taux de mauvaise classification globale"),
                       verbatimTextOutput("TT1"),
                       br(),
                       strong("AUC"),
                       verbatimTextOutput("AUCsvm"),
                       br(),
                       strong("Indice de Gini"),
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
                        numericInput("C3", label=h5("Valeur du paramètre de coût C souhaitée"),
                                     value=10, min =0, max = 20000, step =1),
                        bsTooltip("C3", "Paramètre Pour tout type de Kernel. Valeur par défaut : 1",
                                  placement ="bottom", trigger = "hover"),
                        
                        
                        numericInput("Deg3", label=h5("Valeur du degré souhaitée"),
                                     value=2, min =0, max = 20000, step =1),
                        bsTooltip("Deg3", "Paramètre Pour le Kernel Polynomial. Valeur par défaut : 3",
                                  placement ="bottom", trigger = "hover"),
                        
                        
                        numericInput("Gam3", label=h5("Valeur du paramètre Gamma souhaitée"),
                                     value=0.1, min =0, max = 20000, step =0.01),
                        bsTooltip("Gam3", "Paramètre pour tout type de Kernel, excepté le Kernel Linéaire. Valeur par défaut : 1/(nombre de colonne de la base)",
                                  placement = "bottom", trigger = "hover"),
                        
                        
                        numericInput("Co3", label=h5("Valeur du paramètre Coef0 souhaitée"),
                                     value=0.1, min =0, max = 20000, step =0.01),
                        bsTooltip("Co3", "Paramètre pour les Kernels Polynomial et Sigmoïde. Valeur par défaut : 0", placement = "bottom", trigger = "hover"),
                        br(),
                        br(),
                        sliderInput("ech3", label = "taille de l'échantillon d'apprentissage
                                    souhaitée (%)", min = 0, 
                                    max = 100, value = 70),
                        bsTooltip("ech2", "En général, entre 60 et 90%. Le pourcentage restant servira à évaluer les performances des SVM", placement = "bottom", 
                                  trigger = "hover")
                      ),
                      mainPanel(
                    
                        plotOutput("ROCsvm2"),
                        br(),
                        strong("Performances du SVM"),
                        br(),
                        br(),
                        strong(h6("matrice de confusion")),
                        verbatimTextOutput("conf2"),
              
                        strong(h6("Taux de mauvaise classification sur la modalité d'intérêt")),
                        verbatimTextOutput("T2"),
                        
                        strong(h6("Taux de mauvaise classification globale")),
                        verbatimTextOutput("TT2"),
                        
                        strong(h6("AUC")),
                        verbatimTextOutput("AUCsvm2"),
                        
                        strong(h6("Indice de Gini")),
                        verbatimTextOutput("GINIsvm2"),
                        br(),
                        br(),
                        strong("Performances de la regression logistique"),
                        br(),
                        br(),
                        strong(h6("matrice de confusion")),
                        verbatimTextOutput("conf3"),
                        
                        strong(h6("Taux de mauvaise classification sur la modalité d'intérêt")),
                        verbatimTextOutput("T3"),
                        
                        strong(h6("Taux de mauvaise classification globale")),
                        verbatimTextOutput("TT3"),
                        
                        strong(h6("AUC")),
                        verbatimTextOutput("AUCglm"),
                        
                        strong(h6("Indice de Gini")),
                        verbatimTextOutput("GINIglm")
                      )
                    )
           )
           
)