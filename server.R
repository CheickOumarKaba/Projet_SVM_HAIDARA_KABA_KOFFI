library(shiny)
library(e1071)
library(ROCR)
library(ISLR)
library(caret)
library(DMwR)
library(DT)
library(kableExtra)
library(knitr)
library(rmarkdown)

base <-readRDS("data\\newdata.rds")


attach(base)



function(input, output, session) {
  
  ###1ère page
  
  #output$down <- downloadHandler({"README.html"})
    
  
  ####2eme page 
  
  ## visualiser la table 
  output$viewdata <- renderDataTable({datatable(base)})
  
  ## plot variable d'intéret 
  output$Yplot <- renderPlot({plot(base[,input$Y],main="Répartition des modalités de Class",col="Gray")})
  
  ## plot variable explicative en fonction de classe
  output$Xplot <- renderPlot({boxplot(base[,input$X]~base$Class,ylab=NULL,xlab=NULL,col="gray",
                               main= "Représentation graphique de la variable explicative sélectionnée en fonction des modalités de Class")})
  

####3ème page
  
  output$svmSummary <-renderPrint({
    
    ## définition échantillon d'app/test 
    
    set.seed(1234)
    train <- sample(1:nrow(base),nrow(base)*input$ech/100)
    data.train=base[train,]
    data.test=base[-train,]
    class.test=base$Class[-train]
    
    ### fonction if() pour les différents choix de kernels
              
             ## kernel lineaire
    
               if(input$typekernel=="Linéaire"){
                 svm.fit <- svm(Class ~ . , data=data.train,
                                kernel="linear",probability=T,cost=input$C)
                 print(summary(svm.fit))
               }
    
            ## kernel ploynomial
    
             else if (input$typekernel=="Polynomial"){
               svm.fit <- svm(Class ~ . , data=data.train,
                              kernel="polynomial",probability=T,cost=input$C,degree=input$Deg,
                               gamma=input$Gam,coef0=input$Co)
               print(summary(svm.fit))
             }
    
          ## kernel radial
    
           else if (input$typekernel=="Radial"){
             svm.fit <- svm(Class ~ . , data=data.train,
                         kernel="radial",probability=T,cost=input$C,gamma=input$Gam)
             print(summary(svm.fit))
           }
    
        ## kernel sigmoide
    
         else if (input$typekernel=="Sigmoïde"){
            svm.fit <- svm(Class ~ . , data=data.train,
                       kernel="sigmoid",probability=T,cost=input$C,gamma=input$Gam,coef0=input$Co)
            print(summary(svm.fit))
           }
    
  })
  
  #### 4e page 
  
  output$ROCsvm <-renderPlot({
    
    ### echantillon d'app/test
    
    set.seed(1234)
    train <- sample(1:nrow(base),nrow(base)*input$ech2/100)
    data.train=base[train,]
    data.test=base[-train,]
    class.test=base$Class[-train]
    
    ## si choix kernel linéaire
    
    if(input$typekernel2=="Linéaire"){
      
      ## SVM linéaire
      
      svm.fit <- svm(Class ~ . , data=data.train,
                     kernel="linear",probability=T,cost=input$C2)
      
      # préditiction pour courbe ROC
      
      pred.svm <- predict(svm.fit,data.test[,-31],probability = T)
      svm.fit.probs=attributes(pred.svm)$probabilities[,"1"]
      svm.fit.pred=prediction(svm.fit.probs,class.test)
      perf.svm <- performance(svm.fit.pred,"tpr","fpr")
      
      # courbe ROC
      
      plot(perf.svm,col="green")
      abline(0,1,col="black")
      
      #AUC
      
      output$AUCsvm <-renderPrint({
      auc.roc <- performance(svm.fit.pred,"auc")
      auc1=auc.roc@y.values
      print(auc1)
      })
      
      # Gini
      
      output$GINIsvm <-renderPrint({
      auc.roc2 <- performance(svm.fit.pred,"auc")
      auc2=as.numeric(auc.roc2@y.values)
      gini=2*auc2-1
      print(gini)
      })
      
    }
    
    ## si choix kernel poly
    
    else if(input$typekernel2=="Polynomial"){
      
      # SVM poly
      
      svm.fit <- svm(Class ~ . , data=data.train,
                     kernel="polynomial",probability=T,cost=input$C2,degree=input$Deg2,
                     gamma=input$Gam2,coef0=input$Co2)
      
      # prediction ROC
      
      pred.svm <- predict(svm.fit,data.test[,-31],probability = T)
      svm.fit.probs=attributes(pred.svm)$probabilities[,"1"]
      svm.fit.pred=prediction(svm.fit.probs,class.test)
      perf.svm <- performance(svm.fit.pred,"tpr","fpr")
      
      # courbe ROC
      
      plot(perf.svm,col="yellow")
      abline(0,1,col="black")
    
      #AUC
      
      output$AUCsvm <-renderPrint({
        auc.roc <- performance(svm.fit.pred,"auc")
        auc1=auc.roc@y.values
        print(auc1)
      })
      
      #gini 
      
      output$GINIsvm <-renderPrint({
        auc.roc2 <- performance(svm.fit.pred,"auc")
        auc2=as.numeric(auc.roc2@y.values)
        gini=2*auc2-1
        print(gini)
      })
      
    }
    
    ## si choix kernel Radial
    
    else if(input$typekernel2=="Radial"){
      
      # SVM radial
      svm.fit <- svm(Class ~ . , data=data.train,
                     kernel="radial",probability=T,cost=input$C2,gamma=input$Gam2)
      
      # pred pour courbe ROC
      
      pred.svm <- predict(svm.fit,data.test[,-31],probability = T)
      svm.fit.probs=attributes(pred.svm)$probabilities[,"1"]
      svm.fit.pred=prediction(svm.fit.probs,class.test)
      perf.svm <- performance(svm.fit.pred,"tpr","fpr")
      
      #courbe ROC
      
      plot(perf.svm,col="red")
      abline(0,1,col="black")
      
      #AUC
      
      output$AUCsvm <-renderPrint({
        auc.roc <- performance(svm.fit.pred,"auc")
        auc1=auc.roc@y.values
        print(auc1)
      })
      
      #gini
      
      output$GINIsvm <-renderPrint({
        auc.roc2 <- performance(svm.fit.pred,"auc")
        auc2=as.numeric(auc.roc2@y.values)
        gini=2*auc2-1
        print(gini)
      })
      
    }
    
    ## si choix Kernel sigmoide
    
    else if(input$typekernel2=="Sigmoïde"){
      
      #SVM sigmoide
      
      svm.fit <- svm (Class ~ . , data=data.train,
                    kernel="sigmoid",probability=T,cost=input$C2,gamma=input$Gam2,coef0=input$Co2)
      
      #prediction courbe ROC
      
      pred.svm <- predict(svm.fit,data.test[,-31],probability = T)
      svm.fit.probs=attributes(pred.svm)$probabilities[,"1"]
      svm.fit.pred=prediction(svm.fit.probs,class.test)
      perf.svm <- performance(svm.fit.pred,"tpr","fpr")
      
      #courbe ROC
      
      plot(perf.svm,col="gray")
      abline(0,1,col="black")
      
      #AUC
      
      output$AUCsvm <-renderPrint({
        auc.roc <- performance(svm.fit.pred,"auc")
        auc1=auc.roc@y.values
        print(auc1)
      })
      
      #Gini
      
      output$GINIsvm <-renderPrint({
        auc.roc2 <- performance(svm.fit.pred,"auc")
        auc2=as.numeric(auc.roc2@y.values)
        gini=2*auc2-1
        print(gini)
      })
      
    }
       
  })
  
##### 5e page
  
  
  output$ROCsvm2 <-renderPlot({
    
    ## echantillon d'app/test
    
    set.seed(1234)
    train <- sample(1:nrow(base),nrow(base)*input$ech3/100)
    data.train=base[train,]
    data.test=base[-train,]
    class.test=base$Class[-train]
    
    ## si choix kernel lineaire
    
    if(input$typekernel3=="Linéaire"){
      
      # SVM lineaire/prevision courbe ROC
      
      svm.fit.comp <- svm(Class ~ . , data=data.train,
                     kernel="linear",probability=T,cost=input$C3)
      pred.svm.comp <- predict(svm.fit.comp,data.test[,-31],probability = T)
      svm.fit.probs.comp=attributes(pred.svm.comp)$probabilities[,"1"]
      svm.fit.pred.comp=prediction(svm.fit.probs.comp,class.test)
      perf.svm.comp <- performance(svm.fit.pred.comp,"tpr","fpr")
      
      # glm/ prevision courbe ROC
      
      glm.fit <-glm(Class ~ ., data=base,subset=train,family=binomial)
      glm.probs <- predict(glm.fit,data.test,type="response")
      glm.fit.roc=prediction(glm.probs,class.test)
      perf.glm.roc <- performance(glm.fit.roc,"tpr","fpr")
      
      # courbe ROC SVM lineaire/glm
      
      plot(perf.svm.comp,col="green")
      plot(perf.glm.roc,col="blue",add=T)
      legend("bottomright", legend=c("Kernel linéaire", "Regression logistique"),
             col=c("green","blue"), lty=1:2, cex=0.8)
      abline(0,1,col="black")
      
      # AUC svm lineaire
      
      output$AUCsvm2 <-renderPrint({
        auc.roc.comp <- performance(svm.fit.pred.comp,"auc")
        auc.comp=auc.roc.comp@y.values
        print(auc.comp)
      })
      
      # Gini svm lineaire
      
      output$GINIsvm2 <-renderPrint({
        auc.roc.comp2 <- performance(svm.fit.pred.comp,"auc")
        auc.comp2=as.numeric(auc.roc.comp2@y.values)
        gini.comp=2*auc.comp2-1
        print(gini.comp)
      })

      # AUC glm
      
      output$AUCglm <-renderPrint({
        auc.roc.glm <- performance(glm.fit.roc,"auc")
        auc.glm=as.numeric(auc.roc.glm@y.values)
        print(auc.glm)
      })
      
      # gini glm
      
      output$GINIglm <-renderPrint({
        auc.roc.glm2 <- performance(glm.fit.roc,"auc")
        auc.glm2=as.numeric(auc.roc.glm2@y.values)
        gini.glm=2*auc.glm2-1
        print(gini.glm)
      })
      
    }
   
    ## si choix kernel polynomial
    
    else if(input$typekernel3=="Polynomial"){
      
      #svm poly/prevision courbe ROC
      
      svm.fit.comp <- svm(Class ~ . , data=data.train,
                          kernel="polynomial",probability=T,cost=input$C3,degree=input$Deg3,
                          gamma=input$Gam3,coef0=input$Co3)
      pred.svm.comp <- predict(svm.fit.comp,data.test[,-31],probability = T)
      svm.fit.probs.comp=attributes(pred.svm.comp)$probabilities[,"1"]
      svm.fit.pred.comp=prediction(svm.fit.probs.comp,class.test)
      perf.svm.comp <- performance(svm.fit.pred.comp,"tpr","fpr")
      
      #glm/prevision ROC
      
      glm.fit <-glm(Class ~ ., data=base,subset=train,family=binomial)
      glm.probs <- predict(glm.fit,data.test,type="response")
      glm.fit.roc=prediction(glm.probs,class.test)
      perf.glm.roc <- performance(glm.fit.roc,"tpr","fpr")
      
      # courbe ROC svmpoly/glm
      
      plot(perf.svm.comp,col="yellow")
      plot(perf.glm.roc,col="blue",add=T)
      legend("bottomright", legend=c("Kernel Polynomial", "Regression logistique"),
             col=c("yellow","blue"), lty=1:2, cex=0.8)
      abline(0,1,col="black")
      
      # AUC svm poly
      
      output$AUCsvm2 <-renderPrint({
        auc.roc.comp <- performance(svm.fit.pred.comp,"auc")
        auc.comp=auc.roc.comp@y.values
        print(auc.comp)
      })
      
      # gini svm poly
      
      output$GINIsvm2 <-renderPrint({
        auc.roc.comp2 <- performance(svm.fit.pred.comp,"auc")
        auc.comp2=as.numeric(auc.roc.comp2@y.values)
        gini.comp=2*auc.comp2-1
        print(gini.comp)
      })
      
      # AUC glm
      
      output$AUCglm <-renderPrint({
        auc.roc.glm <- performance(glm.fit.roc,"auc")
        auc.glm=as.numeric(auc.roc.glm@y.values)
        print(auc.glm)
      })
      
      # gini glm
      
      output$GINIglm <-renderPrint({
        auc.roc.glm2 <- performance(glm.fit.roc,"auc")
        auc.glm2=as.numeric(auc.roc.glm2@y.values)
        gini.glm=2*auc.glm2-1
        print(gini.glm)
      })
      
    }
    
    
    ## choix kernel radial
    
    else if(input$typekernel3=="Radial"){
      
      # SVM radial
      
      svm.fit.comp <-  svm(Class ~ . , data=data.train,
                       kernel="radial",probability=T,cost=input$C3,gamma=input$Gam3)
      
      # SVM radial/previsio  courbe ROC
      
      pred.svm.comp <- predict(svm.fit.comp,data.test[,-31],probability = T)
      svm.fit.probs.comp=attributes(pred.svm.comp)$probabilities[,"1"]
      svm.fit.pred.comp=prediction(svm.fit.probs.comp,class.test)
      perf.svm.comp <- performance(svm.fit.pred.comp,"tpr","fpr")
      
      #glm/prevision ROC
      
      glm.fit <-glm(Class ~ ., data=base,subset=train,family=binomial)
      glm.probs <- predict(glm.fit,data.test,type="response")
      glm.fit.roc=prediction(glm.probs,class.test)
      perf.glm.roc <- performance(glm.fit.roc,"tpr","fpr")
      
      # courbe ROC svm rad/glm
      
      plot(perf.svm.comp,col="red")
      plot(perf.glm.roc,col="blue",add=T)
      legend("bottomright", legend=c("Kernel radial", "Regression logistique"),
             col=c("red","blue"), lty=1:2, cex=0.8)
      abline(0,1,col="black")
      
      # AUC svm rad
      
      output$AUCsvm2 <-renderPrint({
        auc.roc.comp <- performance(svm.fit.pred.comp,"auc")
        auc.comp=auc.roc.comp@y.values
        print(auc.comp)
      })
      
      # gini svm rad
      
      output$GINIsvm2 <-renderPrint({
        auc.roc.comp2 <- performance(svm.fit.pred.comp,"auc")
        auc.comp2=as.numeric(auc.roc.comp2@y.values)
        gini.comp=2*auc.comp2-1
        print(gini.comp)
      })
      
      # AUC glm
      
      output$AUCglm <-renderPrint({
        auc.roc.glm <- performance(glm.fit.roc,"auc")
        auc.glm=as.numeric(auc.roc.glm@y.values)
        print(auc.glm)
      })
      
      # gini glm
      
      output$GINIglm <-renderPrint({
        auc.roc.glm2 <- performance(glm.fit.roc,"auc")
        auc.glm2=as.numeric(auc.roc.glm2@y.values)
        gini.glm=2*auc.glm2-1
        print(gini.glm)
      })
      
    }
  
    ## choix kernel sigmoide
    
    else if(input$typekernel3=="Sigmoïde"){
      
      # SVM sigmoide
      
      svm.fit.comp <- svm (Class ~ . , data=data.train,
                        kernel="sigmoid",probability=T,cost=input$C3,gamma=input$Gam3,coef0=input$Co3)
      
      # SVM sigmoide/prevision courbe ROC
      
      pred.svm.comp <- predict(svm.fit.comp,data.test[,-31],probability = T)
      svm.fit.probs.comp=attributes(pred.svm.comp)$probabilities[,"1"]
      svm.fit.pred.comp=prediction(svm.fit.probs.comp,class.test)
      perf.svm.comp <- performance(svm.fit.pred.comp,"tpr","fpr")
      
      #glm/prevision ROC
      
      glm.fit <-glm(Class ~ ., data=base,subset=train,family=binomial)
      glm.probs <- predict(glm.fit,data.test,type="response")
      glm.fit.roc=prediction(glm.probs,class.test)
      perf.glm.roc <- performance(glm.fit.roc,"tpr","fpr")
      
      # courbe ROC svm sigmoide/glm
      
      plot(perf.svm.comp,col="gray")
      plot(perf.glm.roc,col="blue",add=T)
      legend("bottomright", legend=c("Kernel Sigmoïde", "Regression logistique"),
             col=c("gray","blue"), lty=1:2, cex=0.8)
      abline(0,1,col="black")
      
      # AUC svm sigmoide
      
      output$AUCsvm2 <-renderPrint({
        auc.roc.comp <- performance(svm.fit.pred.comp,"auc")
        auc.comp=auc.roc.comp@y.values
        print(auc.comp)
      })
      
      # gini svm sigmoide
      
      output$GINIsvm2 <-renderPrint({
        auc.roc.comp2 <- performance(svm.fit.pred.comp,"auc")
        auc.comp2=as.numeric(auc.roc.comp2@y.values)
        gini.comp=2*auc.comp2-1
        print(gini.comp)
      })
      
      # AUC glm
      
      output$AUCglm <-renderPrint({
        auc.roc.glm <- performance(glm.fit.roc,"auc")
        auc.glm=as.numeric(auc.roc.glm@y.values)
        print(auc.glm)
      })
      
      # gini glm
      
      output$GINIglm <-renderPrint({
        auc.roc.glm2 <- performance(glm.fit.roc,"auc")
        auc.glm2=as.numeric(auc.roc.glm2@y.values)
        gini.glm=2*auc.glm2-1
        print(gini.glm)
      })
      
    } 
    
  })
  
}