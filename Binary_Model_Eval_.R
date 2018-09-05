
#┌─────────────────────────────────────────────────────────────────────────────────────────────┐
# Lift & Gaint Table

  LiftKSandGain <- function(depvar, predcol, groups=10) {
      if(!require(dplyr)){
        install.packages("dplyr")
        library(dplyr)}
      if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
      if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
      helper = data.frame(cbind(depvar, predcol))
      helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
      gaintable = helper %>% group_by(bucket)  %>%
        summarise_at(vars(depvar), funs(min = min(predcol),
                                        max = max(predcol),
                                        median = median(predcol),
                                        avg = mean(predcol),
                                        total = n(),
                                        totalresp=sum(., na.rm = FALSE))) %>%
        mutate(Cumresp = cumsum(totalresp),
               PercentOfEvents = totalresp/sum(totalresp)*100,
               PercentOfCumEvents = cumsum(totalresp)/sum(totalresp)*100,
               NonEvents = total - totalresp,
               PercentOfCumNonEvents = cumsum(total - totalresp)/sum(total - totalresp)*100,
               KS = PercentOfCumEvents - PercentOfCumNonEvents,
               Gain=Cumresp/sum(totalresp)*100,
               Cumlift=Gain/(bucket*(100/groups)))
      
      return(gaintable)
   }

  # To run the function...
  dt = lift(df$target , df$prediction, groups = 10)

  # To plot the curve...
  graphics::plot(dt$bucket, dt$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket")

#└─────────────────────────────────────────────────────────────────────────────────────────────┘




#┌─────────────────────────────────────────────────────────────────────────────────────────────┐
# Generate Confusion Matrix

  genConfusion <- function(scores, actual) {

      # Remove any NAs
      touse = !is.na(scores) & !is.na(actual)
      scores = scores[touse]
      actual = actual[touse]
      
      N = length(scores)

      # Create ranks using the ranks CDF
      ranks = ecdf(scores)(scores)

      # Sort
      temp = order(ranks)
      scores = scores[temp]
      ranks = ranks[temp]
      actual = actual[temp]
      obs = rep(1,N)

      # Compute confusion matrix elements for cutoffs
      sensitivity = cumsum(actual)/sum(actual)
      FNR = 1 - sensitivity
      precision = cumsum(actual)/cumsum(obs)
      reviewrate = cumsum(obs)/sum(obs)
      specificity = rev(cumsum(rev(1-actual)))/sum(1-actual)
      FPR = 1 - specificity
      prevelance = sum(actual)/N
      TP = cumsum(actual)
      FP = cumsum(obs) - TP
      TN = rev(cumsum(rev(1-actual))) - 1
      FN = N - TP - FP - TN
      accuracy = (TP + TN)/N

      # Create a dataframe to hold the confusion matrix elements
      df = data.frame(cbind(
          scores,
          actual,
          ranks,
          sensitivity,
          FNR,
          precision,
          reviewrate,
          specificity,
          prevelance,
          TP,
          FP,
          TN,
          FN,
          accuracy
        ))

      return(df)
  }

#└─────────────────────────────────────────────────────────────────────────────────────────────┘




#┌─────────────────────────────────────────────────────────────────────────────────────────────┐
# Plot Elimination Curve

  plotEliminationCurve <- function(CMs, modelNames, vline= NULL, title) {
    
      # Plot % Defaults Eliminated vs. Acceptance Rate (100% to 0%)
      
      # Glue all the confusion matrices together
      
      if (class(CMs) != 'list') {
        CMs = list(CMs)
      }
      
      plot_df = NULL
      for (i in 1:length(CMs)) {
        CM = CMs[[i]]
        CM$model_name = modelNames[i]
        if (is.null(plot_df)) {
          plot_df = CM
        } else {
          plot_df = plyr::rbind.fill(plot_df, CM)
        }
      }
      
     
      random_df = data.frame(sensitivity =  c(1,0),
                             reviewrate = c(1,0),
                             model_name = 'Random')
      
      plot_df = plyr::rbind.fill(plot_df, random_df)
      
      plot_df$accepted = 1-plot_df$reviewrate
      
      
      default_plot_colors = c('steelblue1', 'darkorange3', 'hotpink2', 'gray45', 'blue', 'red')
      

      curve = ggplot(plot_df) + 
        geom_line(aes(x = accepted, y = sensitivity, color = model_name), 
                  size=1.5,
                  show.legend = TRUE) + 
        
        geom_point(aes(x = accepted, y = sensitivity, color = model_name),
                   size=1,
                   show.legend = FALSE)
        
      if (!is.null(vline)) {  
        curve = curve + geom_vline(xintercept = vline, linetype = "longdash")
      }
       
      
      curve = curve +  
        # Legend colors
        scale_color_manual(values=c(default_plot_colors[1:length(CMs)], 'grey')) +

        # Y Axis
        ylab(title) +
        scale_y_continuous(labels = scales::percent, 
                           limits = c(0,1),
                           breaks = seq(0,1,.1)) +
        
        # X Axis
        xlab("Acceptance Rate (%)") +
        scale_x_continuous(labels = scales::percent, 
                           trans = "reverse", 
                           limits = c(1,0),
                           breaks = seq(0,1,.1)) +
        
        # Theme
        theme(axis.text=element_text(size=13, color = "black"),
              axis.title=element_text(size=14,face="bold"),
              panel.grid.major = element_line(color = "grey", linetype = "dotted"), 
              panel.grid.minor = element_blank(),                 # No Minor Grid Lines
              axis.line.x = element_line(colour = "black"),       # Black axes
              axis.line.y = element_line(colour = "black"),       
              panel.background  = element_rect(fill = 'white'),   # White background
              
              # Legend Formatting
              legend.title=element_blank(),                       # Remove title for all legends
              legend.key = element_blank(),                       # No box around legend elements
              legend.text = element_text(size = 14)               # Size of text
              )
        
      
      return(curve)
      
      # Tester
      # plotDefaultElimination(list(genConfusion(df_all$SurvProb[touse], df_all$CO[touse]), data.frame(reviewrate = c(1,.5,0), sensitivity = c(1,0.7,0))), c('Fallbrook', 'Delmar'), 0.80)
   }

  plotDefaultElimination <- function(CMs, modelNames, vline=NULL) {
      plotEliminationCurve(CMs, modelNames, vline, "% of Defaults Eliminated")
    }

#└─────────────────────────────────────────────────────────────────────────────────────────────┘




#┌─────────────────────────────────────────────────────────────────────────────────────────────┐
# Plot Cumulative Bad Rate

  plotCumulativeBadRate = function(bad, score, bad_label, weight = NULL, min_rank = 0, max_rank = 1) {
  
    if (length(bad) != length(score)) {
      stop("Bad and Score vectors must be equal length")
    }
    
    if (is.null(weight)) {
      weight = rep(1, length(bad))
    }
    
    # Only keep records where both the score and bad value are not NA
    keep = !is.na(bad) & !is.na(score) & !is.na(weight)
    bad = bad[keep]
    score = score[keep]
    weight = weight[keep]
    
    # Sort by score
    x = order(score)
    bad = bad[x]
    score = score[x]
    weight = weight[x]
    
    # Compute cumulative bad vs rank
    bad_rate = cumsum(bad)/cumsum(weight)
    ranked = ecdf(score)(score)
    
    # Only keep ranks before min and max (for more stable plotting at low ranks)
    keep = ranked >= min_rank & ranked <= max_rank
    bad_rate = bad_rate[keep]
    ranked = ranked[keep]
    
    # Dataframe for plotting
    df = data.frame(bad_rate, ranked)
    
    # plot
    g = ggplot(df) + 
        geom_line(aes(ranked, bad_rate), color = 'grey', size=2) +
        geom_hline(yintercept = sum(bad)/sum(weight), linetype = "longdash") +
      
                    
      
      
        # Y-Axis
        ylab(paste0('Cumulative ', bad_label)) +
        scale_y_continuous(labels = scales::percent, 
                          limits = c(0,0.35),
                          breaks = seq(0,1,.05)) +
      
        # X Axis
        xlab("% in Bottom Tier") +
        scale_x_continuous(labels = scales::percent, 
                           limits = c(0,max_rank),
                           breaks = seq(0,1,.05)) + 
      
        annotate("text", x = 0, y = sum(bad)/sum(weight) + 0.01, 
                 hjust = 0, vjust = 0, 
                 color = 'black',
                 fontface = 'bold',
                 label = paste0(sprintf('Total %s = %3.1f%%', bad_label, 100*sum(bad)/sum(weight)))) + 
      
        theme(axis.text=element_text(size=13, color = "black"),
              axis.title=element_text(size=14,face="bold"),
              panel.grid.major = element_line(color = "grey", linetype = "dotted"), 
              panel.grid.minor = element_blank(),                 # No Minor Grid Lines
              axis.line.x = element_line(colour = "black"),       # Black axes
              axis.line.y = element_line(colour = "black"),       
              panel.background  = element_rect(fill = 'white'),   # White background
              
              # Legend Formatting
              legend.title=element_blank(),                       # Remove title for all legends
              legend.key = element_blank(),                       # No box around legend elements
              legend.text = element_text(size = 14)               # Size of text
          )
    return(g)
  }

#└─────────────────────────────────────────────────────────────────────────────────────────────┘




#┌─────────────────────────────────────────────────────────────────────────────────────────────┐
# Plot Discovery Rate

  plotDiscoveryRate <- function(CM) {
    curve = ggplot(CM) +
      geom_line(aes(x = reviewrate, y = sensitivity), color="black", size=1.5) +
      geom_abline(intercept = 0, slope = 1, colour = "gray", size=1) +
      ylab("Fraction of All Charge Offs") +
      xlab("Review Rate") +
      theme(axis.text=element_text(size=12, color = "black"),
            axis.title=element_text(size=14,face="bold"))
    
    return(curve)
  }

#└─────────────────────────────────────────────────────────────────────────────────────────────┘




#┌─────────────────────────────────────────────────────────────────────────────────────────────┐
# ROC style table & Curve

          # Make some roc style curve and bin by deciles
          make_analysis_table <- function(prob, y, nq=10) {
              ## Rank rating
              
            #   prob = runif(length(prob))
            #   prob = df$SF_Charged_Off
              predicted_ecdf = ecdf(prob)
              score = do.call(predicted_ecdf, list(prob))
              if(is.factor(y)) {
                y = as.numeric(as.character(y))
              }
              rank = order(score)
              y = y[rank]
              score = score[rank]
              cum_y = cumsum(y)/sum(y)
              plot_df = data.frame(score = score, cum_y = cum_y)
              curve = ggplot(plot_df) +
                geom_line(aes(x = score, y = cum_y), color="black", size=1.5) +
                geom_abline(intercept = 0, slope = 1, colour = "gray", size=1) +
                ylab("Fraction of All Charge Offs") +
                xlab("Fraction of Funded Loans, ordered by Score") +
                theme(axis.text=element_text(size=12, color = "black"),
                      axis.title=element_text(size=14,face="bold"))
              
              nq = 10
              out = data.frame()
              for (i in seq(1/nq,1,1/nq)) {
                slice = score <= i  & score > i - 1/nq
                cnt = length(y[slice])
                nbad = sum(y[slice])
                rate = nbad/cnt
                out = rbind(out, data.frame(cnt = cnt, nbad = nbad, rate = rate))
              }
              
              results = list(plot_df = plot_df, curve = curve, quantiles = out, dist = predicted_ecdf)
              
              return(results)
          }



          plotCumulativeCO <- function(CMs, modelNames, vline= NULL) {
            
              # Plot Cumulative Charge Off Rate vs. Acceptance Rate
              # Glue all the confusion matrices together
              
              if (class(CMs) != 'list') {
                CMs = list(CMs)
              }
              
              plot_df = NULL
              for (i in 1:length(CMs)) {
                CM = CMs[[i]]
                CM$model_name = modelNames[i]
                if (is.null(plot_df)) {
                  plot_df = CM
                } else {
                  plot_df = plyr::rbind.fill(plot_df, CM)
                }
              }
              
              plot_df$accepted = 1-plot_df$reviewrate
              plot_df$bad_rate = plot_df$FN/(plot_df$TN + plot_df$FN)
              
              
              random_df = data.frame(bad_rate =  c(0,max(plot_df$bad_rate, na.rm = TRUE)),
                                     accepted = c(0,1),
                                     model_name = 'Random')
              
              plot_df = plyr::rbind.fill(plot_df, random_df)
              

              
              
              default_plot_colors = c('steelblue1', 'darkorange3', 'hotpink2', 'gray45')
              
              
              curve = ggplot(plot_df) + 
                geom_line(aes(x = accepted, y = sensitivity, color = model_name), 
                          size=1.5,
                          show.legend = TRUE) + 
                
                geom_point(aes(x = accepted, y = sensitivity, color = model_name),
                           size=1,
                           show.legend = FALSE)
              
              if (!is.null(vline)) {  
                curve = curve + geom_vline(xintercept = vline, linetype = "longdash")
              }
              
              curve = curve +  
                # Legend colors
                scale_color_manual(values=c(default_plot_colors[1:length(CMs)], 'grey')) +
                
                # Y Axis
                ylab("% of Defaults Eliminated") +
                scale_y_continuous(labels = scales::percent, 
                                   limits = c(0,1),
                                   breaks = c(0,1)) +
                
                # X Axis
                xlab("Acceptance Rate (%)") +
                scale_x_continuous(labels = scales::percent, 
                                   limits = c(1,0),
                                   breaks = c(0,vline,1)) +
                
                # Theme
                theme(axis.text=element_text(size=12, color = "black"),
                      axis.title=element_text(size=13,face="bold"),
                      panel.grid.major = element_blank(),                 # No Major
                      panel.grid.minor = element_blank(),                 # No Minor Grid Lines
                      axis.line.x = element_line(colour = "black"),       # Black axes
                      axis.line.y = element_line(colour = "black"),       
                      panel.background  = element_rect(fill = 'white'),   # White background
                      
                      # Legend Formatting
                      legend.title=element_blank(),                       # Remove title for all legends
                      legend.key = element_blank(),                       # No box around legend elements
                      legend.text = element_text(size = 12)               # Size of text
                )
              
              
              return(curve)
              
              # Tester
              # plotDefaultElimination(list(genConfusion(df_all$SurvProb[touse], df_all$CO[touse]), data.frame(reviewrate = c(1,.5,0), sensitivity = c(1,0.7,0))), c('Fallbrook', 'Delmar'), 0.80)
          }


          # Comput the AUC 
          compute.auc <- function(pred, y) {
              requireNamespace("ROCR", quietly = TRUE)
              prob <- ROCR::prediction(pred, y,  label.ordering = levels(y))
              auc <-  slot(ROCR::performance(prob, "auc"), "y.values")[[1]] 
              return(auc)
            }


         # plot ROC 
         plotROC = function(co_models, df, quiet = FALSE, title) {
              predicted = rep(NA, NROW(df))
              for (m in co_models) {
                new_probs = predict(m, newdata = df)
                predicted = coalesce(predicted, new_probs)
              }
              
              y = df$CO
              
              touse = !is.na(predicted) & !is.na(y)
              
              y = y[touse]
              predicted = predicted[touse]
              
              requireNamespace("ROCR", quietly = TRUE)
              requireNamespace("ggplot2", quietly = TRUE)
              # Compute and save AUC
              prob <- ROCR::prediction(predicted, y,  label.ordering = levels(y))
              auc <-  slot(ROCR::performance(prob, "auc"), "y.values")[[1]] 
              
              # Make the ROC and save it
              tprfpr <- ROCR::performance(prob, "tpr", "fpr")
              tpr <- unlist(slot(tprfpr, "y.values"))
              fpr <- unlist(slot(tprfpr, "x.values"))
              roc <- data.frame(tpr, fpr)
              
              
              requireNamespace("ggplot2", quietly = TRUE)
              curve = ggplot2::ggplot(roc) +
                ggplot2::geom_line(aes(x = fpr, y = tpr), color="black", size=1.5) +
                ggplot2::geom_abline(intercept = 0, slope = 1, colour = "gray", size=1) +
                ggplot2::ylab("Sensitivity") +
                ggplot2::xlab("1 - Specificity")
              if(!missing(title)) {
                curve = curve + ggplot2::ggtitle(sprintf("%s, AUC = %4.3f", title, auc)) + 
                  ggplot2::theme(plot.title = element_text(lineheight=.8, face="bold"))
              }
              if(!quiet) {
                plot(curve)
                print(sprintf("AUC = %3.4f", auc))
              }
              return(list(roc = roc, auc = auc, curve = curve))
            
         }
#└─────────────────────────────────────────────────────────────────────────────────────────────┘