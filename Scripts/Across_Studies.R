###function for plotting multiple plots

multiplot <- function(..., plotlist=NULL, file, cols=2, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
    numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#loading packages

library(lme4)
library(car)
library(visreg)

#Statistical analysis testing the effect of disturbance on Community-weighted spore size mean
#and IQR across different studies

#Merging different studies
AcrossStudiesTraits<-
  rbind(CommTraitsBonfin2013,
        CommTraitsOehl2011[c(1:6,8,9,10,13,14,15,18,19,21),],
      CommTraitsSturmer2001,
      CommTraitsBrundrett2013[c(5,2,7,8,9,12,18,27,36,43,44,48,61,62,66,74,77,92,95,110,113),])
        
      #multiplot(PlotBonfin,PlotSturmer,PlotOehl,PlotBrundrett)


#Statistical modelling of Community-weighted Spore mean along disturbance gradients
#(measured as bray curtis distance from a reference undistrubed site per study)

TraitModel1<-lmer(CWMean~bray.dist+
                   (1|Study/Reference),data = AcrossStudiesTraits)

              Anova(TraitModel1)
              summary(TraitModel1)
              
              #Visualizing the model
              visreg(TraitModel1)

#Statistical modelling of Community-weighted Spore mean along disturbance gradients
#(measured as bray curtis distance from a reference undistrubed site per study)
TraitModel2<-lmer(IQR~bray.dist+
              (1|Study/Reference),data = AcrossStudiesTraits)

              Anova(TraitModel2)
              summary(TraitModel2)

              #Visualizing the model
              visreg(TraitModel2)
