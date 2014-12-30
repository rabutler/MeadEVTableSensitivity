# had to manually load scales so that pretty_breaks() could be found
library(ggplot2)
library(scales)
library(zoo)
library(RWDataPlot)
library(plyr)

# creates all the figures necessary to look at the changes in the natural flow due to changes
# in the Mead elevation-volume and elevation-area tables
getSensitivityFigures <- function()
{
  # read in the data
  oldEV <- read.table('../data/Mead_Vol_tbl_1963.txt')
  exEV <- read.table('../data/Mead_Vol_tbl_Existing.txt')
  newEV <- read.table('../data/Mead_Vol_tbl_2009.txt')
  oldEA <- read.table('../data/Mead_Area_tbl_1963.txt')
  exEA <- read.table('../data/Mead_Area_tbl_Existing.txt')
  newEA <- read.table('../data/Mead_Area_tbl_2009.txt')
  simData <- read.csv('../data/MeadEVCompSensitivityResults.csv', comment.char = "#", row.names = 1)
  
  # format the data
  names(oldEV) <- c('Elevation','Value')
  oldEV$Table <- 1963
  oldEV$Variable <- 'Volume'
  names(oldEA) <- c('Elevation','Value')
  oldEA$Table <- 1963
  oldEA$Variable <- 'Area'
  names(newEV) <- c('Elevation','Value')
  newEV$Table <- 2009
  newEV$Variable <- 'Volume'
  names(newEA) <- c('Elevation','Value')
  newEA$Table <- 2009
  newEA$Variable <- 'Area'
  names(exEV) <- c('Elevation','Value')
  exEV$Table <- 'Existing'
  exEV$Variable <- 'Volume'
  names(exEA) <- c('Elevation','Value')
  exEA$Table <- 'Existing'
  exEA$Variable <- 'Area'
  tt <- zoo::as.yearmon(rownames(simData), '%B-%y')
  simRes <- rbind(data.frame(Time = tt, Table = "Existing", NatFlow = simData$ExistingEVTable), 
                  data.frame(Time = tt, Table = "2009", NatFlow = simData$NewEVTable))
  simDataAnn <- as.data.frame(RWDataPlot::sumMonth2Annual(simData))
  simResAnn <- rbind(data.frame(Time = 1971:2012, Table = "Existing", NatFlow = simDataAnn$ExistingEVTable), 
                     data.frame(Time = 1971:2012, Table = "2009", NatFlow = simDataAnn$NewEVTable))
  
  # combine into one data frame
  evea <- rbind(oldEV, newEV, exEV, oldEA, newEA, exEA)
  
  # remove the 'fake' elevation entries
  evea <- evea[evea$Elevation <= 1235,]
  # convert volume to MAF
  evea$Value[evea$Variable == 'Volume'] <- evea$Value[evea$Variable == 'Volume']/1000000
  
  # create a plot comparing the EV and EA curves
  evPlot <- ggplot(evea[evea$Variable == 'Volume',],aes(Elevation,Value,color = Table))
  evPlot <- evPlot + geom_line(size=1) + labs(list(title = 'Lake Mead Elevation-Volume Curves', 
                                              y = 'Volume [MAF]', x = 'Elevation [ft]'))
  
  evPlotZoom <- ggplot(evea[evea$Variable == 'Volume' & evea$Elevation <= 950,],aes(Elevation,Value,color = Table))
  evPlotZoom <- evPlotZoom + geom_line(size=1) + labs(list(title = 'Lake Mead Elevation-Volume Curves', 
                                                   y = 'Volume [MAF]', x = 'Elevation [ft]'))
  
  eaPlot <- ggplot(evea[evea$Variable == 'Area',],aes(Elevation,Value,color = Table))
  eaPlot <- eaPlot + geom_line(size=1) + labs(list(title = 'Lake Mead Elevation-Area Curves', 
                                                   y = 'Surface Area [acres]', x = 'Elevation [ft]'))
  
  
  # compare the existing tables to the 2009 tables: MONTHLY
  monDiff <- data.frame(Time = tt,
    NatFlow = simRes$NatFlow[simRes$Table == '2009'] - simRes$NatFlow[simRes$Table == 'Existing'])

  monNFDiff <- ggplot(monDiff, aes(Time, NatFlow))
  monNFDiff <- monNFDiff + geom_line() + scale_x_yearmon() + ylab('Difference [acre-ft]') + 
      ggtitle('Difference in monthly intervening natural flow above Mead\nResults from 2009 tables minus results from "existing" tables') + 
      geom_hline(data = monDiff, aes(yintercept=mean(NatFlow),color = 'red')) + scale_y_continuous(labels = comma)
  
  
  monNF <- ggplot(simRes, aes(Time, NatFlow, color = Table)) + geom_line() + scale_x_yearmon() +
           labs(list(title = 'Monthly Intervening Natural Flow above Mead', 
                     y = 'Natural Flow [acre-ft]')) + 
           scale_y_continuous(labels = comma)
  
  # compare the existing tables to the 2009 tables: ANNUAL
  annNF <- ggplot(simResAnn, aes(Time, NatFlow, color = Table, group = Table)) + geom_line() +
           labs(list(title = 'Annual Intervening Natural Flow above Mead',
                     y = 'Natural Flow [acre-ft]', x = 'Year')) +
           scale_y_continuous(labels = comma)
  annDiff <- data.frame(Year = 1971:2012, NFDiff = simResAnn$NatFlow[simResAnn$Table == '2009'] -
                          simResAnn$NatFlow[simResAnn$Table == 'Existing'], Variable = 'Annual Difference [acre-ft]')
  annDiff <- rbind(annDiff, data.frame(Year = 1971:2012, 
                                       NFDiff = annDiff$NFDiff/simResAnn$NatFlow[simResAnn$Table == '2009']*100,
                                       Variable = 'Annual Percent Diff. [%]'))
  annNFDiffMeans <- plyr::ddply(annDiff, .(Variable), summarize,val = mean(NFDiff))
  annNFDiff <- ggplot(annDiff,aes(Year,NFDiff)) + geom_line() + 
               facet_grid(Variable~., scales = 'free') +
               labs(list(title = 'Difference in annual intervening natural flow above Mead\nResults from 2009 tables minus results from "existing" tables',
                         y = '')) +  scale_y_continuous(labels = comma) +
               geom_hline(data = annNFDiffMeans, mapping=aes(yintercept=val), col = 'red')
  
  # plot the differences between the 2009 and existing EV tables
  evea <- evea[evea$Elevation <= 1229,]
  volDiff <- data.frame(VolDiff = evea$Value[evea$Table == '2009' & evea$Variable == 'Volume'] - 
                              evea$Value[evea$Table == 'Existing' & evea$Variable == 'Volume'], 
                        Elevation = evea$Elevation[evea$Table == '2009' & evea$Variable == 'Volume'])
  volDiff$VolDiff <- volDiff$VolDiff
  volDiff$Dec2010 <- 1086.3
  volDiff$Jan2011 <- 1091.73
  volDiffPlot <- ggplot(volDiff, aes(Elevation, VolDiff)) + geom_point() + 
                 geom_vline(aes(xintercept=Dec2010), col = 'red') + 
                 geom_vline(aes(xintercept=Jan2011), col = 'blue') + 
                 labs(list(title = 'Storage from 2009 EV table minus storage from \"existing\" EV table',
                           y = 'Difference in storage [MAF]'), x = 'Elevation [ft]')
  
  # create output list for use in RMarkdown file
  otpt <- list()
  otpt$evPlot <- evPlot
  otpt$eveaData <- evea
  otpt$eaPlot <- eaPlot
  otpt$monNFDiff <- monNFDiff
  otpt$monNF <- monNF
  otpt$annNF <- annNF
  otpt$annNFDiff <- annNFDiff
  otpt$annNFDiffMeansData <- annNFDiffMeans
  otpt$volDiffPlot <- volDiffPlot
  otpt$evPlotZoom <- evPlotZoom
  otpt
}