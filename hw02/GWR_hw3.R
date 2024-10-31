options(scipen=999)


library(sf)
library(sp)
library(spdep)
library(spgwr)
library(tmap)
library(spatialreg)
library(whitestrap)
library(lmtest)
library(tseries)

shp <-st_read("RegressionDataFIN.shp") %>%
  st_transform(4326)

queens <- read.gal("Lecture 1 - RegressionData.shp (2)/RegressionData.queen.gal", region.id=NULL, override.id=FALSE)
queenlist<-nb2listw(queens, style = 'W')


laaaa <-st_read("data/Lecture 1 - RegressionData.shp/RegressionData.shp") 

las <- as_Spatial(laaaa)
lasbw<-gwr.sel(formula=LNMEDHVAL~PCTBACHMOR+PCTVACANT+PCTSINGLES+LNNBELPOV, 
            data=las,
            method = "aic",
            adapt = TRUE)
lasbw

#Setting an adaptive bandwidth
shps <- as_Spatial(shp)  #These analyses are easier to do when the data are of the SpatialPolygonsDataFrame class
class (shps)
bw<-gwr.sel(formula=LNMEDHVAL~PCTBACHMOR+PCTVACANT+PCTSINGLES+LNNBELPOV, 
            data=shps,
            method = "aic",
            adapt = TRUE)
bw
# 0.008130619


gwrmodel<-gwr(formula=LNMEDHVAL~PCTBACHMOR+PCTVACANT+PCTSINGLES+LNNBELPOV,
              data=las,
              adapt = lasbw, #adaptive bandwidth determined by proportion of observations accounted for
              gweight=gwr.Gauss,
              se.fit=TRUE, #to return local standard errors
              hatmatrix = TRUE)
gwrmodel



summary(gwrmodel$SDF)


gwrresults<-as.data.frame(gwrmodel$SDF)

las$coefPCTVACANTst<-gwrresults$PCTVACANT/gwrresults$PCTVACANT_se
las$coefPCTSINGLESst<-gwrresults$PCTSINGLES/gwrresults$PCTSINGLES_se
las$coefPCTBACHMORst<-gwrresults$PCTBACHMOR/gwrresults$PCTBACHMOR_se
las$coefLNNBELPOVst<-gwrresults$LNNBELPOV/gwrresults$LNNBELPOV_se

las$gwrE<-gwrresults$gwr.e
las$localR2<-gwrresults$localR2

las.sf <- st_as_sf(las)


gwrMoranMc<-moran.mc(gwrresults$gwr.e, queenlist, nsim=999, alternative="two.sided")


ggplot(as.data.frame(gwrMoranMc$res[c(1:999)]), aes(gwrMoranMc$res[c(1:999)])) +
  geom_histogram(binwidth = 0.01, fill="darkorchid4") +
  geom_vline(aes(xintercept = gwrMoranMc$statistic), colour = "darkgreen",size=1) +
  geom_vline(aes(xintercept = gwrMoranMc$p.value), colour = "white",size=.5) +
  scale_x_continuous(limits = c(-1, 1)) +
  annotate(geom = "text", x = 0.2, y = 200,
           label = "Pseudo p value - 0.01  \nMoran's I - 0.33", hjust = "left",
           family = "Helvetica", fontface="bold", size=3)+
  labs(title="Observed and permuted Moran's I",
       subtitle= "Observed Moran's I in green; 999 permutations",
       x="Moran's I",
       y=NULL)+
  theme_minimal()

moran.plot(gwrresults$gwr.e, queenlist)


PCTVACANT <-tm_shape(las.sf)+
  tm_polygons(fill="coefPCTVACANTst", col="grey50", lwd = 0.5,
              fill.legend = tm_legend(title = "", position = tm_pos_in("right", "bottom")),
              tm_scale_intervals(values = "tableau.red_blue_white_diverging",
                                 breaks=c(-Inf, -2, 0, 2, Inf)))+
  tm_title(text="Percent vacant housing units", size=1.2, fontface="bold", fontfamily="Palatino",
           position = tm_pos_out("center", "top"))

LNNBELPOV <- tm_shape(las.sf)+
  tm_polygons(fill="coefLNNBELPOVst", col="grey50", lwd = 0.5,
              fill.legend = tm_legend(title = "", position = tm_pos_in("right", "bottom")),
              tm_scale_intervals(values = "tableau.red_blue_white_diverging",
              breaks=c(-Inf, -2, 0, 2, Inf)))+
  tm_title(text="Number of residents living in poverty", size=1.2, fontface="bold", fontfamily="Palatino",
           position = tm_pos_out("center", "top"))

PCTBACH <- tm_shape(las.sf)+
  tm_polygons(fill="coefPCTBACHMORst", col="grey50", lwd = 0.5,
              fill.legend = tm_legend(title = "", position = tm_pos_in("right", "bottom")),
              tm_scale_intervals(values = "tableau.red_blue_white_diverging",
                                 breaks=c(-Inf, -2, 0, 2, Inf)))+
  tm_title(text="Percent of residents with Bachelor's  \nDegree or higher", size=1.2, fontface="bold", fontfamily="Palatino",
           position = tm_pos_out("center", "top"))

PCTSINGLES <- tm_shape(las.sf)+
  tm_polygons(fill="coefPCTSINGLESst", col="grey50", lwd = 0.5,
              fill.legend = tm_legend(title = "", position = tm_pos_in("right", "bottom")),
              tm_scale_intervals(values = "tableau.red_blue_white_diverging",
                                 breaks=c(-Inf, -2, 0, 2, Inf)))+
  tm_title(text="Percent of detached single-family homes", size=1.2, fontface="bold", fontfamily="Palatino",
           position = tm_pos_out("center", "top"))

tmap_arrange(PCTVACANT, LNNBELPOV, PCTBACH, PCTSINGLES, widths = c(.5, .5))


rsquaredmap <- tm_shape(las.sf)+
  tm_polygons(fill="localR2", col="grey50", lwd = 0.2,
              fill.legend = tm_legend(title = "", position = tm_pos_in("right", "bottom")),
              tm_scale_intervals(values = "carto.yl_burg", breaks = c(0,0.27,0.4, 0.53,0.67, Inf)))+
  tm_title(text="Local R-squared", size=1.2, fontface="bold", fontfamily="Palatino",
           position = tm_pos_out("center", "top"))
