#' @title maxentRaster
#' Generate a raster from a spatial distribution model
#' fitted with glmnet via maxnet package
#' @param layers A raster brick of enviromental layers
#' @param occ A points data frame
#' @param fclasses A vector with feature classes parameters
#' @return A list with a raster and a model fitted with glmnet
#'
#' @importFrom raster raster rasterToPoints rasterFromXYZ
#' @importFrom stats na.omit
#' @importFrom maxnet maxnet maxnet.formula
#' @export
maxentRaster <- function(layers, occ, fclasses){
     pts_layers <- raster::rasterToPoints(layers)
     backgr <- maxnetmap::randomRows(pts_layers[ ,1:2], 1000)
     train_env <- raster::extract(layers, occ)
     train_env <- stats::na.omit(train_env)
     colnames(occ) <- colnames(backgr)
     data_pts <- rbind(occ, backgr)
     absvals <- raster::extract(layers, backgr)
     absvals <- stats::na.omit(absvals)
     presabs <- c(rep(1, nrow(train_env)), rep(0, nrow(absvals)))
     sdmdata <- data.frame(cbind(presabs, rbind(train_env, absvals)))
     me <- maxnet::maxnet(sdmdata$presabs, sdmdata[,-1],
                          maxnet::maxnet.formula(sdmdata$presabs,
                                         sdmdata[,-1],
                                         classes = fclasses) )
     pred <- stats::predict(me, pts_layers[ ,-c(1,2)], type="cloglog")
     df_pred <- cbind(pts_layers[ ,c(1,2)], pred)
     r <- raster::rasterFromXYZ(df_pred)
     return(list(r, me) )
 }

