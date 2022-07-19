#' Plot raster image
#'
#' Plot raster image centered at `x` and `y` coordinates scaled to diameter `d`.
#'
#' The `rasters()` function is a convenient wrapper around [graphics::rasterImage()] with similar
#' interface to the [Shapes] functions available in this package (such as [square()], [circle()]
#' and [rcpoly()]).
#'
#' The raster image is plotted centered at the `x` and `y` coordinates and scaled to the diameter
#' size `d`. When `dx` and `dy` are `NA`, the proportions of the raster are kept unchanged,
#' otherwise they are scaled to the specified size in either direction. This scaling is done
#' before rotation.
#'
#' Unlike in `rasterImage`, the rotation is performed clockwise and the rotation axis is the center
#' of the raster (i.e., the provided x and y coordinates), rather than the bottom left coordinate
#' `x0`. This rotation is performed after scaling.
#'
#' As of yet, the rotation is accurate only when the aspect ratio is set to 1 (`asp=1`) through
#' the `graphics::plot.window()` call.
#'
#' The `rasters()` function is fully vectorized.
#'
#' @param x,y coordinates
#' @param image raster image
#' @param d **optional** diameter, see details
#' @param dx,dy **optional** diameter in either coordinate direction
#' @param rotate **optional** clockwise rotation in degrees (0-360°)
#' @param ... **optional** other parameters passed to [graphics::rasterImage()],
#'  such as `interpolate`
#'
#' @examples
#' # create plotting window
#' plot.new(); plot.window(c(-1,1), c(-1,1), asp=1); axis(1); axis(2)
#'
#' # create raster image, alpha is convenient when overplotting
#' img = matrix(adjustcolor("black", alpha.f=0.3), 3, 3)
#' img[2, 2] = adjustcolor("white", alpha.f=0.3)
#' img = as.raster(img)
#'
#' rasters(0, 0, img)
#'
#' # interpolate=FALSE makes quite a difference
#' rasters(0, 0, img, interpolate=FALSE)
#'
#' # arguments are vectorized, standard recycling rules apply
#' rasters(0, 0, img, interpolate=FALSE, rotate=c(30, 60, 90))
#' rasters(c(-1, -0.5,  0.5, 1), c(1, 0.5, -0.5, -1), img, interpolate=FALSE)
#'
#' @export
#' @seealso [grDevices::as.raster()] and [graphics::rasterImage()]
rasters = function(x, y, image, d=0.9, dx=NA, dy=NA, rotate=0, ...){
    if(grDevices::is.raster(image))
        image = list(image)

    Map(f=raster, x=x, y=y, image=image, d=d, dx=dx, dy=dy, rotate=rotate, ...)
    
    invisible(NULL)
    }

# internal 
raster = function(x, y, image, d=0.9, dx=NA, dy=NA, rotate=0, ...){

    if(is.na(dx) && is.na(dy)){
        prop = dim(image) / max(dim(image))
        dx = d * prop[1]
        dy = d * prop[2] 
        }

    if(is.na(dx))
        dx = d
    if(is.na(dy))
        dy = d

    if(rotate == 0){
        graphics::rasterImage(image, x - dx/2, y - dy/2, x + dx/2, y + dy/2, angle=0, ...)
        invisible(NULL)
        }

    origin = c(x - dx/2, y - dy/2)
    cent = .rotate(c(x, y), -rotate, origin=origin)
    
    x = x - cent[1] + x
    y = y - cent[2] + y

    graphics::rasterImage(
        image,
        x - dx/2,
        y - dy/2,
        x + dx/2,
        y + dy/2,
        angle=-rotate,
        ...
        )
    }


# internal
.rotate = function(p, angle, origin){
    angle = angle * pi / 180
    
    co = cos(angle)
    si = sin(angle)
    
    x1 = p[1] - origin[1]
    y1 = p[2] - origin[2]
    
    x2 = x1 * co - y1 * si + origin[1]
    y2 = x1 * si + y1 * co + origin[2]
    
    c(x2, y2)
    }
