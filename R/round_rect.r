#' Plot a rectangle with rounded corners
#'
#' Plot one or more rectangles with rounded corners.
#'
#' The rounded corner is a part of a circle (one quarter) drawn between two points of neighbouring
#' sides of a rectangle. The relative position of these two points determine how rounded will the
#' final shape be. The position of these points is determined by the parameters `xr` and `yr`,
#' which determine the proportion of side x and y, from which the rounded corner is drawn.
#' Values between 0 and 1 are permitted, but given the symmetricity of a rectangle, values  larger
#' than 0.5 are reflected back (modulo 0.5). When both xr and yr are 0 or 1, normal rectangle
#' without rounded corners is drawn. When xr and yr are 0.5, ellipsis is drawn.
#'
#' @param x1,y1 x and y coordinates of the bottom left corner
#' @param x2,y2 x and y coordinates of the top right corner
#' @param xr,yr the proportion of length of a side of the rectangle, from which the rounded corners
#'              start. Values between 0 and 1 are permitted. See details.
#' @param n the number of points used to approximate the curvature of the rounded corner
#' @param ... other parameters passed to [graphics::polygon()].
#' @return No return value, called for side effects
#'
#' @export
round_rect = function(x1, y1, x2, y2, xr=0.2, yr=0.2, n=10, ...){
    Map(f=.round_rect, x1=x1, y1=y1, x2=x2, y2=y2, xr=xr, yr=yr, n=n, ...)
    invisible(NULL)
    }

# internal
.round_rect = function(
    x1, y1, x2, y2, xr=0.2, yr=0.2, n=10,
    col=NA, border=NULL, lty=par("lty"), lwd=par("lwd"),
    ...
    ){
    stopifnot(xr > 0 && xr < 1)
    stopifnot(yr > 0 && yr < 1)

    xr = xr %% 0.5
    yr = yr %% 0.5

    xs = x2 - x1
    ys = y2 - y1

    curve = seq(0, pi/2, length.out=n) |> sin()

    x = c(x1, rev(1-curve)*xs*xr+x1, xs*xr+x1,
          xs*(1-xr)+x1, curve*xs*xr + xs*(1-xr)+x1, x2)
    y = c(ys*yr+y1, (1-curve)*ys*yr+y1, y1,
          y1, rev(1-curve)*ys*yr+y1, ys*yr+y1)

    graphics::polygon(
        x = c(x, x2, rev(x), x1),
        y = c(y, ys*(1-yr)+y1, y1 + y2 - rev(y), ys*yr+y1),
        col = col, border = border, lty = lty, lwd = lwd,
        ...
        )
    }
