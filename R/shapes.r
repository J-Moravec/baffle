square = function(x, y, d=1, col=par("fg"), border=NA, lty=par("lty"), lwd=par("lwd")){
    rect(x - d/2, y - d/2, x + d/2, y + d/2, col=col, border=border, lty=lty, lwd=lwd)
    }
