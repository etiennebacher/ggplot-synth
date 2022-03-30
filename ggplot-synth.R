my_path_plot <- function(
  synth.res = NA,
  dataprep.res = NA,
  tr.intake = NA,
  Ylab = c("Y Axis"),
  Xlab = c("Time"),
  Ylim = NA,
  Legend = c("Treated",
             "Synthetic"),
  Legend.position = c("topright"),
  Main = NA,
  Z.plot = FALSE,
  ggplot = FALSE
) {
  if (Z.plot == FALSE) {
    if (sum(is.na(dataprep.res$Y1plot)) > 0) {
      stop(
        "\n\n#####################################################",
        "\nYou have missing Y data for the treated!\n\n"
      )
    }
    if (sum(is.na(dataprep.res$Y0plot)) > 0) {
      stop(
        "\n\n#####################################################",
        "\nYou have missing Y data for the controls!\n\n"
      )
    }
    y0plot1 <- dataprep.res$Y0plot %*% synth.res$solution.w
    if (sum(is.na(Ylim)) > 0) {
      Y.max <- max(c(y0plot1, dataprep.res$Y1plot))
      Y.min <- min(c(y0plot1, dataprep.res$Y1plot))
      Ylim <- c((Y.min - 0.3 * Y.min), (0.3 * Y.max +
                                          Y.max))
    }
    if (isFALSE(ggplot)) {
      plot(
        dataprep.res$tag$time.plot,
        dataprep.res$Y1plot,
        t = "l",
        col = "black",
        lwd = 2,
        main = Main,
        ylab = Ylab,
        xlab = Xlab,
        xaxs = "i",
        yaxs = "i",
        ylim = Ylim
      )
      lines(
        dataprep.res$tag$time.plot,
        y0plot1,
        col = "black",
        lty = "dashed",
        lwd = 2,
        cex = 4 / 5
      )
    } else {
      if (!require("ggplot2")) {
        stop("You need to install the package `ggplot2` in order to use the option `ggplot = TRUE`.")
      } else {
        data_plot <- data.frame(
          x = dataprep.res$tag$time.plot,
          y = dataprep.res$Y1plot[, 1],
          y2 = y0plot1[, 1]
        )
        my_plot <- ggplot(data_plot, aes(x, y)) +
          geom_line() +
          geom_line(aes(y = y2), linetype = "dashed") +
          labs(title = ifelse(!is.na(Main), Main, "")) +
          xlab(Xlab) +
          ylab(Ylab) +
          ylim(Ylim)
      }
    }
  }

  else {
    z0plot <- dataprep.res$Z0 %*% synth.res$solution.w
    if (sum(is.na(Ylim)) > 0) {
      Y.max <- max(c(z0plot, dataprep.res$Z1))
      Y.min <- min(c(z0plot, dataprep.res$Z1))
      Ylim <- c((Y.min - 0.3 * Y.min), (0.3 * Y.max +
                                          Y.max))
    }
    if (isFALSE(ggplot)) {
      plot(
        dataprep.res$tag$time.optimize.ssr,
        z0plot,
        t = "l",
        col = "black",
        lwd = 2,
        main = Main,
        ylab = Ylab,
        xlab = Xlab,
        xaxs = "i",
        yaxs = "i",
        ylim = Ylim
      )
      lines(
        dataprep.res$tag$time.optimize.ssr,
        dataprep.res$Z1,
        col = "black",
        lty = "dashed",
        lwd = 2,
        cex = 4 / 5
      )
    } else {
      data_plot <- data.frame(
        x = dataprep.res$tag$time.optimize.ssr,
        y = z0plot[, 1],
        y2 = dataprep.res$Z1[, 1]
      )
      my_plot <- ggplot(data_plot, aes(x, y)) +
        geom_line() +
        geom_line(aes(y = y2), linetype = "dashed") +
        labs(title = ifelse(!is.na(Main), Main, "")) +
        xlab(Xlab) +
        ylab(Ylab) +
        ylim(Ylim)
    }
  }

  if (isFALSE(ggplot)) {
    abline(
      v = tr.intake,
      lty = 3,
      col = "black",
      lwd = 2
    )
  } else {
    if (!is.na(tr.intake)) {
      my_plot <- my_plot +
        geom_vline(xintercept = tr.intake, linetype = "dotted")
    }
  }

  if (sum(is.na(Legend)) == 0) {
    if (isFALSE(ggplot)) {
      legend(
        Legend.position,
        legend = Legend,
        lty = 1:2,
        col = c("black", "black"),
        lwd = c(2, 2),
        cex = 6 / 7
      )
    } else {
      my_plot <- my_plot +
        scale_linetype_manual(values = Legend)
      if (!is.null(Legend.position)) {
        warning("Argument `Legend.position` doesn't work when `ggplot = TRUE`.")
      }
    }
  }

  if (isTRUE(ggplot)) {
    return(my_plot)
  }

}


my_gaps_plot <- function(
  synth.res = NA,
  dataprep.res = NA,
  Ylab = c("Title"),
  Xlab = c("Time"),
  Main = c("Gaps: Treated - Synthetic"),
  tr.intake = NA,
  Ylim = NA,
  Z.plot = FALSE,
  ggplot = FALSE
){
  if (Z.plot == FALSE) {
    if (sum(is.na(dataprep.res$Y1plot)) > 0) {
      stop(
        "\n\n#####################################################",
        "\nYou have missing Y data for the treated!\n\n"
      )
    }
    if (sum(is.na(dataprep.res$Y0plot)) > 0) {
      stop(
        "\n\n#####################################################",
        "\nYou have missing Y data for the controls!\n\n"
      )
    }
    gap <- dataprep.res$Y1plot - (dataprep.res$Y0plot %*%
                                    synth.res$solution.w)
    if (sum(is.na(Ylim)) > 0) {
      Ylim <- c(-(0.3 * max(abs(gap)) + max(abs(gap))),
                (0.3 * max(abs(gap)) + max(abs(gap))))
    }
    if (isFALSE(ggplot)) {
      plot(
        dataprep.res$tag$time.plot,
        gap,
        t = "l",
        col = "black",
        lwd = 2,
        main = Main,
        ylab = Ylab,
        xlab = Xlab,
        ylim = Ylim,
        xaxs = "i",
        yaxs = "i"
      )
    } else {
      if (!require("ggplot2")) {
        stop("You need to install the package `ggplot2` in order to use the option `ggplot = TRUE`.")
      } else {
        data_plot <- data.frame(
          x = dataprep.res$tag$time.plot,
          y = gap[, 1]
        )
        my_plot <- ggplot(data_plot, aes(x, y)) +
          geom_line()
      }
    }
  }
  else {
    gap <- dataprep.res$Z1 - (dataprep.res$Z0 %*% synth.res$solution.w)
    if (sum(is.na(Ylim)) > 0) {
      Ylim <- c(-(0.3 * max(abs(gap)) + max(abs(gap))),
                (0.3 * max(abs(gap)) + max(abs(gap))))
    }
    if (isFALSE(ggplot)) {
      plot(
        dataprep.res$tag$time.optimize.ssr,
        gap,
        t = "l",
        col = "black",
        lwd = 2,
        main = Main,
        ylab = Ylab,
        xlab = Xlab,
        ylim = Ylim,
        xaxs = "i",
        yaxs = "i"
      )
    } else {
      data_plot <- data.frame(
        x = dataprep.res$tag$time.optimize.ssr,
        y = gap[, 1]
      )
      my_plot <- ggplot(data_plot, aes(x, y)) +
        geom_line()
    }
  }

  if (isFALSE(ggplot)) {
    abline(
      h = 0,
      col = "black",
      lty = "dashed",
      lwd = 2
    )
    abline(
      v = tr.intake,
      col = "black",
      lty = "dotted",
      lwd = 2
    )
  } else {
    my_plot <- my_plot +
      geom_hline(yintercept = 0, linetype = "dashed")

    if (!is.na(tr.intake)) {
      my_plot <- my_plot +
        geom_vline(xintercept = tr.intake, linetype = "dotted")
    }

    return(my_plot)
  }

}
