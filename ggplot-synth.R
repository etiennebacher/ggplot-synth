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
  if (isTRUE(ggplot)) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("You need to install the package `ggplot2` in order to use the option `ggplot = TRUE`.")
    }
    if (!is.null(Legend.position)) {
      message("Argument `Legend.position` doesn't work when `ggplot = TRUE`.")
    }
  }
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
      data_plot <- data.frame(
        x = rep(dataprep.res$tag$time.plot, 2),
        y = c(dataprep.res$Y1plot[, 1], y0plot1[, 1]),
        type = rep(Legend, each = length(dataprep.res$tag$time.plot))
      )
      values <- c("solid", "dashed")
      names(values) <- Legend
      my_plot <- ggplot2::ggplot(
        data_plot,
        ggplot2::aes(x, y, linetype = type)
      ) +
        ggplot2::geom_line() +
        ggplot2::labs(title = ifelse(!is.na(Main), Main, "")) +
        ggplot2::xlab(Xlab) +
        ggplot2::ylab(Ylab) +
        ggplot2::ylim(Ylim) +
        ggplot2::scale_linetype_manual(
          name = NULL,
          values = values
        )
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
        x = rep(dataprep.res$tag$time.optimize.ssr, 2),
        y = c(z0plot[, 1], dataprep.res$Z1[, 1]),
        type = rep(Legend, each = length(dataprep.res$tag$time.optimize.ssr))
      )
      values <- c("solid", "dashed")
      names(values) <- Legend
      my_plot <- ggplot2::ggplot(data_plot, ggplot2::aes(x, y, linetype = type)) +
        ggplot2::geom_line() +
        ggplot2::labs(title = ifelse(!is.na(Main), Main, "")) +
        ggplot2::xlab(Xlab) +
        ggplot2::ylab(Ylab) +
        ggplot2::ylim(Ylim) +
        ggplot2::scale_linetype_manual(
          name = NULL,
          values = values
        )
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
        ggplot2::geom_vline(xintercept = tr.intake, linetype = "dotted")
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
    }
  } else {
    if (isTRUE(ggplot)) {
      my_plot <- my_plot +
        theme(legend.position = "none")
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
  if (isTRUE(ggplot)) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop("You need to install the package `ggplot2` in order to use the option `ggplot = TRUE`.")
    }
  }
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
      data_plot <- data.frame(
        x = dataprep.res$tag$time.plot,
        y = gap[, 1]
      )
      my_plot <- ggplot2::ggplot(data_plot, ggplot2::aes(x, y)) +
        ggplot2::geom_line() +
        ggplot2::labs(title = ifelse(!is.na(Main), Main, "")) +
        ggplot2::xlab(Xlab) +
        ggplot2::ylab(Ylab) +
        ggplot2::ylim(Ylim)
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
      my_plot <- ggplot2::ggplot(data_plot, ggplot2::aes(x, y)) +
        ggplot2::geom_line() +
        ggplot2::labs(title = ifelse(!is.na(Main), Main, "")) +
        ggplot2::xlab(Xlab) +
        ggplot2::ylab(Ylab) +
        ggplot2::ylim(Ylim)
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
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed")

    if (!is.na(tr.intake)) {
      my_plot <- my_plot +
        ggplot2::geom_vline(xintercept = tr.intake, linetype = "dotted")
    }

    return(my_plot)
  }

}
