#' print_table from bruceR (internal-use only)
#'
#' This is copied from bruceR.
#'
#' @param x Matrix, data.frame (or data.table), or any model object (e.g., \code{lm, glm, lmer, glmer, ...}).
#' @param row.names Whether to print row names. Default is \code{TRUE}.
#' @param nsmalls A number or numeric vector specifying the number of decimal places of output. Default is \code{3}.
#'
#' @importFrom stats coef
#' @importFrom glue glue
#'
#' @export
#'
#' @keywords internal
#'
print_table <- function(x, row.names = TRUE, nsmalls = 3) {
  ## Preprocess data.frame ##

  `%nonein%` <- function(x, vector) {
    !any(x %in% vector)
  }

  `%notin%` <- function(x, vector) {
    match(x, vector, nomatch = 0) == 0
  }

  formatF <- function(x, nsmall = 3) {
    format(x, digits = 0, nsmall = nsmall, scientific = F)
  }

  rep_char <- function(char, rep.times) {
    paste(rep(char, rep.times), collapse = "")
  }
  sig.trans <- function(p) {
    ifelse(is.na(p) | p > 1 | p < 0, "",
      ifelse(p < .001, "***",
        ifelse(p < .01, "** ",
          ifelse(p < .05, "*  ",
            ifelse(p < .10, ".  ", "   ")
          )
        )
      )
    )
  }

  p.trans <- function(p, nsmall.p = 3) {
    mapply(function(p, nsmall.p) {
      ifelse(is.na(p) | p > 1 | p < 0, "",
        ifelse(p < 10^-nsmall.p, gsub("0(?=\\.)", "", Glue("<{10^-nsmall.p:.{nsmall.p}}"), perl = T),
          gsub("0(?=\\.)", " ", Glue("{p:.{nsmall.p}}"), perl = T)
        )
      )
    }, p, nsmall.p)
  }


  # Good-looking tabs !!!
  # Print("\u2500\u2501\u2502\u2503\u2504\u2505\u2506\u2507\u2508\u2509")
  linechar1 <- "\u2501" # top-and-down '=' [bug in some computers!]
  linechar2 <- "\u2500" # in-table '-'
  linechar1 <- linechar2

  if (class(x) %nonein% c("matrix", "data.frame", "data.table")) {
    coef.table <- coef(summary(x))
    if (!is.null(coef.table)) x <- coef.table
  }
  x <- as.data.frame(x)
  sig <- NULL
  if (length(nsmalls) == 1) nsmalls <- rep(nsmalls, length(x))
  for (j in 1:length(x)) {
    if (grepl("Pr\\(|pval", names(x)[j])) {
      sig <- formatF(sig.trans(x[, j]), 0) # formatF will make * left-aligned
      names(x)[j] <- "p"
      x[, j] <- p.trans(x[, j])
    } else {
      x[, j] <- formatF(x[, j], nsmalls[j])
    }
    if (grepl("S\\.E\\.|Std\\. Error|^se$", names(x)[j])) {
      x[, j] <- paste0("(", x[, j], ")") # add ( ) to S.E.
      x[grepl("\\.", x[, j]) == FALSE, j] <- "" # remove ( ) from blank S.E.
      if (grepl("S\\.E\\.", names(x)[j]) == FALSE) names(x)[j] <- "S.E."
    }
    # if(grepl("\\[", names(x)[j])) x[,j]=paste0("[", x[,j], ",")
    # if(grepl("\\]", names(x)[j])) x[,j]=paste0(x[,j], "]")
    # if(grepl("^[Ee]stimate$", names(x)[j])) names(x)[j]="Coef."
    names(x)[j] <- gsub(" value|val$", "", names(x)[j])
  }
  if (is.null(sig) == FALSE & "sig" %notin% names(x)) {
    p.pos <- which(names(x) == "p")
    nvars <- length(names(x))
    if (p.pos < nvars) {
      x <- cbind(x[1:p.pos], ` ` = sig, x[(p.pos + 1):nvars])
    } else {
      x <- cbind(x, ` ` = sig)
    }
    x$` ` <- as.character(x$` `)
  }

  ## Compute length to generate line-chars ##
  title.length <- nchar(names(x))
  vars.length <- c() # bug: vars.length=apply(apply(x, 2, nchar), 2, max)
  for (j in 1:length(x)) vars.length[j] <- max(nchar(x[, j]))

  ## Generate a row with 'linechar2' ##
  n.lines <- apply(rbind(title.length, vars.length), 2, max) + 1
  n.lines.rn <- max(nchar(row.names(x))) + 1
  n.lines.table <- n.lines.rn + sum(n.lines)
  line.row <- data.frame()
  for (j in 1:length(x)) {
    line.row[1, j] <- rep_char(linechar2, n.lines[j])
  }
  names(line.row) <- names(x)
  row.names(line.row)[1] <- rep_char(linechar2, n.lines.rn)

  ## Row-bind and deal with 'row.names' (T or F) ##
  x <- rbind(line.row, x)
  if (row.names == FALSE) {
    n.lines.table <- n.lines.table - n.lines.rn
  }
  table.line <- rep_char(linechar1, n.lines.table)

  ## Output ##
  Print(table.line)
  if (row.names == TRUE) {
    cat(rep_char(" ", n.lines.rn))
  }
  for (j in 1:length(x)) {
    cat(sprintf(glue("% {n.lines[j]}s"), names(x)[j]))
  }
  cat("\n")
  for (i in 1:nrow(x)) {
    if (row.names == TRUE) {
      cat(sprintf(glue("%-{n.lines.rn}s"), row.names(x[i, ])))
    }
    for (j in 1:length(x)) {
      cat(sprintf(glue("% {n.lines[j]}s"), ifelse(is.na(x[i, j]) | grepl("NA$", x[i, j]), "", x[i, j])))
    }
    cat("\n")
  }
  Print(table.line)
}
