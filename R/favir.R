# Copyright 2010 Benedict Escoto
#
# This file is part of FAViR.

# FAViR is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.

# FAViR is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
# License for more details.

# You should have received a copy of the GNU General Public License
# along with FAViR.  If not, see <http://www.gnu.org/licenses/>.

InitPaper <- function() {
  # Call this when starting a Sweave session
  .InitGraphDir()
  unlockBinding("kGlobals", asNamespace("favir"))
  kGlobals$interactive <<- FALSE
}

kGraphDir <- "favir-graphs"
.InitGraphDir <- function() {
  # Make sure the favir-graphs directory exists
  if(file.access(kGraphDir, mode=0) != 0)
    dir.create(kGraphDir)
}

FavirSweave <- function(filenames) {
  # Convert some sweave files into PDFs
  for (filename in filenames) {
    Sweave(paste(filename, ".Rnw", sep=""))
    texi2dvi(paste(filename, ".tex", sep=""), pdf=TRUE)
  }
}


################################################################
# Generic utility functions

OnError <- function() {
  # Print a stack trace and dump frames
  cat("----------------------------------------------------\n", file=stderr())
  cat("Trackback:\n", file=stderr())
  for(i in 1:sys.nframe()) {
    trace.call <- sys.call(which=i)
    cat("    ", as.character(as.expression(trace.call)), "\n", sep="",
        file=stderr())
  }
  cat("To examine the frame dump, run this R command:\n", file=stderr())
  cat('    load("last.dump.rda"); debugger()\n', file=stderr())
  cat("----------------------------------------------------\n", file=stderr())
  dump.frames(to.file=TRUE)
}

Assert <- function(boolean, err.msg=NULL) {
  # Abort and display traceback and err.msg if boolean is false
  if(isTRUE(boolean))
    return(invisible(NULL))
  call.list <- as.list(match.call())
  cat("----------------------------------------------------\n", file=stderr())
  cat("Error: Expression \"", as.character(as.expression(call.list[[2]])),
      "\"\n    evaluates to \"", boolean, "\" instead of TRUE.\n",
      sep="", file=stderr())
  cat("----------------------------------------------------\n", file=stderr())
  if (!kGlobals$interactive)
    OnError()
  stop(err.msg)
}

AssertSingle <- function(..., err.msg=NULL) {
  # Raise an error message if value does not have length 1
  lengths <- sapply(list(...), length) # vector of lengths
  if(all(lengths == 1))
    return(NULL)
  bad.index <- which(lengths != 1)[1]
  bad.value <- list(...)[[bad.index]]
  bad.call <- as.list(match.call())[[1 + bad.index]]
  cat("----------------------------------------------------\n", file=stderr())
  cat("Error: Expression \"", as.character(as.expression(bad.call)),
      "\"\n    evaluates to ", as.character(bad.value), " and has length ",
      length(bad.value), " instead of 1.\n", sep="", file=stderr())
  cat("----------------------------------------------------\n", file=stderr())
  if (!kGlobals$interactive)
    OnError()
  stop(err.msg)
}

"%SplitOn%" <- function(string, substring) {
  # Split the string based on specified char, like Python's split
  AssertSingle(string, substring)
  split <- strsplit(string, substring, fixed=TRUE)[[1]]

  # Now need to make sure no trailing char
  l <- nchar(string)
  if (identical(substr(string, l, l), substring))
    return(append(split, ""))
  else return(split)
}

RowIndicies <- function(x) seq(length.out=nrow(x))

Last <- function(x, n=1, allow.fewer=FALSE) {
  # Return the last n elements of vector or data frame x
  Assert(n >= 0, "n should be a single non-negative integer")

  if (is.data.frame(x)) {
    if (n == 0)
      return(x[NULL, ])
    len <- nrow(x)
    if (len < n) {
      Assert(allow.fewer, "Data frame is too short in call to Last()")
      return(x)
    }
    if (n == 1)
      return(x[len, , drop=FALSE])
    else return(x[(len - n + 1):len, ])
  }
  
  if (n == 0)
    return(x[NULL])
  len <- length(x)
  if (len < n) {
    Assert(allow.fewer, "Vector is too short in call to Last()")
    return(x)
  }
  return(x[(len - n + 1):len])
}

LookupDF <- function(df, ..., allow.NULL=FALSE) {
  # Lookup and return the relevant rows from data frame df
  #
  # This is similar to vlookup in excel---only one row will be
  # returned.  if allow.NULL is true, if there are no matches NULL
  # will be returned.  Otherwise, signal error.
  lookup.args <- list(...)
  arg.names <- names(lookup.args)
  df.names <- names(df)
  stopifnot(arg.names %in% df.names)
  bool.vec <- rep(TRUE, nrow(df))

  for (name in arg.names) {
    arg.val <- lookup.args[[name]]
    AssertSingle(arg.val, "Vector not allowed inside field condition")
    bool.vec <- bool.vec & (df[[name]] == arg.val)
  }

  result <- df[bool.vec, , drop=FALSE]
  num.matches <- nrow(result)
  stopifnot(num.matches <= 1)
  if (num.matches == 0) {
    stopifnot(allow.NULL)
    return(NULL)
  }
  return(result)
}

FavirLoadModule <- function(filename) {
  # Source the given file and return a new favir.environment
  favirmod.dir <- file.path(system.file(package="favir"), "inst", "modules")
  # try local directory first, then default to favir module dir
  path <- ifelse(file.access(filename, mode=0) == 0,
                 filename,
                 file.path(favirmod.dir, filename))
  e <- new.env()
  print(paste("Sourcing file", path))
  evalq(source(path, local=TRUE), e)
  return(structure(list(e), class="favir.environment"))
}

`$.favir.environment` <- function(fe, objname, ...) {
  # Return an element from a favir.environment
  return(get(objname, fe[[1]], inherits=FALSE))
}

with.favir.environment <- function(data, expr, ...) {
  # Run with inside a favir environment (e.g. "with(data, ls())")
  eval(expr, data[[1]])
}



################################################################
# Code for making pretty tables

### definitions and graphics defaults
# Three sets of colors in color scheme: labeled M (color main),
# A (alternate A), and B (alternateB).  In each group, the
# five colors are about the same hue and arranged from lightest to darkest
favir.colors <- c(M1="#F1EBE6", # Main color defaults to brown
                  M2="#D3BDA8", M3="#B8987C", M4="#7C6C5C", M5="#583008",
                  A1="#C3E5D0", # Alternate 1 is a green
                  A2="#A4E5BC", A3="#569C70", A4="#435A4B", A5="#06401B",
                  B1="#C0CDE9", # Alternate 2 is a blue
                  B2="#7C93C5", B3="#435D94", B4="#35425D", B5="#0A1F4B")

FavirDF <- function(df, field.formatters=list(),
                    field.headings=list(), default.formatter=NULL,
                    row.colors=NULL, orientation="normal",
                    caption="", label=NULL, text.size="normalsize") {
  # Return version of data frame df with latex annotation
  #
  # Args:
  # df - a data frame
  #
  # Output: An object of type favir.data.frame, which is like
  # the original object but contains formatting suggestions.
  class(df) <- unique(c("favir.data.frame", class(df)))
  len <- length(df)

  # Set default formatter
  if (!is.null(default.formatter))
    DefaultFormatter(df) <- default.formatter

  # Set field formatters
  attributes(df)$favir.field.formatters <- list()
  FieldFormatters(df) <- field.formatters

  # Set field headings
  attributes(df)$favir.field.headings <- list()
  FieldHeadings(df) <- field.headings

  # Set row colors
  if (is.null(row.colors))
    RowColors(df) <- DefaultRowColoring(df)
  else RowColors(df) <- row.colors

  TextSize(df) <- text.size

  # Set field groups and group headings
  attributes(df)$favir.field.groups <- list()
  attributes(df)$favir.group.headings <- list()
  attributes(df)$favir.group.formatters <- list()
  attributes(df)$favir.orientation <- orientation
  attributes(df)$favir.caption <- caption
  attributes(df)$favir.label <- label
  attributes(df)$favir.summary.row <- NULL
  
  return(df)
}

DefaultFormatter <- function(fdf) {
  # Return the default cell formatting function
  return(attributes(fdf)$favir.default.formatter)
}

`DefaultFormatter<-` <- function(fdf, value) {
  # Set the default formatting function function
  Assert(mode(value) == "function", "Formatter must be a function")
  attributes(fdf)$favir.default.formatter <- value
  return(fdf)
}

MakeFormatter <- function(digits=0, format="f", big.mark=",",
                          math.mode=TRUE, ...) {
  # Short cut for making a formatter; parameters passed to formatC
  params <- list(digits=digits, format=format, big.mark=big.mark, ...)
  Formatter <- function(val) {
    # This is the actual formatting function
    formatc.input <- ifelse(is.na(val) | is.null(val), 0, val)
    formatc.output <- do.call(formatC, c(list(x=formatc.input), params))
    Helper <- function(single) {
      # Convert a single value from number to character
      if (!math.mode)
        return(single)
      return(paste("$", single, "$", sep=""))
    }
    return(sapply(ifelse(is.na(val) | is.null(val), "", formatc.output), Helper))
  }
  return(Formatter)
}

# Declare some common formatters
.MakePercentFormatter <- function(digits) {
  # Return a formatting function that turns numbers into percents with
  # specified number of digits.  For example, 0.032 -> "3.2"
  Result <- function(x) {
    # This is the formatting function
    if (mode(x) != "numeric")
      return(paste("$", x, "$", sep=""))
    return(ifelse(is.null(x) | is.na(x),
                  "",
                  paste("$", formatC(x * 100, digits=digits, format="f"), "$",
                        sep="")))
  }
  return(Result)
}

formatters <- list(comma=MakeFormatter(digits=0, big.mark=","),
                   comma0=MakeFormatter(digits=0, big.mark=","),
                   comma1=MakeFormatter(digits=1, big.mark=","),
                   comma2=MakeFormatter(digits=2, big.mark=","),
                   comma3=MakeFormatter(digits=3, big.mark=","),
                   flat=MakeFormatter(digits=0, big.mark=""),
                   flat0=MakeFormatter(digits=0, big.mark=""),
                   flat1=MakeFormatter(digits=1, big.mark=""),
                   flat2=MakeFormatter(digits=2, big.mark=""),
                   flat3=MakeFormatter(digits=3, big.mark=""),
                   space=MakeFormatter(digits=0, big.mark=" "),
                   space0=MakeFormatter(digits=0, big.mark=" "),
                   space1=MakeFormatter(digits=1, big.mark=" "),
                   space2=MakeFormatter(digits=2, big.mark=" "),
                   space3=MakeFormatter(digits=3, big.mark=" "),
                   percent=.MakePercentFormatter(1),
                   percent0=.MakePercentFormatter(1),
                   percent1=.MakePercentFormatter(1),
                   percent2=.MakePercentFormatter(2),
                   percent3=.MakePercentFormatter(3))

kGlobals <- list( # This holds global formatting options
  default.formatter=MakeFormatter(), # Default global formatter
  # The list below maps field group names to formatting functions
  field.group.formatters=list(),
  interactive=TRUE # whether we are running inside a paper or not
)

SetGlobalDefaultFormatter <- function(formatter) {
  # Warning, sets global default formatter to given function
  unlockBinding("kGlobals", asNamespace("favir"))
  kGlobals$default.formatter <<- formatter
}

FieldFormatters <- function(fdf) {
  # Return list from df headers to cell formatting functions
  return(attributes(fdf)$favir.field.formatters)
}

`FieldFormatters<-` <- function(fdf, value) {
  # Set the column formatting functions
  Assert(all(names(value) %in% names(fdf)), "Formatted specified for bad col")
  for (name in names(value))
    attributes(fdf)$favir.field.formatters[name] <- value[name]
  return(fdf)
}

FieldHeadings <- function(fdf) {
  # Return list that maps df names to their printable field titles
  return(attributes(fdf)$favir.field.headings)
}

`FieldHeadings<-` <- function(fdf, value) {
  # Set field headings in a favir.data.frame.
  Assert(all(names(value) %in% names(fdf)), "Heading specified for bad col")
  for (name in names(value))
    attributes(fdf)$favir.field.headings[name] <- value[name]
  return(fdf)
}

RowColors <- function(fdf) {
  # Return the row coloring vector
  #
  # Args: fdf - the favir data frame
  #
  # Output: a character vector, one for each row, of latex colors
  return(attributes(fdf)$favir.row.colors)
}

`RowColors<-` <- function(fdf, value) {
  # Set the row colors
  #
  # Args: fdf - the favir data frame to update
  # value - a vector of character color names (one per row of data frame)
  #
  # Output: This updates 
  Assert(length(value) == nrow(fdf), "Color vec doesn't match number of rows")
  attributes(fdf)$favir.row.colors <- value
  return(fdf)
}

DefaultRowColoring <- function(df, primary="white", secondary="colorM1") {
  # Return the default row coloring for data frame df
  if (nrow(df) <= 5)
    return(rep(primary, nrow(df)))
  return(ifelse((1:nrow(df) - 1) %% 6 <= 2, primary, secondary))
}

FieldSettings <- function(fdf, field) {
  # Return field settings for the given df field
  #
  # Args:
  # fdf - a favir.data.frame
  # field - name of the fields in the data frame
  #
  # Output: a list with the following elements
  # heading - the printable field title
  # formatter - a function used to format the data in that field
  Assert(length(field) == 1 && field %in% names(fdf), "Bad column definition")
  result <- list()

  field.headings <- FieldHeadings(fdf)
  if (field %in% names(field.headings))
    result$heading <- field.headings[[field]]

  formatters <- FieldFormatters(fdf)
  if (field %in% names(formatters))
    result$formatter <- formatters[[field]]

  return(result)
}

`FieldSettings<-` <- function(fdf, field, value) {
  # Update the field settings for given field
  #
  # Args:
  # fdf - a favir.data.frame
  # field - the field name to update
  # value - a list of field settings, with names heading and formatter
  #
  # Output: the result is updating the fdf
  Assert(length(field) == 1 && field %in% names(fdf), "Bad column name given")
  Assert(all(names(value) %in% c("heading", "formatter")), "Unknown setting")
  if ("heading" %in% names(value))
    FieldHeadings(fdf)[[field]] <- value$heading
  if ("formatter" %in% names(value)) {
    Assert(mode(value$formatter) == "function", "Formatter must be a function")
    FieldFormatters(fdf)[[field]] <- value$formatter
  }
  return(fdf)
}

ListFieldGroups <- function(fdf) {
  # Return a character vector contining all existing field groups names
  return(names(attributes(fdf)$favir.field.groups))
}

FieldGroup <- function(fdf, group.name, as.names=TRUE) {
  # Return the existing fields in the given group, or NULL if no group exists
  Assert(group.name %in% ListFieldGroups(fdf), "Unknown Group")
  field.indicies <- attributes(fdf)$favir.field.groups[[group.name]]
  if (as.names)
    return(names(fdf)[field.indicies])
  else return(field.indicies)
}

`FieldGroup<-` <- function(fdf, group.name, value) {
  # Set the field group with given name to contain the fields given in value
  Assert(length(unique(value)) == length(value), "Some field repeated")
  if (mode(value) == "character") { # value is vec of field names
    Assert(all(value %in% names(fdf)), "Bad field given")
    value <- which(names(fdf) %in% value)
  } else if (mode(value) == "numeric") { # value is vec of field indicies
    Assert(all(value %in% seq(along=fdf)), "Numeric field out of range")
  } else stop("Invalid type for field group assignment")
  attributes(fdf)$favir.field.groups[[group.name]] <- value
  return(fdf)
}

ParentFieldGroups <- function(fdf, field) {
  # Return a vector of field group names that contain the given field
  #
  # field is interpreted as a name or number depending on its class
  AssertSingle(field)
  if ("character" %in% class(field))
    predicate <- function(group) field %in% FieldGroup(fdf, group)
  else predicate <- function(group) field %in% FieldGroup(fdf, group,
                                                          as.names=FALSE)
  return(Filter(predicate, ListFieldGroups(fdf)))
}

GroupFormatter <- function(fdf, group.name) {
  # Return the group formatter for group.name, or NULL if there isn't one
  return(attributes(fdf)$favir.group.formatters[[group.name]])
}

`GroupFormatter<-` <- function(fdf, group.name, value) {
  # Set the field group formatter for group.name to formatter
  AssertSingle(group.name, value)
  Assert("function" %in% class(value), "Formatter must be a function")
  attributes(fdf)$favir.group.formatters[[group.name]] <- value
  return(fdf)
}

GetGlobalGroupFormatter <- function(group.name) {
  # Return the global field formatter for the given group, or NULL
  return(kGlobals$field.group.formatters[[group.name]])
}

SetGlobalGroupFormatter <- function(group.name, formatter) {
  # Set globally the given formatter to be used for that field group
  #
  # Args:
  # group.name - the name of a field group
  # formatter - the cell formatting function to use for that group's data
  AssertSingle(group.name, formatter)
  Assert("function" %in% class(formatter), "Second arg needs to be function")
  unlockBinding("kGlobals", asNamespace("favir"))
  kGlobals$field.group.formatters[[group.name]] <<- formatter
}

# Set a couple of common defaults
SetGlobalGroupFormatter("money", formatters$comma)
SetGlobalGroupFormatter("origin", formatters$flat)
SetGlobalGroupFormatter("LDF", formatters$flat2)
SetGlobalGroupFormatter("dev", formatters$flat)
SetGlobalGroupFormatter("percent", formatters$percent1)

.IsGroupContiguous <- function(fdf, group.name) {
  # Returns TRUE if the group joins contiguous fields, false otherwise
  field.indicies <- FieldGroup(fdf, group.name, as.names=FALSE)
  highlo <- range(field.indicies)
  if (length(field.indicies) - 1 == highlo[2] - highlo[1])
    return(TRUE)
  return(FALSE)
}

GroupHeading <- function(fdf, group.name) {
  # Return the heading for given group, or NULL if group has no heading
  Assert(length(group.name) == 1 && group.name %in% ListFieldGroups(fdf))
  return(attributes(fdf)$favir.group.headings[[group.name]])
}

`GroupHeading<-` <- function(fdf, group.name, value) {
  # Set the displayed heading for group to the given value
  Assert(length(group.name) == 1 && group.name %in% ListFieldGroups(fdf))
  AssertSingle(length(value) == 1, "Group heading should be a single string")
  Assert(.IsGroupContiguous(fdf, group.name),
         "Group headings can only be applied to contiguous fields")
  attributes(fdf)$favir.group.headings[[group.name]] <- value
  return(fdf)
}

.SortedGroupHeadings <- function(fdf) {
  # Return char vec of groups with headings in column order
  GetFirstCol <- function(group.name) {
    # Return the index of the first column in the given group
    return(min(FieldGroup(fdf, group.name, as.names=FALSE)))
  }

  groups <- names(attributes(fdf)$favir.group.headings)
  if (length(groups) == 0)
    return(NULL)
  first.cols <- sapply(groups, GetFirstCol)
  return(groups[order(first.cols)])
}

Orientation <- function(fdf) {
  # Return the orientation of the table
  return(attributes(fdf)$favir.orientation)
}

`Orientation<-` <- function(fdf, value) {
  # Set the orientation for the table.
  Assert(identical(value, "normal") || identical(value, "sideways"))
  attributes(fdf)$favir.orientation <- value
  return(fdf)
}

Caption <- function(fdf) {
  # Return the caption of the table
  return(attributes(fdf)$favir.caption)
}

`Caption<-` <- function(fdf, value) {
  # Set the caption string for the table.
  AssertSingle(value, "Caption should be a single string")
  attributes(fdf)$favir.caption <- value
  return(fdf)
}

Label <- function(fdf) {
  # Return the table's latex reference label
  return(attributes(fdf)$favir.label)
}

`Label<-` <- function(fdf, value) {
  # Set the table's latex reference label
  AssertSingle(value, "Label should be a single string")
  attributes(fdf)$favir.label <- value
  return(fdf)
}

SummaryRow <- function(fdf) {
  # Return the table's summary row
  return(attributes(fdf)$favir.summary.row)
}

`SummaryRow<-` <- function(fdf, value) {
  # Set the table's summary row.  See SummaryRow() comments.
  Assert(is.null(value) || "list" %in% class(value), "Expecting list as arg")
  if (!is.null(value))
    Assert(all(names(value) %in% names(fdf)), "Invalid field name")
  attributes(fdf)$favir.summary.row <- value
  return(fdf)
}

TextSize <- function(fdf) {
  # Get the favir data frame's text size
  return(attributes(fdf)$favir.text.size)
}

`TextSize<-` <- function(fdf, value) {
  # Set the fdf's text size
  AssertSingle(value)
  options <- c("tiny", "scriptsize", "footnotesize", "small", "normalsize",
               "large", "Large", "LARGE", "huge", "Huge")
  Assert(value %in% options, "Invalid text size given")
  attributes(fdf)$favir.text.size <- value
  return(fdf)
}

            
######### Latex code detail section - create latex from a favir df

.SplitHeadings <- function(char.vec, threshold=11, max.rows=3,
                           split.char=" ") {
  # Split the character vector into multiple rows
  #
  # Input:
  # char.vec - a vector of strings to split
  # threshold - the number of characters per string required to split
  # max.rows - the maximum number of rows to split the string into
  #
  # Output: a matrix of strings.  Each column corresponds to the same
  # entry in char.vec.  Unused entries, from the top, will be filled
  # with the empty string "".
  SplitSingle <- function(s) {
    # Split a single string into a vector of multiple strings
    str.comps <- s %SplitOn% split.char
    split.list <- SplitHelper(str.comps, max.rows)
    return(sapply(split.list, Collapse))
  }

  SplitHelper <- function(char.vec, max.rows) {
    # Return a list of string components
    #
    # The basic idea is to start from the end, and keep prepending
    # words until we are over the threshold, then move on to the
    # previous line.
    if (max.rows == 1 || length(char.vec) <= 1)
      return(list(char.vec))
    cur <- Last(char.vec)
    for (i in (length(char.vec) - 1):1) {
      if (nchar(Collapse(c(char.vec[i], cur))) > threshold) {
        split.rest <- SplitHelper(char.vec[1:i], max.rows - 1)
        return(c(split.rest, list(cur)))
      }
      cur <- c(char.vec[i], cur)
    }
    return(list(cur))
  }

  Collapse <- function(char.vec) {
    # Join the pieces of char.vec together again and return single string
    return(paste(char.vec, collapse=split.char))
  }
  
  result.list <- lapply(char.vec, SplitSingle)
  num.rows <- max(sapply(result.list, length))
  result <- matrix("", nrow=num.rows, ncol=length(char.vec))
  for (i in seq(along=char.vec)) {
    splat <- result.list[[i]]
    result[(num.rows - length(splat) + 1):num.rows, i] <- splat
  }
  return(result)
}

kTablePrefix1 <- "\\begin{figure}[htp]"
kTablePrefix2 <- "
\\begin{center}
\\begin{tikzpicture}
\\node (tbl) {
\\begin{tabular}"
kTableSuffix1 <- "\\end{tabular}};
\\begin{pgfonlayer}{background}
\\draw[rounded corners,left color=colorM2,right color=colorM3,draw=colorM5]
    ($(tbl.north west)+(0.14,0.00)$) rectangle
           ($(tbl.south east)-(0.14,0.00)$);
\\end{pgfonlayer}
\\end{tikzpicture}"
kTableSuffix2 <- "\\end{center}\\end{figure}\n"
kSidewaysTablePrefix1 <- "\\begin{sidewaysfigure}"
kSidewaysTableSuffix2 <- "\\end{center}\\end{sidewaysfigure}\n"

.ColumnTypes <- function(data.matrix) {
  # Return the latex column types for each column
  # Below, subtract 2 because of the math mode symbols $
  data.lengths <- apply(data.matrix, 2,
                        function(col) max(nchar(col)) - 2)
  return(paste("D{!}{!}{", data.lengths, ".0}", sep=""))
}

PreLatexFDF <- function(favir.df) {
  # Return a pre-latex version favir data frame 
  #
  # Args:
  # favir.df - an object of class favir.data.frame
  #
  # Output: An object of type favir.prelatex.table, which can easily
  # be transformed into latex
  GetFormatter <- function(field.index) {
    # Return the formatting function for the given field
    #
    # Here is the order of precedence given to conflicting field
    # formatting directives:
    #   1.  Explicit local (to favir.df) field formatter
    #   2.  Local field group formatter (in alphabetical order)
    #   3.  Global field group formatter (in alphabetical order)
    #   4.  Local default formatter
    #   5.  Global default formatter
    AssertSingle(field.index)
    field.name <- names(favir.df)[field.index]
    ff <- FieldFormatters(favir.df)[[field.name]]
    if (!is.null(ff)) # Choice 1
      return(ff)

    field.groups <- ParentFieldGroups(favir.df, field.index)
    for (field.group in field.groups)
      if (!is.null(GroupFormatter(favir.df, field.group)))
        return(GroupFormatter(favir.df, field.group)) # Choice 2        
    for (field.group in field.groups)
      if (!is.null(GetGlobalGroupFormatter(field.group)))
        return(GetGlobalGroupFormatter(field.group)) # Choice 3

    if (!is.null(DefaultFormatter(favir.df)))
      return(DefaultFormatter(favir.df)) # Choice 4
    return(kGlobals$default.formatter) # Choice 5
  }

  DataMatrix <- function() {
    # Return character matrix of formatted values
    m <- matrix(NA, nrow=nrow(favir.df), ncol=length(favir.df))
    for (i in seq(along=favir.df)) {
      data.col <- favir.df[[i]]
      if ("factor" %in% class(data.col))
        data.col <- as.character(data.col)
      m[, i] <- GetFormatter(i)(data.col)
    }
    return(m)
  }

  DataRowPrefixes <- function(row.colors) {
    # Return prefixes to each data row
    return(paste("\\rowcolor{", row.colors, "}", sep=""))
  }

  GroupRow <- function() {
    # Return string containing the latex code for the group headings
    groups <- .SortedGroupHeadings(favir.df)
    if (length(groups) == 0)
      return(NULL)

    heading.vec <- NULL
    last.col <- 0
    for (group in groups) {
      begin.col <- min(FieldGroup(favir.df, group, as.names=FALSE))
      heading.vec <- c(heading.vec, rep("", begin.col - last.col - 1))
      last.col <- max(FieldGroup(favir.df, group, as.names=FALSE))
      col.length <- last.col - begin.col + 1
      heading <- GroupHeading(favir.df, group)
      heading.vec <- c(heading.vec, paste("\\multicolumn{", col.length,
                                          "}{c}{", heading, "}", sep=""))
    }
    return(paste(heading.vec, collapse=" & "))
  }

  GroupRules <- function() {
    # Return the latex code to make midrules after the group headings
    groups <- .SortedGroupHeadings(favir.df)
    if (length(groups) == 0)
      return(NULL)
    begin.cols <- sapply(groups,
              function(g) min(FieldGroup(favir.df, g, as.names=FALSE)))
    end.cols <- sapply(groups,
              function(g) max(FieldGroup(favir.df, g, as.names=FALSE)))
    return(paste("\\cmidrule(rl){", begin.cols, "-", end.cols, "}",
                 sep="", collapse=""))
  }
  
  HeadingMatrix <- function() {
    # Return heading matrix with long headings split between rows
    headings <- names(favir.df)
    for (i in seq(along=headings))
      if (headings[i] %in% names(FieldHeadings(favir.df)))
        headings[i] <- FieldHeadings(favir.df)[[headings[i]]]
    return(.SplitHeadings(headings))
  }

  FormattedHeadings <- function(heading.matrix) {
    # Return heading matrix enclosed in a multicolumn environment
    result <- paste("\\multicolumn{1}{c}{", heading.matrix, "}", sep="")
    dim(result) <- dim(heading.matrix)
    return(result)
  }

  PrefixAndSuffix <- function() {
    # Return a list that has prefix, suffix1 and suffix 2 set
    result <- list()
    if (identical(Orientation(favir.df), "normal")) {
      # Table has normal orientation
      result$prefix1 <- kTablePrefix1
      result$suffix2 <- kTableSuffix2
    } else { # Table is sideways
      result$prefix1 <- kSidewaysTablePrefix1
      result$suffix2 <- kSidewaysTableSuffix2
    }
    result$prefix2 <- kTablePrefix2
    result$suffix1 <- kTableSuffix1
    return(result)
  }

  FormatSummaryRow <- function() {
    # Return the formatted entries in the summary row as a vector, or NULL
    if (is.null(SummaryRow(favir.df)))
      return(NULL)

    Helper <- function(field.index) {
      # Return formatted row entry
      field <- names(favir.df)[field.index]
      raw.entry <- SummaryRow(favir.df)[[field]]
      if (is.null(raw.entry))
        return("")
      #if ("character" %in% class(raw.entry)) # center character strings
      #  return(paste("\\multicolumn{1}{c}{", raw.entry, "}", sep=""))
      return(GetFormatter(field.index)(raw.entry))
    }

    formatted <- sapply(seq(along=favir.df), Helper)
    hline <- "\\arrayrulecolor{colorM5}\\hline"
    # use first row's color
    row.color <- DataRowPrefixes(RowColors(favir.df)[1]) 
    strut <- "\\rule{0pt}{1.2em}"
    formatted[1] <- paste(hline, row.color, strut, formatted[1], sep="")
    return(formatted)
  }
  
  result <- PrefixAndSuffix()
  result$text.size <- paste("\\", TextSize(favir.df), sep="")
  result$group.row <- GroupRow()
  result$group.rules <- GroupRules()
  result$heading.matrix <- HeadingMatrix()
  result$formatted.headings <- FormattedHeadings(result$heading.matrix)
  result$data.prefix <- "\\addlinespace"
  result$data.matrix <- DataMatrix()
  # extra data to be inserted before any data row
  result$extra.rows <- rep("", nrow(result$data.matrix))
  result$column.types <- .ColumnTypes(result$data.matrix)
  result$caption <- paste("\\caption{", Caption(favir.df), "}",
                          collapse="", sep="")
  result$label <- paste("\\label{", Label(favir.df), "}",
                        collapse="", sep="")
  result$heading.row.prefixes <- rep("", nrow(result$heading.matrix))
  result$data.row.prefixes <- DataRowPrefixes(RowColors(favir.df))
  result$summary.row <- FormatSummaryRow()
  
  class(result) <- "favir.prelatex"
  return(result)
}

print.favir.prelatex <- function(x, ...) {
  # Print a PreLatex object
  Assert("favir.prelatex" %in% class(x))
  column.spec <- paste("{", paste(x$column.types, collapse=""), "}",
                       collapse="")

  if (is.null(x$group.row))
    group.code <- ""
  else group.code <- paste(x$group.row, "\\\\\n", x$group.rules,
                           "\n", collapse="")

  heading.rows <- apply(x$formatted.headings, 1,
                        function(...) paste(..., collapse=" & "))
  heading <- paste(x$heading.row.prefixes, heading.rows,
                   sep="\n", collapse="\\\\\n")
  
  data.rows <- apply(x$data.matrix, 1,
                     function(...) paste(..., collapse=" & "))
  data <- paste(x$extra.rows, x$data.row.prefixes,
                data.rows, sep="\n", collapse="\\\\\n")
  summary.row <- ifelse(is.null(x$summary.row), "",
                        paste("\\\\\n", paste(x$summary.row, collapse=" & ")))
  result <- paste(x$prefix1, x$text.size, x$prefix2, column.spec, group.code,
                  heading, "\\\\\n", x$data.prefix, data, summary.row,
                  x$suffix1, x$caption, x$label, x$suffix2, collapse="")
  cat(result)
  return(invisible(result))
}

RBindPreLatex <- function(..., include.headings=TRUE) {
  # Join number of PreLatex objects together in a single PreLatex object
  RBindLenient <- function(matrix1, matrix2) {
    # Bind matricies 1 and 2 together on row, but allow for column padding
    Pad <- function(m, n) {
      # Add columns of "" to matrix m so it's number of col is n
      padding <- matrix("", nrow=nrow(m), ncol=n - ncol(m))
      return(cbind(m, padding))
    }
    if (ncol(matrix1) == ncol(matrix2))
      return(rbind(matrix1, matrix2))
    if (ncol(matrix1) < ncol(matrix2))
      return(rbind(Pad(matrix1, ncol(matrix2)), matrix2))
    return(rbind(matrix1, Pad(matrix2, ncol(matrix1))))
  }

  args <- list(...)
  Assert(all(sapply(args, function(obj) "favir.prelatex" %in% class(obj))),
         "RBindPreLatex takes prelatex objects as arguments")
  Assert(length(args) >= 2)
  result <- args[[1]]
  for (i in 2:length(args)) {
    if (include.headings) {
      if (!is.null(args[[i]]$group.row))
        group.code <- paste(args[[i]]$group.row, "\\\\\n", args[[i]]$group.rules,
                            "\n", collapse="")
      else group.code <- ""
      heading.rows <- apply(args[[i]]$formatted.headings, 1,
                            function(...) paste(..., collapse=" & "))
      heading.code <- paste(args[[i]]$heading.row.prefixes, heading.rows,
                            sep="\n", collapse="\\\\\n")
      extra.row <- paste("\\addlinespace", group.code, heading.code, "\\\\",
                         args[[i]]$data.prefix, args[[i]]$extra.rows[1],
                         sep="", collapse="")
    } else {
      #num.col <- max(ncol(result$data.matrix), ncol(args[[i]]$data.matrix))
      #rule <- paste("\\cmidrule(rl){1-", num.col, "}\n", sep="")
      extra.row <- paste(args[[i]]$data.prefix, args[[i]]$extra.rows[1],
                         sep="", collapse="")
    }
    result$extra.rows <- c(result$extra.rows, extra.row,
                           tail(args[[i]]$extra.rows, -1))
    result$data.matrix <- RBindLenient(result$data.matrix,
                                       args[[i]]$data.matrix)
    result$data.row.prefixes <- c(result$data.row.prefixes,
                                  args[[i]]$data.row.prefixes)
    result$column.types <- .ColumnTypes(result$data.matrix)
  }
  return(result)
}

print.favir.data.frame <- function(x, ...) {
  # Print a favir data frame
  Assert("favir.data.frame" %in% class(x))
  return(invisible(print(PreLatexFDF(x))))
}

#######################################################################
# Non-table related latex code

IncludeGraph <- function(graph, caption="", label=NULL,
                         width=12.5, height=12.5, filename=NULL) {
  # Write the graph to a PDF file and produce the latex to include the graph
  #
  # Arguments:
  # graph - the ggplot object to include
  # filename - file to save plot to.  Otherwise derive name from graph
  # caption - the latex caption for the graph
  # label - the latex reference label for the graph
  # width, height - dimensions in centimeters for the graph
  #
  # Output: No return value, but create a PDF file on disk and print
  # latex code necessary to include PDF in a document.
  thunk <- function() print(graph)
  graph.name <- as.character(as.expression(as.list(match.call())[[2]]))
  .IncludeGraphHelper(thunk, graph.name, caption=caption, label=label,
                      width=width, height=height, filename=filename)
}

IncludeGrid <- function(graph.list, ...) {
  # Write a grid of graphs to PDF and output latex
  #
  # Arguments: like IncludeGraph, but take graph.list instead of graph
  # graph.list - a list of graphs or other printable objects.  The keys must
  #              have the form "x.y" to indicate the row and column number.
  ParseName <- function(key) {
    # Parse the graph list names and return two vectors
    split <- key %SplitOn% "."
    Assert(length(split) == 2, "List names must have the form 'XX.YY'")
    coords <- as.integer(split)
    Assert(all(coords > 0 && coords < 99), "Bad coordinates given")
    return(coords)
  }

  Assert(length(graph.list) == length(names(graph.list)), "Unnamed element")
  Assert(length(graph.list) >= 2, "Call IncludeGrid with two or more graphs")
  args <- list(...)
  if ("label" %in% names(args))
    graph.name <- args$label
  else {
    Assert("caption" %in% names(args),
           "Unable to name your graph--please provide a label or caption")
    graph.name <- args$caption
  }

  coord.matrix <- sapply(names(graph.list), ParseName)
  Assert(nrow(coord.matrix) == 2, "Expected a two dimensional grid")
  num.viewports <- c(max(coord.matrix[1, ]), max(coord.matrix[2, ]))
    
  thunk <- function() {
    # .IncludeGraphHelper runs this to actually print the total graph
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(num.viewports[1],
                                             num.viewports[2])))
    for (i in 1:length(graph.list))
      print(graph.list[[i]], vp=viewport(layout.pos.row=coord.matrix[1, i],
                                         layout.pos.col=coord.matrix[2, i]))
  }

  .IncludeGraphHelper(thunk, graph.name, ...)
}

.IncludeGraphHelper <- function(thunk, graph.name, filename=NULL, caption="",
                                label=NULL, width=12.5, height=12.5) {
  # Helper function for IncludeGraph and IncludeGrid
  AssertSingle(caption, width, height)
  if (is.null(filename)) { # Extract filename candidate from graph name
    # Now get rid of spaces and periods, which latex doesn't like in filenames
    prefix <- gsub(" ", "_", gsub(".", "_", graph.name, fixed=TRUE), fixed=TRUE)
    filename <- file.path(kGraphDir, paste(prefix, ".pdf", sep=""))
  }
  pdf(file=filename, paper="special",
      width=width/2.54, height=height/2.54) # pdf() dims in inches
  thunk()
  dev.off()
  cat(paste("\\begin{figure}\\begin{center}\\includegraphics[width=",
            width, "cm,height=", height, "cm]{",
            filename, "}\\caption{", caption, "}", sep=""))
  if (!is.null(label))
    cat(paste("\\label{", label, "}", sep=""))
  cat("\\end{center}\\end{figure}")
}

.SetTheme <- function() {
  # Set the theming parameters for ggplot2
  theme_update(panel.background=theme_rect(fill=favir.colors["M1"],
                                           colour=favir.colors["M1"]),
               legend.key=theme_rect(fill=favir.colors["M1"],
                                     colour=favir.colors["M1"]),
               strip.background=theme_rect(fill=favir.colors["M2"],
                                           colour=favir.colors["M2"]))
  update_geom_defaults("point", aes(colour=favir.colors["M5"],
                                    size=2.5, shape=1))
  update_geom_defaults("jitter", aes(colour=favir.colors["M5"],
                                     size=2.5, shape=1))
  update_geom_defaults("line", aes(colour=favir.colors["A5"]))
  update_geom_defaults("abline", aes(colour=favir.colors["A5"]))
  update_geom_defaults("smooth", aes(colour=favir.colors["A5"]))
  update_geom_defaults("bar", aes(colour=favir.colors["M3"],
                                  fill=favir.colors["M3"]))
}
.SetTheme()

kPrelude1 <- ("\\documentclass[letterpaper,12pt]{article}
\\usepackage[hmargin=1in,bmargin=1.5in,tmargin=1.2in]{geometry}
\\usepackage{Sweave}
\\usepackage{color}
\\usepackage{booktabs}
\\usepackage{colortbl}
\\usepackage{dcolumn}
\\usepackage{multirow}
\\usepackage{rotating}
\\usepackage{tabularx}

\\usepackage{tikz}
\\usetikzlibrary{calc}
\\pgfdeclarelayer{background}
\\pgfdeclarelayer{foreground}
\\pgfsetlayers{background,main,foreground}")

kPrelude2 <- ("%%%%%%%%%%%%%%% End Colors, Start Logo

\\newcommand{\\favirlogo}{
\\begin{minipage}{3cm}  
\\fontfamily{pcr}\\bfseries\\Huge
\\begin{tikzpicture}
  % Length and size definitions
  \\def\\logobackwidth{1.3}
  \\def\\logobackheight{0.5}
  \\def\\logobackradius{0.15}
  \\def\\logobackgap{0.03}
  \\def\\logotextjitter{0.05}

  % Fill in one background rectangle by tracing clockwise path
  \\def\\logopath{(\\logobackgap, \\logobackgap) -- +(0, \\logobackheight)
                         --  +($(\\logobackwidth, \\logobackheight)
                                    -(\\logobackradius, 0)$)
                         arc (90:0:\\logobackradius)
                         --  ++($(0, \\logobackradius)-(0, \\logobackheight)$)
                         --  +(\\logobackwidth,0)
                         -- cycle}
  \\fill[colorM1] \\logopath;
  % Then copy it with two reflections and one rotation
  \\fill[colorM1,cm={1,0,0,-1,(0,0)}] \\logopath;
  \\fill[colorM1,cm={-1,0,0,1,(0,0)}] \\logopath;
  \\fill[colorM1,cm={-1,0,0,-1,(0,0)}] \\logopath;

  % Now place letters
  \\node[colorM5] at (-0.78,\\logotextjitter) {F};
  \\node[colorM5] at (-0.32,-\\logotextjitter) {A};
  \\node[colorM5] at (0.05,\\logotextjitter) {V};
  \\node[colorM5] at (0.4,-\\logotextjitter) {\\LARGE i};
  \\node[colorB3] at (0.79,\\logotextjitter) {R};
  \\fill[colorB3] (0.383,0.125) circle (0.06); % dot the i
\\end{tikzpicture}
\\end{minipage}}

%%%%%%%%%%%%%%%%% End Logo, Start Headers and page definition

\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\renewcommand{\\headrule}{{\\color{colorM5}%
  \\hrule width\\headwidth height\\headrulewidth \\vskip-\\headrulewidth}}
\\rfoot{\\favirlogo\\hspace{2cm}}
\\cfoot{\\thepage}

%%%%%%%%%%%%%%%% End Headers, Preamble

\\begin{document}
\\thispagestyle{plain}")

IncludePrelude <- function(title, author, subtitle="", header.lines="") {
  # Insert the latex prelude into the document
  cat(kPrelude1)
  cat(.MakeColorLines(favir.colors))
  cat(header.lines)
  cat(kPrelude2)

  AssertSingle(title, author, subtitle)
  cat(.MakeTitleLines(title, author, subtitle))
}

.MakeColorLines <- function(named.color.vec) {
  # Return latex character string defining the given colors
  #
  # Argument is a vector of colors like "#35425D".  Each should be
  # named; each name will be defined to be that color
  #
  # Output: color definition string lines that look like this:
  # \definecolor{colorM1}{RGB}{242,236,231}
  # where M1 was the name of the color in this case
  Helper <- function(color) {
    # Return a color triple like "88,248,8"
    return(paste(as.vector(col2rgb(color)), collapse=","))
  }
  stopifnot(length(names(named.color.vec)) == length(named.color.vec))
  return(paste("\\definecolor{color", names(named.color.vec), "}{RGB}{",
               sapply(named.color.vec, Helper), "}\n", sep=""))
}

kURL <- "http://www.favir.net"
.MakeTitleLines <- function(title, author, subtitle) {
  # Return latex code for the title and author of a paper
  title.line <- paste("\\begin{center}{\\LARGE", title, "}\\\\")
  subtitle.line <- ifelse(nchar(subtitle) == 0, "",
                          paste("\\vspace{1ex}{\\large", subtitle, "}\\\\"))
  author.line <- paste("\\vspace{1em}{\\large By ", author, "}\\\\")
  logo.line <- paste("{\\footnotesize
\\vspace{2em} \\hspace{-1.9in} \\scalebox{2}{\\favirlogo}\\\\
\\vspace{1ex}
\\noindent This paper is produced mechanically as part of FAViR.\\\\
See \\texttt{", kURL, "} for more information.\\\\ \\vspace{1em}}", sep="")
  return(paste(title.line, subtitle.line, author.line, logo.line,
               "\\end{center}\n", sep="\n"))
}

IncludeLegal <- function(author, year) {
  # Return latex code for legal notice at the end of papers
  copyright <- paste("Copyright \\copyright \\hspace{0.7ex}", year, author)
  source.license <- paste("
This paper is part of the FAViR project.  All the R source code used
to produce it is freely distributable under the GNU General Public
License.  See \\texttt{", kURL, "} for more information on
FAViR or to download the source code for this paper.", sep="")
  gnu.permissive <- "
Copying and distribution of this paper itself, with or without
modification, are permitted in any medium without royalty provided the
copyright notice and this notice are preserved.  This paper is offered
as-is, without any warranty."
  vague.cya <- "
This paper is intended for educational purposes only and should not be
used to violate anti-trust law.  The authors and FAViR editors do not
necessarily endorse the information or techniques in this paper and
assume no responsibility for their accuracy."
  cat(paste(copyright, source.license, gnu.permissive, vague.cya, sep="\n\n"))
}
