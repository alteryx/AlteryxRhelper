## Alteryx R Style guide

This is a **modified** version of the [style guide](http://adv-r.had.co.nz/Style.html) by Hadley Wickham

## Notation and naming

### File names

File names should be meaningful and end in `.R`.

    # Good
    fit-models.R
    utility-functions.R

    # Bad
    foo.r
    stuff.r

If files need to be run in sequence, prefix them with numbers:

    0-download.R
    1-parse.R
    2-explore.R

### Object names

> "There are only two hard things in Computer Science: cache invalidation and 
> naming things." 
>
> --- Phil Karlton

Variable names should be __snake_case__ and function names should be __camelCase__. Generally, variable names should be nouns and function names should be verbs. Strive for names that are concise and meaningful (this is not easy!).

```{r, eval = FALSE}
# Good
day_one
day1
computeMean

# Bad
firstDayOfMonth
DayOne
dayone
djm1
compute_mean
```

Where possible, avoid using names of existing functions and variables. This will cause confusion for the readers of your code.

```{r, eval = FALSE}
# Bad
T <- FALSE
c <- 10
mean <- function(x) sum(x)
```

## Syntax

### Spacing

Place spaces around all infix operators (`=`, `+`, `-`, `<-`, etc.). The same rule applies when using `=` in function calls. Always put a space after a comma, and never before (just like in regular English).

```{r, eval = FALSE}
# Good
average <- mean(feet / 12 + inches, na.rm = TRUE)

# Bad
average<-mean(feet/12+inches,na.rm=TRUE)
```

There's a small exception to this rule: `:`, `::` and `:::` don't need spaces around them.

```{r, eval = FALSE}
# Good
x <- 1:10
base::get

# Bad
x <- 1 : 10
base :: get
```

Place a space before left parentheses, except in a function call.

```{r, eval = FALSE}
# Good
if (debug) do(x)
plot(x, y)

# Bad
if(debug)do(x)
plot (x, y)
```

Extra spacing (i.e., more than one space in a row) is ok if it improves alignment of equal signs or assignments (`<-`).

```{r, eval = FALSE}
list(
  total = a + b + c, 
  mean  = (a + b + c) / n
)
```

Do not place spaces around code in parentheses or square brackets (unless there's a comma, in which case see above).

```{r, eval = FALSE}
# Good
if (debug) do(x)
diamonds[5, ]

# Bad
if ( debug ) do(x)  # No spaces around debug
x[1,]   # Needs a space after the comma
x[1 ,]  # Space goes after comma not before
```

### Curly braces

An opening curly brace should never go on its own line and should always be followed by a new line. A closing curly brace should always go on its own line, unless it's followed by `else`.

Always indent the code inside curly braces.

```{r, eval = FALSE}
# Good

if (y < 0 && debug) {
  message("Y is negative")
}

if (y == 0) {
  log(x)
} else {
  y ^ x
}

# Bad

if (y < 0 && debug)
message("Y is negative")

if (y == 0) {
  log(x)
} 
else {
  y ^ x
}
```

It's ok to leave very short statements on the same line:

```{r, eval = FALSE}
if (y < 0 && debug) message("Y is negative")
```

### Line length

Strive to limit your code to 80 characters per line. This fits comfortably on a printed page with a reasonably sized font. If you find yourself running out of room, this is a good indication that you should encapsulate some of the work in a separate function.

### Indentation

When indenting your code, use two spaces. Never use tabs or mix tabs and spaces.

The only exception is if a function definition runs over multiple lines. In that case, indent the second line by four spaces:

```{r, eval = FALSE}
long_function_name <- function(a = "a long argument", 
    b = "another argument", c = "another long argument") {
  # As usual code is indented by two spaces.
}
```

### Assignment

Use `<-`, not `=`, for assignment. \index{assignment}

```{r}
# Good
x <- 5
# Bad
x = 5
```

## Organisation

### Commenting guidelines

Comment your code. Each line of a comment should begin with the comment symbol and a single space: `# `. Comments should explain the why, not the what. \index{comments}

Use commented lines of `-` and `=` to break up your file into easily readable chunks.

```{r, eval = FALSE}
# Load data ---------------------------

# Plot data ---------------------------
```

Use [roxygen2](https://github.com/cran/roxygen2) or [maxygen](https://github.com/gaborcsardi/maxygen) to document functions, if they will eventually find their way into a package.

```r
#' Add two numbers
#'
#' @param x first number
#' @param y second number
#' @return numeric equal to the sum of the two numbers.
#' @seealso \code{\link{nchar}} which this function wraps
#' @examples
#'   add(1, 2)
addTwoNumbers <- function(x, y){
  x + y
}
```