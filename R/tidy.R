## S3 wrappers and tidiers for lda.bases
##
## by Artem Sokolov

#' Linear Discriminant Analysis
#'
#' Linear Discriminant Analysis finds directions in high-dimensional space that maximize the ratio of
#'   inter-class variance / within-class variance.
#'
#' @param X matrix or data.frame, where each row is a sample and each column is a feature.
#' @param y a vector of class labels with length equal to the number of rows in X. If X is a data.frame,
#'   y can also be an index (integer) or name (character) of the column to be used as labels.
#' @param formula an alternative way to specify the LDA model. See below for examples.
#' @param data a data.frame associated with the formula-based model specification.
#' @param lambda regularization parameter. Higher values lead to less overfitting and the cost of poorer separation between the classes. (Default: 0.1)
#' @param ... further arguments passed to or from other methods
#' @return An object of class LDA that contains variance quotients in $d and LDA component loadings in $v. See \code{tidy.LDA}, \code{glance.LDA} and \code{augment.LDA} for tidy downstream usage of the LDA object.
#' @examples
#' ## All of the following are equivalent ways to train an LDA model on the built-in iris dataset
#' model1 <- LDA( iris, "Species" )
#' model2 <- LDA( iris, 5 )
#' model3 <- LDA( iris[,1:4], iris[,5] )
#' model4 <- LDA( as.matrix( iris[,1:4] ), iris[,5] )
#' model5 <- LDA( Species ~ ., iris )
#' model6 <- LDA( Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, iris )
#' @export
LDA <- function( X, ... )
    UseMethod( "LDA" )

#' @describeIn LDA accepts a data.frame and column index / name, or an external labels vector
#' @export
LDA.data.frame <- function( X, y, lambda = 0.1, ... )
{
    ## Using a specific column as labels
    if( length(y) == 1 )
    {
        ## y is a column name
        if( is.character( y ) )
        {
            i <- grep( y, colnames(X) )
            if( length(i) < 1 )
                stop( "No such column ", y )
            if( length(i) > 1 )
                stop( "Multiple columns match ", y )
            return( LDA( X, i, lambda ) )
        }

        ## y is a column index
        if( is.numeric( y ) )
        {
            ## Isolate the corresponding portions of the data.frame and forward the call
            return( LDA( as.matrix(X[,-y]), X[,y], lambda ) )
        }

        ## All other objects are unrecognized
        stop( "y must be a column name, column index, or a labels vector" )
    }

    ## Providing new labels
    LDA( as.matrix(X), y, lambda )
}

#' @describeIn LDA accepts a matrix and an external labels vector
#' @export
LDA.matrix <- function( X, y, lambda = 0.1, ... )
{
    res <- lda.bases( X, y, lambda )
    class( res ) <- "LDA"
    res
}

#' @describeIn LDA accepts a formula and the corresponding data.frame
#' @export
LDA.formula <- function( formula, data, lambda = 0.1, ... )
{
    ## Retrieve the formula terms and variable names
    tt <- stats::terms( formula, data = data )
    vv <- unlist( lapply( as.list( attr( tt, "variables" ) )[-1], deparse ) )
    
    ## Identify the response variable
    r <- attr( tt, "response" )
    if( r == 0 )
        stop( "Please provide a response variable" )

    ## Route the call to LDA.data.frame
    LDA( data[,vv], r, lambda )
}

#' Implementation of broom::tidy() for LDA objects
#'
#' Produces a summary of an LDA object, expressed as a data.frame
#' 
#' @param L an LDA object, as produced by LDA()
#' @return a data.frame with LDA component loadings
#' @examples
#' \dontrun{
#' model <- LDA( Species ~ ., iris )
#' broom::tidy( model )
#' ##       Feature      LDA1        LDA2
#' ## 1 Sepal.Length  0.626970 -0.01449191
#' ## 2  Sepal.Width  1.181041 -2.14917074
#' ## 3 Petal.Length -2.135565  0.52600987
#' ## 4  Petal.Width -1.828151 -1.89227871
#' ##  
#' }
#' @export
tidy.LDA <- function( L )
{
    X <- as.data.frame( L$v )
    X <- cbind( Feature = rownames(X), X )
    rownames(X) <- NULL
    X
}

#' Implementation of broom::glance() for LDA objects
#'
#' Produces a one-line summary of an LDA object
#'
#' @param L an LDA object, as produced by LDA()
#' @return a data.frame of eigenvalues from the associated generalized eigenvalue problem
#' @examples
#' \dontrun{
#' model <- LDA( Species ~ ., iris )
#' broom::glance( model )
#' ##       VQ1       VQ2
#' ## 1 23.42386 0.2167796
#' ##
#' }
#' @export
glance.LDA <- function( L )
{
    X <- as.list( L$d )
    names( X ) <- paste0( "VQ", 1:length(X) )
    as.data.frame(X)
}

#' Implementation of broom::augment() for LDA objects
#'
#' Augments a matrix or a data.frame with its projections onto the LDA components
#'
#' @param L an LDA object, as produced by LDA()
#' @param newdata a matrix or a data.frame containing data to be projected onto the LDA components
#' @return a data.frame of eigenvalues from the associated generalized eigenvalue problem
#' @examples
#' \dontrun{
#' model <- LDA( Species ~ ., iris )
#' head( broom::augment( model, iris ) )
#' ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species     LDA1      LDA2
#' ## 1          5.1         3.5          1.4         0.2  setosa 3.975771 -7.238048
#' ## 2          4.9         3.0          1.4         0.2  setosa 3.259856 -6.160564
#' ## 3          4.7         3.2          1.3         0.2  setosa 3.584227 -6.640101
#' ## 4          4.6         3.1          1.5         0.2  setosa 2.976313 -6.318533
#' ## 5          5.0         3.6          1.4         0.2  setosa 4.031178 -7.451516
#' ## 6          5.4         3.9          1.7         0.4  setosa 3.629979 -8.322717
#' ##
#' }
#' @export
augment.LDA <- function( L, newdata )
{
    ## Ensure the new data contains all the required variables
    vDiff <- setdiff( rownames( L$v ), colnames( newdata ) )
    if( length(vDiff) > 0 )
        stop( "The following variables are missing from newdata:\n",
             paste( vDiff, collapse=" " ) )

    ## Extract the relevant portion of the input matrix / data.frame
    ##  and project it onto the LDA components
    X <- as.matrix( newdata[, rownames(L$v)] )
    Xp <- X %*% L$v

    ## Augment the original matrix / data.frame
    cbind( newdata, Xp )
}

