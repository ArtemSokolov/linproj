## Linear Discriminant Analysis
##
## by Artem Sokolov

## Finds the first nz eigenvalues/-vectors of the generalized
##   eigenvalue problem A x = \lambda B x
## A and B are assumed to be square
gsep <- function( A, B, nz=min(ncol(A),ncol(B)) )
{
    ## Load LAPACK
    dyn.load( La_library() )
    stopifnot( is.loaded( "dsygvx" ) )

    ## Verify dimensionality
    n <- nrow(A)
    stopifnot( ncol(A) == n )
    stopifnot( all( dim(B) == c(n,n) ) )
    
    res <- .Fortran( "dsygvx",
                    as.integer( 1 ), 		# 1: ITYPE
                    as.character( 'V' ),	# 2: JOBZ
                    as.character( 'I' ),	# 3: RANGE
                    as.character( 'U' ),	# 4: UPLO
                    as.integer( n ),		# 5: N
                    as.double( A ), 		# 6: A
                    as.integer( n ),		# 7: LDA
                    as.double( B ),		# 8: B
                    as.integer( n ),		# 9: LDB
                    as.double( 0.0 ),		# 10: VL
                    as.double( 0.0 ),		# 11: VU
                    as.integer( n-nz+1 ),	# 12: IL
                    as.integer( n ),		# 13: IU
                    as.double( 1e-5 ),		# 14: ABSTOL
                    M = integer(1),		# 15: M
                    W = double(n),		# 16: W
                    Z = double(n*n),		# 17: Z
                    as.integer( n ),		# 18: LDZ
                    WORK = double(8*n),		# 19: WORK
                    as.integer( 8*n ),		# 20: LWORK
                    IWORK = integer(5*n),	# 21: IWORK
                    IFAIL = integer( n ),	# 22: IFAIL
                    INFO = integer( 1 )		# 23: INFO
             )

    if( res$INFO != 0 )
        stop( "Call to dsygvx() failed with error code ", res$INFO, "\n" )
    
    ## Reverse the order to be highest to lowest
    m <- res$M
    list( d = res$W[m:1], v = matrix( res$Z, n, n )[,m:1,drop=FALSE] )
}

## Computes a cross-class scatter:
## Input:
## 	X - p x n data matrix of n samples in p dimensions
## 	y - n x 1 vector of class assignments
## Output:
## 	SB - between-class scatter
## 	SW - within-class scatter
cc.scatter <- function( X, y )
{
    yf <- factor( y )
    cc <- levels( yf )
    k <- length( cc )
    p <- nrow( X )
    n <- ncol( X )
    stopifnot( length(y) == n )

    cat( "Mean-centering the data...\n" )
    m <- apply( X, 1, mean )
    X <- X - m
    
    cat( "Computing the mean for each class...\n" )
    mc <- matrix( 0, p, k )
    for( i in 1:k )
      mc[,i] <- apply( X[,y == cc[i],drop=FALSE], 1, mean )

    cat( "Computing within-cluster scatter...\n" )
    M <- X - mc[,as.integer(yf)]
    SW <- M %*% t(M) / n
    
    cat( "Computing between-cluster scatter...\n" )
    SB <- X %*% t(X) / n - SW
    
    list( SB=SB, SW=SW )
}

## Computes a set of bases for unregularized LDA
## X - an n-by-p matrix of input-space features
## y - an n-by-1 vector of labels
## lambda - regularization coefficient
lda.bases <- function( X, y, lambda = 0.1 )
{
    stopifnot( length(y) == nrow(X) )
    yf <- factor(y)
    K <- length( levels(yf) )
    p <- ncol(X)

    ## Compute the scatter matrices
    cs <- cc.scatter( t(X), y )
    SW <- cs$SW

    ## The within-cluster scatter has to be full rank
    ## Slightly whiten the matrix as necessary
    SW <- SW + lambda * max(abs(SW)) * diag(p)

    ## Compute the bases
    cat( "Solving the generalized eigenvalue problem...\n" )
    r <- gsep( cs$SB, SW, K-1 )
    rownames(r$v) <- colnames(X)
    colnames(r$v) <- paste0( "LDA", 1:ncol(r$v) )

    r
}

