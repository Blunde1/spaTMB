library(Matrix)
# after MakeADFun
attach(obj$env)
# update h
obj$env$h <- function(theta=par, order=0, hessian, L, ...) {
    if(order == 0) {
        ##logdetH <- determinant(hessian)$mod
        logdetH <- 2*determinant(L)$mod
        ans <- -f(theta,order=0) + .5*logdetH - length(random)/2*log(2*pi) #### updated
        if(LaplaceNonZeroGradient){
            grad <- f(theta,order=1)[random]
            ans - .5* sum(grad * as.numeric( solveCholesky(L, grad) ))
        } else
            ans
    }
    else if(order == 1) {
        if(LaplaceNonZeroGradient)stop("Not correct for LaplaceNonZeroGradient=TRUE")
        ##browser()
        e <- environment(spHess)
        solveSubset <- function(L).Call("tmb_invQ",L,PACKAGE="TMB")
        solveSubset2 <- function(L).Call("tmb_invQ_tril_halfdiag",L,PACKAGE="TMB")
        ## FIXME: The following two lines are not efficient:
        ## 1. ihessian <- tril(solveSubset(L))
        ## 2. diag(ihessian) <- .5*diag(ihessian)
        ## Make option to solveSubset to return lower triangular part
        ## with diagonal halved. As it is now the output of solveSubset is
        ## symm _with upper storage_ (!) (side effect of cholmod_ptranspose)
        ## therefore tril takes long time. Further, "diag<-" is too slow.
        ## FIXED! :
        ihessian <- solveSubset2(L)
        ## Profile case correction (1st order case only)
        if(!is.null(profile)){
            ## Naive way:
            ##   ihessian[profile,] <- 0
            ##   ihessian[,profile] <- 0
            ## However, this would modify sparseness pattern and also not
            ## account for 'ihessian' being permuted:
            perm <- L@perm+1L
            ihessian <- .Call("tmb_sparse_izamd", ihessian, profile[perm], 0.0, PACKAGE="TMB")
        }
        
        ## General function to lookup entries A subset B.
        ## lookup.old <- function(A,B){
        ##   A <- as(tril(A),"dtTMatrix")
        ##   B <- as(tril(B),"dtTMatrix")
        ##   match(paste(A@i,A@j),paste(B@i,B@j))
        ## }
        ## General function to lookup entries A in B[r,r] assuming pattern of A
        ## is subset of pattern of B[r,r].
        lookup <- function(A,B,r=NULL){
            A <- tril(A); B <- tril(B)
            B@x[] <- seq.int(length.out=length(B@x)) ## Pointers to full B matrix (Can have up to 2^31-1 non-zeros)
            if(!is.null(r)){
                ## Goal is to get:
                ##     B <- forceSymmetric(B)
                ##     B <- B[r,r,drop=FALSE]
                ## However the internal Matrix code for
                ## "B[r,r,drop=FALSE]" creates temporary "dgCMatrix"
                ## thereby almost doubling the number of non-zeros. Need
                ## solution that works with max (2^31-1) non-zeros:
                B <- .Call("tmb_half_diag", B, PACKAGE="TMB")
                B <- tril( B[r,r,drop=FALSE] ) + tril( t(B)[r,r,drop=FALSE] )
            }
            m <- .Call("match_pattern", A, B, PACKAGE="TMB") ## Same length as A@x with pointers to B@x
            B@x[m]
        }
        if(is.null(e$ind1)){
            ## hessian: Hessian of random effect part only.
            ## ihessian: Inverse subset of hessian (same dim but larger pattern!).
            ## Hfull: Pattern of full hessian including fixed effects.
            if (!silent) cat("Matching hessian patterns... ")
            iperm <- invPerm(L@perm+1L)
            e$ind1 <- lookup(hessian,ihessian,iperm) ## Same dimensions
            e$ind2 <- lookup(hessian,e$Hfull,random)  ## Note: dim(Hfull)>dim(hessian) !
            if (!silent) cat("Done\n")
        }
        w <- rep(0,length=length(e$Hfull@x))
        w[e$ind2] <- ihessian@x[e$ind1]
        ## Reverse mode evaluate ptr in rangedirection w
        ## now gives .5*tr(Hdot*Hinv) !!
        ## return
        as.vector( -f(theta,order=1) ) +
            .Call("EvalADFunObject", e$ADHess$ptr, theta,
                  control=list(
                      order=as.integer(1),
                      hessiancols=as.integer(0),
                      hessianrows=as.integer(0),
                      sparsitypattern=as.integer(0),
                      rangecomponent=as.integer(1),
                      rangeweight=as.double(w),
                      dumpstack=as.integer(0),
                      doforward=as.integer(1)
                  ),
                  PACKAGE=DLL)
    }## order == 1
    else stop(sprintf("'order'=%d not yet implemented", order))
} ## end{ h }