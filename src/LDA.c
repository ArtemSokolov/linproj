// Interface between R and Fortran routine dsygvx that solves the generalized eigenvalue problem
//
// by Artem Sokolov

#include <R.h>
#include <R_ext/Lapack.h>

void dsygvx_c( int *n, double *a, double *b, int *il,
	       int *m, double *w, double *z,
	       double *work, int *iwork,
	       int *ifail, int *info )
{
  int itype = 1;
  char jobz = 'V';
  char range = 'I';
  char uplo = 'U';

  double vl = 0.0;
  double vu = 0.0;

  double abstol = 1e-5;

  int lwork = 8 * (*n);

  F77_CALL(dsygvx)( &itype, &jobz, &range, &uplo,
		    n, a, n,	// lda == n
		    b, n,	// ldb == n
		    &vl, &vu,
		    il, n,	// iu == n
		    &abstol,
		    m, w, z,
		    n,		// ldz == n
		    work,
		    &lwork,
		    iwork, ifail, info );
}
