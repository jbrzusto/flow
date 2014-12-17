/*
  filter_pulses.c

  @param pulses - VECSXP of pulse data, each being a raw vector; samples
                 are assumed to be unsigned little endian 16-bit integers.
  @param dim - INTSXP of length 2; number of samples, pulses in output matrix
  @param group - INTSXP with same length as pulselist.  Gives the group
                 to which each pulse belongs.  Groups are in the range
                 1..ns  Outside that range means pulse is to be ignored.
                 Groups must be consecutive.
  @param maxGroupSize - INTSXP of length 1; maximum number of pulses in 
                 a group.
  @param mode - INTSXP of length 1;  0 means first pulse; 1 means mean;
                2 means mean of all but max;
  @return numeric matrix of dimensions ns x np 

*/

#include "R.h"
#define USE_RINTERNALS
#include "Rinternals.h"
#include "Rdefines.h"
#include "R_ext/Rdynload.h"
#include <stdint.h>

SEXP
filter_pulses (SEXP pulses, SEXP dim, SEXP group, SEXP maxGroupSize, SEXP mode)
{
  uint16_t * pp[INTEGER(maxGroupSize)[0]]; // pulse pointers
  int nr = INTEGER(dim)[0];
  int nc = INTEGER(dim)[1];

  SEXP rv;
  PROTECT(rv = allocVector(REALSXP, nr * nc));
  int m = INTEGER(mode)[0];

  int i=0, j=0; // indices into output matrix (i for row, j for col)
  int k=0; // index into pulse groups
  int ip = 0; // index into pulses

  int npg = 0; // number of pulses in current group

  int np = LENGTH(pulses); // number of pulses

  int * g = INTEGER(group); // pointer to group numbers

  double *out = REAL(rv);

  int lastGroup;

  for (i = 0; i < nr && ip < np; /**/) {
    // accumulate the pulses for a group
    lastGroup = g[ip];
    for (k=0; g[ip] == lastGroup && ip < np; ++ip, ++k)
      pp[k] = (uint16_t *) RAW(VECTOR_ELT(pulses, ip));

    if (lastGroup >= 1 && lastGroup <= np) {

      npg = k; // number of pulses in this group
      // filter this group of pulses into a single output
      if (m == 0 || npg == 1) {
        // copy first pulse; also do this if there's only 
        // one pulse in this group
        for (j = 0; j < nc; ++j)
          *out++ = pp[0][j];

      } else if (m == 1) {
        // get mean
        for (j = 0; j < nc; ++j) {
          int sum = 0;
          for (k = 0; k < npg; ++k)
            sum += pp[k][j];
          *out++ = sum / (double) npg;
        }

      } else if (m == 2) {
        // get mean of all but max
        for (j = 0; j < nc; ++j) {
          int sum, max;
          sum = max = pp[0][j];
          for (k = 1; k < npg; ++k) {
            sum += pp[k][j];
            if (pp[k][j] > max)
              max = pp[k][j];
          }
          *out++ = (sum - max) / (npg - 1.0);
        }
      }
      ++i; // we've processed a row of output
    }
    ++ip;
  }
  SEXP dims;
  PROTECT(dims=allocVector(INTSXP, 2));
  INTEGER(dims)[0] = nr;
  INTEGER(dims)[1] = nc;

  SET_DIM(rv, dims);
  UNPROTECT(2);
  return rv;
}

R_CallMethodDef flow_call_methods[]  = {
  {"filter_pulses", (DL_FUNC) & filter_pulses, 5},
  {NULL, NULL, 0}
};

void
R_init_flow (DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines (info, NULL, flow_call_methods, NULL, NULL);
  R_useDynamicSymbols (info, TRUE);
}

void
R_unload_flow (DllInfo *info)
{
  /* Release resources. */
}
