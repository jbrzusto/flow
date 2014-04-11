##
## variable: rangeCorrectionScale
##
## This variable, if not NULL, contains a list of scaling coefficients to be
## used to equalize power against range.  Its length is equal to the number of
## samples per pulse.  Each raw sample value should be multiplied by the
## corresponding value of rangeCorrectionScale to get a range-equalized power
## value.

rangeCorrectionScale = NULL
