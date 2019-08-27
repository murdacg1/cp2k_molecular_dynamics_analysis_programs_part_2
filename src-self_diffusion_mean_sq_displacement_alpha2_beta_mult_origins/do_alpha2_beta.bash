#!/bin/bash

module list
module purge
module load gcc
module list

HEAD=/global/homes/g/garold/ANALYZE

BIN=$HEAD/src-alpha2_beta/alpha2_beta.x
time $BIN < alpha2_beta.in > alpha2_beta.out
