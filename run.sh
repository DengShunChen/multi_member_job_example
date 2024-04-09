#!/bin/bash
#PJM -L node=2
#PJM -L elapse=0:10:00
#PJM -L rscgrp=small
#PJM -L node-mem=unlimited
#PJM --no-stging
#PJM --mpi "proc=96"
#PJM -j
#PJM -s

. /users/xa09/sample/setup_mpi+omp.fx1000 1

time mpiexec -n 48 ./test.exe : -n 48 ./test.exe

echo "DONE"
