#!/bin/bash
#PJM -L node=6
#PJM -L elapse=0:10:00
#PJM -L rscgrp=small
#PJM -L node-mem=unlimited
#PJM --no-stging
#PJM --mpi "proc=288"
#PJM -j
#PJM -s

. /users/xa09/sample/setup_mpi+omp.fx1000 1

MEMBERS=3

NODES=2
PPN=48

WKDIR=$(pwd)

for m in $(seq $MEMBERS); do

  mkdir -p ${WKDIR}/mem${m}
  cd ${WKDIR}/mem${m}

  rm -f vcoordfile
  for n in $(seq $NODES); do
    for p in $(seq $PPN); do
      echo "($((NODES*(m-1)+n-1)))" >> vcoordfile
    done
  done

  time mpiexec -n $((NODES*PPN)) -vcoordfile vcoordfile -of OUT.log ../test.exe &

done

wait

echo "DONE"
