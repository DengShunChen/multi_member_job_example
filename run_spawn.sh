#!/bin/bash
#PJM -L node=7
#PJM -L elapse=0:5:00
#PJM -L rscgrp=small
#PJM -L node-mem=unlimited
#PJM --no-stging
#PJM --mpi "proc=336"
####PJM --mpi "rank-map-bynode"
#PJM -j
#PJM -s

. /users/xa09/sample/setup_mpi+omp.fx1000 1

set -x

MEMBERS=3

NODES=2
PPN=48

WKDIR=$(pwd)

for m in $(seq $MEMBERS); do
  mkdir -p ${WKDIR}/mem${m}
  cd ${WKDIR}/mem${m} 

  # link member executable
  ln -fs ../test.exe .

  # create vcoordfile
  rm -f vcoordfile
  for n in $(seq $NODES); do
    for p in $(seq $PPN); do
      echo "($((NODES*(m-1)+n-1)))" >> vcoordfile
    done
  done


done

cd ${WKDIR}

#make build 
rm -rf vcoordfile
for p in $(seq $MEMBERS); do
    echo "($((NODES*MEMBERS)))" >> vcoordfile
done

time mpiexec -n ${MEMBERS} -vcoordfile vcoordfile  ./mpi_spawn.exe $((NODES*PPN))

echo "DONE"
