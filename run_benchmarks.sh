#!/bin/bash

MPSPDZPATH="MP-SPDZ"

INPUTDIR="inputs"
OUTPUTDIR="outputs"
BATCHSIZE=50
SEEDS=(1331)
SPLITS=(4 2) # Also specifies player count
HEIGHT=8 
MAXHEIGHT=12


SPLITSTR="$(printf '%d,' "${SPLITS[@]}")"
SPLITSTR=[${SPLITSTR%?}]

PLAYERINPUTCOUNT=$[2**$MAXHEIGHT]
DIR=$PWD

mkdir -p $INPUTDIR
mkdir -p $OUTPUTDIR
mkdir -p $MPSPDZPATH/Player-Data

PLAYERS=${#SPLITS[@]}
. $MPSPDZPATH/Scripts/run-common.sh

# Generate test data
for SEED in ${SEEDS[@]}; do  
  i=0
  while [ $i -lt ${#SPLITS[@]} ] 
  do
    python3 generate_data.py $PLAYERINPUTCOUNT $[$SEED + $i] > $MPSPDZPATH/Player-Data/Random-Input-$SEED-P$i-0
    ((i++))
  done
done


while [ $HEIGHT -le $MAXHEIGHT ] 
do
  for SEED in ${SEEDS[@]}; do  
    FILENAME=tree_$HEIGHT\_$SEED
    TREEPATH=$DIR/$INPUTDIR/$FILENAME
    OUTPUTPATH=$DIR/$OUTPUTDIR/$FILENAME\_compilation_times
    ./generateRandomTree $HEIGHT ${#SPLITS[@]} $SEED > $TREEPATH
    ./generateMPC "$SPLITSTR" < $TREEPATH > $TREEPATH.mpc

    cd $MPSPDZPATH
    echo Compiling $FILENAME
    { time ./compile.py -M $TREEPATH.mpc ; } 2> $OUTPUTPATH
    
    echo Running $FILENAME
    OUTPUTPATH=$DIR/$OUTPUTDIR/$FILENAME\_run_result
    run_player mascot-party.x -v -b $BATCHSIZE -IF Player-Data/Random-Input-$SEED $FILENAME > $OUTPUTPATH
    cd $DIR
  done
  ((HEIGHT++))
done
