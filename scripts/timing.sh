#!/bin/bash

# Ben Ganis
# 6/13/16
# Make table of ipars times for all test subdirectories of a work directory.

dirlist=`ls | grep Dome_`

echo "Directory,Init. Time,Vis. Time,Coeff. Time,Flash Time,Solver Time,Update Time,Total Time,Lin. Iter,Newt. Iter,Time Steps"
for dir in $dirlist; do
  cd $dir
  outfile1=`ls | grep ipars.o | tail -1`
  outfile2=`cat IPARS.IN | awk 'NR==2'`
  finishTime=`tail -n 10 $outfile1 | awk '/FINISH/ && /status=0/ { print $1 " " $2 " " $3 " " $4 }'`
  #echo "dir=$dir; outfile1=$outfile1; outfile2=$outfile2; finishTime=$finishTime"
  if [ -z "$finishTime" ]; then
    #echo "No FINISH detected"
    true
  elif ! [ -f "$outfile2" ]; then
    #echo "No IPARS output file found"
    true
  else
    initTime=`tail -n 50 $outfile2 | awk '/TOTAL INITIALIZATION TIME/ { print $6 }'`
    visTime=`tail -n 50 $outfile2 | awk '/VISUALIZATION TOTAL TIME/ { print $6 }'`
    coeffTime=`tail -n 50 $outfile2 | awk '/TOTAL COEFFICIENT TIME/ { print $6 }'`
    flashTime=`tail -n 50 $outfile2 | awk '/TOTAL FLASH TIME/ { print $9 }'`
    solverTime=`tail -n 50 $outfile2 | awk '/TOTAL LINEAR SOLVER TIME/ { print $7 }'`
    updateTime=`tail -n 50 $outfile2 | awk '/VARIABLE UPDATE TIME/ { print $6 }'`
    totalTime=`tail -n 50 $outfile2 | awk '/^ TOTAL TIME     / { print $5 }'`
    linIter=`tail -n 50 $outfile2 | awk '/TOTAL LINEAR ITERATIONS/ { print $7 }'`
    newtIter=`tail -n 50 $outfile2 | awk '/TOTAL NEWTONIAN ITERATIONS/ { print $7 }'`
    timeSteps=`tail -n 50 $outfile2 | awk '/TOTAL TIME STEP/ { print $7 }'`
    #echo "Init. Time   $initTime" 
    #echo "Vis. Time    $visTime"
    #echo "Coeff. Time  $coeffTime"
    #echo "Flash Time   $flashTime"
    #echo "Solver Time  $solverTime"
    #echo "Update Time  $updateTime"
    #echo "Total Time   $totalTime"
    #echo "Lin. Iter.   $linIter"
    #echo "Newt. Iter.  $newtIter"
    #echo "Time Steps   $timeSteps"
    echo "$dir,$initTime,$visTime,$coeffTime,$flashTime,$solverTime,$updateTime,$totalTime,$linIter,$newtIter,$timeSteps"
  fi
  cd ..
done

