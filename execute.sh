#!/bin/bash -l

# Name the job
#$ -N A3

# Join input and output streams
#$ -j y

# Request enough time
#$ -l h_rt=12:00:00

# Request parallel environment
#$ -pe omp 4


module load R/4.2.1
Rscript Assignment3.R