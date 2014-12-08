svm\_sgd\_haskell
=========

Machine Learning algorithms (Support Vector Machine, Logistic Regression, etc.)  based on Stochastic Gradient Descent approximation.

Memory Tuning How To
--------

### Input Data Format

        1  2:2.5  5:5.1 
        -1 3:3.1  4:4.5
        -1 5:5.5  7:7.7

### Input Data Set

[epsilon](http://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/epsilon_normalized.bz2) (note, file size 4G). Take a subset of the data for testing.

### Compiling

> ghc -o sg.o2 -outputdir outdir -O2 --make svm_sgd_haskell/src/Sgd/Svm/Main.hs -fforce-recomp -rtsopts -isvm_sgd_haskell/src -prof -auto-all -caf-all 

### Running with RTS

> time ./sg.o2 $inputfile -e 1 --dontnormalize=True +RTS -K100M -p

### Tuning Results

#### Tuning Effort
* t0: inital implementation, remove the function call `predLoss = T.testMany ...` from `Main.hs`. 
* t1: besides t0, also manually fix `dim` and `length x` in `Train.hs`. The idea is that by removing such function calls, the requirement of all data in memory is removed (see discussion on [reddit](http://www.reddit.com/r/haskell/comments/2og2i6/ask_for_review_my_first_haskell_project_machine/).

#### Table
* Column: input data set with varying number of data points and size.
* Row: tuning effort.
* Cell: total memory in use by the `svm\_sgd\_haskell` (output from RTS).

Tuning\Data Points (Size) | 1000 (29 MB) | 2000 (58 MB) | 3000 (87 MB) | 4000 (116 MB) | 5000 (145 MB)
-------------------------|--------------|--------------|--------------|---------------|--------------
t0 | 1203 MB | 2314 MB | 3834 MB | 4271 MB | 5667 MB 
t1 | 1202 MB | 2314 MB | 3834 MB | 4270 MB | 5665 MB 

#### Summary

In SGD, the memory consumption is supposed to be constant. In the implementation (tuning effort t0 and t1), the memory consumptions are identical and are linear. 
