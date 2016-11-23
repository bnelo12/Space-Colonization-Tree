# Space Colinization Tree Generator

This Haskell file will create non-deterministic fractal based trees using an algorithm called space colinization.

##Example


##Libraries
This module requires one third-party library not included in the base Haskell instalation for graphics rendering.

###Gloss
A simple vector graphics library to make rendering easy.
>	cabal update

>	cabal install gloss

##Installation
>	git clone https://github.com/bnelo12/space-colinization-tree.git

Navigate into the directory and open up ghci. 

>	Prelude> :l space_colinization.hs

>	Prelude> main

##Parameters
There are four parameters for generating different types of trees. The are located in space_colinization.hs. The recomended parameters are:
>	growDistance         = 10.0

>	maxDistance          = 50.0

>	minDistance          = 10.0

>	numberOfLeaves       = 200

Also the position of the leaves can be changed by modifying the constraints in the generateLeaves function.

