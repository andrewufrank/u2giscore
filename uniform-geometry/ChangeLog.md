Journal of work in uniform-geometry
0.0.1.1
- try with qhull from qhull split, only the minimal functionality, no examples 
            qhull 0.1.0.2
        - still limited to ghc 8.10.7 due to toysolver limit base <4.15
        does not work, missing symbol convexHull

- try with qhull 0.1.0.1 from hackage
        problem (occasionally) with include "xx" 
        replaced with <xx> where problems. in fromOthersExperiments/qhull
        seems to fix it 
- problems reported
    moved executable to app
    move Uniform to src
0.0.1.2
- export delaunay2 with list of V2 

0.0.1.3
- produce a tesselation data structure with the data for the half quad 
- problems 
    - :l app/Main.hs in .ghci not working?