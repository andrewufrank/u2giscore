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
    much simpler code!

0.0.1.4
- restructure point2d
    - two points used always: V2 and Point2d (with name and V2 coords)
    - points used in other packages 
        - hgeometry HPoint (with names or without)
        - qhull [Double]
        - gloss/glut (x,y)
        - linear V2
        conversion using instances to convert
        
    The information about faces left/right is removed when the qhull C code result is converted to haskell (using sorted sets and lists)
    The difficulty cannot be overcome by testing with coordinaes, as 
    the circumcenter of the triangle (delaunay) can be on the edge and the decision is arbitrary. Consistency cannot be reconstructed!

0.0.1.5 trying again with hgeometry



- problems 
    - :l app/Main.hs in .ghci not automatically loaded