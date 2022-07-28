a uniform method to access geometry functions

The goal of the package is first to provide a uniform interface to 
functions for geometric operations in two dimensions (2D), with minimal complications with non-standard functionality used in the imported packages (e.g. returns are Bool, Maybe in lieu of more "advanced" types).
An extension to three dimensions (3D) or n dimension is possible.

Experience shows that geometry packages use, among other variations making use difficult,  different representations for points. This packages uses `V2` from `Linear.Algebra` (from hackage `linear`) for the interface. 

Functions to convert to other formats are included and are used internally to access useful code. The package starts as interface to other packages and explains the long list of dependencies. 

Formats considered intially
- Point from `hgeometry` 
- the formats used in `qhull`
- Point2d, a wrapper around V2 to include a point name (similar to `Point :+` and with a similar convention to work with `'` versions of functions)



uniform means:
- same functions with identical semantics independent of representation
- all functions are total (or become so using Maybe or Either)

This packages exports data types for 
- Point2d, Edges, Faces and 
- PointID, EdgeID and FaceID.
- Length and Angles (in radians)

which are mostly imported from other packages (mostyl from `vector-space`)
There are functions to make and convert these.

The goal is access to functions to compute
- shortest path,
- triangulations, and 
- Voronoy tesselations. 

They are 
