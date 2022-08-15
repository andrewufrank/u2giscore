# A uniform method to access geometry functions

The goal of the package is first to provide a uniform interface to 
functions for geometric operations in two dimensions (2D), with minimal complications with non-standard functionality used in the imported packages (e.g. returns are Bool, Maybe in lieu of more "advanced" types).
An extension to three dimensions (3D) or n dimension is possible.

Experience shows that geometry packages use, among other variations making use difficult,  different representations for points. This packages uses `V2` from `Linear.Algebra` (from hackage `linear`) for the interface and wraps this in a `Point2d` data type which ads an identifier to the coordinates.

Generally, a package for geometry easily usable for others should:

- allow input and output for a _point with identifier_ format, in order to connect the results with the input;
- use simple, standard Haskell types in the interface (i.e. for input and output) which allow `read . show`; 
    - lenses (and similar devices) for more comfortable access, 
    - pattern synonyms, etc. and 
    - more performant data storage methods (`Vec`, `NonEmptyList`, `IndexList`, `Sets`, etc. etc. ) 
    are welcome, but should not be mandatory to understand when reading the result data. Such advanced features beyond _everyday_ Haskell add an additional burden to learn to use a package and provide notoriously difficult to understand error messages (which Haskell is anyhow well known for). 
- avoid clever tricks (e.g. `Data.Ext` to add information to a type) but just defined the necessary data types (and make their constructors available),

- indicate clearly special cases, which the functions are not ready do handle (e.g. collinear points in input for a Delaunay triangulation, points which lead to isoceles, etc.).

- make sure that all information required in results are directly accessible (e.g. order of points, left/right of faces). 

`uniform-geometry` provides a compact but minimal format to represent triangulations as plane graphs with their dual (i.e. Delaunay triangulations and Voronoi subdivisions) using ideas from Guibas and Stolfi with using only references by identifiers. It is a simple step from a RDF (triple). 

Functions to convert to other formats are included and are used internally to access useful code. The package starts as interface to other packages and explains the long list of dependencies. 

Formats considered intially
- Point from `hgeometry` 
- the formats used in `qhull` (given up, as information about order of nodes, edges and faces is not preserved in haskell when taken from c code)
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
