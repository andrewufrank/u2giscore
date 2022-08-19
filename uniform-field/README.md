# A uniform method to realize the field data type 

This package shows how the `field` data type in a Geographic Information System (GIS) can be realized as a function

    point -> value 

The field is initially constructed from a set of values at specific points (regular or irregularly spaced) and then converted to a function (preferably by a Fourier transformation). Values for the field at arbitrary locations can then be obtained. 



# General idea for "uniform" packages:

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

uniform means:
- same functions with identical semantics independent of representation
- all functions are total (or become so using Maybe or Either)
