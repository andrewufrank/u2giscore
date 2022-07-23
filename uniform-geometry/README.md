a uniform method to access geometry functions

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
