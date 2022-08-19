Journal of work in uniform-relations
## 0.0.1.0
- extract the triple store and the relatios handling code and bring it to a separate package
- order:
    -- NaiveTripleStore 

    
## 0.0.1.1 building for hgeometry 
## 0.0.1.2 cleaning up for hgeometry
- missing: circumcenter and incenter

## 0.0.1.3 split off the example HQ 
- 0.0.1.3.2 split off from u2to to u2coregis

## questions
construct voronoi and delaunay from smaller hq triangles (node - mid hq - face), which can be outside?

# material
TripLL is a hackage project to access the level store (from Google) which is VERY fast key-value db. 
TripLL stores all relations multiple times, but one could break the triplestore into key-value stores for each relation (possibly with both directions stored)