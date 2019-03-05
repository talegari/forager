# Implementation Checklist
> package: forager

Legend:

 - ??Not sure
 - TODO: Idea is clear, to be implemented. Without a name refers to Srikanth.
 - DONE: done

## Design

- forage should be called with a 'what'
  - ex: 
    - forage(model, data, what = "dist", ...)
    - forage(model, data, what = "outlier", ...)
- Add plot methods when necessary (low priority)
- Support modelList (low priority)


## Whats

- dist (TODO, dependency on parallelDist PR)
    - ranger/randomForest: Use hamming from 'parallelDist' package
  
- simil (TODO)
    - from dist
  
- knn (TODO)

- frnn (TODO)

- Outlier detection
  
  - method:
    
    - proximity:
      
        - dist: (DONE with dependency on dist / knn / frnn)
        - knn/frnn: (TODO)
    - uncertainity: (mostly TODO Radha)
      
        - regression: 
          
            - quantreg:
             
                - ranger: distance from median / length(prediction interval)
                - quantregForest: do the same (see what else it implements)
            - rf:
              
                - ranger: Use infjk or jk
        - randomForest: ??Implement this (TODO)
      
- prediction intervals: (TODO) closely follows Outlier detection

- margin: (TODO) classification only

- tree correlation: (TODO) mtry, bias-variance (reg only) from wager's paper

## plot methods
 TODO
