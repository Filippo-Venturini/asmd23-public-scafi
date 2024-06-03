# Lab 11 - Aggregate Computing in Scafi

## Programs variations

In the first part all the variation requested to the file *Programs.scala* are performed.

**Case 9:**
```
override def main() = branch(sense1)(rep(0){e => branch(e < 1000)(e+1)(e)})(0)
```

Here, if the sensor 1 is active, then the value of the node is incremented from 0 to 1000, otherwise it's 0.
If we use the **mux** operator insthead of **branch** and we stop and restart a node, it doesn't reset the value to 0, and this happens because of the semantic of **mux** in which both branch are evaluated before choosing one of them.

**Case 12:**
```
override def main() = foldhood(Set[ID]())((s, id)=>s ++ id)(nbr{branch(sense1)(Set(mid()))(Set())})
```

In this case, using **foldhood** we start from an empty set of ID's. and as accumulator we simply use the concatenations of Set. The fold operation is done for all the neighbors that has the sensor 1 active.

**Case 8:**
```
override def main() = minHoodPlus((nbrRange, nbr{mid}))._2
```

Here it builds a collection of pair (distance, ID) from each neighboor and takes only the ID of the minimum using **minHoodPlus**.

**Case 14:**
```
override def main() = rep(0){ x => mid() max maxHoodPlus(nbr{x max mid()}) }
```

In this case, for each neighbor check the max between the ID's (current node and neighbor), and then it takes the max of the neighborhood including the current node.

**Case 16:**
```
override def main() = rep(Double.MaxValue):
    d => mux[Double](sense1){0.0}{mux(sense2)(minHoodPlus(nbr{d}+nbrRange*5))(minHoodPlus(nbr{d}+nbrRange))}
```

In this last example it perform the gradient calculation, but if some node has the sensor 2 active, it multiply the **nbrRange** by 5.

## Partition

In the above code, a function for realize partitions is crafted:

```
class Partition extends AggregateProgramSkeleton:

  private def partition(sourcesID: Set[Int]) = rep((Double.MaxValue, Int.MaxValue)):
    d => mux[(Double, Int)](sourcesID.contains(mid())){(0.0, mid())}{minHoodPlus(nbr{d._1}+nbrRange, d._2 min nbr{d._2})}

  override def main() = partition(Set(1, 10))
```

The overall idea, is to accumulate pairs of distance from the source of the partition and the correspondent ID. For make the function general, it takes as input the ID of the sources,
and basing on that configure each node during the rep as a source node of the partition or not.
