import scala.math._
import scala.collection.mutable._
import scala.util.Random

//Arrays are mutable, and becouse the centroids have to change we choose that over listbuffer

object Kmeans extends App {

  def createRandomPoint(d: Int): ListBuffer[Double] = {
    val r = scala.util.Random
    return ListBuffer.fill(d)(r.nextDouble)
  }

  //Generate dataset (List with n points of d dimention)
  def createListOfPoints(n: Int, d: Int): ListBuffer[ListBuffer[Double]] = {
    return ListBuffer.fill(n)(createRandomPoint(d))
  }

  //Functions
  def euclideanDistance(
      p: ListBuffer[Double],
      q: ListBuffer[Double]
  ): Double = {
    var dist = 0.0
    var i = 0
    while (i < p.length) {
      val d = p(i) - q(i)
      dist += d * d
      i = i + 1
    }
    sqrt(dist)
  }

  //Nearest Centroid

  def nearestCentroid(
      c: ListBuffer[ListBuffer[Double]],
      p: ListBuffer[Double]
  ): (Int) = {
    var min = Double.PositiveInfinity;
    var index = 0;
    var closestCentroid = -1;
    var i = 0
    while (i < c.length) {
      index += 1;
      var distance = euclideanDistance(c(i), p);
      if (min > distance) {
        min = distance;
        closestCentroid = index;
      }
      i = i + 1
    }
    (closestCentroid);
  }

  //Cluster points
  def clusterPoints(
      d: ListBuffer[ListBuffer[Double]],
      c: ListBuffer[ListBuffer[Double]]
  ): ListBuffer[(Int, ListBuffer[Double])] = {

    var clustersList = ListBuffer[(Int, ListBuffer[Double])]()

    for (i <- 0 to d.length - 1) {
      var point = d(i)
      var clusterNumber = nearestCentroid(c, point)
      var tuple = (clusterNumber, point)
      /*       println("Centroide asigned" + clusterNumber) */
      clustersList += tuple
    }

    return clustersList
  }

  //Recalculate centroids d: Clustered Centroids - c: Centroids
  def calculateCentroid(
      clusters: ListBuffer[(Int, ListBuffer[Double])],
      centroids: ListBuffer[ListBuffer[Double]],
      pointsDimention: Int
  ): ListBuffer[ListBuffer[Double]] = {

    var i = 1
    while (i < centroids.length + 1) {
      for (j <- 0 to pointsDimention - 1) {
        var sum = 0.0
        for (k <- 0 to clusters.length - 1) {
          if (clusters(k)._1 == i) {
            var point = clusters(k)._2
            sum = sum + point(j)
          }
          var mean = sum / clusters.length
          centroids(i - 1)(j) = mean
        }
      }
      i = i + 1

    }
    return centroids
  }

  val pointsDimention = 2
  val numberOfPoints = 10
  val k = 3
  val dataset = createListOfPoints(numberOfPoints, pointsDimention)
  var centroids = createListOfPoints(k, pointsDimention)
  var clusteredPoints = clusterPoints(dataset, centroids)
  println("Centroid")
  println(centroids)

  println("Points")
  println(clusteredPoints)

  var newCentroids =
    calculateCentroid(clusteredPoints, centroids, pointsDimention)
  println("New Centroid")
  print(newCentroids)

  /* var centroidsCluster = clusterPoints(centroids) */
  /*   var distanceBetwen = euclideanDistance(dataset(0), centroids(0))
  print(centroids) */
}
