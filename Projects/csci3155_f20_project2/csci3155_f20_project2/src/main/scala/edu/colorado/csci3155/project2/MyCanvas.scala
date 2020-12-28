package edu.colorado.csci3155.project2

/* A class to maintain a canvas. */
import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{Graphics2D}

/* A figure is a sealed trait. It can be a Polygon or a "MyCircle"*/
sealed trait Figure {
    def getBoundingBox: (Double, Double, Double, Double)
    def translate(shiftX: Double, shiftY: Double): Figure

    def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double): Unit
    def rotate(theta: Double): Figure
}

/*
 Class Polygon
   A polygon is defined by a list of its vertices
 */

case class Polygon(val cList: List[(Double, Double)]) extends Figure {
    //TODO: Define the bounding box of the polygon                                                                         --Done
    override def getBoundingBox: (Double, Double, Double, Double ) = {
        //xmin
        val xList = cList.foldLeft[List[Double]]  (Nil)( (acc,elt:(Double,Double)) => acc ++ List(elt._1))
        val yList = cList.foldLeft[List[Double]]  (Nil)( (acc,elt:(Double,Double)) => acc ++ List(elt._2))
        val xMin = xList.min
        val xMax = xList.max
        val yMin = yList.min
        val yMax = yList.max
        (xMin,xMax,yMin,yMax)
    } //getBoundingBox
    //TODO: Create a new polygon by shifting each vertex in cList by (x,y)                                                 --Done
    //    Do not change the order in which the vertices appear
    override def translate(shiftX: Double, shiftY: Double): Polygon =
    {
        val shiftList = cList.map( (elt:(Double,Double)) => (elt._1 + shiftX, elt._2 + shiftY))
        Polygon(shiftList)
    }

    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val xPoints: Array[Int] = new Array[Int](cList.length)
        val yPoints: Array[Int] = new Array[Int](cList.length)
        for (i <- 0 until cList.length){
            xPoints(i) = ((cList(i)._1 + shiftX )* scaleX).toInt
            yPoints(i) = ((cList(i)._2 + shiftY) * scaleY).toInt
        }
        g.drawPolygon(xPoints, yPoints, cList.length)
    }
    override def rotate(theta: Double):  Polygon ={                                                   // TODO:            --Done
        val newList = cList.foldLeft[List[(Double, Double)]] ((Nil)) ((acc, elt:(Double,Double)) => {
            val x = elt._1
            val y = elt._2
            val newX= x* math.cos(theta) - y*math.sin(theta)
            val newY= x* math.sin(theta) + y*math.cos(theta)
            val newVertex = (newX,newY)
            acc ++ List(newVertex)
        })
        Polygon(newList)
    }
}

/*
  Class MyCircle
  Define a circle with a given center c and radius r
 */
case class MyCircle(val c: (Double, Double), val r: Double) extends Figure {
    //TODO: Define the bounding box for the circle                                                                        -- Done
    override def getBoundingBox: (Double, Double, Double, Double) = {
        val xMin = c._1 - r
        val xMax = c._1 + r
        val yMin = c._2 - r
        val yMax = c._2 + r
        (xMin,xMax,yMin,yMax)
    }


    //TODO: Create a new circle by shifting the center                                                                    --Done
    override def translate(shiftX: Double, shiftY: Double): MyCircle = {
        val newCenter_X = c._1 + shiftX
        val newCenter_Y = c._2 + shiftY
        val newCenter = (newCenter_X, newCenter_Y)
        MyCircle(newCenter, r)
    }

    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val centerX = ((c._1 + shiftX) * scaleX) .toInt
        val centerY = ((c._2 + shiftY) * scaleY) .toInt
        val radX = (r * scaleX).toInt
        val radY = (r * math.abs(scaleY)).toInt
        //g.draw(new Ellipse2D.Double(centerX, centerY, radX, radY))
        g.drawOval(centerX-radX, centerY-radY, 2*radX, 2*radY)
    }

    override def rotate(theta: Double): Figure = {                                  //TODO                                 --Done
        val x = c._1
        val y = c._2
        val newX= x* math.cos(theta) - y*math.sin(theta)
        val newY= x* math.sin(theta) + y*math.cos(theta)

        val newCenter = (newX,newY)
        MyCircle(newCenter,r)
    }
}

/*
  Class : MyCanvas
  Define a canvas through a list of figure objects. Figure objects can be circles or polygons.
 */
class MyCanvas (val listOfObjects: List[Figure]) {
    // TODO: Write a function to get the boundingbox for the entire canvas.                                               --Done
    // Hint: use existing boundingbox functions defined in each figure.
    def getBoundingBox: (Double, Double, Double, Double) = {
        //run through list to xMinList, xMaxList,...
        //        val xList = cList.foldLeft[List[Double]]  (Nil)( (acc,elt:(Double,Double)) => acc ++ List(elt._1))
        val xMinList = listOfObjects.foldLeft[List[Double]] (Nil) ( (acc, obj:Figure) => acc ++ List(obj.getBoundingBox._1 ))
        val xMaxList = listOfObjects.foldLeft[List[Double]] (Nil) ( (acc, obj:Figure) => acc ++ List(obj.getBoundingBox._2 ))
        val yMinList = listOfObjects.foldLeft[List[Double]] (Nil) ( (acc, obj:Figure) => acc ++ List(obj.getBoundingBox._3 ))
        val yMaxList = listOfObjects.foldLeft[List[Double]] (Nil) ( (acc, obj:Figure) => acc ++ List(obj.getBoundingBox._4 ))
        val xMin = xMinList.min
        val xMax = xMaxList.max
        val yMin = yMinList.min
        val yMax = yMaxList.max
        (xMin,xMax,yMin,yMax)
    }

    //TODO: Write a function to translate each figure in the canvas by shiftX, shiftY                                     --Done
    def translate(shiftX: Double, shiftY: Double): MyCanvas = {
        val shifted_listOfObjects = listOfObjects.foldLeft[List[Figure]] ((Nil)) ((acc, obj:Figure) => acc ++ List(obj.translate(shiftX,shiftY)))
        new MyCanvas(shifted_listOfObjects)
    }

    //TODO: Write a function that will return a new MyCanvas object that places                                          --Done
    // all the objects in myc2 to the right of the objects in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeRight(myc2: MyCanvas):MyCanvas = {
        //get boundingBoxes
        val boundingBox_C1 = getBoundingBox
        val boundingBox_C2 = myc2.getBoundingBox
        //define xShift
        val xShift = boundingBox_C1._2 - boundingBox_C1._1
        val yShift = ((boundingBox_C1._4 - boundingBox_C1._3)/2) - ((boundingBox_C2._4 - boundingBox_C2._3)/2)
        val c2_hat = myc2.translate(xShift,yShift)
        overlap(c2_hat)

    }

    //TODO: Write a function that will return a new MyCanvas object that places                                           --Done
    // all the figures in myc2 on top of the figures in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeTop(myc2: MyCanvas): MyCanvas = {
        // get boundingBoxes
        val boundingBox_C1 = getBoundingBox
        val boundingBox_C2 = myc2.getBoundingBox
        //define xShift
        val xShift = ((boundingBox_C1._2 - boundingBox_C1._1)/2) - ((boundingBox_C2._2 - boundingBox_C2._1)/2)
        val yShift = boundingBox_C1._4 - boundingBox_C1._3
        val c2_hat = myc2.translate(xShift,yShift)
        overlap(c2_hat)

    }

    //TODO: Write a function that will rotate each figure in the canvas using                                            --Done
    // the angle `ang` defined in radians.
    // Suggestion: first write rotation functions for polygon and circle.
    //             those functions have not been added in the classes but you can do so with the
    //             appropriate signature.
    // rotating a polygon is simply rotating each vertex.
    // rotating a circle is simply rotating the center with radius unchanged.
    def rotate(theta: Double): MyCanvas = {
        val shifted_listOfObjects = listOfObjects.foldLeft[List[Figure]] ((Nil)) ((acc, obj:Figure) => acc ++ List(obj.rotate(theta)))
        new MyCanvas(shifted_listOfObjects)

    }

    // Function to draw the canvas. Do not edit.
    def render(g: Graphics2D, xMax: Double, yMax: Double) = {
        val (lx1, ux1, ly1, uy1) = this.getBoundingBox
        val shiftx = -lx1
        val shifty = -uy1
        val scaleX = xMax/(ux1 - lx1  + 1.0)
        val scaleY = yMax/(uy1 - ly1 + 1.0)
        listOfObjects.foreach(f => f.render(g,scaleX, -scaleY, shiftx, shifty))
    }

    def overlap(c2: MyCanvas): MyCanvas = {
        new MyCanvas(listOfObjects ++ c2.listOfObjects)
    }

    // DO NOT EDIT THE CODE BELOW
    override def toString: String = {
        listOfObjects.foldLeft[String] ("") { case (acc, fig) => acc ++ fig.toString }
    }
    // DO NOT EDIT
    def getListOfObjects: List[Figure] = listOfObjects

    // DO NOT EDIT
    def numPolygons: Int =
        listOfObjects.count {
            case Polygon(_) => true
            case _ => false }

    //DO NOT EDIT
    def numCircles: Int = {
        listOfObjects.count {
            case MyCircle(_,_) => true
            case _ => false }
    }
    //DO NOT EDIT
    def numVerticesTotal: Int = {
        listOfObjects.foldLeft[Int](0) ((acc, f) =>
            f match {
                case Polygon(lst1) => acc + lst1.length
                case _ => acc
            }
        )
    }
}
