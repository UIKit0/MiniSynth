package by.buneyeu.minisynth

object Implicits {

  class ArrayMath(val arr1: Array[Double]) extends AnyVal {

    //TODO remove code copying
    
    def *(arr2: Array[Double]) = {
      require(arr1.length == arr2.length, "Arrays should have the same length!")
      val multArr = Array.fill(arr1.length)(0.0)
      for (i <- 0 until arr1.length)
        multArr(i) = arr1(i) * arr2(i)
      multArr
    }
    def +(arr2: Array[Double]) = {
      require(arr1.length == arr2.length, "Arrays should have the same length!")
      val multArr = Array.fill(arr1.length)(0.0)
      for (i <- 0 until arr1.length)
        multArr(i) = arr1(i) + arr2(i)
      multArr
    }
  }

  implicit def ArrayMath(f: Array[Double]) = new ArrayMath(f)

}