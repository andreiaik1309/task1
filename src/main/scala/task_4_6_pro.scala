import scala.reflect.ClassTag

object task_4_6_pro extends App {
  // Сортировка вставками
  def insertionSort[A](arr: Array[A])(implicit ord: Ordering[A]): Array[A] = {
    for {
      i <- 1 until arr.length
      key = arr(i)
      j = i - 1
      if j >= 0 && ord.gt(arr(j), key)
    } {
      arr(j + 1) = arr(j)
      var k = j
      while (k > 0 && ord.gt(arr(k - 1), key)) {
        arr(k) = arr(k - 1)
        k -= 1
      }
      arr(k) = key
    }
    arr
  }


  // Сортировка пузырьком
  def bubbleSort[A](arr: Array[A])(implicit ord: Ordering[A]): Array[A] = {
    for {
      i <- 0 until arr.length - 1
      j <- 0 until arr.length - i - 1
      if ord.gt(arr(j), arr(j + 1))
    } {
      val temp = arr(j)
      arr(j) = arr(j + 1)
      arr(j + 1) = temp
    }
    arr
  }


  // Сортировка слиянием
  def mergeSort[A: ClassTag](arr: Array[A])(implicit ord: Ordering[A]): Array[A] = {
    // Сортировка слиянием
    def merge(left: Array[A], right: Array[A])(implicit ord: Ordering[A]): Array[A] = {
      var merged = Array.empty[A]
      var i, j = 0

      while (i < left.length && j < right.length) {
        if (ord.lteq(left(i), right(j))) {
          merged = merged :+ left(i)
          i += 1
        } else {
          merged = merged :+ right(j)
          j += 1
        }
      }

      while (i < left.length) {
        merged = merged :+ left(i)
        i += 1
      }

      while (j < right.length) {
        merged = merged :+ right(j)
        j += 1
      }

      merged
    }

    if (arr.length <= 1) arr
    else {
      val middle = arr.length / 2
      val (left, right) = arr.splitAt(middle)
      merge(mergeSort(left), mergeSort(right))
    }
  }


  // Быстрая сортировка
  def quickSort[A: ClassTag](arr: Array[A])(implicit ord: Ordering[A]): Array[A] = {
    if (arr.length <= 1) arr
    else {
      val pivot = arr(arr.length / 2)
      Array.concat(
        quickSort(arr.filter(ord.lt(_, pivot))),
        arr.filter(ord.equiv(_, pivot)),
        quickSort(arr.filter(ord.gt(_, pivot)))
      )
    }
  }


  // Генерация случайного массива указанного размера
  def generateRandomArray(size: Int): Array[Int] = {
    val random = scala.util.Random
    Array.fill(size)(random.nextInt())
  }

  // Тестируем алгоритмы сортировки
  val sizes = Array(100, 1000, 10000)

  for (size <- sizes) {
    val array = generateRandomArray(size)

    val (_, insertionTime) = measureTime {
      insertionSort(array.clone())
    }
    println(s"Insertion Sort for size $size took $insertionTime nanoseconds")

    val (_, bubbleTime) = measureTime {
      bubbleSort(array.clone())
    }
    println(s"Bubble Sort for size $size took $bubbleTime nanoseconds")

    val (_, mergeTime) = measureTime {
      mergeSort(array.clone())
    }
    println(s"Merge Sort for size $size took $mergeTime nanoseconds")

    val (_, quickTime) = measureTime {
      quickSort(array.clone())
    }
    println(s"Quick Sort for size $size took $quickTime nanoseconds")

    println() // Пустая строка для разделения результатов
  }

  def measureTime[A](block: => A): (A, Long) = {
    val startTime = System.nanoTime()
    val result = block
    val endTime = System.nanoTime()
    val elapsedTime = endTime - startTime
    (result, elapsedTime)
  }



}