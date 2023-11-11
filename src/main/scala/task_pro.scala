case class Employee(name: String, salary: Double)
object Task_pro extends App {

  // Задача 1
  // Исходные данные: список с окладами сотрудников
  val salaries = List(100.0, 150.0, 200.0, 80.0, 120.0, 75.0, 350.0, 90.0, 130.0)

  // Исходнвые данные: значения среднего уровня окладов по рынку по категориям
  val juniorMarketAverage = 120.0
  val middleMarketAverage = 180.0
  val seniorMarketAverage = 370.0

  // Функция для вычисления индекса отклонения среднего оклада по категории
  def calculateIndex(salaries: List[Double], marketAverage: Double): Double = {
    val categoryAverage = salaries.sum / salaries.length
    (marketAverage - categoryAverage) / categoryAverage
  }
  // Для каждой категории считает индекс повышения оклада
  val juniorIndex = calculateIndex(salaries.filter(_ <= 100), juniorMarketAverage)
  val middleIndex = calculateIndex(salaries.filter(s => s > 100 && s <= 200), middleMarketAverage)
  val seniorIndex = calculateIndex(salaries.filter(_ > 200), seniorMarketAverage)

  // Индексируем оклады сотрудников
  val indexedSalaries = salaries.map {
    case salary if salary <= 100 => salary * (1 + juniorIndex)
    case salary if salary > 100 && salary <= 200 => salary * (1 + middleIndex)
    case salary => salary * (1 + seniorIndex)
  }

  // округляем до 2-ух знаков после запятой
  val indexedSalariesRounded = indexedSalaries.map(salary => BigDecimal(salary).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)


  // Выводим результат
  println("Результат индексации окладов сотрудников:")
  println(indexedSalariesRounded )

  // Задача 2
  // Создание структуры Employee
  val employees = List(
    Employee("John Doe", indexedSalariesRounded.head),
    Employee("Jane Smith", indexedSalariesRounded (1)),
    Employee("Ivan Kirdanov", indexedSalariesRounded (2)),
    Employee("Andrey Yakovlev", indexedSalariesRounded (3)),
    Employee("Makar Dudnikov", indexedSalariesRounded (4)),
    Employee("Mark Dudnikov", indexedSalariesRounded (5)),
    Employee("Vitol Join", indexedSalariesRounded (6)),
    Employee("Bob Sarty", indexedSalariesRounded (7)),
    Employee("Greg Grumberg", indexedSalariesRounded (8))
  )
  println("Список ФИ сотрудников с окладами:")
  println(employees)

  // Задание 3
  // Вывести фамилию и имя сотрудников с самой высокой и самой низкой зарплатой
  val highestSalaryEmployee = employees.maxBy(_.salary)
  val lowestSalaryEmployee = employees.minBy(_.salary)
  println(s"Самый высокий оклад: ${highestSalaryEmployee.name} - ${highestSalaryEmployee.salary}")
  println(s"Самый низкий оклад: ${lowestSalaryEmployee.name} - ${lowestSalaryEmployee.salary}")


  // Задание 4
  // Зашифровать данные сотрудников
  def encryptNameAndSalary(employee: Employee): Employee = {
    val lastName = employee.name.split(" ").lastOption.getOrElse("")
    val encryptedName = lastName.toLowerCase.replaceAll("[aeiou]", "").reverse
    Employee(encryptedName, employee.salary)
  }

  // Зашифрованные данные сотрудников
  val encryptedEmployees = employees.map(encryptNameAndSalary)
  println("Зашифрованные данные сотрудников:")
  encryptedEmployees.foreach(println)

  // Задание 5
  def processString(input: String): String = {
    val reversedString = input.reverse
    val lowercaseString = reversedString.toLowerCase
    val stringWithoutExclamation = lowercaseString.replace("!", "")
    s"$stringWithoutExclamation and goodbye python!"
  }

  // Пример использования
  val result = processString("Hello, Scala!")
  println(result)

  // Задание 6
  // Функция для вычисления степени двойки (обычной рекурсией)
  def powerOfTwoRecursive(x: Int): Long = {
    if (x == 0) 1
    else 2 * powerOfTwoRecursive(x - 1)
  }

  // Функция для вычисления степени двойки (хвостовой рекурсией)
  def powerOfTwoTailRecursive(x: Int, acc: Long = 1): Long = {
    if (x == 0) acc
    else powerOfTwoTailRecursive(x - 1, acc * 2)
  }

  // Пример использования
  val resultRecursive = powerOfTwoRecursive(5)
  val resultTailRecursive = powerOfTwoTailRecursive(5)
  println("Вычисление степени числа 2:")
  println(s"2^5 (Recursive): $resultRecursive")
  println(s"2^5 (Tail Recursive): $resultTailRecursive")



}