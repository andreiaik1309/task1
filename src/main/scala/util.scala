object Task extends App {
  // Задача a
  // Вывести фразу «Hello, Scala!» справа налево
  val phrase = "Hello, Scala!"
  val reversedPhrase = phrase.reverse
  println(reversedPhrase)

  // Перевести всю фразу в нижний регистр
  val lowerCasePhrase = phrase.toLowerCase
  println(lowerCasePhrase)

  // Удалить символ "!"
  val withoutExclamation = lowerCasePhrase .dropRight(1)
  println(withoutExclamation)

  // Добавить в конец фразы «and goodbye python!»
  val finalPhrase = withoutExclamation + " and goodbye python!"
  println(finalPhrase)


  // Задача b
  def calculateMonthlySalary(annualIncome: Double, bonusPercentage: Double, foodCompensation: Double): Double = {
    val monthlySalary = (annualIncome * (1 - bonusPercentage / 100) - foodCompensation) / 12 * 0.87
    monthlySalary
  }

  val annualIncome = 100000.0
  val bonusPercentage = 10.0
  val foodCompensation = 5000.0
  val monthlySalary = calculateMonthlySalary(annualIncome, bonusPercentage, foodCompensation)
  println(s"Ежемесячный оклад: $monthlySalary")

  // Задача c
  def calculateDeviation(employeeSalary: Double, departmentSalaries: List[Double]): Double = {
    val averageSalary = departmentSalaries.sum / departmentSalaries.length
    val deviationPercent = (employeeSalary - averageSalary) / averageSalary * 100
    deviationPercent
  }

  val departmentSalaries = List(100.0, 150.0, 200.0, 80.0, 120.0, 75.0)
  val employeeSalary = 130.0
  val deviation = calculateDeviation(employeeSalary, departmentSalaries)
  println(s"Отклонение от среднего: $deviation%")


  // Задача d
  var employeeSalaries = List(100.0, 150.0, 200.0, 80.0, 120.0, 75.0)

  // Рассчитать новую зарплату и добавить в список
  val employeeIndex = 2 // предположим, что сотрудник с индексом 2 плохо себя вел
  val deviationAmount = -5 // добавляем или отнимаем сумму в зависимости от поведения сотрудника
  val newSalary = employeeSalaries(employeeIndex) + deviationAmount
  employeeSalaries = employeeSalaries.updated(employeeIndex, newSalary)

  // Найти самую высокую и самую низкую зарплату
  val maxSalary = employeeSalaries.max
  val minSalary = employeeSalaries.min
  println(s"Самая высокая зарплата: $maxSalary")
  println(s"Самая низкая зарплата: $minSalary")

  // Задача e
  // Добавить новых сотрудников
  val newEmployees = List(350.0, 90.0)
  employeeSalaries = employeeSalaries ++ newEmployees

  // Отсортировать список сотрудников по уровню оклада
  val sortedEmployeeSalaries = employeeSalaries.sorted
  println(s"Отсортированный список: $sortedEmployeeSalaries")


  // Задача f
  // Добавить нового сотрудника с окладом 130 тысяч
  val newEmployeeSalary = 130.0
  employeeSalaries = (employeeSalaries :+ newEmployeeSalary).sorted
  println(s"Список с новым сотрудником: $employeeSalaries")

  // Задача q
  // Вывести номера сотрудников категории middle (вилка от 100 до 200)
  val middleEmployeeIndexes = employeeSalaries.indices.filter(i => employeeSalaries(i) >= 100 && employeeSalaries(i) <= 200)
  println(s"Сотрудники категории middle: $middleEmployeeIndexes")

  // Задача h
  val inflationRate = 0.07
  employeeSalaries = employeeSalaries.map(salary => salary * (1 + inflationRate))
  println(s"Список после индексации на уровень инфляции: $employeeSalaries")
}

