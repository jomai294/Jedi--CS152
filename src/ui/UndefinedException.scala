package ui

class UndefinedException(e: String = "Undefined Exception") extends JediException(e) {
  def message: String = e
  
}