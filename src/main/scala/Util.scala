
object Id {
  private var nextId = 0
  def gen = {
    nextId += 1
    nextId
  }
}
