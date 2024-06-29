package visualizations

type Pos = (Int, Int)

sealed trait Change
case class ChangeChar(char: Char)         extends Change
case class ChangeColor(color: String)     extends Change
case class ChangeTooltip(tooltip: String) extends Change

case class Frame(changes: Map[Pos, Seq[Change]], description: Option[String] = None)
