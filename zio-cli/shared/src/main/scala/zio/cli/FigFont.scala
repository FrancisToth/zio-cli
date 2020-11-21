package zio.cli

sealed trait FigFont
object FigFont {
  case class Characters() extends FigFont
  case class Driver()     extends FigFont
  case class Figure()     extends FigFont
  case class Hardblanks() extends FigFont

  def apply(header: Header, comments: String): FigFont = ???
}

sealed trait LayoutMode
object LayoutMode {
  case object FullSize    extends LayoutMode
  case object FittingOnly extends LayoutMode
  case object Smushing    extends LayoutMode
}


sealed trait PrintDirection
object PrintDirection {
  case object Left2Right extends PrintDirection
}

case class Header(
  signature: (Char, Char, Char, Char, Char),
  hardblank: Char,
  characterSets: CharacterSets,
  oldLayout: Layout,
  commentLines: Int,
  printDirection: Option[PrintDirection],
  fullLayout: Option[Int],
  codetagCount: Option[Int]
)
object Header {
  def make(
    signature: (Char, Char, Char, Char, Char),
    hardblank: Char = '$',
    characterSets: CharacterSets,
    oldLayout: Layout,
    commentLines: Int,
    printDirection: Option[PrintDirection],
    fullLayout: Option[Int],
    codetagCount: Option[Int]
  ): Either[Error, Header] =
    for {
      hb <- validateHardblank(hardblank)
    } yield Header(
      signature,
      hb,
      characterSets,
      oldLayout,
      commentLines,
      printDirection,
      fullLayout,
      codetagCount
    )

  private val validateHardblank: Char => Either[Error, Char] = {
    val invalid = List(' ', '\n', '\r', '\u0000')
    c => if (invalid.contains(c)) Left(Error.hardblank(c, invalid)) else Right(c)
  }

  sealed trait Error
  object Error {
    case class InvalidHardblank(current: Char, valid: List[Char]) extends Error
    def hardblank(c: Char, valid: List[Char]) = InvalidHardblank(c, valid)
  }
}

sealed trait Axis
object Axis {
  sealed trait Horizontal extends Axis
  sealed trait Vertical   extends Axis
  sealed trait Both       extends Horizontal with Vertical
}

sealed trait SmushingRule { self =>
  type AxisT <: Axis

  def ++(that: SmushingRule.Aux[AxisT]): SmushingRule.Aux[AxisT] =
    SmushingRule.And(self, that)
}
object SmushingRule {

  type Aux[A <: Axis] = SmushingRule { type AxisT = A }

  case object EqualCharacter extends SmushingRule { type AxisT = Axis.Both       }
  case object Underscore     extends SmushingRule { type AxisT = Axis.Both       }
  case object Hierarchy      extends SmushingRule { type AxisT = Axis.Both       }
  case object OppositePair   extends SmushingRule { type AxisT = Axis.Horizontal }
  case object BigX           extends SmushingRule { type AxisT = Axis.Horizontal }
  case object Hardblank      extends SmushingRule { type AxisT = Axis.Horizontal }
  case object HorizontalLine extends SmushingRule { type AxisT = Axis.Vertical   }
  case object VerticalLine   extends SmushingRule { type AxisT = Axis.Vertical   }
  case class And[A <: Axis](
    left: Aux[A],
    right: Aux[A]
  ) extends SmushingRule { type AxisT = A }
}

sealed trait Layout
object Layout {

  case class Layout0()

  type Horizontal = DefaultLayout with Axis.Horizontal
  type Vertical = DefaultLayout with Axis.Vertical

  def apply(
    horizontalLayout: Horizontal,
    horizontalSmush: Horizontal,
    verticalLayout: Vertical,
    verticalSmush: Vertical,
  ): Layout0 = ???

  sealed trait DefaultLayout
  object DefaultLayout {
    case object FullWidth  extends DefaultLayout with Axis.Horizontal
    case object FullHeight extends DefaultLayout with Axis.Vertical
    case object Fitting    extends DefaultLayout with Axis.Both
    case object Smushing   extends DefaultLayout with Axis.Both
  }

  sealed trait Smushing { type AxisT <: Axis }
  object Smushing {
    case object Universal extends Smushing { type AxisT = Axis.Both }
    case class Controlled[A <: Axis](
      rules: List[SmushingRule.Aux[A]]
    ) extends Smushing { type AxisT = Axis.Both }
  }
}
case class CharacterSets(height: Int, baseLine: Int, maxLength: Int)
case class Character(subCharacter: List[Char])
