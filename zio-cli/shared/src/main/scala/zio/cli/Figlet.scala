package zio.cli

object Figlet {
  final case class Font(
    id: String,
    name: String,
    header: Header,
    comment: String,
    settings: FontSettings,
    characters: Map[Char, Character]
  )

  final case class Character(
    fontId: String,
    name: Char,
    lines: SubLines,
    endmark: Char,
    width: Int,
    comment: Option[String],
    position: Int
  )

  sealed trait HorizontalLayout
  object HorizontalLayout {
    final case object FullWidth                                             extends HorizontalLayout
    final case object HorizontalFitting                                     extends HorizontalLayout
    final case object UniversalSmushing                                     extends HorizontalLayout
    final case class ControlledSmushing(rules: Seq[HorizontalSmushingRule]) extends HorizontalLayout
  }

  sealed trait HorizontalSmushingRule
  object HorizontalSmushingRule {
    final case object EqualCharacter extends HorizontalSmushingRule
    final case object Underscore     extends HorizontalSmushingRule
    final case object Hierarchy      extends HorizontalSmushingRule
    final case object OppositePair   extends HorizontalSmushingRule
    final case object BigX           extends HorizontalSmushingRule
    final case object Hardblank      extends HorizontalSmushingRule
  }

  sealed trait VerticalLayout
  object VerticalLayout {
    final case object FullHeight                                           extends VerticalLayout
    final case object VerticalFitting                                      extends VerticalLayout
    final case object UniversalSmushing                                    extends VerticalLayout
    final case class ControlledSmushing(rules: Seq[VerticalSmushingRules]) extends VerticalLayout
  }

  sealed trait VerticalSmushingRules
  object VerticalSmushingRules {
    final case object EqualCharacter            extends VerticalSmushingRules
    final case object Underscore                extends VerticalSmushingRules
    final case object Hierarchy                 extends VerticalSmushingRules
    final case object HorizontalLine            extends VerticalSmushingRules
    final case object VerticalLineSupersmushing extends VerticalSmushingRules
  }

  final case class FontSettings(
    hLayout: HorizontalLayout,
    vLayout: VerticalLayout,
    printDirection: PrintDirection
  )

  final case class Header(
    signature: (Char, Char, Char, Char, Char),
    hardblank: Char,
    height: Int,
    baseLine: Int,
    maxLength: Int,
    oldLayout: Seq[OldLayout],
    commentLines: Int,
    printDirection: Option[PrintDirection],
    fullLayout: Option[Int],
    codetagCount: Option[Int]
  )

  sealed abstract class PrintDirection(val value: Int)
  object PrintDirection {
    final case object LeftToRight extends PrintDirection(value = 0)
    final case object RightToLeft extends PrintDirection(value = 1)
  }

  sealed abstract class FullLayout(val value: Int)

  object FullLayout {

    final case object EqualCharacterHorizontalSmushing extends FullLayout(value = 1)
    final case object UnderscoreHorizontalSmushing     extends FullLayout(value = 2)
    final case object HierarchyHorizontalSmushing      extends FullLayout(value = 4)
    final case object OppositePairHorizontalSmushing   extends FullLayout(value = 8)
    final case object BigXHorizontalSmushing           extends FullLayout(value = 16)
    final case object HardblankHorizontalSmushing      extends FullLayout(value = 32)
    final case object HorizontalFitting                extends FullLayout(value = 64)
    final case object HorizontalSmushing               extends FullLayout(value = 128)
    final case object EqualCharacterVerticalSmushing   extends FullLayout(value = 256)
    final case object UnderscoreVerticalSmushing       extends FullLayout(value = 512)
    final case object HierarchyVerticalSmushing        extends FullLayout(value = 1024)
    final case object HorizontalLineVerticalSmushing   extends FullLayout(value = 2048)
    final case object VerticalLineSupersmushing        extends FullLayout(value = 4096)
    final case object VerticalFitting                  extends FullLayout(value = 8192)
    final case object VerticalSmushing                 extends FullLayout(value = 16384)

    val horizontalSmushingRules: Vector[FullLayout] = Vector(
      EqualCharacterHorizontalSmushing,
      UnderscoreHorizontalSmushing,
      HierarchyHorizontalSmushing,
      OppositePairHorizontalSmushing,
      BigXHorizontalSmushing,
      HardblankHorizontalSmushing
    )

    val verticalSmushingRules: Vector[FullLayout] = Vector(
      EqualCharacterVerticalSmushing,
      UnderscoreVerticalSmushing,
      HierarchyVerticalSmushing,
      HorizontalLineVerticalSmushing,
      VerticalLineSupersmushing
    )
  }

  sealed abstract class OldLayout(val value: Int)

  object OldLayout {

    final case object FullWidth              extends OldLayout(value = -1)
    final case object HorizontalFitting      extends OldLayout(value = 0)
    final case object EqualCharacterSmushing extends OldLayout(value = 1)
    final case object UnderscoreSmushing     extends OldLayout(value = 2)
    final case object HierarchySmushing      extends OldLayout(value = 4)
    final case object OppositePairSmushing   extends OldLayout(value = 8)
    final case object BigXSmushing           extends OldLayout(value = 16)
    final case object HardblankSmushing      extends OldLayout(value = 32)
  }

  final case class SubLines(value: Seq[String]) {
    protected def pure(value: Seq[String]): SubLines =
      SubLines(value)

    lazy val width: Int =
      value.map(_.length).headOption.getOrElse(0)

    lazy val toSubcolumns: SubColumns =
      SubColumns(value.transpose.map(_.mkString))
  }

  object SubLines {
    def zero(height: Int): SubLines =
      SubLines(Vector.fill(height)(""))
  }

  final case class SubColumns(value: Seq[String]) {
    protected def pure(value: Seq[String]): SubColumns =
      SubColumns(value)

    lazy val height: Int =
      value.map(_.length).headOption.getOrElse(0)

    lazy val toSublines: SubLines =
      SubLines(value.transpose.map(_.mkString))
  }

  object SubColumns {
    def zero(height: Int): SubColumns =
      SubColumns(Vector(" " * height))
  }
}
