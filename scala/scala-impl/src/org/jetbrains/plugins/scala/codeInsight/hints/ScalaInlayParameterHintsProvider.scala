package org.jetbrains.plugins.scala
package codeInsight
package hints

import java.{util => ju}

import com.intellij.codeInsight.hints.{Option => HintOption, _}
import com.intellij.lang.Language
import com.intellij.lang.java.JavaLanguage
import com.intellij.psi.{PsiElement, PsiMethod}
import org.jetbrains.plugins.scala.codeInspection.collections.MethodRepr
import org.jetbrains.plugins.scala.extensions.{PsiMethodExt, PsiNamedElementExt}
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.base.ScPatternList
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScReferenceExpression, ScSugarCallExpr}
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.types.ScType

import scala.collection.JavaConverters

class ScalaInlayParameterHintsProvider extends InlayParameterHintsProvider {

  import ScalaInlayParameterHintsProvider._

  import JavaConverters._

  override def getSupportedOptions: ju.List[HintOption] =
    HintTypes.map(HintType.asOption).asJava

  override def getParameterHints(element: PsiElement): ju.List[InlayInfo] =
    parameterHints(element).asJava

  override def getHintInfo(element: PsiElement): HintInfo =
    hintInfo(element).orNull

  override def getInlayPresentation(inlayText: String): String =
    InlayInfo.presentation(inlayText)

  override def getDefaultBlackList: ju.Set[String] =
    DefaultBlackList.asJava

  override def getBlackListDependencyLanguage: Language = JavaLanguage.INSTANCE

  override def canShowHintsWhenDisabled: Boolean = true
}

object ScalaInlayParameterHintsProvider {

  private val DefaultBlackList = Set.empty[String]

  private val HintTypes = List(
    ParameterHintType,
    ReturnTypeHintType,
    PropertyHintType,
    LocalVariableHintType
  )

  def instance: ScalaInlayParameterHintsProvider =
    InlayParameterHintsExtension.INSTANCE.forLanguage(ScalaLanguage.INSTANCE) match {
      case provider: ScalaInlayParameterHintsProvider => provider
    }

  private def parameterHints(element: PsiElement): Seq[InlayInfo] =
    HintTypes.flatMap { hintType =>
      hintType(element)
    }

  private def hintInfo(element: PsiElement): Option[HintInfo] = {
    import HintInfo._
    HintTypes.find(_.isDefinedAt(element)).collect {
      case ParameterHintType => new MethodInfo("", ju.Collections.emptyList())
      case hintType => new OptionInfo(hintType: HintOption)
    }
  }

  private[this] type HintFunction = PartialFunction[PsiElement, Seq[InlayInfo]]

  private[hints] sealed abstract class HintType protected(private val defaultValue: Boolean,
                                                          private val idSegments: String*)
    extends HintFunction {

    private val option: HintOption = {
      val id = "scala" +: idSegments :+ "hint"
      new HintOption(id.mkString("."), s"Show ${idSegments.mkString(" ")} hints", defaultValue)
    }

    protected val delegate: HintFunction

    override final def isDefinedAt(element: PsiElement): Boolean = delegate.isDefinedAt(element)

    override final def apply(element: PsiElement): Seq[InlayInfo] =
      if (this.isEnabled && delegate.isDefinedAt(element)) delegate(element)
      else Seq.empty
  }

  private[hints] object HintType {
    implicit def asOption(hintType: HintType): HintOption = hintType.option
  }

  private[hints] case object ParameterHintType extends HintType(defaultValue = true, "parameter", "name") {

    override protected val delegate: HintFunction = {
      case MethodRepr(expression, _, Some(ScReferenceExpression(method: PsiMethod)), arguments) if !expression.isInstanceOf[ScSugarCallExpr] =>
        method.parameters.zip(arguments).map {
          case (parameter, argument) => InlayInfo(parameter.name, argument)
        }
    }
  }

  private[hints] case object ReturnTypeHintType extends HintType(defaultValue = false, "function", "return", "type") {

    override protected val delegate: HintFunction = {
      case function: ScFunction if !function.hasExplicitType =>
        function.returnType.toSeq
          .map(InlayInfo(_, function.parameterList))
    }
  }

  private[hints] abstract class DefinitionHintType(isLocal: Boolean, idSegments: String*)
    extends HintType(defaultValue = false, idSegments :+ "type": _*) {

    import DefinitionHintType._

    override protected val delegate: HintFunction = {
      case Definition(definition, patternList) if isValid(definition) =>
        definition.`type`().toSeq
          .map(InlayInfo(_, patternList))
    }

    private def isValid(definition: ScValueOrVariable) =
      !definition.hasExplicitType && definition.isLocal == isLocal
  }

  private[this] object DefinitionHintType {

    private object Definition {

      def unapply(element: PsiElement): Option[(ScValueOrVariable, ScPatternList)] = element match {
        case definition: ScPatternDefinition => Some(definition, definition.pList)
        case definition: ScVariableDefinition => Some(definition, definition.pList)
        case _ => None
      }
    }

  }

  private[hints] case object PropertyHintType extends DefinitionHintType(isLocal = false, "property")

  private[hints] case object LocalVariableHintType extends DefinitionHintType(isLocal = true, "local", "variable")

  private object InlayInfo {

    private[this] val TypeInfoPrefix = "@TYPE@"

    def apply(text: String, anchor: PsiElement, isParameter: Boolean = true): InlayInfo = {
      val textRange = anchor.getTextRange
      val offset = if (isParameter) textRange.getStartOffset else textRange.getEndOffset
      new InlayInfo(text, offset)
    }

    def apply(`type`: ScType, anchor: PsiElement): InlayInfo =
      apply(TypeInfoPrefix + `type`.presentableText, anchor, isParameter = false)

    def presentation(text: String): String = {
      import ScalaTokenTypes.tCOLON
      text.stripPrefix(TypeInfoPrefix) match {
        case `text` => text + tCOLON
        case strippedText => s"$tCOLON $strippedText"
      }
    }

  }

}