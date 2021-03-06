package org.jetbrains.plugins.scala.lang.refactoring.changeSignature

import java.util
import java.util.Comparator

import com.intellij.openapi.project.Project
import com.intellij.refactoring.RefactoringHelper
import com.intellij.usageView.UsageInfo

/**
  * Nikolay.Tropin
  * 12-Jan-17
  */
class ScalaChangeSignatureRefactoringHelper extends RefactoringHelper[Unit] {
  override def prepareOperation(usages: Array[UsageInfo]): Unit = {
    def priority(usageInfo: UsageInfo) = usageInfo match {
      case _: ParameterUsageInfo => 0
      case _: MethodUsageInfo => 1
      case _: AnonFunUsageInfo => 1
      case _: ScalaNamedElementUsageInfo => 2
      case _ => 3
    }

    if (usages.exists(UsageUtil.scalaUsage)) {
      util.Arrays.sort(usages, new Comparator[UsageInfo] {
        override def compare(o1: UsageInfo, o2: UsageInfo): Int = priority(o1) - priority(o2)
      })
    }
  }

  override def performOperation(project: Project, operationData: Unit): Unit = {}
}
