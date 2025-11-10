package sifive.enterprise.firrtl

import firrtl.annotations.{Annotation, ModuleTarget, Named, SingleTargetAnnotation}

case class FullAsyncResetAnnotation(
  target: Named
) extends SingleTargetAnnotation[Named] {
  override def duplicate(n: Named): Annotation = this.copy(n)
}
