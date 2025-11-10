package xs

import chisel3.Data

package object utils {
  type SRAMQueue[T <: Data] = _root_.xs.utils.sram.SramQueue[T]
  type CircularQueuePtr[T <: CircularQueuePtr[T]] = _root_.xs.utils.queue.CircularQueuePtr[T]
  type MimoQueue[T <: Data] = _root_.xs.utils.queue.MimoQueue[T]
  type OverrideableQueue[T <: Data] = _root_.xs.utils.queue.OverrideableQueue[T]
  type HasCircularQueuePtrHelper = _root_.xs.utils.queue.HasCircularQueuePtrHelper
}
