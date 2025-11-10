package zhujiang.chi

import chisel3._

object RespErr {
    val width = 2

    val NormalOkay    = "b00".U(width.W)
    val ExclusiveOkay = "b01".U(width.W)
    val DataError     = "b10".U(width.W)
    val NonDataError  = "b11".U(width.W)
}

object Order {
    val width = 2

    val None            = "b00".U(width.W)
    val RequestAccepted = "b01".U(width.W)
    val RequestOrder    = "b10".U(width.W)
    val OWO             = "b10".U(width.W)
    val EndpointOrder   = "b11".U(width.W)

    def isRequestOrder(order: UInt): Bool = order >= RequestOrder
}

class MemAttr extends Bundle {

    val allocate = Bool()

    val cacheable = Bool()

    val device = Bool()

    val ewa = Bool()
}

object MemAttr {
    def apply(allocate: Bool, cacheable: Bool, device: Bool, ewa: Bool): MemAttr = {
        val memAttr = Wire(new MemAttr)
        memAttr.allocate  := allocate
        memAttr.cacheable := cacheable
        memAttr.device    := device
        memAttr.ewa       := ewa
        memAttr
    }
    def apply(): MemAttr = apply(false.B, false.B, false.B, false.B)
}
