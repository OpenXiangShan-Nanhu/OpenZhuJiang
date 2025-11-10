package dongjiang.frontend.decode

import dongjiang._
import dongjiang.frontend._
import dongjiang.frontend.decode.Decode.DecodeType
import dongjiang.frontend.decode.Inst._
import dongjiang.frontend.decode.Code._
import dongjiang.frontend.decode.DecodeCHI._
import dongjiang.bundle._
import dongjiang.bundle.ChiChannel._
import zhujiang.chi.ReqOpcode._
import zhujiang.chi.RspOpcode._
import zhujiang.chi.DatOpcode._
import zhujiang.chi.SnpOpcode._
import zhujiang.chi._
import chisel3._
import chisel3.util._

object Dataless_LAN {

    def makeUnique: DecodeType = (
        fromLAN | toLAN | reqIs(MakeUnique) | expCompAck,
        Seq(
            (sfMiss | llcIs(I))           -> first(cmtRsp(Comp) | resp(UC) | wriSRC(true)),
            (sfMiss | llcIs(SC))          -> first(cmtRsp(Comp) | resp(UC) | wriSRC(true) | wriLLC(I)),
            (sfMiss | llcIs(UC))          -> first(cmtRsp(Comp) | resp(UC) | wriSRC(true) | wriLLC(I)),
            (sfMiss | llcIs(UD))          -> first(cmtRsp(Comp) | resp(UC) | wriSRC(true) | wriLLC(I)),
            (srcMiss | othHit | llcIs(I)) -> (snpOth(SnpMakeInvalid), Seq((rspIs(SnpResp) | respIs(I)) -> second(cmtRsp(Comp) | resp(UC) | wriSRC(true) | wriSNP(false)))),
            (srcHit | othMiss | llcIs(I)) -> first(cmtRsp(Comp) | resp(UC)),
            (srcHit | othHit | llcIs(I))  -> (snpOth(SnpMakeInvalid), Seq((rspIs(SnpResp) | respIs(I)) -> second(cmtRsp(Comp) | resp(UC) | wriSRC(true) | wriSNP(false))))
        )
    )

    def evict: DecodeType = (
        fromLAN | toLAN | reqIs(Evict),
        Seq(
            (sfMiss | llcIs(I))           -> first(cmtRsp(Comp) | resp(I)),
            (sfMiss | llcIs(SC))          -> first(cmtRsp(Comp) | resp(I)),
            (sfMiss | llcIs(UC))          -> first(cmtRsp(Comp) | resp(I)),
            (sfMiss | llcIs(UD))          -> first(cmtRsp(Comp) | resp(I)),
            (srcMiss | othHit | llcIs(I)) -> first(cmtRsp(Comp) | resp(I)),
            (srcHit | othMiss | llcIs(I)) -> first(cmtRsp(Comp) | resp(I) | wriSRC(false)),
            (srcHit | othHit | llcIs(I))  -> first(cmtRsp(Comp) | resp(I) | wriSRC(false))
        )
    )

    def cleanShared: DecodeType = (
        fromLAN | toLAN | reqIs(CleanShared),
        Seq(
            (sfMiss | llcIs(I))  -> first(cmtRsp(Comp) | resp(I)),
            (sfMiss | llcIs(SC)) -> first(cmtRsp(Comp) | resp(I)),
            (sfMiss | llcIs(UC)) -> first(cmtRsp(Comp) | resp(I)),
            (sfMiss | llcIs(UD)) -> first(needDB | tdop("read", "send") | write(WriteNoSnpFull), cmtRsp(Comp) | resp(I) | wriLLC(UC)),
            (srcMiss | othHit | llcIs(I)) -> (snpAll(SnpCleanShared) | needDB, Seq(
                (rspIs(SnpResp) | respIs(I))         -> second(cmtRsp(Comp) | resp(I) | wriSNP(false)),
                (rspIs(SnpResp) | respIs(UC))        -> second(cmtRsp(Comp) | resp(I)),
                (rspIs(SnpResp) | respIs(SC))        -> second(cmtRsp(Comp) | resp(I)),
                (datIs(SnpRespData) | respIs(UC_PD)) -> second(tdop("send") | write(WriteNoSnpPtl), waitSecDone | cmtRsp(Comp)),
                (datIs(SnpRespData) | respIs(SC_PD)) -> second(tdop("send") | write(WriteNoSnpPtl), waitSecDone | cmtRsp(Comp)),
                (datIs(SnpRespData) | respIs(I_PD))  -> second(tdop("send") | write(WriteNoSnpPtl), waitSecDone | cdop("save") | cmtRsp(Comp) | wriSNP(false) | wriLLC(UC))
            )),
            (srcHit | othMiss | llcIs(I)) -> (snpAll(SnpCleanShared) | needDB, Seq(
                (rspIs(SnpResp) | respIs(I))         -> second(cmtRsp(Comp) | resp(I) | wriSNP(false)),
                (rspIs(SnpResp) | respIs(UC))        -> second(cmtRsp(Comp) | resp(I)),
                (rspIs(SnpResp) | respIs(SC))        -> second(cmtRsp(Comp) | resp(I)),
                (datIs(SnpRespData) | respIs(UC_PD)) -> second(tdop("send") | write(WriteNoSnpPtl), waitSecDone | cmtRsp(Comp)),
                (datIs(SnpRespData) | respIs(SC_PD)) -> second(tdop("send") | write(WriteNoSnpPtl), waitSecDone | cmtRsp(Comp)),
                (datIs(SnpRespData) | respIs(I_PD))  -> second(tdop("send") | write(WriteNoSnpPtl), waitSecDone | cdop("save") | cmtRsp(Comp) | wriSNP(false) | wriLLC(UC))
            )),
            (srcHit | othHit | llcIs(I)) -> first(cmtRsp(Comp) | resp(I))
        )
    )

    def cleanInvalid: DecodeType = (
        fromLAN | toLAN | reqIs(CleanInvalid),
        Seq(
            (sfMiss | llcIs(I))  -> first(cmtRsp(Comp) | resp(I)),
            (sfMiss | llcIs(SC)) -> first(cmtRsp(Comp) | resp(I) | wriLLC(I)),
            (sfMiss | llcIs(UC)) -> first(cmtRsp(Comp) | resp(I) | wriLLC(I)),
            (sfMiss | llcIs(UD)) -> first(needDB | tdop("read", "send") | write(WriteNoSnpFull), cmtRsp(Comp) | resp(I) | wriLLC(I)),
            (srcMiss | othHit | llcIs(I)) -> (snpAll(SnpCleanInvalid) | needDB, Seq(
                (rspIs(SnpResp) | respIs(I))        -> second(cmtRsp(Comp) | resp(I) | wriSNP(false)),
                (datIs(SnpRespData) | respIs(I_PD)) -> second(tdop("send") | write(WriteNoSnpPtl), waitSecDone | cmtRsp(Comp) | wriSNP(false))
            )),
            (srcHit | othMiss | llcIs(I)) -> (snpAll(SnpCleanInvalid) | needDB, Seq(
                (rspIs(SnpResp) | respIs(I))        -> second(cmtRsp(Comp) | resp(I) | wriSNP(false)),
                (datIs(SnpRespData) | respIs(I_PD)) -> second(tdop("send") | write(WriteNoSnpPtl), waitSecDone | cmtRsp(Comp) | wriSNP(false))
            )),
            (srcHit | othHit | llcIs(I)) -> (snpAll(SnpMakeInvalid), Seq(
                (rspIs(SnpResp) | respIs(I)) -> second(cmtRsp(Comp) | resp(I) | wriSNP(false))
            ))
        )
    )

    def makeInvalid: DecodeType = (
        fromLAN | toLAN | reqIs(MakeInvalid),
        Seq(
            (sfMiss | llcIs(I))  -> first(cmtRsp(Comp) | resp(I)),
            (sfMiss | llcIs(SC)) -> first(cmtRsp(Comp) | resp(I) | wriLLC(I)),
            (sfMiss | llcIs(UC)) -> first(cmtRsp(Comp) | resp(I) | wriLLC(I)),
            (sfMiss | llcIs(UD)) -> first(cmtRsp(Comp) | resp(I) | wriLLC(I)),
            (srcMiss | othHit | llcIs(I)) -> (snpAll(SnpMakeInvalid), Seq(
                (rspIs(SnpResp) | respIs(I)) -> second(cmtRsp(Comp) | resp(I) | wriSNP(false))
            )),
            (srcHit | othMiss | llcIs(I)) -> (snpAll(SnpMakeInvalid), Seq(
                (rspIs(SnpResp) | respIs(I)) -> second(cmtRsp(Comp) | resp(I) | wriSNP(false))
            )),
            (srcHit | othHit | llcIs(I)) -> (snpAll(SnpMakeInvalid), Seq(
                (rspIs(SnpResp) | respIs(I)) -> second(cmtRsp(Comp) | resp(I) | wriSNP(false))
            ))
        )
    )

    def table: Seq[DecodeType] = Seq(makeUnique, evict, cleanShared, cleanInvalid, makeInvalid)
}
