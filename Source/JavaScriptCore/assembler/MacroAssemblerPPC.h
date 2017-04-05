/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=4 sw=4 et tw=79:
 *
 * ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Mozilla SpiderMonkey JavaScript 1.9 code.
 *
 * The Initial Developer of the Original Code is
 *   Cameron Kaiser <classilla@floodgap.com> and the TenFourFox team
 *   Benjamin Stuhl <bks24@cornell.edu>
 *   David Kilbridge <twisk@pacbell.net>
 * 
 * Portions created by the Initial Developer are Copyright (C) 2010-2011
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 * 
 * ***** END LICENSE BLOCK ***** */

#ifndef MacroAssemblerPPC_h
#define MacroAssemblerPPC_h

#if ENABLE(ASSEMBLER) && CPU(PPC)

#include "PPCAssembler.h"
#include "AbstractMacroAssembler.h"

/* 32-bit PowerPC macroassembler. It rocks. */

namespace JSC {

class MacroAssemblerPPC : public AbstractMacroAssembler<PPCAssembler, MacroAssemblerPPC> {
public:
    typedef PPCRegisters::FPRegisterID FPRegisterID;

    static const Scale ScalePtr = TimesFour;

    enum RelationalCondition {
        Equal = PPCAssembler::ConditionEQ,
        NotEqual = PPCAssembler::ConditionNE,

        // Unsigned comparisons. We must select the proper cmp-family
        // instruction if we get one of these.
        Above = PPCAssembler::ConditionGT | PPCAssembler::ConditionUnsigned,
        AboveOrEqual = PPCAssembler::ConditionGE | PPCAssembler::ConditionUnsigned,
        Below = PPCAssembler::ConditionLT | PPCAssembler::ConditionUnsigned,
        BelowOrEqual = PPCAssembler::ConditionLE | PPCAssembler::ConditionUnsigned,

#define PPC_USE_UNSIGNED_COMPARE(x) (x & PPCAssembler::ConditionUnsigned)

        GreaterThan = PPCAssembler::ConditionGT,
        GreaterThanOrEqual = PPCAssembler::ConditionGE,
        LessThan = PPCAssembler::ConditionLT,
        LessThanOrEqual = PPCAssembler::ConditionLE
    };

    enum ResultCondition {
        // This flag has to be handled from the XER bits.
        Overflow = PPCAssembler::ConditionXEROV,

        // Set for us in the condreg after a recorded arithmetic operation.
        Signed = PPCAssembler::ConditionLT, // OPPCC, appendix C, p.670
        Zero = PPCAssembler::ConditionEQ,
        NonZero = PPCAssembler::ConditionNE
    };
    enum DoubleCondition {
        // These conditions will only evaluate to true if the comparison is ordered - i.e. neither operand is NaN.
        DoubleEqual = PPCAssembler::DoubleConditionEQ,
        DoubleNotEqual = PPCAssembler::DoubleConditionNE,
        DoubleGreaterThan = PPCAssembler::DoubleConditionGT,
        DoubleGreaterThanOrEqual = PPCAssembler::DoubleConditionGE,
        DoubleLessThan = PPCAssembler::DoubleConditionLT,
        DoubleLessThanOrEqual = PPCAssembler::DoubleConditionLE,

        // If either operand is NaN, these conditions always evaluate to true.
        DoubleEqualOrUnordered = PPCAssembler::DoubleConditionEQ_U,
        DoubleNotEqualOrUnordered = PPCAssembler::DoubleConditionNE_U,
        DoubleGreaterThanOrUnordered = PPCAssembler::DoubleConditionGT_U,
        DoubleGreaterThanOrEqualOrUnordered = PPCAssembler::DoubleConditionGE_U,
        DoubleLessThanOrUnordered = PPCAssembler::DoubleConditionLT_U,
        DoubleLessThanOrEqualOrUnordered = PPCAssembler::DoubleConditionLE_U
    };

    static const RegisterID framePointerRegister = PPCRegisters::r13;
    static const RegisterID stackPointerRegister = PPCRegisters::r1;
    static const RegisterID tempRegister = PPCRegisters::r0;
#warning AmigaOS: should we use r10 for linkRegister
    static const RegisterID linkRegister = PPCRegisters::r10;
    // The only problem with using r0 is in computing effective addresses.
    // This leads to some gyration in load/store macros; read on. So, we
    // also use another register for temporary work.
    static const RegisterID addressTempRegister = PPCRegisters::r12;
    // Finally, branch32WithUnaligned etc. needs a *third* scratch register.
    // I don't really like this, but we don't use r11, so we'll clobber that.
    static const RegisterID emergencyTempRegister = PPCRegisters::r11;

    static const FPRegisterID fpTempRegister = PPCRegisters::f0;

    // Some of these operations need a double (2 word/64 bit)-sized area to
    // spill working data, mostly in the GPR<->FPR moves. OS X can use the
    // reserved space in the linkage area nominally used by the compiler. You
    // may need to change this for your OS (and see the other ABI dependent
    // parts).
#if OS(DARWIN)
#define PPC_LINKAGE_FREE 16
#if !defined(_ARCH_PPCSQ) && !defined(PPC_SOFT_SQRT)
// Enable software square root if this is not a G5.
#define PPC_SOFT_SQRT
#endif
#else
#warning AmigaOS: need to define a free double area for your PPC ABI (Using sam as Apple)
#define PPC_LINKAGE_FREE 16
#if !defined(_ARCH_PPCSQ) && !defined(PPC_SOFT_SQRT)
// Enable software square root if this is not a G5.
#define PPC_SOFT_SQRT
#endif

#endif

    // Optimize checkStackPointer away in opt builds.
#if !defined(DEBUG)
#define checkStackPointer(x) ;
#endif

    void nop() {
        m_assembler.x_nop();
    }

    // Integer arithmetic operations:
    //
    // Operations are typically two operand - operation(source, srcDst)
    // For many operations the source may be an Imm32, the srcDst operand
    // may often be a memory location (explictly described using an Address
    // object).
    //
    // Except for sub(), we model ourselves on ARM because ARM has many of
    // the same limitations on immediate arguments and has a similar register
    // file. SPARC has similar limits too, but the register windows are
    // unique.

    void add32(RegisterID src, RegisterID dest)
    {
        m_assembler.add(dest, dest, src);
    }

    void add32(TrustedImm32 imm, RegisterID dest)
    {
        // Special case
        add32(imm, dest, dest);
    }

    void add32(TrustedImm32 imm, RegisterID src, RegisterID dest)
    {
        if (PPC_IMM_OK_S(imm) && src != tempRegister) {
            m_assembler.addi(dest, src, int16_t(imm.m_value & 0xffff));
        } else {
            ASSERT(src != addressTempRegister);
            move(imm, addressTempRegister);
            m_assembler.add(dest, src, addressTempRegister);
        }
    }

    void add32(TrustedImm32 imm, Address address)
    {
        /* Let's see if we can optimize this basic idea:
           load32(address, tempRegister);
           add32(imm, tempRegister);
           store32(tempRegister, address); */
        if (PPC_IMM_OK_S(imm) && PPC_OFFS_OK(address)) {
            // Fast path. Both the immediate and the offset fit into 16 bits,
            // so we can use lwz, stw and addi.
            load32(address, addressTempRegister); // single lwz
            m_assembler.addi(addressTempRegister, addressTempRegister, int16_t(imm.m_value & 0xffff));
            store32(addressTempRegister, address); // single stw
        } else if (PPC_OFFS_OK(address)) {
            // The offset still fits into 16 bits, so the loads/stores are fast.
            load32(address, addressTempRegister); // single lwz
            move(imm, tempRegister);
            m_assembler.add(addressTempRegister, tempRegister, addressTempRegister);
            store32(addressTempRegister, address); // single stw
        } else if (PPC_IMM_OK_S(imm)) {
            // Well, at least the immediate is small! Try to save a load on
            // the effective address by keeping the offset in tempRegister.
            checkStackPointer(address);
            m_assembler.x_li32(tempRegister, address.offset);
            m_assembler.lwzx(addressTempRegister, address.base, tempRegister);
            m_assembler.addi(addressTempRegister, addressTempRegister, int16_t(imm.m_value & 0xffff));
            m_assembler.stwx(addressTempRegister, address.base, tempRegister);
        } else {
            // The immediate neither fits in 16-bits, nor the offset. Yuck.
            // Fortunately this is pretty rare, so it's not worth optimizing.
            load32(address, addressTempRegister);
            move(imm, tempRegister);
            m_assembler.add(addressTempRegister, tempRegister, addressTempRegister);
            store32(addressTempRegister, address);
        }
    }

    void addPtrNoFlags(TrustedImm32 imm, RegisterID srcDest)
    {
        add32(imm, srcDest);
    }

    void add32(Address src, RegisterID dest)
    {
        load32(src, tempRegister);
        add32(tempRegister, dest);
    }

    void add32(AbsoluteAddress src, RegisterID dest)
    {
        load32(src.m_ptr, tempRegister);
        add32(tempRegister, dest);
    }

    void add32(TrustedImm32 imm, AbsoluteAddress address)
    {
        load32(address.m_ptr, addressTempRegister);
        if (PPC_IMM_OK_S(imm))
            m_assembler.addi(addressTempRegister, addressTempRegister, int16_t(imm.m_value & 0xffff));
        else {
            move(imm, tempRegister);
            m_assembler.add(addressTempRegister, tempRegister, addressTempRegister);
        }
        store32(addressTempRegister, address.m_ptr);
    }

#warning AmigaOS: add64 unimplemented
    void add64(TrustedImm32 imm, AbsoluteAddress address)
    {
        /*
            add32(imm, address)
            sltu  immTemp, dataTemp, cmpTemp    # set carry-in bit
            lw    dataTemp, 4(addrTemp)
            addiu dataTemp, imm.m_value >> 31 ? -1 : 0
            addu  dataTemp, dataTemp, immTemp
            sw    dataTemp, 4(addrTemp)
        */
        /*
        add32(imm, address);
        m_assembler.sltu(immTempRegister, dataTempRegister, cmpTempRegister);
        m_assembler.lw(dataTempRegister, addrTempRegister, 4);
        if (imm.m_value >> 31)
            m_assembler.addiu(dataTempRegister, dataTempRegister, -1);
        m_assembler.addu(dataTempRegister, dataTempRegister, immTempRegister);
        m_assembler.sw(dataTempRegister, addrTempRegister, 4);
        */
    }

    void and32(RegisterID src, RegisterID dest)
    {
        m_assembler.and_rc(dest, dest, src);
    }
    void and32(RegisterID op1, RegisterID op2, RegisterID dest)
    {
        m_assembler.and_rc(dest, op1, op2);
    }


    void and32(TrustedImm32 imm, RegisterID dest)
    {
        and32(imm, dest, dest);
    }

    void and32(TrustedImm32 imm, RegisterID src, RegisterID dest)
    {
        // We can't use i/is optimization like the other bitmask instructions
        // because the final condition will only be set for the upper bits, and
        // the methodjit uses this to test return values. We must test all bits
        // simultaneously. However, if the value is all high halfword, we could
        // use andis_rc. This is used a lot by the code generator, so we should
        // try to make it fast.
        if (PPC_IMM_OK_U(imm)) {
            m_assembler.andi_rc(dest, src, uint16_t(imm.m_value & 0xffff));
        } else if (!(imm.m_value & 0xffff)) {
            m_assembler.andis_rc(dest, src, uint16_t(uint32_t(imm.m_value) >> 16));
        } else {
            move(imm, tempRegister);
            m_assembler.and_rc(dest, src, tempRegister);
        }
    }
    
    void and32(Address address, RegisterID dest)
    {
        load32(address, tempRegister);
        and32(tempRegister, dest);
    }

    void lshift32(RegisterID shift_amount, RegisterID dest)
    {
        // Clamp the shift to the range 0..31
        m_assembler.andi_rc(tempRegister, shift_amount, 0x1f);
        m_assembler.slw(dest, dest, tempRegister);
    }

    void lshift32(RegisterID src, RegisterID shift_amount, RegisterID dest)
    {
        // Clamp the shift to the range 0..31
        m_assembler.andi_rc(tempRegister, shift_amount, 0x1f);
        m_assembler.slw(dest, src, tempRegister);
    }

    void lshift32(RegisterID src, TrustedImm32 imm, RegisterID dest)
    {
        // There is no true "slwi" but we can emulate it with rlwinm.
        // The slwi macro figures out the actual encoding.
        m_assembler.x_slwi(dest, src, imm.m_value & 0x1f);
    }

    void lshift32(TrustedImm32 imm, RegisterID dest)
    {
        lshift32(dest, imm, dest);
    }

    // Invert a relational condition, e.g. == becomes !=, < becomes >=, etc.
    static RelationalCondition invert(RelationalCondition cond)
    {
        return static_cast<RelationalCondition>(cond ^ 1);
    }
    void mul32(RegisterID src, RegisterID dest)
    {
        m_assembler.mullw(dest, dest, src);
    }

    void mul32(TrustedImm32 imm, RegisterID src, RegisterID dest)
    {
        // Use a strength-reduced multiply if at all possible and save cycles.
        if (PPC_IMM_OK_S(imm))
            m_assembler.x_sr_mulli(dest, src, int16_t(imm.m_value & 0xffff));
        else {
            move(imm, tempRegister);
            m_assembler.mullw(dest, tempRegister, src);
        }
    }

    void neg32(RegisterID srcDest)
    {
        m_assembler.neg(srcDest, srcDest);
    }

    void not32(RegisterID srcDest)
    {
        // OPPCC appendix A p.540
        m_assembler.nor(srcDest, srcDest, srcDest);
    }

    void or32(RegisterID src, RegisterID dest)
    {
        or32(dest, dest, src);
    }

    void or32(RegisterID src1, RegisterID src2, RegisterID dest)
    {
        m_assembler.or_(dest, src2, src1);
    }

    void or32(TrustedImm32 imm, RegisterID dest)
    {
        or32(imm, dest, dest);
    }

    void or32(TrustedImm32 imm, Address address)
    {
        load32(address, tempRegister);
        or32(imm, tempRegister);
        store32(tempRegister, address);
    }

    void or32(TrustedImm32 imm, RegisterID src, RegisterID dest)
    {
        // We can get away with this because we don't need to know the
        // final condition. Same for xor32. See and32 for where we can't
        // (i.e., anything that must use the Rc bit).
        m_assembler.ori(dest, src, uint16_t(imm.m_value & 0xffff));
        if (!PPC_IMM_OK_U(imm)) // also need to handle bits 0-15
            m_assembler.oris(dest, src, uint16_t(uint32_t(imm.m_value) >> 16));
    }

    void or32(Address address, RegisterID dest)
    {
        load32(address, tempRegister);
        or32(tempRegister, dest);
    }

    void rshift32(RegisterID shift_amount, RegisterID dest)
    {
        // Clamp the shift to the range 0..31
        m_assembler.andi_rc(tempRegister, shift_amount, 0x1f);
        m_assembler.sraw(dest, dest, tempRegister); // not srw, that's urshift
    }

    void rshift32(TrustedImm32 imm, RegisterID dest)
    {
        rshift32(dest, imm, dest);
    }

    void rshift32(RegisterID src, RegisterID shiftAmount, RegisterID dest)
    {
#warning AmigaOS: what should rshift32(RegisterID src, RegisterId shiftAmount, RegisterID dest) do??
        m_assembler.srawi(dest, src, shiftAmount);
    }
    
    void rshift32(RegisterID src, TrustedImm32 imm, RegisterID dest)
    {
        m_assembler.srawi(dest, src, imm.m_value & 0x1f);
    }

    void urshift32(RegisterID src, RegisterID shiftAmount, RegisterID dest)
    { 
   #warning amigaos: is this correct? is srw's method (dest, src, offset)
       // Clamp the shift to the range 0..31
        m_assembler.andi_rc(tempRegister, shiftAmount, 0x1f);
        m_assembler.srw(dest, src, tempRegister);
    }
    
    void urshift32(RegisterID shift_amount, RegisterID dest)
    {
        // Clamp the shift to the range 0..31
        m_assembler.andi_rc(tempRegister, shift_amount, 0x1f);
        m_assembler.srw(dest, dest, tempRegister);
    }
    
    void urshift32(TrustedImm32 imm, RegisterID dest)
    {
        urshift32(dest, imm, dest);
    }

    void urshift32(RegisterID src, TrustedImm32 imm, RegisterID dest)
    {
        // There is no true "srwi" but we can emulate it with rlwinm.
        // The srwi macro figures out the actual encoding.
        m_assembler.x_srwi(dest, src, imm.m_value & 0x1f);
    }

    void sub32(RegisterID src, RegisterID dest)
    {
        // DANGER! We are reversed relative to ARM, our closest ABI:
        // our subf x, y, z is z - y => x
        // ARM sub  x, y, z is y - z => x
        // In this sense PPC subf is more like ARM rsb, and we should
        // actually follow Sparc since it is set up the same way.
        m_assembler.subf(dest, src, dest);
    }

    void sub32(TrustedImm32 imm, RegisterID dest)
    {
        // We have no reg - imm in PPC, only subfic, which is imm - reg.
        // However, we can emulate it with addi.
        if(PPC_IMM_OK_U(imm) && dest != tempRegister) {
            m_assembler.x_subi(dest, dest, uint16_t(imm.m_value & 0xffff)); // addi operand order
        } else {
            ASSERT(dest != addressTempRegister);
            move(imm, addressTempRegister);
            m_assembler.subf(dest, addressTempRegister, dest);
        }
    }

    void sub32(TrustedImm32 imm, Address address)
    {
        /* Let's see if we can optimize this basic idea:
           load32(address, tempRegister);
           sub32(imm, tempRegister);
           store32(tempRegister, address); */

        if (PPC_IMM_OK_S(imm) && PPC_OFFS_OK(address)) {
            // Fast path. Both the immediate and the offset fit into 16 bits,
            // so we can use lwz, stw and addi.
            load32(address, addressTempRegister); // single lwz
            m_assembler.x_subi(addressTempRegister, addressTempRegister, int16_t(imm.m_value & 0xffff)); // addi operand order
            store32(addressTempRegister, address); // single stw
        } else if (PPC_OFFS_OK(address)) {
            // The offset still fits into 16 bits, so the loads/stores are fast.
            load32(address, addressTempRegister); // single lwz
            move(imm, tempRegister);
            m_assembler.subf(addressTempRegister, tempRegister, addressTempRegister);
            store32(addressTempRegister, address); // single stw
        } else if (PPC_IMM_OK_S(imm)) {
            // Well, at least the immediate is small! Try to save a load on
            // the effective address by keeping the offset in tempRegister.
            checkStackPointer(address);
            m_assembler.x_li32(tempRegister, address.offset);
            m_assembler.lwzx(addressTempRegister, address.base, tempRegister);
            m_assembler.x_subi(addressTempRegister, addressTempRegister, int16_t(imm.m_value & 0xffff));
            m_assembler.stwx(addressTempRegister, address.base, tempRegister);
        } else {
            // The immediate neither fits in 16-bits, nor the offset. Yuck.
            // Fortunately this is pretty rare, so it's not worth optimizing.
            load32(address, addressTempRegister);
            sub32(imm, tempRegister);
            store32(addressTempRegister, address);
        }
    }

    void sub32(Address src, RegisterID dest)
    {
        load32(src, tempRegister);
        sub32(tempRegister, dest);
    }

    void sub32(TrustedImm32 imm, AbsoluteAddress address)
    {
        load32(address.m_ptr, tempRegister);
        sub32(imm, tempRegister);
        store32(tempRegister, address.m_ptr);
    }

    void xor32(RegisterID src, RegisterID dest)
    {
        m_assembler.xor_(dest, dest, src);
    }

    void xor32(RegisterID op1, RegisterID op2, RegisterID dest)
    {
        m_assembler.xor_(dest, op1, op2);
    }

    void xor32(TrustedImm32 imm, RegisterID dest)
    {
        xor32(imm, dest, dest);
    }

    void xor32(TrustedImm32 imm, RegisterID src, RegisterID dest)
    {
        m_assembler.xori(dest, src, uint16_t(imm.m_value & 0xffff));
        if (!PPC_IMM_OK_U(imm)) // also need to handle bits 0-15
            m_assembler.xoris(dest, src, uint16_t(uint32_t(imm.m_value) >> 16));
    }

    void xor32(Address address, RegisterID dest)
    {
        load32(address, tempRegister);
        xor32(tempRegister, dest);
    }
    

    // Memory access operations:
    //
    // Loads are of the form load(address, destination) and stores of the form
    // store(source, address).  The source for a store may be an Imm32.  Address
    // operand objects to loads and store will be implicitly constructed if a
    // register is passed.
    //
    // With regard to addressing, PPC doesn't have all the bizarre address
    // encoding quirks of ARM and is most similar to SPARC ("big RISC"), so
    // we crib off the SPARC macroassembler for this section. Because we know
    // that address.base is always a register, this simplifies it for RISC.
    //
    // BaseIndex is particularly complex because we must shift address.index
    // left by address.scale, add address.offset to it, and then use it as
    // the offset to address.base. There is no atomic PPC instruction that
    // does anything like that. Fortunately, we can steal the SPARC's method
    // because it too is in the same boat (see SPARC load32(BaseIndex,
    // RegisterID)).

    void lea(Address address, RegisterID dest)
    {
        add32(TrustedImm32(address.offset), address.base, dest);
    }

    void lea(BaseIndex address, RegisterID dest)
    {
        // Cribbed from ARM. We can probably do better.
        move(address.index, addressTempRegister);
        if (address.scale != 0)
            lshift32(TrustedImm32(address.scale), addressTempRegister);
        if (address.offset)
            add32(TrustedImm32(address.offset), addressTempRegister);
        add32(address.base, addressTempRegister);
        move(addressTempRegister, dest);
    }
    
    void load32(ImplicitAddress address, RegisterID dest)
    {
        if(PPC_OFFS_OK(address))
            m_assembler.lwz(dest, address.base, address.offset);
        else {
            m_assembler.x_li32(addressTempRegister, address.offset);
            m_assembler.lwzx(dest, address.base, addressTempRegister);
        }
    }

    // Calculate the total offset (suitable for a [ls]*x instruction) from a
    // BaseIndex. This may clobber tempRegister.
    RegisterID generateAddressOffset(BaseIndex address) {
        if (address.scale == TimesOne) {
            if (address.offset == 0) {
                return address.index;
            }
            add32(TrustedImm32(address.offset), address.index, addressTempRegister);
        } else {
            if (PPC_OFFS_OK(address)) {
                m_assembler.x_slwi(addressTempRegister, address.index,
                                    address.scale);
                if (address.offset != 0)
                    m_assembler.addi(addressTempRegister, addressTempRegister,
                                        address.offset);
            } else {
                // Can't use r0 as a displacement, so we have to swap things up.
                m_assembler.x_slwi(tempRegister, address.index, address.scale);
                m_assembler.x_li32(addressTempRegister, address.offset);
                m_assembler.add(addressTempRegister, addressTempRegister,
                                    tempRegister);
            }
        }
        return addressTempRegister;
    }

    void load32(BaseIndex address, RegisterID dest)
    {
        m_assembler.lwzx(dest, address.base, generateAddressOffset(address));
    }

    void load32WithUnalignedHalfWords(BaseIndex address, RegisterID dest)
    {
        // We need both tempRegisters for this, so we can't use them as
        // a destination.
        ASSERT(dest != addressTempRegister);
        ASSERT(dest != tempRegister);

        if (address.scale == TimesOne) {
            m_assembler.add(addressTempRegister, address.base, address.index);
        } else {
            m_assembler.x_slwi(addressTempRegister, address.index, address.scale);
            m_assembler.add(addressTempRegister, addressTempRegister, address.base);
        }
 
        if (PPC_OFFS_OK(address) && PPC_OFFS_INCR_OK(address, 2)) {
            m_assembler.lhz(tempRegister, addressTempRegister, address.offset);
            m_assembler.lhz(dest, addressTempRegister, address.offset + 2);
            m_assembler.x_insertbits0_15(dest, tempRegister);
        } else {
            add32(TrustedImm32(address.offset), addressTempRegister);
            m_assembler.lhz(tempRegister, addressTempRegister, 0);
            m_assembler.lhz(dest, addressTempRegister, 2);
            m_assembler.x_insertbits0_15(dest, tempRegister);
        }
    }

    void load32WithUnalignedHalfWords(Address address, RegisterID dest)
    {
        // We need both tempRegisters for this, so we can't use them as
        // a destination.
        ASSERT(dest != addressTempRegister);
        ASSERT(dest != tempRegister);

        if (PPC_OFFS_OK(address) && PPC_OFFS_INCR_OK(address, 2)) {
            m_assembler.lhz(tempRegister, address.base, address.offset);
            m_assembler.lhz(dest, address.base, address.offset + 2);
            m_assembler.x_insertbits0_15(dest, tempRegister);
        } else {
            add32(TrustedImm32(address.offset), address.base, addressTempRegister);
            m_assembler.lhz(tempRegister, addressTempRegister, 0);
            m_assembler.lhz(dest, addressTempRegister, 2);
            m_assembler.x_insertbits0_15(dest, tempRegister);
        }
    }

    void load32(const void* address, RegisterID dest)
    {
        m_assembler.x_li32(addressTempRegister, (int)address);
        m_assembler.lwz(dest, addressTempRegister, 0);
    }

    void load8(ImplicitAddress address, RegisterID dest)
    {
        if (PPC_OFFS_OK(address))
            m_assembler.lbz(dest, address.base, address.offset);
        else {
            m_assembler.x_li32(addressTempRegister, address.offset);
            m_assembler.lbzx(dest, address.base, addressTempRegister);
        }
    }

    void load8(BaseIndex address, RegisterID dest)
    {
        m_assembler.lbzx(dest, address.base, generateAddressOffset(address));
    }

    void load8SignExtend(ImplicitAddress address, RegisterID dest)
    {
        load8(address, dest);
        m_assembler.extsb(dest, dest);
    }


    void load8SignExtend(BaseIndex address, RegisterID dest)
    {
        load8(address, dest);
        m_assembler.extsb(dest, dest);
    }

    void load8ZeroExtend(ImplicitAddress address, RegisterID dest)
    {
        load8(address, dest);
    }

    void load8ZeroExtend(BaseIndex address, RegisterID dest)
    {
        load8(address, dest);
    }

    DataLabel32 load32WithAddressOffsetPatch(Address address, RegisterID dest)
    {
        DataLabel32 dataLabel(this);
        // We use a special always-patchable li32 to guarantee that only
        // the non-optimized code is always emitted.
        m_assembler.x_p_li32(addressTempRegister, address.offset); // patch
        m_assembler.lwzx(dest, address.base, addressTempRegister);
        return dataLabel;
    }

    DataLabelCompact load32WithCompactAddressOffsetPatch(Address address, RegisterID dest)
    {
        ASSERT_NOT_REACHED();
        DataLabelCompact dataLabel(this);
        load32WithAddressOffsetPatch(address, dest);
        return dataLabel;
    }

    DataLabel32 load64WithAddressOffsetPatch(Address address, RegisterID hi, RegisterID lo)
    {
        DataLabel32 dataLabel(this);

        // SPARC and ARM both load 0, so we will too.
        // We assume it will be repatched immediately.
        m_assembler.x_p_li32(addressTempRegister, 0);
        m_assembler.add(addressTempRegister, address.base, addressTempRegister);
#ifdef _ARCH_PPC64
        m_assembler.ld(lo, addressTempRegister, 0);
        m_assembler.x_srdi(hi, lo, 32);
#else
        m_assembler.lwz(hi, addressTempRegister, 0);
        m_assembler.lwz(lo, addressTempRegister, 4);
#endif

        return dataLabel;
    }

    Label loadPtrWithPatchToLEA(Address address, RegisterID dest)
    {
        Label label(this);
        // This is almost the same as the above. Effectively it is
        // load32(address, dest); as in the SPARC version. However,
        // the patch is done to the lwzx, NOT the x_li32; we just need
        // to use the patchable version so we know where the lwzx will
        // actually be emitted (8 bytes/2 words down from the label).
        m_assembler.x_p_li32(addressTempRegister, address.offset);
        m_assembler.lwzx(dest, address.base, addressTempRegister); // patch
        return label;
    }

    void load16(BaseIndex address, RegisterID dest)
    {
        m_assembler.lhzx(dest, address.base, generateAddressOffset(address));
    }
    
    void load16(ImplicitAddress address, RegisterID dest)
    {
        if (PPC_OFFS_OK(address))
            m_assembler.lhz(dest, address.base, address.offset);
        else {
            m_assembler.x_li32(addressTempRegister, address.offset);
            m_assembler.lhzx(dest, address.base, addressTempRegister);
        }
    }

    void load16Unaligned(BaseIndex address, RegisterID dest)
    {
        load16(address, dest);
    }

    void load16SignExtend(ImplicitAddress address, RegisterID dest)
    {
        if (PPC_OFFS_OK(address))
            m_assembler.lha(dest, address.base, address.offset);
        else {
            m_assembler.x_li32(addressTempRegister, address.offset);
            m_assembler.lhax(dest, address.base, addressTempRegister);
        }
    }

    void load16SignExtend(BaseIndex address, RegisterID dest)
    {
        m_assembler.lhax(dest, address.base, generateAddressOffset(address));
    }

    DataLabel32 store32WithAddressOffsetPatch(RegisterID src, Address address)
    {
        DataLabel32 dataLabel(this);
        checkStackPointer(address);
        // Being patchable, we assume the worst, i.e., that the offset cannot
        // fit into immediate range.
        m_assembler.x_p_li32(addressTempRegister, 0); // patch
        m_assembler.stwx(src, addressTempRegister, address.base);
        return dataLabel;
    }

    void store32(RegisterID src, ImplicitAddress address)
    {
        checkStackPointer(address);
        if (PPC_OFFS_OK(address)) {
            m_assembler.stw(src, address.base, address.offset);
        } else {
            m_assembler.x_li32(addressTempRegister, address.offset);
            m_assembler.stwx(src, address.base, addressTempRegister);
        }
    }

    void store32(RegisterID src, BaseIndex address)
    {
        checkStackPointer(address);
        m_assembler.stwx(src, address.base, generateAddressOffset(address));
    }
    
    void store32(TrustedImm32 imm, BaseIndex address)
    {
        checkStackPointer(address);
        RegisterID offsetRegister = generateAddressOffset(address);

        m_assembler.x_li32(tempRegister, imm.m_value);
        m_assembler.stwx(tempRegister, address.base, offsetRegister);
    }

    void store32(TrustedImm32 imm, ImplicitAddress address)
    {
        checkStackPointer(address);
        if (PPC_OFFS_OK(address)) {
            m_assembler.x_li32(tempRegister, imm.m_value);
            m_assembler.stw(tempRegister, address.base, int16_t(address.offset & 0xffff));
        } else {
            m_assembler.x_li32(addressTempRegister, address.offset);
            m_assembler.x_li32(tempRegister, imm.m_value);
            m_assembler.stwx(tempRegister, address.base, addressTempRegister);
        }
    }

    void store32(RegisterID src, const void* address)
    {
        m_assembler.x_li32(addressTempRegister, (int)address);
        m_assembler.stw(src, addressTempRegister, 0);
    }

    void store32(TrustedImm32 imm, const void* address)
    {
        m_assembler.x_li32(tempRegister, imm.m_value);
        store32(tempRegister, address);
    }

    
    // We are big-endian, so we act like SPARC, not ARM-Android.
    DataLabel32 store64WithAddressOffsetPatch(RegisterID hi, RegisterID lo, Address address)
    {
        DataLabel32 dataLabel(this);

        m_assembler.x_p_li32(addressTempRegister, address.offset);
        m_assembler.add(addressTempRegister, address.base, addressTempRegister);
#ifdef _ARCH_PPC64
        m_assembler.x_insrdi(lo, hi, 32, 0);
        m_assembler.std(lo, addressTempRegister, 0);
#else
        m_assembler.stw(hi, addressTempRegister, 0);
        m_assembler.stw(lo, addressTempRegister, 4);
#endif

        return dataLabel;
    }

    DataLabel32 store64WithAddressOffsetPatch(TrustedImm32 hi, RegisterID lo, Address address)
    {
        DataLabel32 dataLabel(this);

        m_assembler.x_p_li32(addressTempRegister, address.offset);
        m_assembler.add(addressTempRegister, address.base, addressTempRegister);
#ifndef _ARCH_PPC64
        m_assembler.stw(lo, addressTempRegister, 4);
#endif
        m_assembler.x_li32(tempRegister, hi.m_value);
#ifdef _ARCH_PPC64
        m_assembler.x_insrdi(lo, tempRegister, 32, 0);
        m_assembler.std(lo, addressTempRegister, 0);
#else
        m_assembler.stw(tempRegister, addressTempRegister, 0);
#endif
        return dataLabel;
    }

    DataLabel32 store64WithAddressOffsetPatch(TrustedImm32 hi, TrustedImm32 lo, Address address)
    {
        DataLabel32 dataLabel(this);

        m_assembler.x_p_li32(addressTempRegister, address.offset);
        m_assembler.add(addressTempRegister, address.base, addressTempRegister);
#ifdef _ARCH_PPC64
//        m_assembler.x_li64(tempRegister, (((int64_t)hi.m_value << 32) & 0xffffffff00000000) | (((int64_t)lo.m_value) & 0x00000000ffffffff));
        m_assembler.x_li32(emergencyTempRegister, hi.m_value);
        m_assembler.x_li32(tempRegister, lo.m_value);
        m_assembler.x_insrdi(tempRegister, emergencyTempRegister, 32, 0);
        m_assembler.std(tempRegister, addressTempRegister, 0);
#else
        m_assembler.x_li32(tempRegister, hi.m_value);
        m_assembler.stw(tempRegister, addressTempRegister, 0);
        m_assembler.x_li32(tempRegister, lo.m_value);
        m_assembler.stw(tempRegister, addressTempRegister, 4);
#endif
        
        return dataLabel;
    }

    void store8(RegisterID src, ImplicitAddress address)
    {
        if (PPC_OFFS_OK(address)) {
            m_assembler.stb(src, address.base, address.offset);
        } else {
            m_assembler.x_li32(addressTempRegister, address.offset);
            m_assembler.stbx(src, address.base, addressTempRegister);
        }
    }

    void store8(TrustedImm32 imm, ImplicitAddress address)
    {
        m_assembler.x_li(tempRegister, imm.m_value);
        store8(tempRegister, address);
    }

    void store8(TrustedImm32 imm, const void* address)
    {
        m_assembler.x_li(tempRegister, imm.m_value);
        store8(tempRegister, address);
    }
    void store8(RegisterID src, const void* address)
    {
#warning AmigaOS: dont know if you can do x_li with a const void ptr
        // looked at row 805: void store32(RegisterID src, const void* address)
        // for inspiration
        m_assembler.x_li32(addressTempRegister, (int)address);
        m_assembler.stb(src, addressTempRegister, 0);
    }
    void store8(RegisterID src, BaseIndex address)
    {
        m_assembler.stbx(src, address.base, generateAddressOffset(address));
    }

    void store8(TrustedImm32 imm, BaseIndex address)
    {
        RegisterID offsetRegister = generateAddressOffset(address);
        m_assembler.x_li(tempRegister, imm.m_value);
        m_assembler.stbx(tempRegister, address.base, offsetRegister);
    }

    void store16(RegisterID src, ImplicitAddress address)
    {
        if (PPC_OFFS_OK(address)) {
            m_assembler.sth(src, address.base, address.offset);
        } else {
            m_assembler.x_li32(addressTempRegister, address.offset);
            m_assembler.sthx(src, address.base, addressTempRegister);
        }
    }

    void store16(TrustedImm32 imm, ImplicitAddress address)
    {
        m_assembler.x_li(tempRegister, imm.m_value);
        store16(tempRegister, address);
    }

    void store16(RegisterID src, BaseIndex address)
    {
        m_assembler.sthx(src, address.base, generateAddressOffset(address));
    }

    void store16(TrustedImm32 imm, BaseIndex address)
    {
        RegisterID offsetRegister = generateAddressOffset(address);
        m_assembler.x_li(tempRegister, imm.m_value);
        m_assembler.sthx(tempRegister, address.base, offsetRegister);
    }

    // Floating-point operations:
    // The memory portions are based on SPARC; the ALU portions on ARM.
    // However, even our 32-bit PPCs have full 64-bit FPUs, so our memory
    // implementation is simpler than SPARC (and considerably more so than
    // ARM).

    static bool supportsFloatingPoint() { return true; }
    static bool supportsFloatingPointTruncate() { return true; }
    static bool supportsFloatingPointSqrt()
    {
#if defined(_ARCH_PPCSQ) || defined(PPC_SOFT_SQRT)
        // G5s have native fsqrt; G3/G4 can use frsqrte and iterations.
        return true;
#else
        // For everybody else, fall back on the library routine.
        return false;
#endif
    }
    // AmigaOS: think this is true: https://imcs.dvfu.ru/lib.int/docs/Hardware/intro_to_ppc/ppc3_software3.html#FPU Arithmetic Instructions
    static bool supportsFloatingPointAbs() { return true; }

    // create a double out of two 32-bit GPRs
    void fastLoadDouble(RegisterID lo, RegisterID hi, FPRegisterID dest)
    {
        const int d = PPC_LINKAGE_FREE; // Use scratch in outgoing linkage area.
        m_assembler.stw(hi, stackPointerRegister, d);
        m_assembler.stw(lo, stackPointerRegister, d+4);
#ifdef _ARCH_PWR4
        // G5 and POWER4+ do better if the stw and the lfd aren't in the
        // same dispatch group.
        m_assembler.x_nop();
        m_assembler.x_nop();
        m_assembler.x_nop();
#endif
        m_assembler.lfd(dest, stackPointerRegister, d);
    }

    void loadDouble(ImplicitAddress address, FPRegisterID dest)
    {
        if (PPC_OFFS_OK(address))
            m_assembler.lfd(dest, address.base, address.offset);
        else {
            m_assembler.x_li32(addressTempRegister, address.offset);
            m_assembler.lfdx(dest, address.base, addressTempRegister);
        }
    }

    void loadDouble(BaseIndex address, FPRegisterID dest)
    {
        m_assembler.lfdx(dest, address.base, generateAddressOffset(address));
    }

    DataLabelPtr loadDouble(const void *address, FPRegisterID dest)
    {
        DataLabelPtr dlp = moveWithPatch(TrustedImmPtr(address), addressTempRegister);
        m_assembler.lfd(dest, addressTempRegister, 0);
        
        return dlp;
    }

    void loadFloat(ImplicitAddress address, FPRegisterID dest)
    {
        if (PPC_OFFS_OK(address))
            m_assembler.lfs(dest, address.base, address.offset);
        else {
            m_assembler.x_li32(addressTempRegister, address.offset);
            m_assembler.lfsx(dest, address.base, addressTempRegister);
        }
    }

    void loadFloat(BaseIndex address, FPRegisterID dest)
    {
        m_assembler.lfsx(dest, address.base, generateAddressOffset(address));
    }

    void storeDouble(FPRegisterID src, ImplicitAddress address)
    {
        if (PPC_OFFS_OK(address))
            m_assembler.stfd(src, address.base, address.offset);
        else {
            m_assembler.x_li32(addressTempRegister, address.offset);
            m_assembler.stfdx(src, addressTempRegister, address.base);
        }
    }

    void storeDouble(FPRegisterID src, BaseIndex address)
    {
        checkStackPointer(address);
        m_assembler.stfdx(src, address.base, generateAddressOffset(address));
    }

#if 0
    void storeDouble(ImmDouble imm, ImplicitAddress address)
    {
        m_assembler.x_li32(tempRegister, imm.u.s.msb);
        add32(Imm32(address.offset), address.base, addressTempRegister);
        m_assembler.stw(tempRegister, addressTempRegister, 0);
        m_assembler.x_li32(tempRegister, imm.u.s.lsb);
        m_assembler.stw(tempRegister, addressTempRegister, 4);
    }

    void storeDouble(ImmDouble imm, BaseIndex address)
    {
        RegisterID offsetRegister = generateAddressOffset(address);
        m_assembler.x_li32(tempRegister, imm.u.s.msb);
        m_assembler.stwx(tempRegister, address.base, offsetRegister);
        add32(Imm32(4), offsetRegister);
        m_assembler.x_li32(tempRegister, imm.u.s.lsb);
        m_assembler.stwx(tempRegister, address.base, offsetRegister);
    }
#endif

    void storeFloat(FPRegisterID src, ImplicitAddress address)
    {
        if (PPC_OFFS_OK(address))
            m_assembler.stfs(src, address.base, address.offset);
        else {
            m_assembler.x_li32(addressTempRegister, address.offset);
            m_assembler.stfsx(src, addressTempRegister, address.base);
        }
    }

    void storeFloat(FPRegisterID src, BaseIndex address)
    {
        checkStackPointer(address);
        m_assembler.stfsx(src, address.base, generateAddressOffset(address));
    }

#if 0
    void storeFloat(ImmDouble imm, Address address)
    {
        union { float f; int i; } floatBits;
        floatBits.f = float(imm.u.d);
        store32(Imm32(floatBits.i), address);
    }

    void storeFloat(ImmDouble imm, BaseIndex address)
    {
        union { float f; int i; } floatBits;
        floatBits.f = float(imm.u.d);
        store32(Imm32(floatBits.i), address);
    }
#endif

    void moveDouble(FPRegisterID src, FPRegisterID dest)
    {
        m_assembler.fmr(dest, src);
    }

    void absDouble(FPRegisterID src, FPRegisterID dest)
    {
        m_assembler.fabs(dest, src);
    }

    void zeroDouble(FPRegisterID srcDest)
    {
        // FPreg zero is just 64 zero bits, so we dump 64 zero bits in the
        // linkage area.

        const int d = PPC_LINKAGE_FREE; // Use scratch in outgoing linkage area.

        m_assembler.xor_(tempRegister, tempRegister, tempRegister);
#ifdef _ARCH_PPC64
        m_assembler.std(tempRegister, stackPointerRegister, d);
#else
        m_assembler.stw(tempRegister, stackPointerRegister, d);
        m_assembler.stw(tempRegister, stackPointerRegister, d+4);
#endif
#ifdef _ARCH_PWR4
        // G5 and POWER4+ do better if the lfd and the std/stw's aren't in the
        // same dispatch group.
        m_assembler.x_nop();
#endif
        m_assembler.lfd(srcDest, stackPointerRegister, d);
    }

#warning AmigaOS: dont know if swapDouble, moveDoubleToInts, moveIntsToDouble is correct, copied from MIPS
    void swapDouble(FPRegisterID fr1, FPRegisterID fr2)
    {
        moveDouble(fr1, fpTempRegister);
        moveDouble(fr2, fr1);
        moveDouble(fpTempRegister, fr2);
    }
    void moveDoubleToInts(FPRegisterID src, RegisterID dest1, RegisterID dest2)
    {
#warning AmigaOS: really dont know what im doing here:
        // MIPS: m_assembler.vmov(dest1, dest2, src);
        // https://cr.yp.to/2005-590/powerpc-cwg.pdf
        // Figure 3-48
        // fctiw[z] FR2,FR1 # convert to integer
        // stfd FR2,disp(R1) # copy unmodified to memory
        // lwz R3,disp+4(R1) # load the low-order 32 bits

      /*  m_assembler.fctiwz(dest1,src); // convert to integer
        m_assembler.stfd(dest1,tempRegister); // copy unmodified to memory
        m_assembler.lwz(dest1,tempRegister); // load the low-order 32 bits
*/
    }

    void moveIntsToDouble(RegisterID src1, RegisterID src2, FPRegisterID dest, FPRegisterID scratch)
    {
#warning AmigaOS: really dont know what im doing here no 2:
        UNUSED_PARAM(scratch);
        // MIPS: m_assembler.vmov(dest, src1, src2);
        // https://cr.yp.to/2005-590/powerpc-cwg.pdf
        // Figure 3-52
        // # FR2 = 0x4330000080000000
/*
        m_assembler.PPC_addis( R0,R0,0x4330); //# R0 = 0x43300000
        m_assembler.PPC_stw(R0,disp(R1)); // # store upper half
        m_assembler.PPC_xoris( R3,R3,0x8000); // # flip sign bit
        m_assembler.PPC_stw( R3,disp+4(R1)); // # store lower half
        m_assembler.PPC_lfd(FR1,disp(R1)); // # float load double of value
        m_assembler.PPC_fsub( FR1,FR1,FR2); // # subtract 0x4330000080000000
  */      
    }
    void breakDoubleTo32(FPRegisterID srcDest, RegisterID typeReg, RegisterID dataReg) {

        // Dump the FPR into the linkage area, extracting the upper word into
        // typeReg and the lower word into dataReg.

        const int d = PPC_LINKAGE_FREE; // Use scratch in outgoing linkage area.
        
        m_assembler.stfd(srcDest, stackPointerRegister, d);
#ifdef _ARCH_PWR4
        // G5 and POWER4+ do better if the stfd and the lwz aren't in the
        // same dispatch group.
        m_assembler.x_nop();
        m_assembler.x_nop();
        m_assembler.x_nop();
#endif
        m_assembler.lwz(typeReg, stackPointerRegister, d);
        m_assembler.lwz(dataReg, stackPointerRegister, d+4);
    }

    void addDouble(FPRegisterID src, FPRegisterID dest)
    {
        m_assembler.fadd(dest, dest, src);
    }

    void addDouble(Address src, FPRegisterID dest)
    {
        loadDouble(src, fpTempRegister);
        addDouble(fpTempRegister, dest);
    }

    void subDouble(FPRegisterID src, FPRegisterID dest)
    {
        // dest = dest - src (SPARC: fsubd dest, src, dest)
        m_assembler.fsub(dest, dest, src);
    }

    void subDouble(Address src, FPRegisterID dest)
    {
        loadDouble(src, fpTempRegister);
        subDouble(fpTempRegister, dest);
    }

    void mulDouble(FPRegisterID src, FPRegisterID dest)
    {
        m_assembler.fmul(dest, src, dest);
    }

    void mulDouble(Address src, FPRegisterID dest)
    {
        loadDouble(src, fpTempRegister);
        mulDouble(fpTempRegister, dest);
    }

    void divDouble(FPRegisterID src, FPRegisterID dest)
    {
        // (dest / src) => dest SPARC: fdivd dest, src, dest
        m_assembler.fdiv(dest, dest, src);
    }

    // where's the fused multiply add! WTF! we kick ass with that!!

    void sqrtDouble(FPRegisterID src, FPRegisterID dest)
    {
#if defined(_ARCH_PPCSQ) && !defined(DEBUG) && !defined(PPC_SOFT_SQRT)
        // The G5 gets a chance to shine.
        m_assembler.fsqrt(dest, src);
#else
#if defined(PPC_SOFT_SQRT)
        // This is David Kilbridge's Newton-Raphson iterative square root
        // algorithm based on the reciprocal square root instruction on
        // Apple G3s and G4s. Not all PPC cores have this instruction.

// Pick three registers that we don't have to save. (TenFourFox issue 134)
// dest is volatile because it will be clobbered anyway.
#define SQRR1 dest
// This one the methodjit doesn't even know exists.
#define SQRR2 PPCRegisters::f8
// This one is explicitly declared as volatile to the methodjit.
#define SQRR3 PPCRegisters::f1

        // Ensure sufficient space so that if a constant pool gets inserted,
        // it doesn't screw up internal branching (TenFourFox issue 134).
        m_assembler.ensureSpace(40 * 4);

        // Clear relevant bits in the FPSCR.
        // NB: gdb disassembler munges these oddly, but they are correct.
        m_assembler.mtfsb0(0); // fx
        m_assembler.mtfsb0(5); // zx
        m_assembler.mtfsb0(7); // vxsnan
        m_assembler.mtfsb0(22); // vxsqrt
        m_assembler.mtfsb0(30); // set round to nearest
        m_assembler.mtfsb0(31); //  "    "    "    "

        // Temporarily make src the "answer."
        m_assembler.fmr(dest, src);
        // Compute the reciprocal and load flags to CR1: z =~ 1/sqrt(src)
        m_assembler.frsqrte_rc(fpTempRegister, src);

        // If no new exception, commence the spanking. Er, the iterations.
        // No, it's too perilous. But it *is* Likely. And then comes the ...
        m_assembler.bc(PPCRegisters::cr1, PPCAssembler::ConditionGE, 4*4,
            PPCAssembler::LikelyBranch,
            PPCAssembler::DontLinkBranch); // bc+ 4, 4, @0

        // Exception. Get ZX bit to see if we wound up dividing by zero.
        m_assembler.mcrfs(PPCRegisters::cr7, 1);
        // If ZX is true, return zero (jump to the end), because sqrt(0)==0
        // and our reciprocal just tried to divide by zero, so src must be
        // zero.
        m_assembler.bc(PPCRegisters::cr7, PPCAssembler::ConditionGT, 23*4,
            PPCAssembler::NotLikelyBranch,
            PPCAssembler::DontLinkBranch); // bc 12, 29, @3
        // ZX was not. Return the unordered result from fpTempRegister. src
        // was negative or NaN; the answer is unordered.
        m_assembler.b(21*4,
            PPCAssembler::RelativeBranch,
            PPCAssembler::DontLinkBranch); // b @2

        // @0
        // It is not an exception to get the sqrt of +- Infinity, which is
        // itself. Don't iterate on that (check FPRF/FE bit first).
        m_assembler.mcrfs(PPCRegisters::cr7, 4);
        // If set, return src.
        m_assembler.bc(PPCRegisters::cr7, PPCAssembler::ConditionEQ, 20*4,
            PPCAssembler::NotLikelyBranch,
            PPCAssembler::DontLinkBranch); // bc 12, 30, @3

        // Iterations now required. First, load constant for N-R iterations.
        m_assembler.x_lis(tempRegister, 0x3F00); // 0.5 = 0x3f000000
        m_assembler.stw(tempRegister, stackPointerRegister, PPC_LINKAGE_FREE);
        m_assembler.lfs(SQRR3, stackPointerRegister, PPC_LINKAGE_FREE); // 3 ins

        // x = x/2 for the following
        m_assembler.fmul(SQRR1, src, SQRR3); // 4

        // Do three iterations converging to 1/sqrt(x):
        // compute z^2 => T1
        // 1/2 - T1 * x (which is x/2) => T2
        // z + z * T2 => new z
        // It's time to kick ass with fused multiply add. YEE HAW.
#define NRIT    m_assembler.fmul(SQRR2, fpTempRegister, fpTempRegister); \
                m_assembler.fnmsub(SQRR2, SQRR2, SQRR1, SQRR3); \
                m_assembler.fmadd(fpTempRegister, fpTempRegister, SQRR2, \
                    fpTempRegister);

        NRIT
        NRIT
        NRIT
        // 13 ins
#undef NRIT

        // The final iteration gets the last bit correct.
        m_assembler.fadd(SQRR1, SQRR1, SQRR1); // turn x/2 back into x
        m_assembler.fmul(SQRR2, fpTempRegister, SQRR1); // y = z * x
        m_assembler.fnmsub(SQRR1, SQRR2, SQRR2, SQRR1); // x - y^2
        m_assembler.fmul(SQRR1, SQRR1, SQRR3); // divided by 2

        // Finally: result = y + ((x - y^2)/2) * z
        m_assembler.fmadd(fpTempRegister, SQRR1, fpTempRegister, SQRR2);
            // Final result is in fpTempRegister (18 ins)

        // @2
        m_assembler.fmr(dest, fpTempRegister); // 19
        // @3

#undef SQRR1
#undef SQRR2
#undef SQRR3
#else
        // This should never be called because we patched them out in
        // FastBuiltins (right??).
        ASSERT_NOT_REACHED();
#endif
#endif
    }

    void negDouble(FPRegisterID src, FPRegisterID dest)
    {
        m_assembler.fneg(dest, src);
    }

    void convertDoubleToFloat(FPRegisterID src, FPRegisterID dest)
    {
        m_assembler.frsp(dest, src);
    }

    void convertInt32ToDouble(RegisterID src, FPRegisterID dest)
    {
        // PowerPC doesn't have any GPR<->FPR moves, damn it.
        // We borrow our code from the nanojit for this (also see
        // Optimizing PowerPC Code, ch.8, p.156).
        // TODO: put the zero constant we build here each time somewhere
        // common and reference that.

        const int d = PPC_LINKAGE_FREE; // Use scratch in outgoing linkage area.
        ASSERT(dest != fpTempRegister);
        ASSERT(src != tempRegister);

        // Build zero double-precision FP constant.
        m_assembler.x_lis(tempRegister, 0x4330);
        m_assembler.stw(tempRegister, stackPointerRegister, d);
        m_assembler.x_lis(tempRegister, 0x8000);
        m_assembler.stw(tempRegister, stackPointerRegister, d+4);
        // G5 note: this is already one dispatch group

        // Build intermediate float from zero constant.
        m_assembler.lfd(fpTempRegister, stackPointerRegister, d);
        
        // Flip sign of integer value and use as integer component.
        m_assembler.xoris(tempRegister, src, 0x8000);
        m_assembler.stw(tempRegister, stackPointerRegister, d+4);
#ifdef _ARCH_PWR4
        // G5 and POWER4+ do better if the lfd and the stws aren't in the
        // same dispatch group.
        m_assembler.x_nop();
#endif

        // Load and normalize with a subtraction operation.
        m_assembler.lfd(dest, stackPointerRegister, d);
        m_assembler.fsub(dest, dest, fpTempRegister);
    }

    void convertInt32ToDouble(Address address, FPRegisterID dest)
    {
        load32(address, addressTempRegister);
        convertInt32ToDouble(addressTempRegister, dest);
    }            

    void convertUInt32ToDouble(RegisterID src, FPRegisterID dest)
    {
        // PowerPC doesn't have any GPR<->FPR moves, damn it.
        // We borrow our code from the nanojit for this.

        const int d = PPC_LINKAGE_FREE; // Use scratch in outgoing linkage area.

        // Build zero double-precision FP constant.
        m_assembler.x_lis(tempRegister, 0x4330);
        m_assembler.stw(tempRegister, stackPointerRegister, d);
        // Use the integer value as part of the mantissa.
        m_assembler.stw(src, stackPointerRegister, d+4);
#ifdef _ARCH_PWR4
        // Break up operations for G5 to avoid aliasing in dispatch groups.
        m_assembler.x_nop();
#endif

        // Build intermediate float from zero constant.
        m_assembler.lfd(dest, stackPointerRegister, d);
        m_assembler.x_li(tempRegister, 0);
        m_assembler.stw(tempRegister, stackPointerRegister, d+4);
#ifdef _ARCH_PWR4
        m_assembler.x_nop();
#endif

        // Load and normalize with a subtraction operation.
        m_assembler.lfd(fpTempRegister, stackPointerRegister, d);
        m_assembler.fsub(dest, dest, fpTempRegister);
    }

    Jump branchDouble(DoubleCondition cond, FPRegisterID left, FPRegisterID right)
    {
        // All comparison instructions store to CR0 implicitly.
        m_assembler.fcmpu(left, right);
        // The jump has to take into account an unordered comparison result.
        return Jump(m_assembler.m_fbranch(cond));
    }

    void branchConvertDoubleToInt32(FPRegisterID src, RegisterID dest, JumpList& failureCases, FPRegisterID fpTemp)
    {
        // Convert 'src' to an integer and move to 'dest'. If the result is
        // not representable as an integer (i.e., non-integral, or out of
        // range), branch. PPC doesn't have FPR<->GPR moves and that makes
        // this macroop really suck. There is another reason why this is
        // problematic; read on.
        const int d = PPC_LINKAGE_FREE; // safe zone within the linkage area

        // Turn into a fixed-point integer (i.e., truncate).
        m_assembler.fctiwz(fpTempRegister, src);
        // Stuff in the linkage area.
        m_assembler.stfd(fpTempRegister, stackPointerRegister, d);
#ifdef _ARCH_PWR4
        // G5 and POWER4+ do better if the stfd and the lwz aren't in the
        // same dispatch group (empirically tested to have an optimal value
        // of two nops in the nanojit {RIP}).
        m_assembler.x_nop();
        m_assembler.x_nop();
#endif
        // Pull out the lower 32 bits. This is the result.
        m_assembler.lwz(dest, stackPointerRegister, d+4);

        // We don't have enough precision in the macroassembler to deal
        // with the FPSCR, which already knows what the result is. If we
        // didn't have to append a fully formed branch to failureCases, we
        // could do that here with fctiwz. and CR1. This is already
        // considered an expensive operation, so the simplest and most
        // reliable (if technically disgusting) approach at this time is
        // to simply do what SPARC does, which is to convert it back to
        // float and see if it's the same.
        convertInt32ToDouble(dest, fpTemp);

        // If not equal/precision lost or unordered, jump.
        failureCases.append(branchDouble(DoubleNotEqualOrUnordered, src, fpTemp));

        // If the result is zero, it could be -0, and 0 == -0.
        failureCases.append(branchTest32(Zero, dest));
    }

    Jump branchTruncateDoubleToInt32(FPRegisterID src, RegisterID dest)
    {
        // Truncate 'src' to an integer and move to 'dest'. If the result
        // is not representable as a 32 bit value, branch. Since our last
        // function, PPC still hasn't grown any FPR<->GPR moves, because it's
        // a man's processor, man.

#if !defined(_ARCH_PWR4)
        // G3 and G4 only:
        // Ensure sufficient space so that if a constant pool gets inserted,
        // it doesn't screw up internal branching (TenFourFox issue 134).
        // Count x_li32 as two (in worst case). Don't count the G5 nops!
        m_assembler.ensureSpace(16 * 4);
#endif

        // The first part is the same as the last function:
        const int d = PPC_LINKAGE_FREE; // safe zone within the linkage area
        // Turn into a fixed-point integer (i.e., truncate).
        m_assembler.fctiwz(fpTempRegister, src);
        // Stuff in the linkage area.
        m_assembler.stfd(fpTempRegister, stackPointerRegister, d);
#ifdef _ARCH_PWR4
        // G5 and POWER4+ do better if the stfd and the lwz aren't in the
        // same dispatch group (empirically tested to have an optimal value
        // of two nops in the nanojit {RIP}).
        m_assembler.x_nop();
        m_assembler.x_nop();
#endif
        // Pull out the lower 32 bits. This is the result.
        m_assembler.lwz(dest, stackPointerRegister, d+4);

        // But this part is different. Unlike branchConvert, we don't care
        // if a truncation occurred; all we care is that the integer portion
        // is expressed within 32 bits. Fortunately, fctiwz's result will
        // tip us off: if src > 2^31-1, then dest becomes 0x7fffffff, the
        // largest 32-bit positive integer. If src < -2^31, then dest becomes
        // 0x80000000, the largest 32-bit negative integer. So we just test
        // for those two values.

        // Surrogate "summary" register.
        m_assembler.x_li32(addressTempRegister, 0);

        // 0x7fffffff; increment if a match
        m_assembler.x_li32(tempRegister, 0x7fffffff);
        m_assembler.cmplw(tempRegister, dest);
        // Since forward branches are assumed not taken, and we are likely
        // to take it, we set the likely bit.
        m_assembler.bc(PPCRegisters::cr0, PPCAssembler::ConditionNE, 8, // skip next instruction
            PPCAssembler::LikelyBranch, PPCAssembler::DontLinkBranch);
        m_assembler.addi(addressTempRegister, addressTempRegister, 1);

        // 0x80000000
        m_assembler.x_li32(tempRegister, 0x80000000); // sign extends!
        m_assembler.cmplw(tempRegister, dest);
        m_assembler.bc(PPCRegisters::cr0, PPCAssembler::ConditionNE, 8, // skip next instruction
            PPCAssembler::LikelyBranch, PPCAssembler::DontLinkBranch);
        m_assembler.addi(addressTempRegister, addressTempRegister, 1);

        // Branch if addressTempRegister == 1.
        return branch32(Equal, addressTempRegister, TrustedImm32(1));
    }

    Jump branchDoubleNonZero(FPRegisterID reg, FPRegisterID scratch)
    {
#warning AmigaOS: this is what is done in MIPS
        //m_assembler.vmov(scratch, MIPSRegisters::zero, MIPSRegisters::zero);
        return branchDouble(DoubleNotEqual, reg, scratch);
    }

    Jump branchDoubleZeroOrNaN(FPRegisterID reg, FPRegisterID scratch)
    {
#warning AmigaOS: this is what is done in MIPS
        //m_assembler.vmov(scratch, MIPSRegisters::zero, MIPSRegisters::zero);
        return branchDouble(DoubleEqualOrUnordered, reg, scratch);
    }
    
    static void replaceWithJump(CodeLocationLabel instructionStart, CodeLocationLabel destination)
    {
        PPCAssembler::replaceWithJump(instructionStart.dataLocation(), destination.dataLocation());
    }
    // Stack manipulation operations:
    //
    // The ABI is assumed to provide a stack abstraction to memory,
    // containing machine word sized units of data.  Push and pop
    // operations add and remove a single register sized unit of data
    // to or from the stack.  Peek and poke operations read or write
    // values on the stack, without moving the current stack position.
    //
    // This is potentially troublesome on PPC because we have to keep the
    // actual storage location relative to the linkage area, so when we
    // push onto the stack we must remember that the linkage area is still
    // first in line. SPARC has something similar. If we do this wrong, then
    // the stack may get maimed when a function we call tries to write into
    // the linkage area it thinks it has (and instead tramples on this data).
    //
    // Because this makes assumptions about the size of the linkage area,
    // this is ABI dependent.
    
    void push(RegisterID src)
    {
#if OS(DARWIN)
        // Drop stack pointer down. Do this first so that if an IRQ occurs,
        // it still has a (oversize) stack frame it can write in.
        m_assembler.x_subi(stackPointerRegister, stackPointerRegister, 4);
        // Store at the *end* of the *previous* linkage area (to 56(r1)).
        m_assembler.stw(src, stackPointerRegister, 56);
#else
#warning AmigaOS: push is ABI-dependent on PPC and your ABI is not supported yet
        // Drop stack pointer down. Do this first so that if an IRQ occurs,
        // it still has a (oversize) stack frame it can write in.
        m_assembler.x_subi(stackPointerRegister, stackPointerRegister, 4);
        // Store at the *end* of the *previous* linkage area (to 56(r1)).
        m_assembler.stw(src, stackPointerRegister, 56);
#endif
    }

    void pop(RegisterID dest)
    {
#if OS(DARWIN)
        // Reverse these two operations.
        m_assembler.lwz(dest, stackPointerRegister, 56);
        m_assembler.addi(stackPointerRegister, stackPointerRegister, 4);
#else
#warning AmigaOS: pop is ABI-dependent on PPC and your ABI is not supported yet
        // Reverse these two operations.
        m_assembler.lwz(dest, stackPointerRegister, 56);
        m_assembler.addi(stackPointerRegister, stackPointerRegister, 4);
#endif
    }

    void push(Address address)
    {
        load32(address, tempRegister);
        push(tempRegister);
    }

    void push(TrustedImm32 imm)
    {
        move(imm, tempRegister);
        push(tempRegister);
    }

    // Register move operations:
    //
    // Move values in registers.
    // These are PPC-specific.

    void move(TrustedImm32 imm, RegisterID dest)
    {
        // Let the assembler worry about this.
        m_assembler.x_li32(dest, imm.m_value);
    }

    void move(RegisterID src, RegisterID dest)
    {
        m_assembler.or_(dest, src, src); // "mr"
    }

    void move(TrustedImmPtr imm, RegisterID dest)
    {
        move(TrustedImm32(imm), dest);
    }
//#warning AmigaOS: TrustedImm64 converted to TrustedImm32. Not ok... right? 
//    void move(TrustedImm64 imm, RegisterID dest)
//    {
//        move(TrustedImm32(imm.asIntptr()), dest);
//    }


    void swap(RegisterID reg1, RegisterID reg2)
    {
        move(reg1, tempRegister);
        move(reg2, reg1);
        move(tempRegister, reg2);
    }

    void signExtend32ToPtr(RegisterID src, RegisterID dest)
    {
        if (src != dest)
            move(src, dest);
    }

    void zeroExtend32ToPtr(RegisterID src, RegisterID dest)
    {
        if (src != dest)
            move(src, dest);
    }


    // Forwards / external control flow operations:
    //
    // This set of jump and conditional branch operations return a Jump
    // object which may linked at a later point, allow forwards jump,
    // or jumps that will require external linkage (after the code has been
    // relocated).
    //
    // For branches, signed <, >, <= and >= are denoted as l, g, le, and ge
    // respectively, for unsigned comparisons the names b, a, be, and ae are
    // used (representing the names 'below' and 'above').
    //
    // Currently, comparisons all emit to implied CR0. The methodjit isn't
    // designed around the POWER idea of multiple condregs, which is a shame
    // and a waste.

    Jump branch32(RelationalCondition cond, RegisterID left, RegisterID right)
    {
        // always 32-bit comparison, always CR0
        if (PPC_USE_UNSIGNED_COMPARE(cond)) {
            m_assembler.cmplw(left, right);
        } else {
            m_assembler.cmpw(left, right);
        }
        return Jump(m_assembler.m_branch(cond));
    }

    Jump branch32(RelationalCondition cond, RegisterID left, TrustedImm32 right)
    {
        if (PPC_USE_UNSIGNED_COMPARE(cond)) {
            compare32unsigned(left, right);
        } else {
            compare32(left, right);
        }
        return Jump(m_assembler.m_branch(cond));
    }

    Jump branch32(RelationalCondition cond, RegisterID left, Address right)
    {
        load32(right, tempRegister);
        return branch32(cond, left, tempRegister);
    }

    Jump branch32(RelationalCondition cond, Address left, RegisterID right)
    {
        load32(left, tempRegister);
        return branch32(cond, tempRegister, right);
    }

    Jump branch32(RelationalCondition cond, Address left, TrustedImm32 right)
    {
        // Use addressTempRegister: the branch32 we call might use
        // tempRegister.
        load32(left, addressTempRegister);
        return branch32(cond, addressTempRegister, right);
    }

    Jump branch32(RelationalCondition cond, BaseIndex left, TrustedImm32 right)
    {
        load32(left, addressTempRegister);
        return branch32(cond, addressTempRegister, right);
    }

    Jump branchPtr(RelationalCondition cond, BaseIndex left, RegisterID right)
    {
        load32(left, addressTempRegister);
        return branch32(cond, addressTempRegister, right);
    }

    Jump branch32WithUnalignedHalfWords(RelationalCondition cond, BaseIndex left, TrustedImm32 right)
    {
        // DON'T USE A TEMP REGISTER! load32WithUnalignedHalfWords needs them!
        // Instead, we will use our emergency scratch register.
        load32WithUnalignedHalfWords(left, emergencyTempRegister);
        return branch32(cond, emergencyTempRegister, right);
    }

    Jump branch32WithUnalignedHalfWords(RelationalCondition cond, Address left, TrustedImm32 right)
    {

        // this is basically
        //   load32WithUnalignedHalfWords(left, emergencyTempRegister);
        //   return branch32(cond, emergencyTempRegister, right);
        // but hand-written to software-pipeline the loads with the li32
    
        if (PPC_OFFS_OK(left) && PPC_OFFS_INCR_OK(left, 2)) {
            m_assembler.lhz(tempRegister, left.base, left.offset);
            m_assembler.lhz(emergencyTempRegister, left.base, left.offset + 2);
        } else {
            add32(TrustedImm32(left.offset), left.base, addressTempRegister);
            m_assembler.lhz(tempRegister, addressTempRegister, 0);
            m_assembler.lhz(emergencyTempRegister, addressTempRegister, 2);
        }
        
        // li the RHS while the loads are pending
        m_assembler.x_li32(addressTempRegister, right.m_value);
        
        m_assembler.x_insertbits0_15(emergencyTempRegister, tempRegister);
        return branch32(cond, emergencyTempRegister, addressTempRegister);

    }

    Jump branch32(RelationalCondition cond, AbsoluteAddress left, RegisterID right)
    {
        load32(left.m_ptr, tempRegister);
        return branch32(cond, tempRegister, right);
    }

    Jump branch32(RelationalCondition cond, AbsoluteAddress left, TrustedImm32 right)
    {
        load32(left.m_ptr, addressTempRegister);
        return branch32(cond, addressTempRegister, right);
    }

    Jump branch32FixedLength(RelationalCondition cond, RegisterID left, TrustedImm32 right)
    {
        // need to update ICLabels.cpp:INLINE_*_JUMP if this function changes

        // doesn't really need to be patchable, just fixed-length
        m_assembler.x_p_li32(tempRegister, right.m_value);  // 8 bytes
        if (PPC_USE_UNSIGNED_COMPARE(cond)) {
            m_assembler.cmplw(left, tempRegister);
        } else {
            m_assembler.cmpw(left, tempRegister);
        }
        return Jump(m_assembler.m_branch(cond));            // 12 bytes
    }
    
    Jump branch32WithPatch(RelationalCondition cond, RegisterID left, TrustedImm32 right, DataLabel32 &dataLabel)
    {
        dataLabel = DataLabel32(this);
        m_assembler.x_p_li32(tempRegister, right.m_value);
        return branch32(cond, left, tempRegister);
    }

    Jump branch16(RelationalCondition cond, BaseIndex left, RegisterID right)
    {
        load16(left, addressTempRegister);
        return branch32(cond, addressTempRegister, right);
    }

    Jump branch16(RelationalCondition cond, BaseIndex left, TrustedImm32 right)
    {
        load16(left, addressTempRegister);
        return branch32(cond, addressTempRegister, right);
    }

    Jump branch16(RelationalCondition cond, Address left, RegisterID right)
    {
        load16(left, addressTempRegister);
        return branch32(cond, addressTempRegister, right);
    }
        
    Jump branch16(RelationalCondition cond, Address left, TrustedImm32 right)
    {
        load16(left, addressTempRegister);
        return branch32(cond, addressTempRegister, right);
    }

    Jump branch8(RelationalCondition cond, RegisterID left, TrustedImm32 right)
    {
        if (PPC_USE_UNSIGNED_COMPARE(cond)) {
            compare32unsigned(left, right);
        } else {
            compare32(left, right);
        }
        return Jump(m_assembler.m_branch(cond));
    }

    Jump branch8(RelationalCondition cond, Address left, TrustedImm32 right)
    {
        load8(left, addressTempRegister);
        return branch8(cond, addressTempRegister, right);
    }

    Jump branchTest32(ResultCondition cond, RegisterID reg, RegisterID mask)
    {
        ASSERT((cond == Zero) || (cond == NonZero));
        // and_rc = "and." (i.e., with the Rc bit)
        m_assembler.and_rc(tempRegister, reg, mask);
        return Jump(m_assembler.m_branch(cond));
    }

    void test32(RegisterID reg, TrustedImm32 mask = TrustedImm32(-1))
    {
        if (mask.m_value == -1) {
            // testing every bit: no need for a mask
            m_assembler.and_rc(tempRegister, reg, reg);
        } else {
            if (PPC_IMM_OK_U(mask))
                m_assembler.andi_rc(tempRegister, reg, mask.m_value);
            else {
                m_assembler.x_li32(tempRegister, mask.m_value);
                m_assembler.and_rc(tempRegister, reg, tempRegister);
            }
        }
    }

    void test32(ResultCondition cond, RegisterID reg, TrustedImm32 mask = TrustedImm32(-1))
    {
        UNUSED_PARAM(cond);
        test32(reg, mask);
    }

    Jump branch(ResultCondition cond)
    {
        return Jump(m_assembler.m_branch(cond));
    }

    Jump branchTest32(ResultCondition cond, RegisterID reg, TrustedImm32 mask = TrustedImm32(-1))
    {
        ASSERT((cond == Zero) || (cond == NonZero));
        test32(reg, mask);
        return Jump(m_assembler.m_branch(cond));
    }

    Jump branchTest32(ResultCondition cond, Address address, TrustedImm32 mask = TrustedImm32(-1))
    {
        load32(address, addressTempRegister);
        return branchTest32(cond, addressTempRegister, mask);
    }

    Jump branchTest32(ResultCondition cond, BaseIndex address, TrustedImm32 mask = TrustedImm32(-1))
    {
        load32(address, addressTempRegister);
        return branchTest32(cond, addressTempRegister, mask);
    }

    Jump branchTest8(ResultCondition cond, BaseIndex address, TrustedImm32 mask = TrustedImm32(-1))
    {
        load8(address, addressTempRegister);
        return branchTest32(cond, addressTempRegister, mask);
    }

    Jump branchTest8(ResultCondition cond, RegisterID reg, TrustedImm32 mask = TrustedImm32(-1))
    {
        return branchTest32(cond, reg, mask);
    }

    Jump branchTest8(ResultCondition cond, Address address, TrustedImm32 mask = TrustedImm32(-1))
    {
        load8(address, addressTempRegister);
        return branchTest32(cond, addressTempRegister, mask);
    }
    Jump branchTest8(ResultCondition cond, AbsoluteAddress address, TrustedImm32 mask = TrustedImm32(-1))
    {
        #warning AmigaOS: need to move stuff when having absolute address (see in comment below)
        move(TrustedImmPtr(address.m_ptr), tempRegister);
        load8(tempRegister, addressTempRegister);
        return branchTest32(cond, addressTempRegister, mask);
    }
// From MacroAssemblerMIPS.h
//    Jump branchTest8(ResultCondition cond, AbsoluteAddress address, TrustedImm32 mask = TrustedImm32(-1))
//    {
//        move(TrustedImmPtr(address.m_ptr), ARMRegisters::S1);
//        load8(Address(ARMRegisters::S1), ARMRegisters::S1);
//        return branchTest32(cond, ARMRegisters::S1, mask);
//    }
    // JUMP!! 21 JUMP STREET!!

    Jump jump()
    {
        return Jump(m_assembler.m_jump());
    }

    void jump(RegisterID target)
    {
        // This combines the mtctr and bctr into one convenient instruction.
        // ("branch absolutely absolute")
        m_assembler.x_baa(target);
    }

    // Address is a memory location containing the address to jump to
    void jump(Address address)
    {
        load32(address, tempRegister);
        jump(tempRegister);
    }
    
    void jump(BaseIndex address)
    {
        load32(address, tempRegister);
        jump(tempRegister);
    }


    // Arithmetic control flow operations:
    //
    // This set of conditional branch operations branch based
    // on the result of an arithmetic operation.  The operation
    // is performed as normal, storing the result.
    //
    // These check overflow, so simply setting Rc is not enough; we must
    // use the XER as well as the CR.
    
    Jump branchAdd32(ResultCondition cond, Address address, RegisterID dest)
    {
        ASSERT((cond == Overflow) || (cond == Signed) || (cond == Zero) || (cond == NonZero));
        load32(address, addressTempRegister);
        return branchAdd32(cond, addressTempRegister, dest);
    }

    Jump branchAdd32(ResultCondition cond, RegisterID src, RegisterID dest)
    {
        ASSERT((cond == Overflow) || (cond == Signed) || (cond == Zero) || (cond == NonZero));
        m_assembler.addo_rc(dest, dest, src);
        // m_branch writes the special code for Overflow to get the XER OV
        // bit. Like the nanojit, SO is not useful to us because we'll spend
        // too much time clearing it for operations unlikely to set it.
        return Jump(m_assembler.m_branch(cond));
    }

    Jump branchAdd32(ResultCondition cond, TrustedImm32 imm, RegisterID dest)
    {
        return branchAdd32(cond, dest, imm, dest);
    }

    Jump branchAdd32(ResultCondition cond, RegisterID src, TrustedImm32 imm, RegisterID dest)
    {
        ASSERT((cond == Overflow) || (cond == Signed) || (cond == Zero) || (cond == NonZero));
        ASSERT(dest != tempRegister);
        ASSERT(src != tempRegister);
        // There is no PPC immediate add that sets CR and XER. We need both
        // because the compiler may test both types of conditions.
        m_assembler.x_li32(tempRegister, imm.m_value);
        m_assembler.addo_rc(dest, src, tempRegister);
        return Jump(m_assembler.m_branch(cond));
    }

    Jump branchMul32(ResultCondition cond, RegisterID src, RegisterID dest)
    {
        ASSERT(cond == Overflow);
        // We don't need the Rc bit; we only care if this overflowed.
        m_assembler.mullwo(dest, dest, src);
        return Jump(m_assembler.m_branch(cond));
    }

    Jump branchMul32(ResultCondition cond, TrustedImm32 imm, RegisterID src, RegisterID dest)
    {
        ASSERT(cond == Overflow);
        move(imm, tempRegister);
        m_assembler.mullwo(dest, tempRegister, src);
        return Jump(m_assembler.m_branch(cond));
    }

    Jump branchSub32(ResultCondition cond, RegisterID src, RegisterID dest)
    {
        ASSERT((cond == Overflow) || (cond == Signed) || (cond == Zero) || (cond == NonZero));
        m_assembler.subfo_rc(dest, src, dest);
        return Jump(m_assembler.m_branch(cond));
    }

    Jump branchSub32(ResultCondition cond, TrustedImm32 imm, RegisterID dest)
    {
        return branchSub32(cond, dest, imm, dest);
    }

    Jump branchSub32(ResultCondition cond, RegisterID src, TrustedImm32 imm, RegisterID dest)
    {
        ASSERT((cond == Overflow) || (cond == Signed) || (cond == Zero) || (cond == NonZero));
        ASSERT(dest != tempRegister);
        ASSERT(src != tempRegister);
        m_assembler.x_li32(tempRegister, imm.m_value);
        m_assembler.subfo_rc(dest, tempRegister, src);
        return Jump(m_assembler.m_branch(cond));
    }

    Jump branchSub32(ResultCondition cond, RegisterID src1, RegisterID src2, RegisterID dest)
    {
        ASSERT((cond == Overflow) || (cond == Signed) || (cond == Zero) || (cond == NonZero));
        ASSERT(dest != tempRegister);
        ASSERT(src1 != tempRegister);
        ASSERT(src2 != tempRegister);
        m_assembler.subfo_rc(dest, src2, src1);
        return Jump(m_assembler.m_branch(cond));
    }

    Jump branchSub32(ResultCondition cond, TrustedImm32 imm, Address dest)
    {
        // Yuck.
        ASSERT((cond == Overflow) || (cond == Signed) || (cond == Zero) || (cond == NonZero));

        load32(dest, tempRegister);
        move(imm, addressTempRegister);
        m_assembler.subfo_rc(tempRegister, addressTempRegister, tempRegister);
        store32(tempRegister, dest);
        return Jump(m_assembler.m_branch(cond));
    }

    Jump branchOr32(ResultCondition cond, RegisterID src, RegisterID dest)
    {
        m_assembler.or_rc(dest, src, dest);
        return Jump(m_assembler.m_branch(cond));
    }

#warning AmigaOS: what to do wiht these?
// These methods are undeffed in platform.h when not on PPC and !ARM_TRAD..
//// #if !CPU(ARM_TRADITIONAL)
    PatchableJump patchableJump()
    {
        return PatchableJump(jump());
    }

    PatchableJump patchableBranchTest32(ResultCondition cond, RegisterID reg, TrustedImm32 mask = TrustedImm32(-1))
    {
        return PatchableJump(branchTest32(cond, reg, mask));
    }

    PatchableJump patchableBranch32(RelationalCondition cond, RegisterID reg, TrustedImm32 imm)
    {
        return PatchableJump(branch32(cond, reg, imm));
    }

    PatchableJump patchableBranch32(RelationalCondition cond, Address address, TrustedImm32 imm)
    {
        return PatchableJump(branch32(cond, address, imm));
    }
//// #endif

    // Miscellaneous operations:
    // PPC-specific.
    void abortWithReason(AbortReason reason)
    {
        move(TrustedImm32(reason), tempRegister);
        breakpoint();
    }

    void breakpoint()
    {
        // OS X uses the trap instruction (trap being tw 31,r0,r0).
        // I don't know what Linux or AIX use. One way that will be
        // guaranteed to land you in the debugger is using 0x00000000
        // as an instruction (revenge of the 6502).
        m_assembler.x_trap();
    }

    // call()s are always patchable, since we reserve adequate space for
    // every call. If Mozilla starts actually generating nearCall()s, we
    // could take that as a cue to _not_ reserve trampoline space for
    // those calls on G3/G4. (G5 uses the full four words regardless.)
    Call nearCall()
    {
        // When we do, make this LinkableNear.
        return Call(m_assembler.m_call(), Call::Linkable);
    }

    Call call()
    {
        return Call(m_assembler.m_call(), Call::Linkable);
    }

    Call call(RegisterID target)
    {
        return Call(m_assembler.m_call_reg(target), Call::None);
    }

    Call call(Address address)
    {
        load32(address, tempRegister);
        return call(tempRegister);
    }

    void ret()
    {
        m_assembler.blr();
    }

    // PowerPC draws a distinction between signed and unsigned comparisons,
    // which is to say you have to explicitly indicate the type of comparison.
    // This is different than many other processors which will also set for
    // you an unsigned comparison condition code.
    //
    // Besides the obvious need to check the condition code, we must also
    // consider the situation of comparison against an immediate that would
    // ordinarily be sign-extended (such as cmplwi r0,65409 == ff81 == -127).
    // If r0 contained a sign-extended -127 (i.e., 0xffffff81), then they
    // would *not* be equal because the operand is not sign-extended by the
    // CPU prior to comparison. Here's one test case that fails if done wrong:
    //
    // var p = 0; if (typeof p != 'number') print(false);
    //
    // Look at the set32() after the stub call to typeof.

    // This first (and the default case) comparison is always signed.
    void compare32(RegisterID left, TrustedImm32 right)
    {
        if (PPC_IMM_OK_S(right)) {
            m_assembler.cmpwi(left, int16_t(right.m_value & 0xffff));
        } else {
            m_assembler.x_li32(tempRegister, right.m_value);
            m_assembler.cmpw(left, tempRegister);
        }
    }
    // This isn't. Hopefully the right thing is calling us.
    void compare32unsigned(RegisterID left, TrustedImm32 right)
    {
        if (PPC_IMM_OK_U(right)) {
            m_assembler.cmplwi(left, int16_t(right.m_value & 0xffff));
        } else {
            m_assembler.x_li32(tempRegister, right.m_value);
            m_assembler.cmplw(left, tempRegister);
        }
    }
        
    void compare32(RelationalCondition cond, RegisterID left, RegisterID right, RegisterID dest)
    {
        if (PPC_USE_UNSIGNED_COMPARE(cond)) {
            m_assembler.cmplw(left, right);
        } else {
            m_assembler.cmpw(left, right);
        }
        m_assembler.m_set_cond(dest, cond);
    }

    void compare32(RelationalCondition cond, RegisterID left, TrustedImm32 right, RegisterID dest)
    {
        if (PPC_USE_UNSIGNED_COMPARE(cond)) {
            compare32unsigned(left, right);
        } else {
            compare32(left, right);
        }
        m_assembler.m_set_cond(dest, cond);
    }

    void compare32(RelationalCondition cond, RegisterID left, Address right, RegisterID dest)
    {
        load32(right, tempRegister);
        compare32(cond, left, tempRegister, dest);
    }

    void compare32(RelationalCondition cond, Address leftAddr, TrustedImm32 right, RegisterID dest)
    {
        // use addressTempRegister for left, since compare32 may need tempRegister
        load32(leftAddr, addressTempRegister);
        if (PPC_USE_UNSIGNED_COMPARE(cond)) {
            compare32unsigned(addressTempRegister, right);
        } else {
            compare32(addressTempRegister, right);
        }
        m_assembler.m_set_cond(dest, cond);
    }

    void setTest32(ResultCondition cond, Address address, TrustedImm32 mask, RegisterID dest)
    {
        load32(address, tempRegister);
        if(PPC_IMM_OK_U(mask))
            m_assembler.andi_rc(tempRegister, tempRegister, mask.m_value);
        else {
            // use the destination register as scratch space before we set it
            m_assembler.x_li32(dest, mask.m_value);
            m_assembler.and_rc(tempRegister, dest, tempRegister);
        }
        m_assembler.m_set_cond(dest, cond);
    }

    void setTest8(ResultCondition cond, Address address, TrustedImm32 mask, RegisterID dest)
    {
        // We don't have byte registers, so like SPARC, treat as setTest32.
        setTest32(cond, address, mask, dest);
    }

    // These are patchable.
    DataLabel32 moveWithPatch(TrustedImm32 imm, RegisterID dest)
    {
        DataLabel32 dataLabel(this);
        m_assembler.x_p_li32(dest, imm.m_value);
        return dataLabel;
    }

    DataLabelPtr moveWithPatch(TrustedImmPtr imm, RegisterID dest)
    {
        DataLabelPtr dataLabel(this);
        TrustedImm32 mimm = TrustedImm32(imm);
        m_assembler.x_p_li32(dest, mimm.m_value);
        return dataLabel;
    }

    Jump branchPtrWithPatch(RelationalCondition cond, RegisterID left, DataLabelPtr& dataLabel, TrustedImmPtr initialRightValue = TrustedImmPtr(0))
    {
        dataLabel = moveWithPatch(initialRightValue, tempRegister);
        return branch32(cond, left, tempRegister);
    }

    Jump branchPtrWithPatch(RelationalCondition cond, Address left, DataLabelPtr& dataLabel, TrustedImmPtr initialRightValue = TrustedImmPtr(0))
    {
        load32(left, addressTempRegister);
        dataLabel = moveWithPatch(initialRightValue, tempRegister);
        return branch32(cond, addressTempRegister, tempRegister);
    }

    DataLabelPtr storePtrWithPatch(TrustedImmPtr initialValue, ImplicitAddress address)
    {
        DataLabelPtr label = moveWithPatch(initialValue, tempRegister);
        store32(tempRegister, address);
        return label;
    }
    
    DataLabelPtr storePtrWithPatch(ImplicitAddress address)
    {
        return storePtrWithPatch(TrustedImmPtr(0), address);
    }


    Call tailRecursiveCall()
    {
        // Like a normal call, but don't link.
        // XXX
        return Call::fromTailJump(jump());
    }

    Call makeTailRecursiveCall(Jump oldJump)
    {
        return Call::fromTailJump(oldJump);
    }

    PPCAssembler::Condition PPCCondition(RelationalCondition cond)
    {
        return static_cast<PPCAssembler::Condition>(cond);
    }

    PPCAssembler::Condition PPCCondition(ResultCondition cond)
    {
        return static_cast<PPCAssembler::Condition>(cond);
    }

    PPCAssembler::Condition PPCCondition(DoubleCondition cond)
    {
        return static_cast<PPCAssembler::Condition>(cond);
    }

    static void linkCall(void* code, Call call, FunctionPtr function)
    {
        PPCAssembler::linkCall(code, call.m_label, function.value());
    }

    static void repatchCall(CodeLocationCall call, CodeLocationLabel destination)
    {
        PPCAssembler::relinkCall(call.dataLocation(), destination.executableAddress());
    }

    static void repatchCall(CodeLocationCall call, FunctionPtr destination)
    {
        PPCAssembler::relinkCall(call.dataLocation(), destination.executableAddress());
    }

    // PPC-specific utility functions.

    // WARNINGWARNINGWARNINGWARNINGWARNINGGNINRAWGNINRAWGNINRAWGNINRAWGNINRAW
    // 
    // These sections of code are ABI dependent. If your PPC architecture is
    // not one of the supported ABIs, you'll need to add it, you lucky dog!
    //
    // WARNINGWARNINGWARNINGWARNINGWARNINGGNINRAWGNINRAWGNINRAWGNINRAWGNINRAW

#if OS(DARWIN)
    void genPrologue(uint32_t framesize) {
        // Assume OS X linkage area, and handle framesize > 32K.

        // Frames should be 16-byte aligned. Round up if we're short.
        if (framesize & 15)
            framesize = (((framesize >> 4) + 1) << 4);

        m_assembler.x_mflr(tempRegister);
        m_assembler.stw(tempRegister, stackPointerRegister, 8);
        m_assembler.mfcr(tempRegister);
        m_assembler.stw(tempRegister, stackPointerRegister, 4);
        m_assembler.x_li32(tempRegister, -(framesize));
        m_assembler.stwux(stackPointerRegister, stackPointerRegister,
            tempRegister);
    }
    void genEpilogue(uint32_t framesize) {
        // DOES NOT INCLUDE blr -- call a ret() if you want that too

        // Frames should be 16-byte aligned. Round up if we're short.
        if (framesize & 15)
            framesize = (((framesize >> 4) + 1) << 4);

        m_assembler.x_li32(tempRegister, framesize);
        m_assembler.add(stackPointerRegister, stackPointerRegister,
            tempRegister);
        m_assembler.lwz(tempRegister, stackPointerRegister, 4);
        m_assembler.x_mtcr(tempRegister);
        m_assembler.lwz(tempRegister, stackPointerRegister, 8);
        m_assembler.x_mtlr(tempRegister);
    }

    // Support routines so that we can enforce certain blocks of code
    // to be allocated together
    void ensureSpace(int bytes)
    {
        m_assembler.ensureSpace(bytes);
    }
    
#if 0
    int flushCount()
    {
        return m_assembler.flushCount();
    }
#endif
    
    void forceFlushConstantPool()
    {
        m_assembler.forceFlushConstantPool();
    }

    void finalizeAbsoluteJumps(void *finalCodeAddr)
    {
        m_assembler.finalizeAbsoluteJumps(finalCodeAddr);
    }

#ifdef DEBUG
private:
    // This family of functions checks to see if an address is stack pointer
    // relative. If it is, ensure we are not writing into the linkage area,
    // because other than certain specific operations that should NEVER happen.
    void checkStackPointer(Address a) {
        if(a.base != stackPointerRegister) return;
        ASSERT(a.offset >= 24);
    }
    void checkStackPointer(ImplicitAddress a) {
        if(a.base != stackPointerRegister) return;
        ASSERT(a.offset >= 24);
    }
    void checkStackPointer(BaseIndex a) {
        if(a.base != stackPointerRegister) return;
        ASSERT(((a.index << a.scale) + a.offset) >= 24);
    }
#endif

#else
#warning AmigaOS: You lucky dog, you are on an unsupported PowerPC ABI. Add support!
#warning AmigaOS: Don't forget to do this for push and pop while you're at it.
    void genPrologue(uint32_t framesize) {
        // Assume OS X linkage area, and handle framesize > 32K.

        // Frames should be 16-byte aligned. Round up if we're short.
        if (framesize & 15)
            framesize = (((framesize >> 4) + 1) << 4);

        m_assembler.x_mflr(tempRegister);
        m_assembler.stw(tempRegister, stackPointerRegister, 8);
        m_assembler.mfcr(tempRegister);
        m_assembler.stw(tempRegister, stackPointerRegister, 4);
        m_assembler.x_li32(tempRegister, -(framesize));
        m_assembler.stwux(stackPointerRegister, stackPointerRegister,
            tempRegister);
    }
    void genEpilogue(uint32_t framesize) {
        // DOES NOT INCLUDE blr -- call a ret() if you want that too

        // Frames should be 16-byte aligned. Round up if we're short.
        if (framesize & 15)
            framesize = (((framesize >> 4) + 1) << 4);

        m_assembler.x_li32(tempRegister, framesize);
        m_assembler.add(stackPointerRegister, stackPointerRegister,
            tempRegister);
        m_assembler.lwz(tempRegister, stackPointerRegister, 4);
        m_assembler.x_mtcr(tempRegister);
        m_assembler.lwz(tempRegister, stackPointerRegister, 8);
        m_assembler.x_mtlr(tempRegister);
    }

    // Support routines so that we can enforce certain blocks of code
    // to be allocated together
    void ensureSpace(int bytes)
    {
        m_assembler.ensureSpace(bytes);
    }
    
#if 0
    int flushCount()
    {
        return m_assembler.flushCount();
    }
#endif
    
    void forceFlushConstantPool()
    {
        m_assembler.forceFlushConstantPool();
    }

    void finalizeAbsoluteJumps(void *finalCodeAddr)
    {
        m_assembler.finalizeAbsoluteJumps(finalCodeAddr);
    }

#ifdef DEBUG
private:
    // This family of functions checks to see if an address is stack pointer
    // relative. If it is, ensure we are not writing into the linkage area,
    // because other than certain specific operations that should NEVER happen.
    void checkStackPointer(Address a) {
        if(a.base != stackPointerRegister) return;
        ASSERT(a.offset >= 24);
    }
    void checkStackPointer(ImplicitAddress a) {
        if(a.base != stackPointerRegister) return;
        ASSERT(a.offset >= 24);
    }
    void checkStackPointer(BaseIndex a) {
        if(a.base != stackPointerRegister) return;
        ASSERT(((a.index << a.scale) + a.offset) >= 24);
    }
#endif

#endif

};

} // namespace JSC

#endif // ENABLE(ASSEMBLER)

#endif // MacroAssemblerPPC_h

