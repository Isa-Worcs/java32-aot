/*
 * Copyright (c) 2009, 2016, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
package org.graalvm.compiler.asm.i386;

import static jdk.vm.ci.i386.I386.CPU;
import static jdk.vm.ci.i386.I386.XMM;
import static jdk.vm.ci.i386.I386.r12;
import static jdk.vm.ci.i386.I386.r13;
import static jdk.vm.ci.i386.I386.rbp;
import static jdk.vm.ci.i386.I386.rip;
import static jdk.vm.ci.i386.I386.rsp;
import static jdk.vm.ci.code.MemoryBarriers.STORE_LOAD;
import static org.graalvm.compiler.asm.i386.I386AsmOptions.UseAddressNop;
import static org.graalvm.compiler.asm.i386.I386AsmOptions.UseNormalNop;
import static org.graalvm.compiler.asm.i386.I386Assembler.I386BinaryArithmetic.ADD;
import static org.graalvm.compiler.asm.i386.I386Assembler.I386BinaryArithmetic.AND;
import static org.graalvm.compiler.asm.i386.I386Assembler.I386BinaryArithmetic.CMP;
import static org.graalvm.compiler.asm.i386.I386Assembler.I386BinaryArithmetic.OR;
import static org.graalvm.compiler.asm.i386.I386Assembler.I386BinaryArithmetic.SBB;
import static org.graalvm.compiler.asm.i386.I386Assembler.I386BinaryArithmetic.SUB;
import static org.graalvm.compiler.asm.i386.I386Assembler.I386BinaryArithmetic.XOR;
import static org.graalvm.compiler.asm.i386.I386Assembler.I386MOp.DEC;
import static org.graalvm.compiler.asm.i386.I386Assembler.I386MOp.INC;
import static org.graalvm.compiler.asm.i386.I386Assembler.I386MOp.NEG;
import static org.graalvm.compiler.asm.i386.I386Assembler.I386MOp.NOT;
import static org.graalvm.compiler.asm.i386.I386Assembler.OperandSize.BYTE;
import static org.graalvm.compiler.asm.i386.I386Assembler.OperandSize.DWORD;
import static org.graalvm.compiler.asm.i386.I386Assembler.OperandSize.PD;
import static org.graalvm.compiler.asm.i386.I386Assembler.OperandSize.PS;
import static org.graalvm.compiler.asm.i386.I386Assembler.OperandSize.QWORD;
import static org.graalvm.compiler.asm.i386.I386Assembler.OperandSize.SD;
import static org.graalvm.compiler.asm.i386.I386Assembler.OperandSize.SS;
import static org.graalvm.compiler.asm.i386.I386Assembler.OperandSize.WORD;
import static org.graalvm.compiler.core.common.NumUtil.isByte;
import static org.graalvm.compiler.core.common.NumUtil.isInt;
import static org.graalvm.compiler.core.common.NumUtil.isShiftCount;
import static org.graalvm.compiler.core.common.NumUtil.isUByte;

import org.graalvm.compiler.asm.Assembler;
import org.graalvm.compiler.asm.Label;
import org.graalvm.compiler.asm.i386.I386Address.Scale;
import org.graalvm.compiler.core.common.NumUtil;
import org.graalvm.compiler.debug.GraalError;

import jdk.vm.ci.i386.I386;
import jdk.vm.ci.i386.I386.CPUFeature;
import jdk.vm.ci.i386.I386Kind;
import jdk.vm.ci.code.Register;
import jdk.vm.ci.code.Register.RegisterCategory;
import jdk.vm.ci.code.TargetDescription;
import jdk.vm.ci.meta.PlatformKind;

/**
 * This class implements an assembler that can encode most X86 instructions.
 */
public class I386Assembler extends Assembler {

    private static final int MinEncodingNeedsRex = 8;

    /**
     * The x86 condition codes used for conditional jumps/moves.
     */
    public enum ConditionFlag {
        Zero(0x4, "|zero|"),
        NotZero(0x5, "|nzero|"),
        Equal(0x4, "="),
        NotEqual(0x5, "!="),
        Less(0xc, "<"),
        LessEqual(0xe, "<="),
        Greater(0xf, ">"),
        GreaterEqual(0xd, ">="),
        Below(0x2, "|<|"),
        BelowEqual(0x6, "|<=|"),
        Above(0x7, "|>|"),
        AboveEqual(0x3, "|>=|"),
        Overflow(0x0, "|of|"),
        NoOverflow(0x1, "|nof|"),
        CarrySet(0x2, "|carry|"),
        CarryClear(0x3, "|ncarry|"),
        Negative(0x8, "|neg|"),
        Positive(0x9, "|pos|"),
        Parity(0xa, "|par|"),
        NoParity(0xb, "|npar|");

        private final int value;
        private final String operator;

        ConditionFlag(int value, String operator) {
            this.value = value;
            this.operator = operator;
        }

        public ConditionFlag negate() {
            switch (this) {
                case Zero:
                    return NotZero;
                case NotZero:
                    return Zero;
                case Equal:
                    return NotEqual;
                case NotEqual:
                    return Equal;
                case Less:
                    return GreaterEqual;
                case LessEqual:
                    return Greater;
                case Greater:
                    return LessEqual;
                case GreaterEqual:
                    return Less;
                case Below:
                    return AboveEqual;
                case BelowEqual:
                    return Above;
                case Above:
                    return BelowEqual;
                case AboveEqual:
                    return Below;
                case Overflow:
                    return NoOverflow;
                case NoOverflow:
                    return Overflow;
                case CarrySet:
                    return CarryClear;
                case CarryClear:
                    return CarrySet;
                case Negative:
                    return Positive;
                case Positive:
                    return Negative;
                case Parity:
                    return NoParity;
                case NoParity:
                    return Parity;
            }
            throw new IllegalArgumentException();
        }

        public int getValue() {
            return value;
        }

        @Override
        public String toString() {
            return operator;
        }
    }

    /**
     * Constants for X86 prefix bytes.
     */
    private static class Prefix {
        private static final int REX = 0x40;
        private static final int REXB = 0x41;
        private static final int REXX = 0x42;
        private static final int REXXB = 0x43;
        private static final int REXR = 0x44;
        private static final int REXRB = 0x45;
        private static final int REXRX = 0x46;
        private static final int REXRXB = 0x47;
        private static final int REXW = 0x48;
        private static final int REXWB = 0x49;
        private static final int REXWX = 0x4A;
        private static final int REXWXB = 0x4B;
        private static final int REXWR = 0x4C;
        private static final int REXWRB = 0x4D;
        private static final int REXWRX = 0x4E;
        private static final int REXWRXB = 0x4F;
        private static final int VEX_3BYTES = 0xC4;
        private static final int VEX_2BYTES = 0xC5;
    }

    private static class VexPrefix {
        private static final int VEX_R = 0x80;
        private static final int VEX_W = 0x80;
    }

    private static class VexSimdPrefix {
        private static final int VEX_SIMD_NONE = 0x0;
        private static final int VEX_SIMD_66 = 0x1;
        private static final int VEX_SIMD_F3 = 0x2;
        private static final int VEX_SIMD_F2 = 0x3;
    }

    private static class VexOpcode {
        private static final int VEX_OPCODE_NONE = 0x0;
        private static final int VEX_OPCODE_0F = 0x1;
        private static final int VEX_OPCODE_0F_38 = 0x2;
        private static final int VEX_OPCODE_0F_3A = 0x3;
    }

    public static class AvxVectorLen {
        public static final int AVX_128bit = 0x0;
        public static final int AVX_256bit = 0x1;
        public static final int AVX_512bit = 0x2;
        public static final int AVX_NoVec = 0x4;
    }

    public static class EvexTupleType {
        public static final int EVEX_FV = 0;
        public static final int EVEX_HV = 4;
        public static final int EVEX_FVM = 6;
        public static final int EVEX_T1S = 7;
        public static final int EVEX_T1F = 11;
        public static final int EVEX_T2 = 13;
        public static final int EVEX_T4 = 15;
        public static final int EVEX_T8 = 17;
        public static final int EVEX_HVM = 18;
        public static final int EVEX_QVM = 19;
        public static final int EVEX_OVM = 20;
        public static final int EVEX_M128 = 21;
        public static final int EVEX_DUP = 22;
        public static final int EVEX_ETUP = 23;
    }

    public static class EvexInputSizeInBits {
        public static final int EVEX_8bit = 0;
        public static final int EVEX_16bit = 1;
        public static final int EVEX_32bit = 2;
        public static final int EVEX_64bit = 3;
        public static final int EVEX_NObit = 4;
    }

    private I386InstructionAttr curAttributes;

    I386InstructionAttr getCurAttributes() {
        return curAttributes;
    }

    void setCurAttributes(I386InstructionAttr attributes) {
        curAttributes = attributes;
    }

    /**
     * The x86 operand sizes.
     */
    public enum OperandSize {
        BYTE(1, I386Kind.BYTE) {
            @Override
            protected void emitImmediate(I386Assembler asm, int imm) {
                assert imm == (byte) imm;
                asm.emitByte(imm);
            }

            @Override
            protected int immediateSize() {
                return 1;
            }
        },

        WORD(2, I386Kind.WORD, 0x66) {
            @Override
            protected void emitImmediate(I386Assembler asm, int imm) {
                assert imm == (short) imm;
                asm.emitShort(imm);
            }

            @Override
            protected int immediateSize() {
                return 2;
            }
        },

        DWORD(4, I386Kind.DWORD) {
            @Override
            protected void emitImmediate(I386Assembler asm, int imm) {
                asm.emitInt(imm);
            }

            @Override
            protected int immediateSize() {
                return 4;
            }
        },

        QWORD(8, I386Kind.QWORD) {
            @Override
            protected void emitImmediate(I386Assembler asm, int imm) {
                asm.emitInt(imm);
            }

            @Override
            protected int immediateSize() {
                return 4;
            }
        },

        SS(4, I386Kind.SINGLE, 0xF3, true),

        SD(8, I386Kind.DOUBLE, 0xF2, true),

        PS(16, I386Kind.V128_SINGLE, true),

        PD(16, I386Kind.V128_DOUBLE, 0x66, true);

        private final int sizePrefix;
        private final int bytes;
        private final boolean xmm;
        private final I386Kind kind;

        OperandSize(int bytes, I386Kind kind) {
            this(bytes, kind, 0);
        }

        OperandSize(int bytes, I386Kind kind, int sizePrefix) {
            this(bytes, kind, sizePrefix, false);
        }

        OperandSize(int bytes, I386Kind kind, boolean xmm) {
            this(bytes, kind, 0, xmm);
        }

        OperandSize(int bytes, I386Kind kind, int sizePrefix, boolean xmm) {
            this.sizePrefix = sizePrefix;
            this.bytes = bytes;
            this.kind = kind;
            this.xmm = xmm;
        }

        public int getBytes() {
            return bytes;
        }

        public boolean isXmmType() {
            return xmm;
        }

        public I386Kind getKind() {
            return kind;
        }

        public static OperandSize get(PlatformKind kind) {
            for (OperandSize operandSize : OperandSize.values()) {
                if (operandSize.kind.equals(kind)) {
                    return operandSize;
                }
            }
            throw GraalError.shouldNotReachHere("Unexpected kind: " + kind.toString());
        }

        /**
         * Emit an immediate of this size. Note that immediate {@link #QWORD} operands are encoded
         * as sign-extended 32-bit values.
         *
         * @param asm
         * @param imm
         */
        protected void emitImmediate(I386Assembler asm, int imm) {
            throw new UnsupportedOperationException();
        }

        protected int immediateSize() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * Operand size and register type constraints.
     */
    private enum OpAssertion {
        ByteAssertion(CPU, CPU, BYTE),
        ByteOrLargerAssertion(CPU, CPU, BYTE, WORD, DWORD, QWORD),
        WordOrLargerAssertion(CPU, CPU, WORD, DWORD, QWORD),
        DwordOrLargerAssertion(CPU, CPU, DWORD, QWORD),
        WordOrDwordAssertion(CPU, CPU, WORD, QWORD),
        QwordAssertion(CPU, CPU, QWORD),
        FloatAssertion(XMM, XMM, SS, SD, PS, PD),
        PackedFloatAssertion(XMM, XMM, PS, PD),
        SingleAssertion(XMM, XMM, SS),
        DoubleAssertion(XMM, XMM, SD),
        PackedDoubleAssertion(XMM, XMM, PD),
        IntToFloatAssertion(XMM, CPU, DWORD, QWORD),
        FloatToIntAssertion(CPU, XMM, DWORD, QWORD);

        private final RegisterCategory resultCategory;
        private final RegisterCategory inputCategory;
        private final OperandSize[] allowedSizes;

        OpAssertion(RegisterCategory resultCategory, RegisterCategory inputCategory, OperandSize... allowedSizes) {
            this.resultCategory = resultCategory;
            this.inputCategory = inputCategory;
            this.allowedSizes = allowedSizes;
        }

        protected boolean checkOperands(I386Op op, OperandSize size, Register resultReg, Register inputReg) {
            assert resultReg == null || resultCategory.equals(resultReg.getRegisterCategory()) : "invalid result register " + resultReg + " used in " + op;
            assert inputReg == null || inputCategory.equals(inputReg.getRegisterCategory()) : "invalid input register " + inputReg + " used in " + op;

            for (OperandSize s : allowedSizes) {
                if (size == s) {
                    return true;
                }
            }

            assert false : "invalid operand size " + size + " used in " + op;
            return false;
        }
    }

    public abstract static class OperandDataAnnotation extends CodeAnnotation {
        /**
         * The position (bytes from the beginning of the method) of the operand.
         */
        public final int operandPosition;
        /**
         * The size of the operand, in bytes.
         */
        public final int operandSize;
        /**
         * The position (bytes from the beginning of the method) of the next instruction. On I386,
         * RIP-relative operands are relative to this position.
         */
        public final int nextInstructionPosition;

        OperandDataAnnotation(int instructionPosition, int operandPosition, int operandSize, int nextInstructionPosition) {
            super(instructionPosition);

            this.operandPosition = operandPosition;
            this.operandSize = operandSize;
            this.nextInstructionPosition = nextInstructionPosition;
        }

        @Override
        public String toString() {
            return getClass().getSimpleName() + " instruction [" + instructionPosition + ", " + nextInstructionPosition + "[ operand at " + operandPosition + " size " + operandSize;
        }
    }

    /**
     * Annotation that stores additional information about the displacement of a
     * {@link Assembler#getPlaceholder placeholder address} that needs patching.
     */
    public static class AddressDisplacementAnnotation extends OperandDataAnnotation {
        AddressDisplacementAnnotation(int instructionPosition, int operandPosition, int operndSize, int nextInstructionPosition) {
            super(instructionPosition, operandPosition, operndSize, nextInstructionPosition);
        }
    }

    /**
     * Annotation that stores additional information about the immediate operand, e.g., of a call
     * instruction, that needs patching.
     */
    public static class ImmediateOperandAnnotation extends OperandDataAnnotation {
        ImmediateOperandAnnotation(int instructionPosition, int operandPosition, int operndSize, int nextInstructionPosition) {
            super(instructionPosition, operandPosition, operndSize, nextInstructionPosition);
        }
    }

    /**
     * Constructs an assembler for the I386 architecture.
     */
    public I386Assembler(TargetDescription target) {
        super(target);
    }

    public boolean supports(CPUFeature feature) {
        return ((I386) target.arch).getFeatures().contains(feature);
    }

    private static int encode(Register r) {
        assert r.encoding < 16 && r.encoding >= 0 : "encoding out of range: " + r.encoding;
        return r.encoding & 0x7;
    }

    /**
     * Get RXB bits for register-register instruction. In that encoding, ModRM.rm contains a
     * register index. The R bit extends the ModRM.reg field and the B bit extends the ModRM.rm
     * field. The X bit must be 0.
     */
    protected static int getRXB(Register reg, Register rm) {
        int rxb = (reg == null ? 0 : reg.encoding & 0x08) >> 1;
        rxb |= (rm == null ? 0 : rm.encoding & 0x08) >> 3;
        return rxb;
    }

    /**
     * Get RXB bits for register-memory instruction. The R bit extends the ModRM.reg field. There
     * are two cases for the memory operand:<br>
     * ModRM.rm contains the base register: In that case, B extends the ModRM.rm field and X = 0.
     * <br>
     * There is an SIB byte: In that case, X extends SIB.index and B extends SIB.base.
     */
    protected static int getRXB(Register reg, I386Address rm) {
        int rxb = (reg == null ? 0 : reg.encoding & 0x08) >> 1;
        if (!rm.getIndex().equals(Register.None)) {
            rxb |= (rm.getIndex().encoding & 0x08) >> 2;
        }
        if (!rm.getBase().equals(Register.None)) {
            rxb |= (rm.getBase().encoding & 0x08) >> 3;
        }
        return rxb;
    }

    /**
     * Emit the ModR/M byte for one register operand and an opcode extension in the R field.
     * <p>
     * Format: [ 11 reg r/m ]
     */
    protected void emitModRM(int reg, Register rm) {
        assert (reg & 0x07) == reg;
        emitByte(0xC0 | (reg << 3) | (rm.encoding & 0x07));
    }

    /**
     * Emit the ModR/M byte for two register operands.
     * <p>
     * Format: [ 11 reg r/m ]
     */
    protected void emitModRM(Register reg, Register rm) {
        emitModRM(reg.encoding & 0x07, rm);
    }

    protected void emitOperandHelper(Register reg, I386Address addr, int additionalInstructionSize) {
        assert !reg.equals(Register.None);
        emitOperandHelper(encode(reg), addr, false, additionalInstructionSize);
    }

    /**
     * Emits the ModR/M byte and optionally the SIB byte for one register and one memory operand.
     *
     * @param force4Byte use 4 byte encoding for displacements that would normally fit in a byte
     */
    protected void emitOperandHelper(Register reg, I386Address addr, boolean force4Byte, int additionalInstructionSize) {
        assert !reg.equals(Register.None);
        emitOperandHelper(encode(reg), addr, force4Byte, additionalInstructionSize);
    }

    protected void emitOperandHelper(int reg, I386Address addr, int additionalInstructionSize) {
        emitOperandHelper(reg, addr, false, additionalInstructionSize);
    }

    /**
     * Emits the ModR/M byte and optionally the SIB byte for one memory operand and an opcode
     * extension in the R field.
     *
     * @param force4Byte use 4 byte encoding for displacements that would normally fit in a byte
     * @param additionalInstructionSize the number of bytes that will be emitted after the operand,
     *            so that the start position of the next instruction can be computed even though
     *            this instruction has not been completely emitted yet.
     */
    protected void emitOperandHelper(int reg, I386Address addr, boolean force4Byte, int additionalInstructionSize) {
        assert (reg & 0x07) == reg;
        int regenc = reg << 3;

        Register base = addr.getBase();
        Register index = addr.getIndex();

        I386Address.Scale scale = addr.getScale();
        int disp = addr.getDisplacement();

        if (base.equals(I386.rip)) { // also matches addresses returned by getPlaceholder()
            // [00 000 101] disp32
            assert index.equals(Register.None) : "cannot use RIP relative addressing with index register";
            emitByte(0x05 | regenc);
            if (codePatchingAnnotationConsumer != null && addr.instructionStartPosition >= 0) {
                codePatchingAnnotationConsumer.accept(new AddressDisplacementAnnotation(addr.instructionStartPosition, position(), 4, position() + 4 + additionalInstructionSize));
            }
            emitInt(disp);
        } else if (base.isValid()) {
            int baseenc = base.isValid() ? encode(base) : 0;
            if (index.isValid()) {
                int indexenc = encode(index) << 3;
                // [base + indexscale + disp]
                if (disp == 0 && !base.equals(rbp) && !base.equals(r13)) {
                    // [base + indexscale]
                    // [00 reg 100][ss index base]
                    assert !index.equals(rsp) : "illegal addressing mode";
                    emitByte(0x04 | regenc);
                    emitByte(scale.log2 << 6 | indexenc | baseenc);
                } else if (isByte(disp) && !force4Byte) {
                    // [base + indexscale + imm8]
                    // [01 reg 100][ss index base] imm8
                    assert !index.equals(rsp) : "illegal addressing mode";
                    emitByte(0x44 | regenc);
                    emitByte(scale.log2 << 6 | indexenc | baseenc);
                    emitByte(disp & 0xFF);
                } else {
                    // [base + indexscale + disp32]
                    // [10 reg 100][ss index base] disp32
                    assert !index.equals(rsp) : "illegal addressing mode";
                    emitByte(0x84 | regenc);
                    emitByte(scale.log2 << 6 | indexenc | baseenc);
                    emitInt(disp);
                }
            } else if (base.equals(rsp) || base.equals(r12)) {
                // [rsp + disp]
                if (disp == 0) {
                    // [rsp]
                    // [00 reg 100][00 100 100]
                    emitByte(0x04 | regenc);
                    emitByte(0x24);
                } else if (isByte(disp) && !force4Byte) {
                    // [rsp + imm8]
                    // [01 reg 100][00 100 100] disp8
                    emitByte(0x44 | regenc);
                    emitByte(0x24);
                    emitByte(disp & 0xFF);
                } else {
                    // [rsp + imm32]
                    // [10 reg 100][00 100 100] disp32
                    emitByte(0x84 | regenc);
                    emitByte(0x24);
                    emitInt(disp);
                }
            } else {
                // [base + disp]
                assert !base.equals(rsp) && !base.equals(r12) : "illegal addressing mode";
                if (disp == 0 && !base.equals(rbp) && !base.equals(r13)) {
                    // [base]
                    // [00 reg base]
                    emitByte(0x00 | regenc | baseenc);
                } else if (isByte(disp) && !force4Byte) {
                    // [base + disp8]
                    // [01 reg base] disp8
                    emitByte(0x40 | regenc | baseenc);
                    emitByte(disp & 0xFF);
                } else {
                    // [base + disp32]
                    // [10 reg base] disp32
                    emitByte(0x80 | regenc | baseenc);
                    emitInt(disp);
                }
            }
        } else {
            if (index.isValid()) {
                int indexenc = encode(index) << 3;
                // [indexscale + disp]
                // [00 reg 100][ss index 101] disp32
                assert !index.equals(rsp) : "illegal addressing mode";
                emitByte(0x04 | regenc);
                emitByte(scale.log2 << 6 | indexenc | 0x05);
                emitInt(disp);
            } else {
                // [disp] ABSOLUTE
                // [00 reg 100][00 100 101] disp32
                emitByte(0x04 | regenc);
                emitByte(0x25);
                emitInt(disp);
            }
        }
        setCurAttributes(null);
    }

    /**
     * Base class for I386 opcodes.
     */
    public static class I386Op {

        protected static final int P_0F = 0x0F;
        protected static final int P_0F38 = 0x380F;
        protected static final int P_0F3A = 0x3A0F;

        private final String opcode;

        protected final int prefix1;
        protected final int prefix2;
        protected final int op;

        private final boolean dstIsByte;
        private final boolean srcIsByte;

        private final OpAssertion assertion;
        private final CPUFeature feature;

        protected I386Op(String opcode, int prefix1, int prefix2, int op, OpAssertion assertion, CPUFeature feature) {
            this(opcode, prefix1, prefix2, op, assertion == OpAssertion.ByteAssertion, assertion == OpAssertion.ByteAssertion, assertion, feature);
        }

        protected I386Op(String opcode, int prefix1, int prefix2, int op, boolean dstIsByte, boolean srcIsByte, OpAssertion assertion, CPUFeature feature) {
            this.opcode = opcode;
            this.prefix1 = prefix1;
            this.prefix2 = prefix2;
            this.op = op;

            this.dstIsByte = dstIsByte;
            this.srcIsByte = srcIsByte;

            this.assertion = assertion;
            this.feature = feature;
        }

        protected final void emitOpcode(I386Assembler asm, OperandSize size, int rxb, int dstEnc, int srcEnc) {
            if (prefix1 != 0) {
                asm.emitByte(prefix1);
            }
            if (size.sizePrefix != 0) {
                asm.emitByte(size.sizePrefix);
            }
            int rexPrefix = 0x40 | rxb;
            if (size == QWORD) {
                rexPrefix |= 0x08;
            }
            if (rexPrefix != 0x40 || (dstIsByte && dstEnc >= 4) || (srcIsByte && srcEnc >= 4)) {
                asm.emitByte(rexPrefix);
            }
            if (prefix2 > 0xFF) {
                asm.emitShort(prefix2);
            } else if (prefix2 > 0) {
                asm.emitByte(prefix2);
            }
            asm.emitByte(op);
        }

        protected final boolean verify(I386Assembler asm, OperandSize size, Register resultReg, Register inputReg) {
            assert feature == null || asm.supports(feature) : String.format("unsupported feature %s required for %s", feature, opcode);
            assert assertion.checkOperands(this, size, resultReg, inputReg);
            return true;
        }

        @Override
        public String toString() {
            return opcode;
        }
    }

    /**
     * Base class for I386 opcodes with immediate operands.
     */
    public static class I386ImmOp extends I386Op {

        private final boolean immIsByte;

        protected I386ImmOp(String opcode, boolean immIsByte, int prefix, int op, OpAssertion assertion) {
            super(opcode, 0, prefix, op, assertion, null);
            this.immIsByte = immIsByte;
        }

        protected final void emitImmediate(I386Assembler asm, OperandSize size, int imm) {
            if (immIsByte) {
                assert imm == (byte) imm;
                asm.emitByte(imm);
            } else {
                size.emitImmediate(asm, imm);
            }
        }

        protected final int immediateSize(OperandSize size) {
            if (immIsByte) {
                return 1;
            } else {
                return size.bytes;
            }
        }
    }

    /**
     * Opcode with operand order of either RM or MR for 2 address forms.
     */
    public abstract static class I386RROp extends I386Op {

        protected I386RROp(String opcode, int prefix1, int prefix2, int op, OpAssertion assertion, CPUFeature feature) {
            super(opcode, prefix1, prefix2, op, assertion, feature);
        }

        protected I386RROp(String opcode, int prefix1, int prefix2, int op, boolean dstIsByte, boolean srcIsByte, OpAssertion assertion, CPUFeature feature) {
            super(opcode, prefix1, prefix2, op, dstIsByte, srcIsByte, assertion, feature);
        }

        public abstract void emit(I386Assembler asm, OperandSize size, Register dst, Register src);
    }

    /**
     * Opcode with operand order of either RM or MR for 3 address forms.
     */
    public abstract static class I386RRROp extends I386Op {

        protected I386RRROp(String opcode, int prefix1, int prefix2, int op, OpAssertion assertion, CPUFeature feature) {
            super(opcode, prefix1, prefix2, op, assertion, feature);
        }

        protected I386RRROp(String opcode, int prefix1, int prefix2, int op, boolean dstIsByte, boolean srcIsByte, OpAssertion assertion, CPUFeature feature) {
            super(opcode, prefix1, prefix2, op, dstIsByte, srcIsByte, assertion, feature);
        }

        public abstract void emit(I386Assembler asm, OperandSize size, Register dst, Register nds, Register src);
    }

    /**
     * Opcode with operand order of RM.
     */
    public static class I386RMOp extends I386RROp {
        // @formatter:off
        public static final I386RMOp IMUL   = new I386RMOp("IMUL",         P_0F, 0xAF, OpAssertion.ByteOrLargerAssertion);
        public static final I386RMOp BSF    = new I386RMOp("BSF",          P_0F, 0xBC);
        public static final I386RMOp BSR    = new I386RMOp("BSR",          P_0F, 0xBD);
        public static final I386RMOp POPCNT = new I386RMOp("POPCNT", 0xF3, P_0F, 0xB8, CPUFeature.POPCNT);
        public static final I386RMOp TZCNT  = new I386RMOp("TZCNT",  0xF3, P_0F, 0xBC, CPUFeature.BMI1);
        public static final I386RMOp LZCNT  = new I386RMOp("LZCNT",  0xF3, P_0F, 0xBD, CPUFeature.LZCNT);
        public static final I386RMOp MOVZXB = new I386RMOp("MOVZXB",       P_0F, 0xB6, false, true, OpAssertion.WordOrLargerAssertion);
        public static final I386RMOp MOVZX  = new I386RMOp("MOVZX",        P_0F, 0xB7, OpAssertion.DwordOrLargerAssertion);
        public static final I386RMOp MOVSXB = new I386RMOp("MOVSXB",       P_0F, 0xBE, false, true, OpAssertion.WordOrLargerAssertion);
        public static final I386RMOp MOVSX  = new I386RMOp("MOVSX",        P_0F, 0xBF, OpAssertion.DwordOrLargerAssertion);
        public static final I386RMOp MOVSXD = new I386RMOp("MOVSXD",             0x63, OpAssertion.QwordAssertion);
        public static final I386RMOp MOVB   = new I386RMOp("MOVB",               0x8A, OpAssertion.ByteAssertion);
        public static final I386RMOp MOV    = new I386RMOp("MOV",                0x8B);

        // MOVD/MOVQ and MOVSS/MOVSD are the same opcode, just with different operand size prefix
        public static final I386RMOp MOVD   = new I386RMOp("MOVD",   0x66, P_0F, 0x6E, OpAssertion.IntToFloatAssertion, CPUFeature.SSE2);
        public static final I386RMOp MOVQ   = new I386RMOp("MOVQ",   0x66, P_0F, 0x6E, OpAssertion.IntToFloatAssertion, CPUFeature.SSE2);
        public static final I386RMOp MOVSS  = new I386RMOp("MOVSS",        P_0F, 0x10, OpAssertion.FloatAssertion, CPUFeature.SSE);
        public static final I386RMOp MOVSD  = new I386RMOp("MOVSD",        P_0F, 0x10, OpAssertion.FloatAssertion, CPUFeature.SSE);

        // TEST is documented as MR operation, but it's symmetric, and using it as RM operation is more convenient.
        public static final I386RMOp TESTB  = new I386RMOp("TEST",               0x84, OpAssertion.ByteAssertion);
        public static final I386RMOp TEST   = new I386RMOp("TEST",               0x85);
        // @formatter:on

        protected I386RMOp(String opcode, int op) {
            this(opcode, 0, op);
        }

        protected I386RMOp(String opcode, int op, OpAssertion assertion) {
            this(opcode, 0, op, assertion);
        }

        protected I386RMOp(String opcode, int prefix, int op) {
            this(opcode, 0, prefix, op, null);
        }

        protected I386RMOp(String opcode, int prefix, int op, OpAssertion assertion) {
            this(opcode, 0, prefix, op, assertion, null);
        }

        protected I386RMOp(String opcode, int prefix, int op, OpAssertion assertion, CPUFeature feature) {
            this(opcode, 0, prefix, op, assertion, feature);
        }

        protected I386RMOp(String opcode, int prefix, int op, boolean dstIsByte, boolean srcIsByte, OpAssertion assertion) {
            super(opcode, 0, prefix, op, dstIsByte, srcIsByte, assertion, null);
        }

        protected I386RMOp(String opcode, int prefix1, int prefix2, int op, CPUFeature feature) {
            this(opcode, prefix1, prefix2, op, OpAssertion.WordOrLargerAssertion, feature);
        }

        protected I386RMOp(String opcode, int prefix1, int prefix2, int op, OpAssertion assertion, CPUFeature feature) {
            super(opcode, prefix1, prefix2, op, assertion, feature);
        }

        @Override
        public final void emit(I386Assembler asm, OperandSize size, Register dst, Register src) {
            assert verify(asm, size, dst, src);
            boolean isSimd = false;
            boolean noNds = false;

            switch (op) {
                case 0x2A:
                case 0x2C:
                case 0x2E:
                case 0x5A:
                case 0x6E:
                    isSimd = true;
                    noNds = true;
                    break;
                case 0x10:
                case 0x51:
                case 0x54:
                case 0x55:
                case 0x56:
                case 0x57:
                case 0x58:
                case 0x59:
                case 0x5C:
                case 0x5D:
                case 0x5E:
                case 0x5F:
                    isSimd = true;
                    break;
            }

            int opc = 0;
            if (isSimd) {
                switch (prefix2) {
                    case P_0F:
                        opc = VexOpcode.VEX_OPCODE_0F;
                        break;
                    case P_0F38:
                        opc = VexOpcode.VEX_OPCODE_0F_38;
                        break;
                    case P_0F3A:
                        opc = VexOpcode.VEX_OPCODE_0F_3A;
                        break;
                    default:
                        opc = VexOpcode.VEX_OPCODE_NONE;
                        isSimd = false;
                        break;
                }
            }

            if (isSimd) {
                int pre;
                boolean rexVexW = (size == QWORD) ? true : false;
                I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, rexVexW, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, asm.target);
                int curPrefix = size.sizePrefix | prefix1;
                switch (curPrefix) {
                    case 0x66:
                        pre = VexSimdPrefix.VEX_SIMD_66;
                        break;
                    case 0xF2:
                        pre = VexSimdPrefix.VEX_SIMD_F2;
                        break;
                    case 0xF3:
                        pre = VexSimdPrefix.VEX_SIMD_F3;
                        break;
                    default:
                        pre = VexSimdPrefix.VEX_SIMD_NONE;
                        break;
                }
                int encode;
                if (noNds) {
                    encode = asm.simdPrefixAndEncode(dst, Register.None, src, pre, opc, attributes);
                } else {
                    encode = asm.simdPrefixAndEncode(dst, dst, src, pre, opc, attributes);
                }
                asm.emitByte(op);
                asm.emitByte(0xC0 | encode);
            } else {
                emitOpcode(asm, size, getRXB(dst, src), dst.encoding, src.encoding);
                asm.emitModRM(dst, src);
            }
        }

        public final void emit(I386Assembler asm, OperandSize size, Register dst, I386Address src) {
            assert verify(asm, size, dst, null);
            boolean isSimd = false;
            boolean noNds = false;

            switch (op) {
                case 0x10:
                case 0x2A:
                case 0x2C:
                case 0x2E:
                case 0x6E:
                    isSimd = true;
                    noNds = true;
                    break;
                case 0x51:
                case 0x54:
                case 0x55:
                case 0x56:
                case 0x57:
                case 0x58:
                case 0x59:
                case 0x5C:
                case 0x5D:
                case 0x5E:
                case 0x5F:
                    isSimd = true;
                    break;
            }

            int opc = 0;
            if (isSimd) {
                switch (prefix2) {
                    case P_0F:
                        opc = VexOpcode.VEX_OPCODE_0F;
                        break;
                    case P_0F38:
                        opc = VexOpcode.VEX_OPCODE_0F_38;
                        break;
                    case P_0F3A:
                        opc = VexOpcode.VEX_OPCODE_0F_3A;
                        break;
                    default:
                        isSimd = false;
                        break;
                }
            }

            if (isSimd) {
                int pre;
                boolean rexVexW = (size == QWORD) ? true : false;
                I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, rexVexW, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, asm.target);
                int curPrefix = size.sizePrefix | prefix1;
                switch (curPrefix) {
                    case 0x66:
                        pre = VexSimdPrefix.VEX_SIMD_66;
                        break;
                    case 0xF2:
                        pre = VexSimdPrefix.VEX_SIMD_F2;
                        break;
                    case 0xF3:
                        pre = VexSimdPrefix.VEX_SIMD_F3;
                        break;
                    default:
                        pre = VexSimdPrefix.VEX_SIMD_NONE;
                        break;
                }
                if (noNds) {
                    asm.simdPrefix(dst, Register.None, src, pre, opc, attributes);
                } else {
                    asm.simdPrefix(dst, dst, src, pre, opc, attributes);
                }
                asm.emitByte(op);
                asm.emitOperandHelper(dst, src, 0);
            } else {
                emitOpcode(asm, size, getRXB(dst, src), dst.encoding, 0);
                asm.emitOperandHelper(dst, src, 0);
            }
        }
    }

    /**
     * Opcode with operand order of RM.
     */
    public static class I386RRMOp extends I386RRROp {
        protected I386RRMOp(String opcode, int op) {
            this(opcode, 0, op);
        }

        protected I386RRMOp(String opcode, int op, OpAssertion assertion) {
            this(opcode, 0, op, assertion);
        }

        protected I386RRMOp(String opcode, int prefix, int op) {
            this(opcode, 0, prefix, op, null);
        }

        protected I386RRMOp(String opcode, int prefix, int op, OpAssertion assertion) {
            this(opcode, 0, prefix, op, assertion, null);
        }

        protected I386RRMOp(String opcode, int prefix, int op, OpAssertion assertion, CPUFeature feature) {
            this(opcode, 0, prefix, op, assertion, feature);
        }

        protected I386RRMOp(String opcode, int prefix, int op, boolean dstIsByte, boolean srcIsByte, OpAssertion assertion) {
            super(opcode, 0, prefix, op, dstIsByte, srcIsByte, assertion, null);
        }

        protected I386RRMOp(String opcode, int prefix1, int prefix2, int op, CPUFeature feature) {
            this(opcode, prefix1, prefix2, op, OpAssertion.WordOrLargerAssertion, feature);
        }

        protected I386RRMOp(String opcode, int prefix1, int prefix2, int op, OpAssertion assertion, CPUFeature feature) {
            super(opcode, prefix1, prefix2, op, assertion, feature);
        }

        @Override
        public final void emit(I386Assembler asm, OperandSize size, Register dst, Register nds, Register src) {
            assert verify(asm, size, dst, src);
            int pre;
            int opc;
            boolean rexVexW = (size == QWORD) ? true : false;
            I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, rexVexW, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, asm.target);
            int curPrefix = size.sizePrefix | prefix1;
            switch (curPrefix) {
                case 0x66:
                    pre = VexSimdPrefix.VEX_SIMD_66;
                    break;
                case 0xF2:
                    pre = VexSimdPrefix.VEX_SIMD_F2;
                    break;
                case 0xF3:
                    pre = VexSimdPrefix.VEX_SIMD_F3;
                    break;
                default:
                    pre = VexSimdPrefix.VEX_SIMD_NONE;
                    break;
            }
            switch (prefix2) {
                case P_0F:
                    opc = VexOpcode.VEX_OPCODE_0F;
                    break;
                case P_0F38:
                    opc = VexOpcode.VEX_OPCODE_0F_38;
                    break;
                case P_0F3A:
                    opc = VexOpcode.VEX_OPCODE_0F_3A;
                    break;
                default:
                    throw GraalError.shouldNotReachHere("invalid VEX instruction prefix");
            }
            int encode;
            encode = asm.simdPrefixAndEncode(dst, nds, src, pre, opc, attributes);
            asm.emitByte(op);
            asm.emitByte(0xC0 | encode);
        }

        public final void emit(I386Assembler asm, OperandSize size, Register dst, Register nds, I386Address src) {
            assert verify(asm, size, dst, null);
            int pre;
            int opc;
            boolean rexVexW = (size == QWORD) ? true : false;
            I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, rexVexW, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, asm.target);
            int curPrefix = size.sizePrefix | prefix1;
            switch (curPrefix) {
                case 0x66:
                    pre = VexSimdPrefix.VEX_SIMD_66;
                    break;
                case 0xF2:
                    pre = VexSimdPrefix.VEX_SIMD_F2;
                    break;
                case 0xF3:
                    pre = VexSimdPrefix.VEX_SIMD_F3;
                    break;
                default:
                    pre = VexSimdPrefix.VEX_SIMD_NONE;
                    break;
            }
            switch (prefix2) {
                case P_0F:
                    opc = VexOpcode.VEX_OPCODE_0F;
                    break;
                case P_0F38:
                    opc = VexOpcode.VEX_OPCODE_0F_38;
                    break;
                case P_0F3A:
                    opc = VexOpcode.VEX_OPCODE_0F_3A;
                    break;
                default:
                    throw GraalError.shouldNotReachHere("invalid VEX instruction prefix");
            }
            asm.simdPrefix(dst, nds, src, pre, opc, attributes);
            asm.emitByte(op);
            asm.emitOperandHelper(dst, src, 0);
        }
    }

    /**
     * Opcode with operand order of MR.
     */
    public static class I386MROp extends I386RROp {
        // @formatter:off
        public static final I386MROp MOVB   = new I386MROp("MOVB",               0x88, OpAssertion.ByteAssertion);
        public static final I386MROp MOV    = new I386MROp("MOV",                0x89);

        // MOVD and MOVQ are the same opcode, just with different operand size prefix
        // Note that as MR opcodes, they have reverse operand order, so the IntToFloatingAssertion must be used.
        public static final I386MROp MOVD   = new I386MROp("MOVD",   0x66, P_0F, 0x7E, OpAssertion.IntToFloatAssertion, CPUFeature.SSE2);
        public static final I386MROp MOVQ   = new I386MROp("MOVQ",   0x66, P_0F, 0x7E, OpAssertion.IntToFloatAssertion, CPUFeature.SSE2);

        // MOVSS and MOVSD are the same opcode, just with different operand size prefix
        public static final I386MROp MOVSS  = new I386MROp("MOVSS",        P_0F, 0x11, OpAssertion.FloatAssertion, CPUFeature.SSE);
        public static final I386MROp MOVSD  = new I386MROp("MOVSD",        P_0F, 0x11, OpAssertion.FloatAssertion, CPUFeature.SSE);
        // @formatter:on

        protected I386MROp(String opcode, int op) {
            this(opcode, 0, op);
        }

        protected I386MROp(String opcode, int op, OpAssertion assertion) {
            this(opcode, 0, op, assertion);
        }

        protected I386MROp(String opcode, int prefix, int op) {
            this(opcode, prefix, op, OpAssertion.WordOrLargerAssertion);
        }

        protected I386MROp(String opcode, int prefix, int op, OpAssertion assertion) {
            this(opcode, prefix, op, assertion, null);
        }

        protected I386MROp(String opcode, int prefix, int op, OpAssertion assertion, CPUFeature feature) {
            this(opcode, 0, prefix, op, assertion, feature);
        }

        protected I386MROp(String opcode, int prefix1, int prefix2, int op, OpAssertion assertion, CPUFeature feature) {
            super(opcode, prefix1, prefix2, op, assertion, feature);
        }

        @Override
        public final void emit(I386Assembler asm, OperandSize size, Register dst, Register src) {
            assert verify(asm, size, src, dst);
            boolean isSimd = false;
            boolean noNds = false;

            switch (op) {
                case 0x7E:
                    isSimd = true;
                    noNds = true;
                    break;
                case 0x11:
                    isSimd = true;
                    break;
            }

            int opc = 0;
            if (isSimd) {
                switch (prefix2) {
                    case P_0F:
                        opc = VexOpcode.VEX_OPCODE_0F;
                        break;
                    case P_0F38:
                        opc = VexOpcode.VEX_OPCODE_0F_38;
                        break;
                    case P_0F3A:
                        opc = VexOpcode.VEX_OPCODE_0F_3A;
                        break;
                    default:
                        isSimd = false;
                        break;
                }
            }

            if (isSimd) {
                int pre;
                boolean rexVexW = (size == QWORD) ? true : false;
                I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, rexVexW, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, asm.target);
                int curPrefix = size.sizePrefix | prefix1;
                switch (curPrefix) {
                    case 0x66:
                        pre = VexSimdPrefix.VEX_SIMD_66;
                        break;
                    case 0xF2:
                        pre = VexSimdPrefix.VEX_SIMD_F2;
                        break;
                    case 0xF3:
                        pre = VexSimdPrefix.VEX_SIMD_F3;
                        break;
                    default:
                        pre = VexSimdPrefix.VEX_SIMD_NONE;
                        break;
                }
                int encode;
                if (noNds) {
                    encode = asm.simdPrefixAndEncode(src, Register.None, dst, pre, opc, attributes);
                } else {
                    encode = asm.simdPrefixAndEncode(src, src, dst, pre, opc, attributes);
                }
                asm.emitByte(op);
                asm.emitByte(0xC0 | encode);
            } else {
                emitOpcode(asm, size, getRXB(src, dst), src.encoding, dst.encoding);
                asm.emitModRM(src, dst);
            }
        }

        public final void emit(I386Assembler asm, OperandSize size, I386Address dst, Register src) {
            assert verify(asm, size, null, src);
            boolean isSimd = false;

            switch (op) {
                case 0x7E:
                case 0x11:
                    isSimd = true;
                    break;
            }

            int opc = 0;
            if (isSimd) {
                switch (prefix2) {
                    case P_0F:
                        opc = VexOpcode.VEX_OPCODE_0F;
                        break;
                    case P_0F38:
                        opc = VexOpcode.VEX_OPCODE_0F_38;
                        break;
                    case P_0F3A:
                        opc = VexOpcode.VEX_OPCODE_0F_3A;
                        break;
                    default:
                        isSimd = false;
                        break;
                }
            }

            if (isSimd) {
                int pre;
                boolean rexVexW = (size == QWORD) ? true : false;
                I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, rexVexW, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, asm.target);
                int curPrefix = size.sizePrefix | prefix1;
                switch (curPrefix) {
                    case 0x66:
                        pre = VexSimdPrefix.VEX_SIMD_66;
                        break;
                    case 0xF2:
                        pre = VexSimdPrefix.VEX_SIMD_F2;
                        break;
                    case 0xF3:
                        pre = VexSimdPrefix.VEX_SIMD_F3;
                        break;
                    default:
                        pre = VexSimdPrefix.VEX_SIMD_NONE;
                        break;
                }
                asm.simdPrefix(src, Register.None, dst, pre, opc, attributes);
                asm.emitByte(op);
                asm.emitOperandHelper(src, dst, 0);
            } else {
                emitOpcode(asm, size, getRXB(src, dst), src.encoding, 0);
                asm.emitOperandHelper(src, dst, 0);
            }
        }
    }

    /**
     * Opcodes with operand order of M.
     */
    public static class I386MOp extends I386Op {
        // @formatter:off
        public static final I386MOp NOT  = new I386MOp("NOT",  0xF7, 2);
        public static final I386MOp NEG  = new I386MOp("NEG",  0xF7, 3);
        public static final I386MOp MUL  = new I386MOp("MUL",  0xF7, 4);
        public static final I386MOp IMUL = new I386MOp("IMUL", 0xF7, 5);
        public static final I386MOp DIV  = new I386MOp("DIV",  0xF7, 6);
        public static final I386MOp IDIV = new I386MOp("IDIV", 0xF7, 7);
        public static final I386MOp INC  = new I386MOp("INC",  0xFF, 0);
        public static final I386MOp DEC  = new I386MOp("DEC",  0xFF, 1);
        public static final I386MOp PUSH = new I386MOp("PUSH", 0xFF, 6);
        public static final I386MOp POP  = new I386MOp("POP",  0x8F, 0, OpAssertion.WordOrDwordAssertion);
        // @formatter:on

        private final int ext;

        protected I386MOp(String opcode, int op, int ext) {
            this(opcode, 0, op, ext);
        }

        protected I386MOp(String opcode, int prefix, int op, int ext) {
            this(opcode, prefix, op, ext, OpAssertion.WordOrLargerAssertion);
        }

        protected I386MOp(String opcode, int op, int ext, OpAssertion assertion) {
            this(opcode, 0, op, ext, assertion);
        }

        protected I386MOp(String opcode, int prefix, int op, int ext, OpAssertion assertion) {
            super(opcode, 0, prefix, op, assertion, null);
            this.ext = ext;
        }

        public final void emit(I386Assembler asm, OperandSize size, Register dst) {
            assert verify(asm, size, dst, null);
            emitOpcode(asm, size, getRXB(null, dst), 0, dst.encoding);
            asm.emitModRM(ext, dst);
        }

        public final void emit(I386Assembler asm, OperandSize size, I386Address dst) {
            assert verify(asm, size, null, null);
            emitOpcode(asm, size, getRXB(null, dst), 0, 0);
            asm.emitOperandHelper(ext, dst, 0);
        }
    }

    /**
     * Opcodes with operand order of MI.
     */
    public static class I386MIOp extends I386ImmOp {
        // @formatter:off
        public static final I386MIOp MOVB = new I386MIOp("MOVB", true,  0xC6, 0, OpAssertion.ByteAssertion);
        public static final I386MIOp MOV  = new I386MIOp("MOV",  false, 0xC7, 0);
        public static final I386MIOp TEST = new I386MIOp("TEST", false, 0xF7, 0);
        // @formatter:on

        private final int ext;

        protected I386MIOp(String opcode, boolean immIsByte, int op, int ext) {
            this(opcode, immIsByte, op, ext, OpAssertion.WordOrLargerAssertion);
        }

        protected I386MIOp(String opcode, boolean immIsByte, int op, int ext, OpAssertion assertion) {
            this(opcode, immIsByte, 0, op, ext, assertion);
        }

        protected I386MIOp(String opcode, boolean immIsByte, int prefix, int op, int ext, OpAssertion assertion) {
            super(opcode, immIsByte, prefix, op, assertion);
            this.ext = ext;
        }

        public final void emit(I386Assembler asm, OperandSize size, Register dst, int imm) {
            assert verify(asm, size, dst, null);
            emitOpcode(asm, size, getRXB(null, dst), 0, dst.encoding);
            asm.emitModRM(ext, dst);
            emitImmediate(asm, size, imm);
        }

        public final void emit(I386Assembler asm, OperandSize size, I386Address dst, int imm) {
            assert verify(asm, size, null, null);
            emitOpcode(asm, size, getRXB(null, dst), 0, 0);
            asm.emitOperandHelper(ext, dst, immediateSize(size));
            emitImmediate(asm, size, imm);
        }
    }

    /**
     * Opcodes with operand order of RMI.
     *
     * We only have one form of round as the operation is always treated with single variant input,
     * making its extension to 3 address forms redundant.
     */
    public static class I386RMIOp extends I386ImmOp {
        // @formatter:off
        public static final I386RMIOp IMUL    = new I386RMIOp("IMUL", false, 0x69);
        public static final I386RMIOp IMUL_SX = new I386RMIOp("IMUL", true,  0x6B);
        public static final I386RMIOp ROUNDSS = new I386RMIOp("ROUNDSS", true, P_0F3A, 0x0A, OpAssertion.PackedDoubleAssertion);
        public static final I386RMIOp ROUNDSD = new I386RMIOp("ROUNDSD", true, P_0F3A, 0x0B, OpAssertion.PackedDoubleAssertion);
        // @formatter:on

        protected I386RMIOp(String opcode, boolean immIsByte, int op) {
            this(opcode, immIsByte, 0, op, OpAssertion.WordOrLargerAssertion);
        }

        protected I386RMIOp(String opcode, boolean immIsByte, int prefix, int op, OpAssertion assertion) {
            super(opcode, immIsByte, prefix, op, assertion);
        }

        public final void emit(I386Assembler asm, OperandSize size, Register dst, Register src, int imm) {
            assert verify(asm, size, dst, src);
            boolean isSimd = false;
            boolean noNds = false;

            switch (op) {
                case 0x0A:
                case 0x0B:
                    isSimd = true;
                    noNds = true;
                    break;
            }

            int opc = 0;
            if (isSimd) {
                switch (prefix2) {
                    case P_0F:
                        opc = VexOpcode.VEX_OPCODE_0F;
                        break;
                    case P_0F38:
                        opc = VexOpcode.VEX_OPCODE_0F_38;
                        break;
                    case P_0F3A:
                        opc = VexOpcode.VEX_OPCODE_0F_3A;
                        break;
                    default:
                        isSimd = false;
                        break;
                }
            }

            if (isSimd) {
                int pre;
                I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, asm.target);
                int curPrefix = size.sizePrefix | prefix1;
                switch (curPrefix) {
                    case 0x66:
                        pre = VexSimdPrefix.VEX_SIMD_66;
                        break;
                    case 0xF2:
                        pre = VexSimdPrefix.VEX_SIMD_F2;
                        break;
                    case 0xF3:
                        pre = VexSimdPrefix.VEX_SIMD_F3;
                        break;
                    default:
                        pre = VexSimdPrefix.VEX_SIMD_NONE;
                        break;
                }
                int encode;
                if (noNds) {
                    encode = asm.simdPrefixAndEncode(dst, Register.None, src, pre, opc, attributes);
                } else {
                    encode = asm.simdPrefixAndEncode(dst, dst, src, pre, opc, attributes);
                }
                asm.emitByte(op);
                asm.emitByte(0xC0 | encode);
                emitImmediate(asm, size, imm);
            } else {
                emitOpcode(asm, size, getRXB(dst, src), dst.encoding, src.encoding);
                asm.emitModRM(dst, src);
                emitImmediate(asm, size, imm);
            }
        }

        public final void emit(I386Assembler asm, OperandSize size, Register dst, I386Address src, int imm) {
            assert verify(asm, size, dst, null);

            boolean isSimd = false;
            boolean noNds = false;

            switch (op) {
                case 0x0A:
                case 0x0B:
                    isSimd = true;
                    noNds = true;
                    break;
            }

            int opc = 0;
            if (isSimd) {
                switch (prefix2) {
                    case P_0F:
                        opc = VexOpcode.VEX_OPCODE_0F;
                        break;
                    case P_0F38:
                        opc = VexOpcode.VEX_OPCODE_0F_38;
                        break;
                    case P_0F3A:
                        opc = VexOpcode.VEX_OPCODE_0F_3A;
                        break;
                    default:
                        isSimd = false;
                        break;
                }
            }

            if (isSimd) {
                int pre;
                I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, asm.target);
                int curPrefix = size.sizePrefix | prefix1;
                switch (curPrefix) {
                    case 0x66:
                        pre = VexSimdPrefix.VEX_SIMD_66;
                        break;
                    case 0xF2:
                        pre = VexSimdPrefix.VEX_SIMD_F2;
                        break;
                    case 0xF3:
                        pre = VexSimdPrefix.VEX_SIMD_F3;
                        break;
                    default:
                        pre = VexSimdPrefix.VEX_SIMD_NONE;
                        break;
                }
                if (noNds) {
                    asm.simdPrefix(dst, Register.None, src, pre, opc, attributes);
                } else {
                    asm.simdPrefix(dst, dst, src, pre, opc, attributes);
                }
                asm.emitByte(op);
                asm.emitOperandHelper(dst, src, immediateSize(size));
                emitImmediate(asm, size, imm);
            } else {
                emitOpcode(asm, size, getRXB(dst, src), dst.encoding, 0);
                asm.emitOperandHelper(dst, src, immediateSize(size));
                emitImmediate(asm, size, imm);
            }
        }
    }

    public static class SSEOp extends I386RMOp {
        // @formatter:off
        public static final SSEOp CVTSI2SS  = new SSEOp("CVTSI2SS",  0xF3, P_0F, 0x2A, OpAssertion.IntToFloatAssertion);
        public static final SSEOp CVTSI2SD  = new SSEOp("CVTSI2SS",  0xF2, P_0F, 0x2A, OpAssertion.IntToFloatAssertion);
        public static final SSEOp CVTTSS2SI = new SSEOp("CVTTSS2SI", 0xF3, P_0F, 0x2C, OpAssertion.FloatToIntAssertion);
        public static final SSEOp CVTTSD2SI = new SSEOp("CVTTSD2SI", 0xF2, P_0F, 0x2C, OpAssertion.FloatToIntAssertion);
        public static final SSEOp UCOMIS    = new SSEOp("UCOMIS",          P_0F, 0x2E, OpAssertion.PackedFloatAssertion);
        public static final SSEOp SQRT      = new SSEOp("SQRT",            P_0F, 0x51);
        public static final SSEOp AND       = new SSEOp("AND",             P_0F, 0x54, OpAssertion.PackedFloatAssertion);
        public static final SSEOp ANDN      = new SSEOp("ANDN",            P_0F, 0x55, OpAssertion.PackedFloatAssertion);
        public static final SSEOp OR        = new SSEOp("OR",              P_0F, 0x56, OpAssertion.PackedFloatAssertion);
        public static final SSEOp XOR       = new SSEOp("XOR",             P_0F, 0x57, OpAssertion.PackedFloatAssertion);
        public static final SSEOp ADD       = new SSEOp("ADD",             P_0F, 0x58);
        public static final SSEOp MUL       = new SSEOp("MUL",             P_0F, 0x59);
        public static final SSEOp CVTSS2SD  = new SSEOp("CVTSS2SD",        P_0F, 0x5A, OpAssertion.SingleAssertion);
        public static final SSEOp CVTSD2SS  = new SSEOp("CVTSD2SS",        P_0F, 0x5A, OpAssertion.DoubleAssertion);
        public static final SSEOp SUB       = new SSEOp("SUB",             P_0F, 0x5C);
        public static final SSEOp MIN       = new SSEOp("MIN",             P_0F, 0x5D);
        public static final SSEOp DIV       = new SSEOp("DIV",             P_0F, 0x5E);
        public static final SSEOp MAX       = new SSEOp("MAX",             P_0F, 0x5F);
        // @formatter:on

        protected SSEOp(String opcode, int prefix, int op) {
            this(opcode, prefix, op, OpAssertion.FloatAssertion);
        }

        protected SSEOp(String opcode, int prefix, int op, OpAssertion assertion) {
            this(opcode, 0, prefix, op, assertion);
        }

        protected SSEOp(String opcode, int mandatoryPrefix, int prefix, int op, OpAssertion assertion) {
            super(opcode, mandatoryPrefix, prefix, op, assertion, CPUFeature.SSE2);
        }
    }

    public static class AVXOp extends I386RRMOp {
        // @formatter:off
        public static final AVXOp AND       = new AVXOp("AND",             P_0F, 0x54, OpAssertion.PackedFloatAssertion);
        public static final AVXOp ANDN      = new AVXOp("ANDN",            P_0F, 0x55, OpAssertion.PackedFloatAssertion);
        public static final AVXOp OR        = new AVXOp("OR",              P_0F, 0x56, OpAssertion.PackedFloatAssertion);
        public static final AVXOp XOR       = new AVXOp("XOR",             P_0F, 0x57, OpAssertion.PackedFloatAssertion);
        public static final AVXOp ADD       = new AVXOp("ADD",             P_0F, 0x58);
        public static final AVXOp MUL       = new AVXOp("MUL",             P_0F, 0x59);
        public static final AVXOp SUB       = new AVXOp("SUB",             P_0F, 0x5C);
        public static final AVXOp MIN       = new AVXOp("MIN",             P_0F, 0x5D);
        public static final AVXOp DIV       = new AVXOp("DIV",             P_0F, 0x5E);
        public static final AVXOp MAX       = new AVXOp("MAX",             P_0F, 0x5F);
        // @formatter:on

        protected AVXOp(String opcode, int prefix, int op) {
            this(opcode, prefix, op, OpAssertion.FloatAssertion);
        }

        protected AVXOp(String opcode, int prefix, int op, OpAssertion assertion) {
            this(opcode, 0, prefix, op, assertion);
        }

        protected AVXOp(String opcode, int mandatoryPrefix, int prefix, int op, OpAssertion assertion) {
            super(opcode, mandatoryPrefix, prefix, op, assertion, CPUFeature.AVX);
        }
    }

    /**
     * Arithmetic operation with operand order of RM, MR or MI.
     */
    public static final class I386BinaryArithmetic {
        // @formatter:off
        public static final I386BinaryArithmetic ADD = new I386BinaryArithmetic("ADD", 0);
        public static final I386BinaryArithmetic OR  = new I386BinaryArithmetic("OR",  1);
        public static final I386BinaryArithmetic ADC = new I386BinaryArithmetic("ADC", 2);
        public static final I386BinaryArithmetic SBB = new I386BinaryArithmetic("SBB", 3);
        public static final I386BinaryArithmetic AND = new I386BinaryArithmetic("AND", 4);
        public static final I386BinaryArithmetic SUB = new I386BinaryArithmetic("SUB", 5);
        public static final I386BinaryArithmetic XOR = new I386BinaryArithmetic("XOR", 6);
        public static final I386BinaryArithmetic CMP = new I386BinaryArithmetic("CMP", 7);
        // @formatter:on

        private final I386MIOp byteImmOp;
        private final I386MROp byteMrOp;
        private final I386RMOp byteRmOp;

        private final I386MIOp immOp;
        private final I386MIOp immSxOp;
        private final I386MROp mrOp;
        private final I386RMOp rmOp;

        private I386BinaryArithmetic(String opcode, int code) {
            int baseOp = code << 3;

            byteImmOp = new I386MIOp(opcode, true, 0, 0x80, code, OpAssertion.ByteAssertion);
            byteMrOp = new I386MROp(opcode, 0, baseOp, OpAssertion.ByteAssertion);
            byteRmOp = new I386RMOp(opcode, 0, baseOp | 0x02, OpAssertion.ByteAssertion);

            immOp = new I386MIOp(opcode, false, 0, 0x81, code, OpAssertion.WordOrLargerAssertion);
            immSxOp = new I386MIOp(opcode, true, 0, 0x83, code, OpAssertion.WordOrLargerAssertion);
            mrOp = new I386MROp(opcode, 0, baseOp | 0x01, OpAssertion.WordOrLargerAssertion);
            rmOp = new I386RMOp(opcode, 0, baseOp | 0x03, OpAssertion.WordOrLargerAssertion);
        }

        public I386MIOp getMIOpcode(OperandSize size, boolean sx) {
            if (size == BYTE) {
                return byteImmOp;
            } else if (sx) {
                return immSxOp;
            } else {
                return immOp;
            }
        }

        public I386MROp getMROpcode(OperandSize size) {
            if (size == BYTE) {
                return byteMrOp;
            } else {
                return mrOp;
            }
        }

        public I386RMOp getRMOpcode(OperandSize size) {
            if (size == BYTE) {
                return byteRmOp;
            } else {
                return rmOp;
            }
        }
    }

    /**
     * Shift operation with operand order of M1, MC or MI.
     */
    public static final class I386Shift {
        // @formatter:off
        public static final I386Shift ROL = new I386Shift("ROL", 0);
        public static final I386Shift ROR = new I386Shift("ROR", 1);
        public static final I386Shift RCL = new I386Shift("RCL", 2);
        public static final I386Shift RCR = new I386Shift("RCR", 3);
        public static final I386Shift SHL = new I386Shift("SHL", 4);
        public static final I386Shift SHR = new I386Shift("SHR", 5);
        public static final I386Shift SAR = new I386Shift("SAR", 7);
        // @formatter:on

        public final I386MOp m1Op;
        public final I386MOp mcOp;
        public final I386MIOp miOp;

        private I386Shift(String opcode, int code) {
            m1Op = new I386MOp(opcode, 0, 0xD1, code, OpAssertion.WordOrLargerAssertion);
            mcOp = new I386MOp(opcode, 0, 0xD3, code, OpAssertion.WordOrLargerAssertion);
            miOp = new I386MIOp(opcode, true, 0, 0xC1, code, OpAssertion.WordOrLargerAssertion);
        }
    }

    public final void addl(I386Address dst, int imm32) {
        ADD.getMIOpcode(DWORD, isByte(imm32)).emit(this, DWORD, dst, imm32);
    }

    public final void addl(Register dst, int imm32) {
        ADD.getMIOpcode(DWORD, isByte(imm32)).emit(this, DWORD, dst, imm32);
    }

    public final void addl(Register dst, Register src) {
        ADD.rmOp.emit(this, DWORD, dst, src);
    }

    public final void addpd(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x58);
        emitByte(0xC0 | encode);
    }

    public final void addpd(Register dst, I386Address src) {
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        simdPrefix(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x58);
        emitOperandHelper(dst, src, 0);
    }

    public final void addsd(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_F2, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x58);
        emitByte(0xC0 | encode);
    }

    public final void addsd(Register dst, I386Address src) {
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        simdPrefix(dst, dst, src, VexSimdPrefix.VEX_SIMD_F2, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x58);
        emitOperandHelper(dst, src, 0);
    }

    private void addrNop4() {
        // 4 bytes: NOP DWORD PTR [EAX+0]
        emitByte(0x0F);
        emitByte(0x1F);
        emitByte(0x40); // emitRm(cbuf, 0x1, EAXEnc, EAXEnc);
        emitByte(0); // 8-bits offset (1 byte)
    }

    private void addrNop5() {
        // 5 bytes: NOP DWORD PTR [EAX+EAX*0+0] 8-bits offset
        emitByte(0x0F);
        emitByte(0x1F);
        emitByte(0x44); // emitRm(cbuf, 0x1, EAXEnc, 0x4);
        emitByte(0x00); // emitRm(cbuf, 0x0, EAXEnc, EAXEnc);
        emitByte(0); // 8-bits offset (1 byte)
    }

    private void addrNop7() {
        // 7 bytes: NOP DWORD PTR [EAX+0] 32-bits offset
        emitByte(0x0F);
        emitByte(0x1F);
        emitByte(0x80); // emitRm(cbuf, 0x2, EAXEnc, EAXEnc);
        emitInt(0); // 32-bits offset (4 bytes)
    }

    private void addrNop8() {
        // 8 bytes: NOP DWORD PTR [EAX+EAX*0+0] 32-bits offset
        emitByte(0x0F);
        emitByte(0x1F);
        emitByte(0x84); // emitRm(cbuf, 0x2, EAXEnc, 0x4);
        emitByte(0x00); // emitRm(cbuf, 0x0, EAXEnc, EAXEnc);
        emitInt(0); // 32-bits offset (4 bytes)
    }

    public final void andl(Register dst, int imm32) {
        AND.getMIOpcode(DWORD, isByte(imm32)).emit(this, DWORD, dst, imm32);
    }

    public final void andl(Register dst, Register src) {
        AND.rmOp.emit(this, DWORD, dst, src);
    }

    public final void andpd(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x54);
        emitByte(0xC0 | encode);
    }

    public final void andpd(Register dst, I386Address src) {
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        simdPrefix(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x54);
        emitOperandHelper(dst, src, 0);
    }

    public final void bsfq(Register dst, Register src) {
        int encode = prefixqAndEncode(dst.encoding(), src.encoding());
        emitByte(0x0F);
        emitByte(0xBC);
        emitByte(0xC0 | encode);
    }

    public final void bsrl(Register dst, Register src) {
        int encode = prefixAndEncode(dst.encoding(), src.encoding());
        emitByte(0x0F);
        emitByte(0xBD);
        emitByte(0xC0 | encode);
    }

    public final void bswapl(Register reg) {
        int encode = prefixAndEncode(reg.encoding);
        emitByte(0x0F);
        emitByte(0xC8 | encode);
    }

    public final void cdql() {
        emitByte(0x99);
    }

    public final void cmovl(ConditionFlag cc, Register dst, Register src) {
        int encode = prefixAndEncode(dst.encoding, src.encoding);
        emitByte(0x0F);
        emitByte(0x40 | cc.getValue());
        emitByte(0xC0 | encode);
    }

    public final void cmovl(ConditionFlag cc, Register dst, I386Address src) {
        prefix(src, dst);
        emitByte(0x0F);
        emitByte(0x40 | cc.getValue());
        emitOperandHelper(dst, src, 0);
    }

    public final void cmpl(Register dst, int imm32) {
        CMP.getMIOpcode(DWORD, isByte(imm32)).emit(this, DWORD, dst, imm32);
    }

    public final void cmpl(Register dst, Register src) {
        CMP.rmOp.emit(this, DWORD, dst, src);
    }

    public final void cmpl(Register dst, I386Address src) {
        CMP.rmOp.emit(this, DWORD, dst, src);
    }

    public final void cmpl(I386Address dst, int imm32) {
        CMP.getMIOpcode(DWORD, isByte(imm32)).emit(this, DWORD, dst, imm32);
    }

    /**
     * The 8-bit cmpxchg compares the value at adr with the contents of X86.rax, and stores reg into
     * adr if so; otherwise, the value at adr is loaded into X86.rax,. The ZF is set if the compared
     * values were equal, and cleared otherwise.
     */
    public final void cmpxchgb(Register reg, I386Address adr) { // cmpxchg
        prefixb(adr, reg);
        emitByte(0x0F);
        emitByte(0xB0);
        emitOperandHelper(reg, adr, 0);
    }

    /**
     * The 16-bit cmpxchg compares the value at adr with the contents of X86.rax, and stores reg
     * into adr if so; otherwise, the value at adr is loaded into X86.rax,. The ZF is set if the
     * compared values were equal, and cleared otherwise.
     */
    public final void cmpxchgw(Register reg, I386Address adr) { // cmpxchg
        emitByte(0x66); // Switch to 16-bit mode.
        prefix(adr, reg);
        emitByte(0x0F);
        emitByte(0xB1);
        emitOperandHelper(reg, adr, 0);
    }

    /**
     * The 32-bit cmpxchg compares the value at adr with the contents of X86.rax, and stores reg
     * into adr if so; otherwise, the value at adr is loaded into X86.rax,. The ZF is set if the
     * compared values were equal, and cleared otherwise.
     */
    public final void cmpxchgl(Register reg, I386Address adr) { // cmpxchg
        prefix(adr, reg);
        emitByte(0x0F);
        emitByte(0xB1);
        emitOperandHelper(reg, adr, 0);
    }

    public final void cvtsi2sdl(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.CPU);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_F2, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x2A);
        emitByte(0xC0 | encode);
    }

    public final void cvttsd2sil(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.CPU) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_F2, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x2C);
        emitByte(0xC0 | encode);
    }

    protected final void decl(I386Address dst) {
        prefix(dst);
        emitByte(0xFF);
        emitOperandHelper(1, dst, 0);
    }

    public final void divsd(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_F2, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x5E);
        emitByte(0xC0 | encode);
    }

    public final void evmovdquq(Register dst, I386Address src, int vectorLen) {
        assert supports(CPUFeature.AVX512F);
        I386InstructionAttr attributes = new I386InstructionAttr(vectorLen, /* vex_w */ true, /* legacy_mode */ false, /* no_mask_reg */ false, /* uses_vl */ true, target);
        attributes.setAddressAttributes(/* tuple_type */ EvexTupleType.EVEX_FVM, /* input_size_in_bits */ EvexInputSizeInBits.EVEX_NObit);
        attributes.setIsEvexInstruction();
        vexPrefix(src, Register.None, dst, VexSimdPrefix.VEX_SIMD_F3, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x6F);
        emitOperandHelper(dst, src, 0);
    }

    public final void evpcmpeqb(Register kdst, Register nds, I386Address src, int vectorLen) {
        assert supports(CPUFeature.AVX512BW);
        I386InstructionAttr attributes = new I386InstructionAttr(vectorLen, /* rex_w */ false, /* legacy_mode */ false, /* no_mask_reg */ true, /* uses_vl */ false, target);
        attributes.setIsEvexInstruction();
        attributes.setAddressAttributes(/* tuple_type */ EvexTupleType.EVEX_FVM, /* input_size_in_bits */ EvexInputSizeInBits.EVEX_NObit);
        vexPrefix(src, nds, kdst, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x74);
        emitOperandHelper(kdst, src, 0);
    }

    public final void hlt() {
        emitByte(0xF4);
    }

    public final void imull(Register dst, Register src, int value) {
        if (isByte(value)) {
            I386RMIOp.IMUL_SX.emit(this, DWORD, dst, src, value);
        } else {
            I386RMIOp.IMUL.emit(this, DWORD, dst, src, value);
        }
    }

    protected final void incl(I386Address dst) {
        prefix(dst);
        emitByte(0xFF);
        emitOperandHelper(0, dst, 0);
    }

    public void jcc(ConditionFlag cc, int jumpTarget, boolean forceDisp32) {
        int shortSize = 2;
        int longSize = 6;
        long disp = jumpTarget - position();
        if (!forceDisp32 && isByte(disp - shortSize)) {
            // 0111 tttn #8-bit disp
            emitByte(0x70 | cc.getValue());
            emitByte((int) ((disp - shortSize) & 0xFF));
        } else {
            // 0000 1111 1000 tttn #32-bit disp
            assert isInt(disp - longSize) : "must be 32bit offset (call4)";
            emitByte(0x0F);
            emitByte(0x80 | cc.getValue());
            emitInt((int) (disp - longSize));
        }
    }

    public final void jcc(ConditionFlag cc, Label l) {
        assert (0 <= cc.getValue()) && (cc.getValue() < 16) : "illegal cc";
        if (l.isBound()) {
            jcc(cc, l.position(), false);
        } else {
            // Note: could eliminate cond. jumps to this jump if condition
            // is the same however, seems to be rather unlikely case.
            // Note: use jccb() if label to be bound is very close to get
            // an 8-bit displacement
            l.addPatchAt(position());
            emitByte(0x0F);
            emitByte(0x80 | cc.getValue());
            emitInt(0);
        }

    }

    public final void jccb(ConditionFlag cc, Label l) {
        if (l.isBound()) {
            int shortSize = 2;
            int entry = l.position();
            assert isByte(entry - (position() + shortSize)) : "Dispacement too large for a short jmp";
            long disp = entry - position();
            // 0111 tttn #8-bit disp
            emitByte(0x70 | cc.getValue());
            emitByte((int) ((disp - shortSize) & 0xFF));
        } else {
            l.addPatchAt(position());
            emitByte(0x70 | cc.getValue());
            emitByte(0);
        }
    }

    public final void jmp(int jumpTarget, boolean forceDisp32) {
        int shortSize = 2;
        int longSize = 5;
        long disp = jumpTarget - position();
        if (!forceDisp32 && isByte(disp - shortSize)) {
            emitByte(0xEB);
            emitByte((int) ((disp - shortSize) & 0xFF));
        } else {
            emitByte(0xE9);
            emitInt((int) (disp - longSize));
        }
    }

    @Override
    public final void jmp(Label l) {
        if (l.isBound()) {
            jmp(l.position(), false);
        } else {
            // By default, forward jumps are always 32-bit displacements, since
            // we can't yet know where the label will be bound. If you're sure that
            // the forward jump will not run beyond 256 bytes, use jmpb to
            // force an 8-bit displacement.

            l.addPatchAt(position());
            emitByte(0xE9);
            emitInt(0);
        }
    }

    public final void jmp(Register entry) {
        int encode = prefixAndEncode(entry.encoding);
        emitByte(0xFF);
        emitByte(0xE0 | encode);
    }

    public final void jmp(I386Address adr) {
        prefix(adr);
        emitByte(0xFF);
        emitOperandHelper(rsp, adr, 0);
    }

    public final void jmpb(Label l) {
        if (l.isBound()) {
            int shortSize = 2;
            int entry = l.position();
            assert isByte((entry - position()) + shortSize) : "Dispacement too large for a short jmp";
            long offs = entry - position();
            emitByte(0xEB);
            emitByte((int) ((offs - shortSize) & 0xFF));
        } else {

            l.addPatchAt(position());
            emitByte(0xEB);
            emitByte(0);
        }
    }

    // This instruction produces ZF or CF flags
    public final void kortestql(Register src1, Register src2) {
        assert supports(CPUFeature.AVX512BW);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rex_w */ true, /* legacy_mode */ true, /* no_mask_reg */ true, /* uses_vl */ false, target);
        int encode = vexPrefixAndEncode(src1, Register.None, src2, VexSimdPrefix.VEX_SIMD_NONE, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x98);
        emitByte(0xC0 | encode);
    }

    public final void kmovql(Register dst, Register src) {
        assert supports(CPUFeature.AVX512BW);
        if (src.getRegisterCategory().equals(I386.MASK)) {
            // kmovql(KRegister dst, KRegister src)
            I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rex_w */ true, /* legacy_mode */ true, /* no_mask_reg */ true, /* uses_vl */ false, target);
            int encode = vexPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_NONE, VexOpcode.VEX_OPCODE_0F, attributes);
            emitByte(0x90);
            emitByte(0xC0 | encode);
        } else {
            // kmovql(KRegister dst, Register src)
            I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rex_w */ true, /* legacy_mode */ true, /* no_mask_reg */ true, /* uses_vl */ false, target);
            int encode = vexPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_F2, VexOpcode.VEX_OPCODE_0F, attributes);
            emitByte(0x92);
            emitByte(0xC0 | encode);
        }
    }

    public final void lead(Register dst, I386Address src) {
        prefix(src, dst);
        emitByte(0x8D);
        emitOperandHelper(dst, src, 0);
    }

    public final void leaq(Register dst, I386Address src) {
        prefixq(src, dst);
        emitByte(0x8D);
        emitOperandHelper(dst, src, 0);
    }

    public final void leave() {
        emitByte(0xC9);
    }

    public final void lock() {
        emitByte(0xF0);
    }

    public final void movapd(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x28);
        emitByte(0xC0 | encode);
    }

    public final void movaps(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_NONE, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x28);
        emitByte(0xC0 | encode);
    }

    public final void movb(I386Address dst, int imm8) {
        prefix(dst);
        emitByte(0xC6);
        emitOperandHelper(0, dst, 1);
        emitByte(imm8);
    }

    public final void movb(I386Address dst, Register src) {
        assert src.getRegisterCategory().equals(I386.CPU) : "must have byte register";
        prefixb(dst, src);
        emitByte(0x88);
        emitOperandHelper(src, dst, 0);
    }

    public final void movl(Register dst, int imm32) {
        int encode = prefixAndEncode(dst.encoding);
        emitByte(0xB8 | encode);
        emitInt(imm32);
    }

    public final void movl(Register dst, Register src) {
        int encode = prefixAndEncode(dst.encoding, src.encoding);
        emitByte(0x8B);
        emitByte(0xC0 | encode);
    }

    public final void movl(Register dst, I386Address src) {
        prefix(src, dst);
        emitByte(0x8B);
        emitOperandHelper(dst, src, 0);
    }

    /**
     * @param wide use 4 byte encoding for displacements that would normally fit in a byte
     */
    public final void movl(Register dst, I386Address src, boolean wide) {
        prefix(src, dst);
        emitByte(0x8B);
        emitOperandHelper(dst, src, wide, 0);
    }

    public final void movl(I386Address dst, int imm32) {
        prefix(dst);
        emitByte(0xC7);
        emitOperandHelper(0, dst, 4);
        emitInt(imm32);
    }

    public final void movl(I386Address dst, Register src) {
        prefix(dst, src);
        emitByte(0x89);
        emitOperandHelper(src, dst, 0);
    }

    /**
     * New CPUs require use of movsd and movss to avoid partial register stall when loading from
     * memory. But for old Opteron use movlpd instead of movsd. The selection is done in
     * {@link I386MacroAssembler#movdbl(Register, I386Address)} and
     * {@link I386MacroAssembler#movflt(Register, Register)}.
     */
    public final void movlpd(Register dst, I386Address src) {
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        simdPrefix(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x12);
        emitOperandHelper(dst, src, 0);
    }

    public final void movlhps(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, src, src, VexSimdPrefix.VEX_SIMD_NONE, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x16);
        emitByte(0xC0 | encode);
    }

    public final void movq(Register dst, I386Address src) {
        movq(dst, src, false);
    }

    public final void movq(Register dst, I386Address src, boolean wide) {
        if (dst.getRegisterCategory().equals(I386.XMM)) {
            I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ wide, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
            simdPrefix(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_F3, VexOpcode.VEX_OPCODE_0F, attributes);
            emitByte(0x7E);
            emitOperandHelper(dst, src, wide, 0);
        } else {
            // gpr version of movq
            prefixq(src, dst);
            emitByte(0x8B);
            emitOperandHelper(dst, src, wide, 0);
        }
    }

    public final void movq(Register dst, Register src) {
        int encode = prefixqAndEncode(dst.encoding, src.encoding);
        emitByte(0x8B);
        emitByte(0xC0 | encode);
    }

    public final void movq(I386Address dst, Register src) {
        if (src.getRegisterCategory().equals(I386.XMM)) {
            I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ true, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
            simdPrefix(src, Register.None, dst, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
            emitByte(0xD6);
            emitOperandHelper(src, dst, 0);
        } else {
            // gpr version of movq
            prefixq(dst, src);
            emitByte(0x89);
            emitOperandHelper(src, dst, 0);
        }
    }

    public final void movsbl(Register dst, I386Address src) {
        prefix(src, dst);
        emitByte(0x0F);
        emitByte(0xBE);
        emitOperandHelper(dst, src, 0);
    }

    public final void movsbl(Register dst, Register src) {
        int encode = prefixAndEncode(dst.encoding, false, src.encoding, true);
        emitByte(0x0F);
        emitByte(0xBE);
        emitByte(0xC0 | encode);
    }

    public final void movsbq(Register dst, I386Address src) {
        prefixq(src, dst);
        emitByte(0x0F);
        emitByte(0xBE);
        emitOperandHelper(dst, src, 0);
    }

    public final void movsbq(Register dst, Register src) {
        int encode = prefixqAndEncode(dst.encoding, src.encoding);
        emitByte(0x0F);
        emitByte(0xBE);
        emitByte(0xC0 | encode);
    }

    public final void movsd(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_F2, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x10);
        emitByte(0xC0 | encode);
    }

    public final void movsd(Register dst, I386Address src) {
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        simdPrefix(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_F2, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x10);
        emitOperandHelper(dst, src, 0);
    }

    public final void movsd(I386Address dst, Register src) {
        assert src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        simdPrefix(src, Register.None, dst, VexSimdPrefix.VEX_SIMD_F2, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x11);
        emitOperandHelper(src, dst, 0);
    }

    public final void movss(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_F3, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x10);
        emitByte(0xC0 | encode);
    }

    public final void movss(Register dst, I386Address src) {
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        simdPrefix(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_F3, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x10);
        emitOperandHelper(dst, src, 0);
    }

    public final void movss(I386Address dst, Register src) {
        assert src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        simdPrefix(src, Register.None, dst, VexSimdPrefix.VEX_SIMD_F3, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x11);
        emitOperandHelper(src, dst, 0);
    }

    public final void mulpd(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x59);
        emitByte(0xC0 | encode);
    }

    public final void mulpd(Register dst, I386Address src) {
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        simdPrefix(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x59);
        emitOperandHelper(dst, src, 0);
    }

    public final void mulsd(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_F2, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x59);
        emitByte(0xC0 | encode);
    }

    public final void mulsd(Register dst, I386Address src) {
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        simdPrefix(dst, dst, src, VexSimdPrefix.VEX_SIMD_F2, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x59);
        emitOperandHelper(dst, src, 0);
    }

    public final void mulss(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_F3, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x59);
        emitByte(0xC0 | encode);
    }

    public final void movswl(Register dst, I386Address src) {
        prefix(src, dst);
        emitByte(0x0F);
        emitByte(0xBF);
        emitOperandHelper(dst, src, 0);
    }

    public final void movw(I386Address dst, int imm16) {
        emitByte(0x66); // switch to 16-bit mode
        prefix(dst);
        emitByte(0xC7);
        emitOperandHelper(0, dst, 2);
        emitShort(imm16);
    }

    public final void movw(I386Address dst, Register src) {
        emitByte(0x66);
        prefix(dst, src);
        emitByte(0x89);
        emitOperandHelper(src, dst, 0);
    }

    public final void movzbl(Register dst, I386Address src) {
        prefix(src, dst);
        emitByte(0x0F);
        emitByte(0xB6);
        emitOperandHelper(dst, src, 0);
    }

    public final void movzbl(Register dst, Register src) {
        I386RMOp.MOVZXB.emit(this, OperandSize.DWORD, dst, src);
    }

    public final void movzbq(Register dst, Register src) {
        I386RMOp.MOVZXB.emit(this, OperandSize.QWORD, dst, src);
    }

    public final void movzwl(Register dst, I386Address src) {
        prefix(src, dst);
        emitByte(0x0F);
        emitByte(0xB7);
        emitOperandHelper(dst, src, 0);
    }

    public final void negl(Register dst) {
        NEG.emit(this, DWORD, dst);
    }

    public final void notl(Register dst) {
        NOT.emit(this, DWORD, dst);
    }

    public final void notq(Register dst) {
        NOT.emit(this, QWORD, dst);
    }

    @Override
    public final void ensureUniquePC() {
        nop();
    }

    public final void nop() {
        nop(1);
    }

    public void nop(int count) {
        int i = count;
        if (UseNormalNop) {
            assert i > 0 : " ";
            // The fancy nops aren't currently recognized by debuggers making it a
            // pain to disassemble code while debugging. If assert are on clearly
            // speed is not an issue so simply use the single byte traditional nop
            // to do alignment.

            for (; i > 0; i--) {
                emitByte(0x90);
            }
            return;
        }

        if (UseAddressNop) {
            //
            // Using multi-bytes nops "0x0F 0x1F [Address]" for AMD.
            // 1: 0x90
            // 2: 0x66 0x90
            // 3: 0x66 0x66 0x90 (don't use "0x0F 0x1F 0x00" - need patching safe padding)
            // 4: 0x0F 0x1F 0x40 0x00
            // 5: 0x0F 0x1F 0x44 0x00 0x00
            // 6: 0x66 0x0F 0x1F 0x44 0x00 0x00
            // 7: 0x0F 0x1F 0x80 0x00 0x00 0x00 0x00
            // 8: 0x0F 0x1F 0x84 0x00 0x00 0x00 0x00 0x00
            // 9: 0x66 0x0F 0x1F 0x84 0x00 0x00 0x00 0x00 0x00
            // 10: 0x66 0x66 0x0F 0x1F 0x84 0x00 0x00 0x00 0x00 0x00
            // 11: 0x66 0x66 0x66 0x0F 0x1F 0x84 0x00 0x00 0x00 0x00 0x00

            // The rest coding is AMD specific - use consecutive Address nops

            // 12: 0x66 0x0F 0x1F 0x44 0x00 0x00 0x66 0x0F 0x1F 0x44 0x00 0x00
            // 13: 0x0F 0x1F 0x80 0x00 0x00 0x00 0x00 0x66 0x0F 0x1F 0x44 0x00 0x00
            // 14: 0x0F 0x1F 0x80 0x00 0x00 0x00 0x00 0x0F 0x1F 0x80 0x00 0x00 0x00 0x00
            // 15: 0x0F 0x1F 0x84 0x00 0x00 0x00 0x00 0x00 0x0F 0x1F 0x80 0x00 0x00 0x00 0x00
            // 16: 0x0F 0x1F 0x84 0x00 0x00 0x00 0x00 0x00 0x0F 0x1F 0x84 0x00 0x00 0x00 0x00 0x00
            // Size prefixes (0x66) are added for larger sizes

            while (i >= 22) {
                i -= 11;
                emitByte(0x66); // size prefix
                emitByte(0x66); // size prefix
                emitByte(0x66); // size prefix
                addrNop8();
            }
            // Generate first nop for size between 21-12
            switch (i) {
                case 21:
                    i -= 11;
                    emitByte(0x66); // size prefix
                    emitByte(0x66); // size prefix
                    emitByte(0x66); // size prefix
                    addrNop8();
                    break;
                case 20:
                case 19:
                    i -= 10;
                    emitByte(0x66); // size prefix
                    emitByte(0x66); // size prefix
                    addrNop8();
                    break;
                case 18:
                case 17:
                    i -= 9;
                    emitByte(0x66); // size prefix
                    addrNop8();
                    break;
                case 16:
                case 15:
                    i -= 8;
                    addrNop8();
                    break;
                case 14:
                case 13:
                    i -= 7;
                    addrNop7();
                    break;
                case 12:
                    i -= 6;
                    emitByte(0x66); // size prefix
                    addrNop5();
                    break;
                default:
                    assert i < 12;
            }

            // Generate second nop for size between 11-1
            switch (i) {
                case 11:
                    emitByte(0x66); // size prefix
                    emitByte(0x66); // size prefix
                    emitByte(0x66); // size prefix
                    addrNop8();
                    break;
                case 10:
                    emitByte(0x66); // size prefix
                    emitByte(0x66); // size prefix
                    addrNop8();
                    break;
                case 9:
                    emitByte(0x66); // size prefix
                    addrNop8();
                    break;
                case 8:
                    addrNop8();
                    break;
                case 7:
                    addrNop7();
                    break;
                case 6:
                    emitByte(0x66); // size prefix
                    addrNop5();
                    break;
                case 5:
                    addrNop5();
                    break;
                case 4:
                    addrNop4();
                    break;
                case 3:
                    // Don't use "0x0F 0x1F 0x00" - need patching safe padding
                    emitByte(0x66); // size prefix
                    emitByte(0x66); // size prefix
                    emitByte(0x90); // nop
                    break;
                case 2:
                    emitByte(0x66); // size prefix
                    emitByte(0x90); // nop
                    break;
                case 1:
                    emitByte(0x90); // nop
                    break;
                default:
                    assert i == 0;
            }
            return;
        }

        // Using nops with size prefixes "0x66 0x90".
        // From AMD Optimization Guide:
        // 1: 0x90
        // 2: 0x66 0x90
        // 3: 0x66 0x66 0x90
        // 4: 0x66 0x66 0x66 0x90
        // 5: 0x66 0x66 0x90 0x66 0x90
        // 6: 0x66 0x66 0x90 0x66 0x66 0x90
        // 7: 0x66 0x66 0x66 0x90 0x66 0x66 0x90
        // 8: 0x66 0x66 0x66 0x90 0x66 0x66 0x66 0x90
        // 9: 0x66 0x66 0x90 0x66 0x66 0x90 0x66 0x66 0x90
        // 10: 0x66 0x66 0x66 0x90 0x66 0x66 0x90 0x66 0x66 0x90
        //
        while (i > 12) {
            i -= 4;
            emitByte(0x66); // size prefix
            emitByte(0x66);
            emitByte(0x66);
            emitByte(0x90); // nop
        }
        // 1 - 12 nops
        if (i > 8) {
            if (i > 9) {
                i -= 1;
                emitByte(0x66);
            }
            i -= 3;
            emitByte(0x66);
            emitByte(0x66);
            emitByte(0x90);
        }
        // 1 - 8 nops
        if (i > 4) {
            if (i > 6) {
                i -= 1;
                emitByte(0x66);
            }
            i -= 3;
            emitByte(0x66);
            emitByte(0x66);
            emitByte(0x90);
        }
        switch (i) {
            case 4:
                emitByte(0x66);
                emitByte(0x66);
                emitByte(0x66);
                emitByte(0x90);
                break;
            case 3:
                emitByte(0x66);
                emitByte(0x66);
                emitByte(0x90);
                break;
            case 2:
                emitByte(0x66);
                emitByte(0x90);
                break;
            case 1:
                emitByte(0x90);
                break;
            default:
                assert i == 0;
        }
    }

    public final void orl(Register dst, Register src) {
        OR.rmOp.emit(this, DWORD, dst, src);
    }

    public final void orl(Register dst, int imm32) {
        OR.getMIOpcode(DWORD, isByte(imm32)).emit(this, DWORD, dst, imm32);
    }

    public final void pop(Register dst) {
        int encode = prefixAndEncode(dst.encoding);
        emitByte(0x58 | encode);
    }

    public void popfq() {
        emitByte(0x9D);
    }

    public final void ptest(Register dst, Register src) {
        assert supports(CPUFeature.SSE4_1);
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F_38, attributes);
        emitByte(0x17);
        emitByte(0xC0 | encode);
    }

    public final void vptest(Register dst, Register src) {
        assert supports(CPUFeature.AVX);
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_256bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = vexPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F_38, attributes);
        emitByte(0x17);
        emitByte(0xC0 | encode);
    }

    public final void pcmpestri(Register dst, I386Address src, int imm8) {
        assert supports(CPUFeature.SSE4_2);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        simdPrefix(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F_3A, attributes);
        emitByte(0x61);
        emitOperandHelper(dst, src, 0);
        emitByte(imm8);
    }

    public final void pcmpestri(Register dst, Register src, int imm8) {
        assert supports(CPUFeature.SSE4_2);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F_3A, attributes);
        emitByte(0x61);
        emitByte(0xC0 | encode);
        emitByte(imm8);
    }

    public final void pmovzxbw(Register dst, I386Address src) {
        assert supports(CPUFeature.SSE4_2);
        // XXX legacy_mode should be: _legacy_mode_bw
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rex_w */ false, /* legacy_mode */ false, /* no_mask_reg */ true, /* uses_vl */ false, target);
        attributes.setAddressAttributes(/* tuple_type */ EvexTupleType.EVEX_HVM, /* input_size_in_bits */ EvexInputSizeInBits.EVEX_NObit);
        simdPrefix(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F_38, attributes);
        emitByte(0x30);
        emitOperandHelper(dst, src, 0);
    }

    public final void vpmovzxbw(Register dst, I386Address src, int vectorLen) {
        assert supports(CPUFeature.AVX);
        // XXX legacy_mode should be: _legacy_mode_bw
        I386InstructionAttr attributes = new I386InstructionAttr(vectorLen, /* rex_w */ false, /* legacy_mode */ false, /* no_mask_reg */ true, /* uses_vl */ false, target);
        attributes.setAddressAttributes(/* tuple_type */ EvexTupleType.EVEX_HVM, /* input_size_in_bits */ EvexInputSizeInBits.EVEX_NObit);
        vexPrefix(src, Register.None, dst, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F_38, attributes);
        emitByte(0x30);
        emitOperandHelper(dst, src, 0);
    }

    public final void push(Register src) {
        int encode = prefixAndEncode(src.encoding);
        emitByte(0x50 | encode);
    }

    public void pushfq() {
        emitByte(0x9c);
    }

    public final void paddd(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0xFE);
        emitByte(0xC0 | encode);
    }

    public final void paddq(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0xD4);
        emitByte(0xC0 | encode);
    }

    public final void pextrw(Register dst, Register src, int imm8) {
        assert dst.getRegisterCategory().equals(I386.CPU) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0xC5);
        emitByte(0xC0 | encode);
        emitByte(imm8);
    }

    public final void pinsrw(Register dst, Register src, int imm8) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.CPU);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0xC4);
        emitByte(0xC0 | encode);
        emitByte(imm8);
    }

    public final void por(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0xEB);
        emitByte(0xC0 | encode);
    }

    public final void pand(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0xDB);
        emitByte(0xC0 | encode);
    }

    public final void pxor(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0xEF);
        emitByte(0xC0 | encode);
    }

    public final void vpxor(Register dst, Register nds, Register src) {
        assert supports(CPUFeature.AVX);
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_256bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = vexPrefixAndEncode(dst, nds, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0xEF);
        emitByte(0xC0 | encode);
    }

    public final void vpxor(Register dst, Register nds, I386Address src) {
        assert supports(CPUFeature.AVX);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_256bit, /* vex_w */ false, /* legacy_mode */ false, /* no_mask_reg */ false, /* uses_vl */ true, target);
        attributes.setAddressAttributes(/* tuple_type */ EvexTupleType.EVEX_FV, /* input_size_in_bits */ EvexInputSizeInBits.EVEX_32bit);
        vexPrefix(src, nds, dst, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0xEF);
        emitOperandHelper(dst, src, 0);
    }

    public final void pslld(Register dst, int imm8) {
        assert isUByte(imm8) : "invalid value";
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        // XMM6 is for /6 encoding: 66 0F 72 /6 ib
        int encode = simdPrefixAndEncode(I386.xmm6, dst, dst, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x72);
        emitByte(0xC0 | encode);
        emitByte(imm8 & 0xFF);
    }

    public final void psllq(Register dst, Register shift) {
        assert dst.getRegisterCategory().equals(I386.XMM) && shift.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, shift, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0xF3);
        emitByte(0xC0 | encode);
    }

    public final void psllq(Register dst, int imm8) {
        assert isUByte(imm8) : "invalid value";
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        // XMM6 is for /6 encoding: 66 0F 73 /6 ib
        int encode = simdPrefixAndEncode(I386.xmm6, dst, dst, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x73);
        emitByte(0xC0 | encode);
        emitByte(imm8);
    }

    public final void psrad(Register dst, int imm8) {
        assert isUByte(imm8) : "invalid value";
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        // XMM4 is for /2 encoding: 66 0F 72 /4 ib
        int encode = simdPrefixAndEncode(I386.xmm4, dst, dst, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x72);
        emitByte(0xC0 | encode);
        emitByte(imm8);
    }

    public final void psrld(Register dst, int imm8) {
        assert isUByte(imm8) : "invalid value";
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        // XMM2 is for /2 encoding: 66 0F 72 /2 ib
        int encode = simdPrefixAndEncode(I386.xmm2, dst, dst, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x72);
        emitByte(0xC0 | encode);
        emitByte(imm8);
    }

    public final void psrlq(Register dst, int imm8) {
        assert isUByte(imm8) : "invalid value";
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        // XMM2 is for /2 encoding: 66 0F 73 /2 ib
        int encode = simdPrefixAndEncode(I386.xmm2, dst, dst, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x73);
        emitByte(0xC0 | encode);
        emitByte(imm8);
    }

    public final void psrldq(Register dst, int imm8) {
        assert isUByte(imm8) : "invalid value";
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(I386.xmm3, dst, dst, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x73);
        emitByte(0xC0 | encode);
        emitByte(imm8);
    }

    public final void pshufd(Register dst, Register src, int imm8) {
        assert isUByte(imm8) : "invalid value";
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x70);
        emitByte(0xC0 | encode);
        emitByte(imm8);
    }

    public final void psubd(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0xFA);
        emitByte(0xC0 | encode);
    }

    public final void rcpps(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ true, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_NONE, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x53);
        emitByte(0xC0 | encode);
    }

    public final void ret(int imm16) {
        if (imm16 == 0) {
            emitByte(0xC3);
        } else {
            emitByte(0xC2);
            emitShort(imm16);
        }
    }

    public final void sarl(Register dst, int imm8) {
        int encode = prefixAndEncode(dst.encoding);
        assert isShiftCount(imm8 >> 1) : "illegal shift count";
        if (imm8 == 1) {
            emitByte(0xD1);
            emitByte(0xF8 | encode);
        } else {
            emitByte(0xC1);
            emitByte(0xF8 | encode);
            emitByte(imm8);
        }
    }

    public final void shll(Register dst, int imm8) {
        assert isShiftCount(imm8 >> 1) : "illegal shift count";
        int encode = prefixAndEncode(dst.encoding);
        if (imm8 == 1) {
            emitByte(0xD1);
            emitByte(0xE0 | encode);
        } else {
            emitByte(0xC1);
            emitByte(0xE0 | encode);
            emitByte(imm8);
        }
    }

    public final void shll(Register dst) {
        int encode = prefixAndEncode(dst.encoding);
        emitByte(0xD3);
        emitByte(0xE0 | encode);
    }

    public final void shrl(Register dst, int imm8) {
        assert isShiftCount(imm8 >> 1) : "illegal shift count";
        int encode = prefixAndEncode(dst.encoding);
        emitByte(0xC1);
        emitByte(0xE8 | encode);
        emitByte(imm8);
    }

    public final void shrl(Register dst) {
        int encode = prefixAndEncode(dst.encoding);
        emitByte(0xD3);
        emitByte(0xE8 | encode);
    }

    public final void subl(I386Address dst, int imm32) {
        SUB.getMIOpcode(DWORD, isByte(imm32)).emit(this, DWORD, dst, imm32);
    }

    public final void subl(Register dst, int imm32) {
        SUB.getMIOpcode(DWORD, isByte(imm32)).emit(this, DWORD, dst, imm32);
    }

    public final void subl(Register dst, Register src) {
        SUB.rmOp.emit(this, DWORD, dst, src);
    }

    public final void subpd(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x5C);
        emitByte(0xC0 | encode);
    }

    public final void subsd(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_F2, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x5C);
        emitByte(0xC0 | encode);
    }

    public final void subsd(Register dst, I386Address src) {
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        simdPrefix(dst, dst, src, VexSimdPrefix.VEX_SIMD_F2, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x5C);
        emitOperandHelper(dst, src, 0);
    }

    public final void testl(Register dst, int imm32) {
        // not using emitArith because test
        // doesn't support sign-extension of
        // 8bit operands
        int encode = dst.encoding;
        if (encode == 0) {
            emitByte(0xA9);
        } else {
            encode = prefixAndEncode(encode);
            emitByte(0xF7);
            emitByte(0xC0 | encode);
        }
        emitInt(imm32);
    }

    public final void testl(Register dst, Register src) {
        int encode = prefixAndEncode(dst.encoding, src.encoding);
        emitByte(0x85);
        emitByte(0xC0 | encode);
    }

    public final void testl(Register dst, I386Address src) {
        prefix(src, dst);
        emitByte(0x85);
        emitOperandHelper(dst, src, 0);
    }

    public final void unpckhpd(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x15);
        emitByte(0xC0 | encode);
    }

    public final void unpcklpd(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x14);
        emitByte(0xC0 | encode);
    }

    public final void xorl(Register dst, Register src) {
        XOR.rmOp.emit(this, DWORD, dst, src);
    }

    public final void xorpd(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x57);
        emitByte(0xC0 | encode);
    }

    public final void xorps(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_NONE, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x57);
        emitByte(0xC0 | encode);
    }

    protected final void decl(Register dst) {
        // Use two-byte form (one-byte form is a REX prefix in 64-bit mode)
        int encode = prefixAndEncode(dst.encoding);
        emitByte(0xFF);
        emitByte(0xC8 | encode);
    }

    protected final void incl(Register dst) {
        // Use two-byte form (one-byte from is a REX prefix in 64-bit mode)
        int encode = prefixAndEncode(dst.encoding);
        emitByte(0xFF);
        emitByte(0xC0 | encode);
    }

    private int prefixAndEncode(int regEnc) {
        return prefixAndEncode(regEnc, false);
    }

    private int prefixAndEncode(int regEnc, boolean byteinst) {
        if (regEnc >= 8) {
            emitByte(Prefix.REXB);
            return regEnc - 8;
        } else if (byteinst && regEnc >= 4) {
            emitByte(Prefix.REX);
        }
        return regEnc;
    }

    private int prefixqAndEncode(int regEnc) {
        if (regEnc < 8) {
            emitByte(Prefix.REXW);
            return regEnc;
        } else {
            emitByte(Prefix.REXWB);
            return regEnc - 8;
        }
    }

    private int prefixAndEncode(int dstEnc, int srcEnc) {
        return prefixAndEncode(dstEnc, false, srcEnc, false);
    }

    private int prefixAndEncode(int dstEncoding, boolean dstIsByte, int srcEncoding, boolean srcIsByte) {
        int srcEnc = srcEncoding;
        int dstEnc = dstEncoding;
        if (dstEnc < 8) {
            if (srcEnc >= 8) {
                emitByte(Prefix.REXB);
                srcEnc -= 8;
            } else if ((srcIsByte && srcEnc >= 4) || (dstIsByte && dstEnc >= 4)) {
                emitByte(Prefix.REX);
            }
        } else {
            if (srcEnc < 8) {
                emitByte(Prefix.REXR);
            } else {
                emitByte(Prefix.REXRB);
                srcEnc -= 8;
            }
            dstEnc -= 8;
        }
        return dstEnc << 3 | srcEnc;
    }

    /**
     * Creates prefix and the encoding of the lower 6 bits of the ModRM-Byte. It emits an operand
     * prefix. If the given operands exceed 3 bits, the 4th bit is encoded in the prefix.
     *
     * @param regEncoding the encoding of the register part of the ModRM-Byte
     * @param rmEncoding the encoding of the r/m part of the ModRM-Byte
     * @return the lower 6 bits of the ModRM-Byte that should be emitted
     */
    private int prefixqAndEncode(int regEncoding, int rmEncoding) {
        int rmEnc = rmEncoding;
        int regEnc = regEncoding;
        if (regEnc < 8) {
            if (rmEnc < 8) {
                emitByte(Prefix.REXW);
            } else {
                emitByte(Prefix.REXWB);
                rmEnc -= 8;
            }
        } else {
            if (rmEnc < 8) {
                emitByte(Prefix.REXWR);
            } else {
                emitByte(Prefix.REXWRB);
                rmEnc -= 8;
            }
            regEnc -= 8;
        }
        return regEnc << 3 | rmEnc;
    }

    private void vexPrefix(int rxb, int ndsEncoding, int pre, int opc, I386InstructionAttr attributes) {
        int vectorLen = attributes.getVectorLen();
        boolean vexW = attributes.isRexVexW();
        boolean isXorB = ((rxb & 0x3) > 0);
        if (isXorB || vexW || (opc == VexOpcode.VEX_OPCODE_0F_38) || (opc == VexOpcode.VEX_OPCODE_0F_3A)) {
            emitByte(Prefix.VEX_3BYTES);

            int byte1 = (rxb << 5);
            byte1 = ((~byte1) & 0xE0) | opc;
            emitByte(byte1);

            int byte2 = ((~ndsEncoding) & 0xf) << 3;
            byte2 |= (vexW ? VexPrefix.VEX_W : 0) | ((vectorLen > 0) ? 4 : 0) | pre;
            emitByte(byte2);
        } else {
            emitByte(Prefix.VEX_2BYTES);

            int byte1 = ((rxb & 0x4) > 0) ? VexPrefix.VEX_R : 0;
            byte1 = (~byte1) & 0x80;
            byte1 |= ((~ndsEncoding) & 0xf) << 3;
            byte1 |= ((vectorLen > 0) ? 4 : 0) | pre;
            emitByte(byte1);
        }
    }

    private void vexPrefix(I386Address adr, Register nds, Register src, int pre, int opc, I386InstructionAttr attributes) {
        int rxb = getRXB(src, adr);
        int ndsEncoding = nds.isValid() ? nds.encoding : 0;
        vexPrefix(rxb, ndsEncoding, pre, opc, attributes);
        setCurAttributes(attributes);
    }

    private int vexPrefixAndEncode(Register dst, Register nds, Register src, int pre, int opc, I386InstructionAttr attributes) {
        int rxb = getRXB(dst, src);
        int ndsEncoding = nds.isValid() ? nds.encoding : 0;
        vexPrefix(rxb, ndsEncoding, pre, opc, attributes);
        // return modrm byte components for operands
        return (((dst.encoding & 7) << 3) | (src.encoding & 7));
    }

    private void simdPrefix(Register xreg, Register nds, I386Address adr, int pre, int opc, I386InstructionAttr attributes) {
        if (supports(CPUFeature.AVX)) {
            vexPrefix(adr, nds, xreg, pre, opc, attributes);
        } else {
            switch (pre) {
                case VexSimdPrefix.VEX_SIMD_66:
                    emitByte(0x66);
                    break;
                case VexSimdPrefix.VEX_SIMD_F2:
                    emitByte(0xF2);
                    break;
                case VexSimdPrefix.VEX_SIMD_F3:
                    emitByte(0xF3);
                    break;
            }
            if (attributes.isRexVexW()) {
                prefixq(adr, xreg);
            } else {
                prefix(adr, xreg);
            }
            switch (opc) {
                case VexOpcode.VEX_OPCODE_0F:
                    emitByte(0x0F);
                    break;
                case VexOpcode.VEX_OPCODE_0F_38:
                    emitByte(0x0F);
                    emitByte(0x38);
                    break;
                case VexOpcode.VEX_OPCODE_0F_3A:
                    emitByte(0x0F);
                    emitByte(0x3A);
                    break;
            }
        }
    }

    private int simdPrefixAndEncode(Register dst, Register nds, Register src, int pre, int opc, I386InstructionAttr attributes) {
        if (supports(CPUFeature.AVX)) {
            return vexPrefixAndEncode(dst, nds, src, pre, opc, attributes);
        } else {
            switch (pre) {
                case VexSimdPrefix.VEX_SIMD_66:
                    emitByte(0x66);
                    break;
                case VexSimdPrefix.VEX_SIMD_F2:
                    emitByte(0xF2);
                    break;
                case VexSimdPrefix.VEX_SIMD_F3:
                    emitByte(0xF3);
                    break;
            }
            int encode;
            int dstEncoding = dst.encoding;
            int srcEncoding = src.encoding;
            if (attributes.isRexVexW()) {
                encode = prefixqAndEncode(dstEncoding, srcEncoding);
            } else {
                encode = prefixAndEncode(dstEncoding, srcEncoding);
            }
            switch (opc) {
                case VexOpcode.VEX_OPCODE_0F:
                    emitByte(0x0F);
                    break;
                case VexOpcode.VEX_OPCODE_0F_38:
                    emitByte(0x0F);
                    emitByte(0x38);
                    break;
                case VexOpcode.VEX_OPCODE_0F_3A:
                    emitByte(0x0F);
                    emitByte(0x3A);
                    break;
            }
            return encode;
        }
    }

    private static boolean needsRex(Register reg) {
        return reg.encoding >= MinEncodingNeedsRex;
    }

    private void prefix(I386Address adr) {
        if (needsRex(adr.getBase())) {
            if (needsRex(adr.getIndex())) {
                emitByte(Prefix.REXXB);
            } else {
                emitByte(Prefix.REXB);
            }
        } else {
            if (needsRex(adr.getIndex())) {
                emitByte(Prefix.REXX);
            }
        }
    }

    private void prefixq(I386Address adr) {
        if (needsRex(adr.getBase())) {
            if (needsRex(adr.getIndex())) {
                emitByte(Prefix.REXWXB);
            } else {
                emitByte(Prefix.REXWB);
            }
        } else {
            if (needsRex(adr.getIndex())) {
                emitByte(Prefix.REXWX);
            } else {
                emitByte(Prefix.REXW);
            }
        }
    }

    private void prefixb(I386Address adr, Register reg) {
        prefix(adr, reg, true);
    }

    private void prefix(I386Address adr, Register reg) {
        prefix(adr, reg, false);
    }

    private void prefix(I386Address adr, Register reg, boolean byteinst) {
        if (reg.encoding < 8) {
            if (needsRex(adr.getBase())) {
                if (needsRex(adr.getIndex())) {
                    emitByte(Prefix.REXXB);
                } else {
                    emitByte(Prefix.REXB);
                }
            } else {
                if (needsRex(adr.getIndex())) {
                    emitByte(Prefix.REXX);
                } else if (byteinst && reg.encoding >= 4) {
                    emitByte(Prefix.REX);
                }
            }
        } else {
            if (needsRex(adr.getBase())) {
                if (needsRex(adr.getIndex())) {
                    emitByte(Prefix.REXRXB);
                } else {
                    emitByte(Prefix.REXRB);
                }
            } else {
                if (needsRex(adr.getIndex())) {
                    emitByte(Prefix.REXRX);
                } else {
                    emitByte(Prefix.REXR);
                }
            }
        }
    }

    private void prefixq(I386Address adr, Register src) {
        if (src.encoding < 8) {
            if (needsRex(adr.getBase())) {
                if (needsRex(adr.getIndex())) {
                    emitByte(Prefix.REXWXB);
                } else {
                    emitByte(Prefix.REXWB);
                }
            } else {
                if (needsRex(adr.getIndex())) {
                    emitByte(Prefix.REXWX);
                } else {
                    emitByte(Prefix.REXW);
                }
            }
        } else {
            if (needsRex(adr.getBase())) {
                if (needsRex(adr.getIndex())) {
                    emitByte(Prefix.REXWRXB);
                } else {
                    emitByte(Prefix.REXWRB);
                }
            } else {
                if (needsRex(adr.getIndex())) {
                    emitByte(Prefix.REXWRX);
                } else {
                    emitByte(Prefix.REXWR);
                }
            }
        }
    }

    public final void addq(Register dst, int imm32) {
        ADD.getMIOpcode(QWORD, isByte(imm32)).emit(this, QWORD, dst, imm32);
    }

    public final void addq(I386Address dst, int imm32) {
        ADD.getMIOpcode(QWORD, isByte(imm32)).emit(this, QWORD, dst, imm32);
    }

    public final void addq(Register dst, Register src) {
        ADD.rmOp.emit(this, QWORD, dst, src);
    }

    public final void addq(I386Address dst, Register src) {
        ADD.mrOp.emit(this, QWORD, dst, src);
    }

    public final void andq(Register dst, int imm32) {
        AND.getMIOpcode(QWORD, isByte(imm32)).emit(this, QWORD, dst, imm32);
    }

    public final void bsrq(Register dst, Register src) {
        int encode = prefixqAndEncode(dst.encoding(), src.encoding());
        emitByte(0x0F);
        emitByte(0xBD);
        emitByte(0xC0 | encode);
    }

    public final void bswapq(Register reg) {
        int encode = prefixqAndEncode(reg.encoding);
        emitByte(0x0F);
        emitByte(0xC8 | encode);
    }

    public final void cdqq() {
        emitByte(Prefix.REXW);
        emitByte(0x99);
    }

    public final void cmovq(ConditionFlag cc, Register dst, Register src) {
        int encode = prefixqAndEncode(dst.encoding, src.encoding);
        emitByte(0x0F);
        emitByte(0x40 | cc.getValue());
        emitByte(0xC0 | encode);
    }

    public final void setb(ConditionFlag cc, Register dst) {
        int encode = prefixAndEncode(dst.encoding, true);
        emitByte(0x0F);
        emitByte(0x90 | cc.getValue());
        emitByte(0xC0 | encode);
    }

    public final void cmovq(ConditionFlag cc, Register dst, I386Address src) {
        prefixq(src, dst);
        emitByte(0x0F);
        emitByte(0x40 | cc.getValue());
        emitOperandHelper(dst, src, 0);
    }

    public final void cmpq(Register dst, int imm32) {
        CMP.getMIOpcode(QWORD, isByte(imm32)).emit(this, QWORD, dst, imm32);
    }

    public final void cmpq(Register dst, Register src) {
        CMP.rmOp.emit(this, QWORD, dst, src);
    }

    public final void cmpq(Register dst, I386Address src) {
        CMP.rmOp.emit(this, QWORD, dst, src);
    }

    public final void cmpxchgq(Register reg, I386Address adr) {
        prefixq(adr, reg);
        emitByte(0x0F);
        emitByte(0xB1);
        emitOperandHelper(reg, adr, 0);
    }

    public final void cvtdq2pd(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_F3, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0xE6);
        emitByte(0xC0 | encode);
    }

    public final void cvtsi2sdq(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.CPU);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ true, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, dst, src, VexSimdPrefix.VEX_SIMD_F2, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x2A);
        emitByte(0xC0 | encode);
    }

    public final void cvttsd2siq(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.CPU) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ true, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_F2, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x2C);
        emitByte(0xC0 | encode);
    }

    public final void cvttpd2dq(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0xE6);
        emitByte(0xC0 | encode);
    }

    protected final void decq(Register dst) {
        // Use two-byte form (one-byte from is a REX prefix in 64-bit mode)
        int encode = prefixqAndEncode(dst.encoding);
        emitByte(0xFF);
        emitByte(0xC8 | encode);
    }

    public final void decq(I386Address dst) {
        DEC.emit(this, QWORD, dst);
    }

    public final void imulq(Register dst, Register src) {
        int encode = prefixqAndEncode(dst.encoding, src.encoding);
        emitByte(0x0F);
        emitByte(0xAF);
        emitByte(0xC0 | encode);
    }

    public final void incq(Register dst) {
        // Don't use it directly. Use Macroincrementq() instead.
        // Use two-byte form (one-byte from is a REX prefix in 64-bit mode)
        int encode = prefixqAndEncode(dst.encoding);
        emitByte(0xFF);
        emitByte(0xC0 | encode);
    }

    public final void incq(I386Address dst) {
        INC.emit(this, QWORD, dst);
    }

    public final void movq(Register dst, long imm64) {
        int encode = prefixqAndEncode(dst.encoding);
        emitByte(0xB8 | encode);
        emitLong(imm64);
    }

    public final void movslq(Register dst, int imm32) {
        int encode = prefixqAndEncode(dst.encoding);
        emitByte(0xC7);
        emitByte(0xC0 | encode);
        emitInt(imm32);
    }

    public final void movdq(Register dst, I386Address src) {
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ true, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        simdPrefix(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x6E);
        emitOperandHelper(dst, src, 0);
    }

    public final void movdq(I386Address dst, Register src) {
        assert src.getRegisterCategory().equals(I386.XMM);
        // swap src/dst to get correct prefix
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ true, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        simdPrefix(src, Register.None, dst, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x7E);
        emitOperandHelper(src, dst, 0);
    }

    public final void movdq(Register dst, Register src) {
        if (dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.CPU)) {
            I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ true, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
            int encode = simdPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
            emitByte(0x6E);
            emitByte(0xC0 | encode);
        } else if (src.getRegisterCategory().equals(I386.XMM) && dst.getRegisterCategory().equals(I386.CPU)) {
            // swap src/dst to get correct prefix
            I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ true, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
            int encode = simdPrefixAndEncode(src, Register.None, dst, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
            emitByte(0x7E);
            emitByte(0xC0 | encode);
        } else {
            throw new InternalError("should not reach here");
        }
    }

    public final void movdl(Register dst, Register src) {
        if (dst.getRegisterCategory().equals(I386.XMM) && src.getRegisterCategory().equals(I386.CPU)) {
            I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
            int encode = simdPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
            emitByte(0x6E);
            emitByte(0xC0 | encode);
        } else if (src.getRegisterCategory().equals(I386.XMM) && dst.getRegisterCategory().equals(I386.CPU)) {
            // swap src/dst to get correct prefix
            I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
            int encode = simdPrefixAndEncode(src, Register.None, dst, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
            emitByte(0x7E);
            emitByte(0xC0 | encode);
        } else {
            throw new InternalError("should not reach here");
        }
    }

    public final void movdl(Register dst, I386Address src) {
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        simdPrefix(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_66, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x6E);
        emitOperandHelper(dst, src, 0);
    }

    public final void movddup(Register dst, Register src) {
        assert supports(CPUFeature.SSE3);
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_F2, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x12);
        emitByte(0xC0 | encode);
    }

    public final void movdqu(Register dst, I386Address src) {
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        simdPrefix(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_F3, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x6F);
        emitOperandHelper(dst, src, 0);
    }

    public final void movdqu(Register dst, Register src) {
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        int encode = simdPrefixAndEncode(dst, Register.None, src, VexSimdPrefix.VEX_SIMD_F3, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x6F);
        emitByte(0xC0 | encode);
    }

    public final void vmovdqu(Register dst, I386Address src) {
        assert supports(CPUFeature.AVX);
        assert dst.getRegisterCategory().equals(I386.XMM);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_256bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        vexPrefix(src, Register.None, dst, VexSimdPrefix.VEX_SIMD_F3, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x6F);
        emitOperandHelper(dst, src, 0);
    }

    public final void vzeroupper() {
        assert supports(CPUFeature.AVX);
        I386InstructionAttr attributes = new I386InstructionAttr(AvxVectorLen.AVX_128bit, /* rexVexW */ false, /* legacyMode */ false, /* noMaskReg */ false, /* usesVl */ false, target);
        vexPrefixAndEncode(I386.xmm0, I386.xmm0, I386.xmm0, VexSimdPrefix.VEX_SIMD_NONE, VexOpcode.VEX_OPCODE_0F, attributes);
        emitByte(0x77);
    }

    public final void movslq(I386Address dst, int imm32) {
        prefixq(dst);
        emitByte(0xC7);
        emitOperandHelper(0, dst, 4);
        emitInt(imm32);
    }

    public final void movslq(Register dst, I386Address src) {
        prefixq(src, dst);
        emitByte(0x63);
        emitOperandHelper(dst, src, 0);
    }

    public final void movslq(Register dst, Register src) {
        int encode = prefixqAndEncode(dst.encoding, src.encoding);
        emitByte(0x63);
        emitByte(0xC0 | encode);
    }

    public final void negq(Register dst) {
        int encode = prefixqAndEncode(dst.encoding);
        emitByte(0xF7);
        emitByte(0xD8 | encode);
    }

    public final void orq(Register dst, Register src) {
        OR.rmOp.emit(this, QWORD, dst, src);
    }

    public final void shlq(Register dst, int imm8) {
        assert isShiftCount(imm8 >> 1) : "illegal shift count";
        int encode = prefixqAndEncode(dst.encoding);
        if (imm8 == 1) {
            emitByte(0xD1);
            emitByte(0xE0 | encode);
        } else {
            emitByte(0xC1);
            emitByte(0xE0 | encode);
            emitByte(imm8);
        }
    }

    public final void shlq(Register dst) {
        int encode = prefixqAndEncode(dst.encoding);
        emitByte(0xD3);
        emitByte(0xE0 | encode);
    }

    public final void shrq(Register dst, int imm8) {
        assert isShiftCount(imm8 >> 1) : "illegal shift count";
        int encode = prefixqAndEncode(dst.encoding);
        if (imm8 == 1) {
            emitByte(0xD1);
            emitByte(0xE8 | encode);
        } else {
            emitByte(0xC1);
            emitByte(0xE8 | encode);
            emitByte(imm8);
        }
    }

    public final void shrq(Register dst) {
        int encode = prefixqAndEncode(dst.encoding);
        emitByte(0xD3);
        emitByte(0xE8 | encode);
    }

    public final void sbbq(Register dst, Register src) {
        SBB.rmOp.emit(this, QWORD, dst, src);
    }

    public final void subq(Register dst, int imm32) {
        SUB.getMIOpcode(QWORD, isByte(imm32)).emit(this, QWORD, dst, imm32);
    }

    public final void subq(I386Address dst, int imm32) {
        SUB.getMIOpcode(QWORD, isByte(imm32)).emit(this, QWORD, dst, imm32);
    }

    public final void subqWide(Register dst, int imm32) {
        // don't use the sign-extending version, forcing a 32-bit immediate
        SUB.getMIOpcode(QWORD, false).emit(this, QWORD, dst, imm32);
    }

    public final void subq(Register dst, Register src) {
        SUB.rmOp.emit(this, QWORD, dst, src);
    }

    public final void testq(Register dst, Register src) {
        int encode = prefixqAndEncode(dst.encoding, src.encoding);
        emitByte(0x85);
        emitByte(0xC0 | encode);
    }

    public final void btrq(Register src, int imm8) {
        int encode = prefixqAndEncode(src.encoding);
        emitByte(0x0F);
        emitByte(0xBA);
        emitByte(0xF0 | encode);
        emitByte(imm8);
    }

    public final void xaddb(I386Address dst, Register src) {
        prefixb(dst, src);
        emitByte(0x0F);
        emitByte(0xC0);
        emitOperandHelper(src, dst, 0);
    }

    public final void xaddw(I386Address dst, Register src) {
        emitByte(0x66); // Switch to 16-bit mode.
        prefix(dst, src);
        emitByte(0x0F);
        emitByte(0xC1);
        emitOperandHelper(src, dst, 0);
    }

    public final void xaddl(I386Address dst, Register src) {
        prefix(dst, src);
        emitByte(0x0F);
        emitByte(0xC1);
        emitOperandHelper(src, dst, 0);
    }

    public final void xaddq(I386Address dst, Register src) {
        prefixq(dst, src);
        emitByte(0x0F);
        emitByte(0xC1);
        emitOperandHelper(src, dst, 0);
    }

    public final void xchgb(Register dst, I386Address src) {
        prefixb(src, dst);
        emitByte(0x86);
        emitOperandHelper(dst, src, 0);
    }

    public final void xchgw(Register dst, I386Address src) {
        emitByte(0x66);
        prefix(src, dst);
        emitByte(0x87);
        emitOperandHelper(dst, src, 0);
    }

    public final void xchgl(Register dst, I386Address src) {
        prefix(src, dst);
        emitByte(0x87);
        emitOperandHelper(dst, src, 0);
    }

    public final void xchgq(Register dst, I386Address src) {
        prefixq(src, dst);
        emitByte(0x87);
        emitOperandHelper(dst, src, 0);
    }

    public final void membar(int barriers) {
        if (target.isMP) {
            // We only have to handle StoreLoad
            if ((barriers & STORE_LOAD) != 0) {
                // All usable chips support "locked" instructions which suffice
                // as barriers, and are much faster than the alternative of
                // using cpuid instruction. We use here a locked add [rsp],0.
                // This is conveniently otherwise a no-op except for blowing
                // flags.
                // Any change to this code may need to revisit other places in
                // the code where this idiom is used, in particular the
                // orderAccess code.
                lock();
                addl(new I386Address(rsp, 0), 0); // Assert the lock# signal here
            }
        }
    }

    @Override
    protected final void patchJumpTarget(int branch, int branchTarget) {
        int op = getByte(branch);
        assert op == 0xE8 // call
                        ||
                        op == 0x00 // jump table entry
                        || op == 0xE9 // jmp
                        || op == 0xEB // short jmp
                        || (op & 0xF0) == 0x70 // short jcc
                        || op == 0x0F && (getByte(branch + 1) & 0xF0) == 0x80 // jcc
        : "Invalid opcode at patch point branch=" + branch + ", branchTarget=" + branchTarget + ", op=" + op;

        if (op == 0x00) {
            int offsetToJumpTableBase = getShort(branch + 1);
            int jumpTableBase = branch - offsetToJumpTableBase;
            int imm32 = branchTarget - jumpTableBase;
            emitInt(imm32, branch);
        } else if (op == 0xEB || (op & 0xF0) == 0x70) {

            // short offset operators (jmp and jcc)
            final int imm8 = branchTarget - (branch + 2);
            /*
             * Since a wrongly patched short branch can potentially lead to working but really bad
             * behaving code we should always fail with an exception instead of having an assert.
             */
            if (!NumUtil.isByte(imm8)) {
                throw new InternalError("branch displacement out of range: " + imm8);
            }
            emitByte(imm8, branch + 1);

        } else {

            int off = 1;
            if (op == 0x0F) {
                off = 2;
            }

            int imm32 = branchTarget - (branch + 4 + off);
            emitInt(imm32, branch + off);
        }
    }

    public void nullCheck(I386Address address) {
        testl(I386.rax, address);
    }

    @Override
    public void align(int modulus) {
        if (position() % modulus != 0) {
            nop(modulus - (position() % modulus));
        }
    }

    /**
     * Emits a direct call instruction. Note that the actual call target is not specified, because
     * all calls need patching anyway. Therefore, 0 is emitted as the call target, and the user is
     * responsible to add the call address to the appropriate patching tables.
     */
    public final void call() {
        if (codePatchingAnnotationConsumer != null) {
            int pos = position();
            codePatchingAnnotationConsumer.accept(new ImmediateOperandAnnotation(pos, pos + 1, 4, pos + 5));
        }
        emitByte(0xE8);
        emitInt(0);
    }

    public final void call(Register src) {
        int encode = prefixAndEncode(src.encoding);
        emitByte(0xFF);
        emitByte(0xD0 | encode);
    }

    public final void int3() {
        emitByte(0xCC);
    }

    public final void pause() {
        emitByte(0xF3);
        emitByte(0x90);
    }

    private void emitx87(int b1, int b2, int i) {
        assert 0 <= i && i < 8 : "illegal stack offset";
        emitByte(b1);
        emitByte(b2 + i);
    }

    public final void fldd(I386Address src) {
        emitByte(0xDD);
        emitOperandHelper(0, src, 0);
    }

    public final void flds(I386Address src) {
        emitByte(0xD9);
        emitOperandHelper(0, src, 0);
    }

    public final void fldln2() {
        emitByte(0xD9);
        emitByte(0xED);
    }

    public final void fldlg2() {
        emitByte(0xD9);
        emitByte(0xEC);
    }

    public final void fyl2x() {
        emitByte(0xD9);
        emitByte(0xF1);
    }

    public final void fstps(I386Address src) {
        emitByte(0xD9);
        emitOperandHelper(3, src, 0);
    }

    public final void fstpd(I386Address src) {
        emitByte(0xDD);
        emitOperandHelper(3, src, 0);
    }

    private void emitFPUArith(int b1, int b2, int i) {
        assert 0 <= i && i < 8 : "illegal FPU register: " + i;
        emitByte(b1);
        emitByte(b2 + i);
    }

    public void ffree(int i) {
        emitFPUArith(0xDD, 0xC0, i);
    }

    public void fincstp() {
        emitByte(0xD9);
        emitByte(0xF7);
    }

    public void fxch(int i) {
        emitFPUArith(0xD9, 0xC8, i);
    }

    public void fnstswAX() {
        emitByte(0xDF);
        emitByte(0xE0);
    }

    public void fwait() {
        emitByte(0x9B);
    }

    public void fprem() {
        emitByte(0xD9);
        emitByte(0xF8);
    }

    public final void fsin() {
        emitByte(0xD9);
        emitByte(0xFE);
    }

    public final void fcos() {
        emitByte(0xD9);
        emitByte(0xFF);
    }

    public final void fptan() {
        emitByte(0xD9);
        emitByte(0xF2);
    }

    public final void fstp(int i) {
        emitx87(0xDD, 0xD8, i);
    }

    @Override
    public I386Address makeAddress(Register base, int displacement) {
        return new I386Address(base, displacement);
    }

    @Override
    public I386Address getPlaceholder(int instructionStartPosition) {
        return new I386Address(rip, Register.None, Scale.Times1, 0, instructionStartPosition);
    }

    private void prefetchPrefix(I386Address src) {
        prefix(src);
        emitByte(0x0F);
    }

    public void prefetchnta(I386Address src) {
        prefetchPrefix(src);
        emitByte(0x18);
        emitOperandHelper(0, src, 0);
    }

    void prefetchr(I386Address src) {
        assert supports(CPUFeature.AMD_3DNOW_PREFETCH);
        prefetchPrefix(src);
        emitByte(0x0D);
        emitOperandHelper(0, src, 0);
    }

    public void prefetcht0(I386Address src) {
        assert supports(CPUFeature.SSE);
        prefetchPrefix(src);
        emitByte(0x18);
        emitOperandHelper(1, src, 0);
    }

    public void prefetcht1(I386Address src) {
        assert supports(CPUFeature.SSE);
        prefetchPrefix(src);
        emitByte(0x18);
        emitOperandHelper(2, src, 0);
    }

    public void prefetcht2(I386Address src) {
        assert supports(CPUFeature.SSE);
        prefix(src);
        emitByte(0x0f);
        emitByte(0x18);
        emitOperandHelper(3, src, 0);
    }

    public void prefetchw(I386Address src) {
        assert supports(CPUFeature.AMD_3DNOW_PREFETCH);
        prefix(src);
        emitByte(0x0f);
        emitByte(0x0D);
        emitOperandHelper(1, src, 0);
    }

    public void rdtsc() {
        emitByte(0x0F);
        emitByte(0x31);
    }

    /**
     * Emits an instruction which is considered to be illegal. This is used if we deliberately want
     * to crash the program (debugging etc.).
     */
    public void illegal() {
        emitByte(0x0f);
        emitByte(0x0b);
    }

    public void lfence() {
        emitByte(0x0f);
        emitByte(0xae);
        emitByte(0xe8);

    }
}
