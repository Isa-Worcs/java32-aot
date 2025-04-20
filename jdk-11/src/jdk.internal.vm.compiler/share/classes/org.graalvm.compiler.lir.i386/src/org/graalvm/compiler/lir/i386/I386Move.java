/*
 * Copyright (c) 2011, 2016, Oracle and/or its affiliates. All rights reserved.
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
package org.graalvm.compiler.lir.i386;

import static java.lang.Double.doubleToRawLongBits;
import static java.lang.Float.floatToRawIntBits;
import static jdk.vm.ci.code.ValueUtil.asRegister;
import static jdk.vm.ci.code.ValueUtil.isRegister;
import static jdk.vm.ci.code.ValueUtil.isStackSlot;
import static org.graalvm.compiler.asm.i386.I386Assembler.ConditionFlag.Equal;
import static org.graalvm.compiler.core.common.GraalOptions.GeneratePIC;
import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.COMPOSITE;
import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.CONST;
import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.HINT;
import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.ILLEGAL;
import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.REG;
import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.STACK;
import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.UNINITIALIZED;
import static org.graalvm.compiler.lir.LIRValueUtil.asJavaConstant;
import static org.graalvm.compiler.lir.LIRValueUtil.isJavaConstant;

import org.graalvm.compiler.asm.Label;
import org.graalvm.compiler.asm.i386.I386Address;
import org.graalvm.compiler.asm.i386.I386Assembler.I386MIOp;
import org.graalvm.compiler.asm.i386.I386Assembler.I386MOp;
import org.graalvm.compiler.asm.i386.I386Assembler.OperandSize;
import org.graalvm.compiler.asm.i386.I386MacroAssembler;
import org.graalvm.compiler.core.common.CompressEncoding;
import org.graalvm.compiler.core.common.LIRKind;
import org.graalvm.compiler.core.common.NumUtil;
import org.graalvm.compiler.core.common.spi.LIRKindTool;
import org.graalvm.compiler.core.common.type.DataPointerConstant;
import org.graalvm.compiler.debug.GraalError;
import org.graalvm.compiler.lir.LIRFrameState;
import org.graalvm.compiler.lir.LIRInstructionClass;
import org.graalvm.compiler.lir.Opcode;
import org.graalvm.compiler.lir.StandardOp.LoadConstantOp;
import org.graalvm.compiler.lir.StandardOp.NullCheck;
import org.graalvm.compiler.lir.StandardOp.ValueMoveOp;
import org.graalvm.compiler.lir.VirtualStackSlot;
import org.graalvm.compiler.lir.asm.CompilationResultBuilder;
import org.graalvm.compiler.options.OptionValues;

import jdk.vm.ci.i386.I386;
import jdk.vm.ci.i386.I386Kind;
import jdk.vm.ci.code.Register;
import jdk.vm.ci.code.StackSlot;
import jdk.vm.ci.meta.AllocatableValue;
import jdk.vm.ci.meta.Constant;
import jdk.vm.ci.meta.JavaConstant;
import jdk.vm.ci.meta.Value;

public class I386Move {

    private abstract static class AbstractMoveOp extends I386LIRInstruction implements ValueMoveOp {
        public static final LIRInstructionClass<AbstractMoveOp> TYPE = LIRInstructionClass.create(AbstractMoveOp.class);

        private I386Kind moveKind;

        protected AbstractMoveOp(LIRInstructionClass<? extends AbstractMoveOp> c, I386Kind moveKind) {
            super(c);
            this.moveKind = moveKind;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            move(moveKind, crb, masm, getResult(), getInput());
        }
    }

    @Opcode("MOVE")
    public static final class MoveToRegOp extends AbstractMoveOp {
        public static final LIRInstructionClass<MoveToRegOp> TYPE = LIRInstructionClass.create(MoveToRegOp.class);

        @Def({REG, HINT}) protected AllocatableValue result;
        @Use({REG, STACK}) protected AllocatableValue input;

        public MoveToRegOp(I386Kind moveKind, AllocatableValue result, AllocatableValue input) {
            super(TYPE, moveKind);
            this.result = result;
            this.input = input;
        }

        @Override
        public AllocatableValue getInput() {
            return input;
        }

        @Override
        public AllocatableValue getResult() {
            return result;
        }
    }

    @Opcode("MOVE")
    public static final class MoveFromRegOp extends AbstractMoveOp {
        public static final LIRInstructionClass<MoveFromRegOp> TYPE = LIRInstructionClass.create(MoveFromRegOp.class);

        @Def({REG, STACK}) protected AllocatableValue result;
        @Use({REG, HINT}) protected AllocatableValue input;

        public MoveFromRegOp(I386Kind moveKind, AllocatableValue result, AllocatableValue input) {
            super(TYPE, moveKind);
            this.result = result;
            this.input = input;
        }

        @Override
        public AllocatableValue getInput() {
            return input;
        }

        @Override
        public AllocatableValue getResult() {
            return result;
        }
    }

    @Opcode("MOVE")
    public static class MoveFromConstOp extends I386LIRInstruction implements LoadConstantOp {
        public static final LIRInstructionClass<MoveFromConstOp> TYPE = LIRInstructionClass.create(MoveFromConstOp.class);

        @Def({REG, STACK}) protected AllocatableValue result;
        private final JavaConstant input;

        public MoveFromConstOp(AllocatableValue result, JavaConstant input) {
            super(TYPE);
            this.result = result;
            this.input = input;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            if (isRegister(result)) {
                const2reg(crb, masm, asRegister(result), input);
            } else {
                assert isStackSlot(result);
                const2stack(crb, masm, result, input);
            }
        }

        @Override
        public Constant getConstant() {
            return input;
        }

        @Override
        public AllocatableValue getResult() {
            return result;
        }
    }

    @Opcode("STACKMOVE")
    public static final class I386StackMove extends I386LIRInstruction implements ValueMoveOp {
        public static final LIRInstructionClass<I386StackMove> TYPE = LIRInstructionClass.create(I386StackMove.class);

        @Def({STACK}) protected AllocatableValue result;
        @Use({STACK, HINT}) protected AllocatableValue input;
        @Alive({OperandFlag.STACK, OperandFlag.UNINITIALIZED}) private AllocatableValue backupSlot;

        private Register scratch;

        public I386StackMove(AllocatableValue result, AllocatableValue input, Register scratch, AllocatableValue backupSlot) {
            super(TYPE);
            this.result = result;
            this.input = input;
            this.backupSlot = backupSlot;
            this.scratch = scratch;
        }

        @Override
        public AllocatableValue getInput() {
            return input;
        }

        @Override
        public AllocatableValue getResult() {
            return result;
        }

        public Register getScratchRegister() {
            return scratch;
        }

        public AllocatableValue getBackupSlot() {
            return backupSlot;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            I386Kind backupKind = (I386Kind) backupSlot.getPlatformKind();
            if (backupKind.isXMM()) {
                // graal doesn't use vector values, so it's safe to backup using DOUBLE
                backupKind = I386Kind.DOUBLE;
            }

            // backup scratch register
            reg2stack(backupKind, crb, masm, backupSlot, scratch);
            // move stack slot
            stack2reg((I386Kind) getInput().getPlatformKind(), crb, masm, scratch, getInput());
            reg2stack((I386Kind) getResult().getPlatformKind(), crb, masm, getResult(), scratch);
            // restore scratch register
            stack2reg(backupKind, crb, masm, scratch, backupSlot);
        }
    }

    @Opcode("MULTISTACKMOVE")
    public static final class I386MultiStackMove extends I386LIRInstruction {
        public static final LIRInstructionClass<I386MultiStackMove> TYPE = LIRInstructionClass.create(I386MultiStackMove.class);

        @Def({STACK}) protected AllocatableValue[] results;
        @Use({STACK}) protected Value[] inputs;
        @Alive({OperandFlag.STACK, OperandFlag.UNINITIALIZED}) private AllocatableValue backupSlot;

        private Register scratch;

        public I386MultiStackMove(AllocatableValue[] results, Value[] inputs, Register scratch, AllocatableValue backupSlot) {
            super(TYPE);
            this.results = results;
            this.inputs = inputs;
            this.backupSlot = backupSlot;
            this.scratch = scratch;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            I386Kind backupKind = (I386Kind) backupSlot.getPlatformKind();
            if (backupKind.isXMM()) {
                // graal doesn't use vector values, so it's safe to backup using DOUBLE
                backupKind = I386Kind.DOUBLE;
            }

            // backup scratch register
            move(backupKind, crb, masm, backupSlot, scratch.asValue(backupSlot.getValueKind()));
            for (int i = 0; i < results.length; i++) {
                Value input = inputs[i];
                AllocatableValue result = results[i];
                // move stack slot
                move((I386Kind) input.getPlatformKind(), crb, masm, scratch.asValue(input.getValueKind()), input);
                move((I386Kind) result.getPlatformKind(), crb, masm, result, scratch.asValue(result.getValueKind()));
            }
            // restore scratch register
            move(backupKind, crb, masm, scratch.asValue(backupSlot.getValueKind()), backupSlot);
        }
    }

    @Opcode("STACKMOVE")
    public static final class I386PushPopStackMove extends I386LIRInstruction implements ValueMoveOp {
        public static final LIRInstructionClass<I386PushPopStackMove> TYPE = LIRInstructionClass.create(I386PushPopStackMove.class);

        @Def({STACK}) protected AllocatableValue result;
        @Use({STACK, HINT}) protected AllocatableValue input;
        private final OperandSize size;

        public I386PushPopStackMove(OperandSize size, AllocatableValue result, AllocatableValue input) {
            super(TYPE);
            this.result = result;
            this.input = input;
            this.size = size;
        }

        @Override
        public AllocatableValue getInput() {
            return input;
        }

        @Override
        public AllocatableValue getResult() {
            return result;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            I386MOp.PUSH.emit(masm, size, (I386Address) crb.asAddress(input));
            I386MOp.POP.emit(masm, size, (I386Address) crb.asAddress(result));
        }
    }

    public static final class LeaOp extends I386LIRInstruction {
        public static final LIRInstructionClass<LeaOp> TYPE = LIRInstructionClass.create(LeaOp.class);

        @Def({REG}) protected AllocatableValue result;
        @Use({COMPOSITE, UNINITIALIZED}) protected I386AddressValue address;
        private final OperandSize size;

        public LeaOp(AllocatableValue result, I386AddressValue address, OperandSize size) {
            super(TYPE);
            this.result = result;
            this.address = address;
            this.size = size;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            if (size == OperandSize.QWORD) {
                masm.leaq(asRegister(result, I386Kind.QWORD), address.toAddress());
            } else {
                assert size == OperandSize.DWORD;
                masm.lead(asRegister(result, I386Kind.DWORD), address.toAddress());
            }
        }
    }

    public static final class LeaDataOp extends I386LIRInstruction {
        public static final LIRInstructionClass<LeaDataOp> TYPE = LIRInstructionClass.create(LeaDataOp.class);

        @Def({REG}) protected AllocatableValue result;
        private final DataPointerConstant data;

        public LeaDataOp(AllocatableValue result, DataPointerConstant data) {
            super(TYPE);
            this.result = result;
            this.data = data;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            masm.leaq(asRegister(result), (I386Address) crb.recordDataReferenceInCode(data));
        }
    }

    public static final class StackLeaOp extends I386LIRInstruction {
        public static final LIRInstructionClass<StackLeaOp> TYPE = LIRInstructionClass.create(StackLeaOp.class);

        @Def({REG}) protected AllocatableValue result;
        @Use({STACK, UNINITIALIZED}) protected AllocatableValue slot;

        public StackLeaOp(AllocatableValue result, AllocatableValue slot) {
            super(TYPE);
            this.result = result;
            this.slot = slot;
            assert slot instanceof VirtualStackSlot || slot instanceof StackSlot;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            masm.leaq(asRegister(result, I386Kind.QWORD), (I386Address) crb.asAddress(slot));
        }
    }

    public static final class MembarOp extends I386LIRInstruction {
        public static final LIRInstructionClass<MembarOp> TYPE = LIRInstructionClass.create(MembarOp.class);

        private final int barriers;

        public MembarOp(final int barriers) {
            super(TYPE);
            this.barriers = barriers;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            masm.membar(barriers);
        }
    }

    public static final class NullCheckOp extends I386LIRInstruction implements NullCheck {
        public static final LIRInstructionClass<NullCheckOp> TYPE = LIRInstructionClass.create(NullCheckOp.class);

        @Use({COMPOSITE}) protected I386AddressValue address;
        @State protected LIRFrameState state;

        public NullCheckOp(I386AddressValue address, LIRFrameState state) {
            super(TYPE);
            this.address = address;
            this.state = state;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            crb.recordImplicitException(masm.position(), state);
            masm.nullCheck(address.toAddress());
        }

        @Override
        public Value getCheckedValue() {
            return address.base;
        }

        @Override
        public LIRFrameState getState() {
            return state;
        }
    }

    @Opcode("CAS")
    public static final class CompareAndSwapOp extends I386LIRInstruction {
        public static final LIRInstructionClass<CompareAndSwapOp> TYPE = LIRInstructionClass.create(CompareAndSwapOp.class);

        private final I386Kind accessKind;

        @Def protected AllocatableValue result;
        @Use({COMPOSITE}) protected I386AddressValue address;
        @Use protected AllocatableValue cmpValue;
        @Use protected AllocatableValue newValue;

        public CompareAndSwapOp(I386Kind accessKind, AllocatableValue result, I386AddressValue address, AllocatableValue cmpValue, AllocatableValue newValue) {
            super(TYPE);
            this.accessKind = accessKind;
            this.result = result;
            this.address = address;
            this.cmpValue = cmpValue;
            this.newValue = newValue;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            assert asRegister(cmpValue).equals(I386.rax) && asRegister(result).equals(I386.rax);

            if (crb.target.isMP) {
                masm.lock();
            }
            switch (accessKind) {
                case BYTE:
                    masm.cmpxchgb(asRegister(newValue), address.toAddress());
                    break;
                case WORD:
                    masm.cmpxchgw(asRegister(newValue), address.toAddress());
                    break;
                case DWORD:
                    masm.cmpxchgl(asRegister(newValue), address.toAddress());
                    break;
                case QWORD:
                    masm.cmpxchgq(asRegister(newValue), address.toAddress());
                    break;
                default:
                    throw GraalError.shouldNotReachHere();
            }
        }
    }

    @Opcode("ATOMIC_READ_AND_ADD")
    public static final class AtomicReadAndAddOp extends I386LIRInstruction {
        public static final LIRInstructionClass<AtomicReadAndAddOp> TYPE = LIRInstructionClass.create(AtomicReadAndAddOp.class);

        private final I386Kind accessKind;

        @Def protected AllocatableValue result;
        @Alive({COMPOSITE}) protected I386AddressValue address;
        @Use protected AllocatableValue delta;

        public AtomicReadAndAddOp(I386Kind accessKind, AllocatableValue result, I386AddressValue address, AllocatableValue delta) {
            super(TYPE);
            this.accessKind = accessKind;
            this.result = result;
            this.address = address;
            this.delta = delta;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            move(accessKind, crb, masm, result, delta);
            if (crb.target.isMP) {
                masm.lock();
            }
            switch (accessKind) {
                case BYTE:
                    masm.xaddb(address.toAddress(), asRegister(result));
                    break;
                case WORD:
                    masm.xaddw(address.toAddress(), asRegister(result));
                    break;
                case DWORD:
                    masm.xaddl(address.toAddress(), asRegister(result));
                    break;
                case QWORD:
                    masm.xaddq(address.toAddress(), asRegister(result));
                    break;
                default:
                    throw GraalError.shouldNotReachHere();
            }
        }
    }

    @Opcode("ATOMIC_READ_AND_WRITE")
    public static final class AtomicReadAndWriteOp extends I386LIRInstruction {
        public static final LIRInstructionClass<AtomicReadAndWriteOp> TYPE = LIRInstructionClass.create(AtomicReadAndWriteOp.class);

        private final I386Kind accessKind;

        @Def protected AllocatableValue result;
        @Alive({COMPOSITE}) protected I386AddressValue address;
        @Use protected AllocatableValue newValue;

        public AtomicReadAndWriteOp(I386Kind accessKind, AllocatableValue result, I386AddressValue address, AllocatableValue newValue) {
            super(TYPE);
            this.accessKind = accessKind;
            this.result = result;
            this.address = address;
            this.newValue = newValue;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            move(accessKind, crb, masm, result, newValue);
            switch (accessKind) {
                case BYTE:
                    masm.xchgb(asRegister(result), address.toAddress());
                    break;
                case WORD:
                    masm.xchgw(asRegister(result), address.toAddress());
                    break;
                case DWORD:
                    masm.xchgl(asRegister(result), address.toAddress());
                    break;
                case QWORD:
                    masm.xchgq(asRegister(result), address.toAddress());
                    break;
                default:
                    throw GraalError.shouldNotReachHere();
            }
        }
    }

    public static void move(CompilationResultBuilder crb, I386MacroAssembler masm, Value result, Value input) {
        move((I386Kind) result.getPlatformKind(), crb, masm, result, input);
    }

    public static void move(I386Kind moveKind, CompilationResultBuilder crb, I386MacroAssembler masm, Value result, Value input) {
        if (isRegister(input)) {
            if (isRegister(result)) {
                reg2reg(moveKind, masm, result, input);
            } else if (isStackSlot(result)) {
                reg2stack(moveKind, crb, masm, result, asRegister(input));
            } else {
                throw GraalError.shouldNotReachHere();
            }
        } else if (isStackSlot(input)) {
            if (isRegister(result)) {
                stack2reg(moveKind, crb, masm, asRegister(result), input);
            } else {
                throw GraalError.shouldNotReachHere();
            }
        } else if (isJavaConstant(input)) {
            if (isRegister(result)) {
                const2reg(crb, masm, asRegister(result), asJavaConstant(input));
            } else if (isStackSlot(result)) {
                const2stack(crb, masm, result, asJavaConstant(input));
            } else {
                throw GraalError.shouldNotReachHere();
            }
        } else {
            throw GraalError.shouldNotReachHere();
        }
    }

    private static void reg2reg(I386Kind kind, I386MacroAssembler masm, Value result, Value input) {
        if (asRegister(input).equals(asRegister(result))) {
            return;
        }
        switch (kind) {
            case BYTE:
            case WORD:
            case DWORD:
                masm.movl(asRegister(result), asRegister(input));
                break;
            case QWORD:
                masm.movq(asRegister(result), asRegister(input));
                break;
            case SINGLE:
                masm.movflt(asRegister(result, I386Kind.SINGLE), asRegister(input, I386Kind.SINGLE));
                break;
            case DOUBLE:
                masm.movdbl(asRegister(result, I386Kind.DOUBLE), asRegister(input, I386Kind.DOUBLE));
                break;
            default:
                throw GraalError.shouldNotReachHere("kind=" + kind);
        }
    }

    public static void reg2stack(I386Kind kind, CompilationResultBuilder crb, I386MacroAssembler masm, Value result, Register input) {
        I386Address dest = (I386Address) crb.asAddress(result);
        switch (kind) {
            case BYTE:
                masm.movb(dest, input);
                break;
            case WORD:
                masm.movw(dest, input);
                break;
            case DWORD:
                masm.movl(dest, input);
                break;
            case QWORD:
                masm.movq(dest, input);
                break;
            case SINGLE:
                masm.movflt(dest, input);
                break;
            case DOUBLE:
                masm.movsd(dest, input);
                break;
            default:
                throw GraalError.shouldNotReachHere();
        }
    }

    public static void stack2reg(I386Kind kind, CompilationResultBuilder crb, I386MacroAssembler masm, Register result, Value input) {
        I386Address src = (I386Address) crb.asAddress(input);
        switch (kind) {
            case BYTE:
                masm.movsbl(result, src);
                break;
            case WORD:
                masm.movswl(result, src);
                break;
            case DWORD:
                masm.movl(result, src);
                break;
            case QWORD:
                masm.movq(result, src);
                break;
            case SINGLE:
                masm.movflt(result, src);
                break;
            case DOUBLE:
                masm.movdbl(result, src);
                break;
            default:
                throw GraalError.shouldNotReachHere();
        }
    }

    public static void const2reg(CompilationResultBuilder crb, I386MacroAssembler masm, Register result, JavaConstant input) {
        /*
         * Note: we use the kind of the input operand (and not the kind of the result operand)
         * because they don't match in all cases. For example, an object constant can be loaded to a
         * long register when unsafe casts occurred (e.g., for a write barrier where arithmetic
         * operations are then performed on the pointer).
         */
        switch (input.getJavaKind().getStackKind()) {
            case Int:
                // Do not optimize with an XOR as this instruction may be between
                // a CMP and a Jcc in which case the XOR will modify the condition
                // flags and interfere with the Jcc.
                masm.movl(result, input.asInt());

                break;
            case Long:
                // Do not optimize with an XOR as this instruction may be between
                // a CMP and a Jcc in which case the XOR will modify the condition
                // flags and interfere with the Jcc.
                if (input.asLong() == (int) input.asLong()) {
                    // Sign extended to long
                    masm.movslq(result, (int) input.asLong());
                } else if ((input.asLong() & 0xFFFFFFFFL) == input.asLong()) {
                    // Zero extended to long
                    masm.movl(result, (int) input.asLong());
                } else {
                    masm.movq(result, input.asLong());
                }
                break;
            case Float:
                // This is *not* the same as 'constant == 0.0f' in the case where constant is -0.0f
                if (Float.floatToRawIntBits(input.asFloat()) == Float.floatToRawIntBits(0.0f)) {
                    masm.xorps(result, result);
                } else {
                    masm.movflt(result, (I386Address) crb.asFloatConstRef(input));
                }
                break;
            case Double:
                // This is *not* the same as 'constant == 0.0d' in the case where constant is -0.0d
                if (Double.doubleToRawLongBits(input.asDouble()) == Double.doubleToRawLongBits(0.0d)) {
                    masm.xorpd(result, result);
                } else {
                    masm.movdbl(result, (I386Address) crb.asDoubleConstRef(input));
                }
                break;
            case Object:
                // Do not optimize with an XOR as this instruction may be between
                // a CMP and a Jcc in which case the XOR will modify the condition
                // flags and interfere with the Jcc.
                if (input.isNull()) {
                    masm.movq(result, 0x0L);
                } else if (crb.target.inlineObjects) {
                    crb.recordInlineDataInCode(input);
                    masm.movq(result, 0xDEADDEADDEADDEADL);
                } else {
                    masm.movq(result, (I386Address) crb.recordDataReferenceInCode(input, 0));
                }
                break;
            default:
                throw GraalError.shouldNotReachHere();
        }
    }

    public static boolean canMoveConst2Stack(JavaConstant input) {
        switch (input.getJavaKind().getStackKind()) {
            case Int:
                break;
            case Long:
                break;
            case Float:
                break;
            case Double:
                break;
            case Object:
                if (input.isNull()) {
                    return true;
                } else {
                    return false;
                }
            default:
                return false;
        }
        return true;
    }

    public static void const2stack(CompilationResultBuilder crb, I386MacroAssembler masm, Value result, JavaConstant input) {
        I386Address dest = (I386Address) crb.asAddress(result);
        final long imm;
        switch (input.getJavaKind().getStackKind()) {
            case Int:
                imm = input.asInt();
                break;
            case Long:
                imm = input.asLong();
                break;
            case Float:
                imm = floatToRawIntBits(input.asFloat());
                break;
            case Double:
                imm = doubleToRawLongBits(input.asDouble());
                break;
            case Object:
                if (input.isNull()) {
                    imm = 0;
                } else {
                    throw GraalError.shouldNotReachHere("Non-null object constants must be in register");
                }
                break;
            default:
                throw GraalError.shouldNotReachHere();
        }

        switch ((I386Kind) result.getPlatformKind()) {
            case BYTE:
                assert NumUtil.isByte(imm) : "Is not in byte range: " + imm;
                I386MIOp.MOVB.emit(masm, OperandSize.BYTE, dest, (int) imm);
                break;
            case WORD:
                assert NumUtil.isShort(imm) : "Is not in short range: " + imm;
                I386MIOp.MOV.emit(masm, OperandSize.WORD, dest, (int) imm);
                break;
            case DWORD:
            case SINGLE:
                assert NumUtil.isInt(imm) : "Is not in int range: " + imm;
                masm.movl(dest, (int) imm);
                break;
            case QWORD:
            case DOUBLE:
                masm.movlong(dest, imm);
                break;
            default:
                throw GraalError.shouldNotReachHere("Unknown result Kind: " + result.getPlatformKind());
        }
    }

    public abstract static class PointerCompressionOp extends I386LIRInstruction {
        protected final LIRKindTool lirKindTool;
        protected final CompressEncoding encoding;
        protected final boolean nonNull;

        @Def({REG, HINT}) private AllocatableValue result;
        @Use({REG, CONST}) private Value input;
        @Alive({REG, ILLEGAL, UNINITIALIZED}) private AllocatableValue baseRegister;

        protected PointerCompressionOp(LIRInstructionClass<? extends PointerCompressionOp> type, AllocatableValue result, Value input,
                        AllocatableValue baseRegister, CompressEncoding encoding, boolean nonNull, LIRKindTool lirKindTool) {

            super(type);
            this.result = result;
            this.input = input;
            this.baseRegister = baseRegister;
            this.encoding = encoding;
            this.nonNull = nonNull;
            this.lirKindTool = lirKindTool;
        }

        public static boolean hasBase(OptionValues options, CompressEncoding encoding) {
            return GeneratePIC.getValue(options) || encoding.hasBase();
        }

        public final Value getInput() {
            return input;
        }

        public final AllocatableValue getResult() {
            return result;
        }

        protected final Register getBaseRegister() {
            return asRegister(baseRegister);
        }

        protected final int getShift() {
            return encoding.getShift();
        }

        protected final void move(LIRKind kind, CompilationResultBuilder crb, I386MacroAssembler masm) {
            I386Move.move((I386Kind) kind.getPlatformKind(), crb, masm, result, input);
        }
    }

    public static class CompressPointerOp extends PointerCompressionOp {
        public static final LIRInstructionClass<CompressPointerOp> TYPE = LIRInstructionClass.create(CompressPointerOp.class);

        public CompressPointerOp(AllocatableValue result, Value input, AllocatableValue baseRegister, CompressEncoding encoding, boolean nonNull, LIRKindTool lirKindTool) {
            this(TYPE, result, input, baseRegister, encoding, nonNull, lirKindTool);
        }

        protected CompressPointerOp(LIRInstructionClass<? extends PointerCompressionOp> type, AllocatableValue result, Value input,
                        AllocatableValue baseRegister, CompressEncoding encoding, boolean nonNull, LIRKindTool lirKindTool) {

            super(type, result, input, baseRegister, encoding, nonNull, lirKindTool);
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            move(lirKindTool.getObjectKind(), crb, masm);

            Register resReg = asRegister(getResult());
            if (hasBase(crb.getOptions(), encoding)) {
                Register baseReg = getBaseRegister();
                if (!nonNull) {
                    masm.testq(resReg, resReg);
                    masm.cmovq(Equal, resReg, baseReg);
                }
                masm.subq(resReg, baseReg);
            }

            int shift = getShift();
            if (shift != 0) {
                masm.shrq(resReg, shift);
            }
        }
    }

    public static class UncompressPointerOp extends PointerCompressionOp {
        public static final LIRInstructionClass<UncompressPointerOp> TYPE = LIRInstructionClass.create(UncompressPointerOp.class);

        public UncompressPointerOp(AllocatableValue result, Value input, AllocatableValue baseRegister, CompressEncoding encoding, boolean nonNull, LIRKindTool lirKindTool) {
            this(TYPE, result, input, baseRegister, encoding, nonNull, lirKindTool);
        }

        protected UncompressPointerOp(LIRInstructionClass<? extends PointerCompressionOp> type, AllocatableValue result, Value input,
                        AllocatableValue baseRegister, CompressEncoding encoding, boolean nonNull, LIRKindTool lirKindTool) {

            super(type, result, input, baseRegister, encoding, nonNull, lirKindTool);
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            move(lirKindTool.getNarrowOopKind(), crb, masm);
            emitUncompressCode(masm, asRegister(getResult()), getShift(), hasBase(crb.getOptions(), encoding) ? getBaseRegister() : null, nonNull);
        }

        public static void emitUncompressCode(I386MacroAssembler masm, Register resReg, int shift, Register baseReg, boolean nonNull) {
            if (shift != 0) {
                masm.shlq(resReg, shift);
            }

            if (baseReg != null) {
                if (nonNull) {
                    masm.addq(resReg, baseReg);
                    return;
                }

                if (shift == 0) {
                    // if encoding.shift != 0, the flags are already set by the shlq
                    masm.testq(resReg, resReg);
                }

                Label done = new Label();
                masm.jccb(Equal, done);
                masm.addq(resReg, baseReg);
                masm.bind(done);
            }
        }
    }
}
