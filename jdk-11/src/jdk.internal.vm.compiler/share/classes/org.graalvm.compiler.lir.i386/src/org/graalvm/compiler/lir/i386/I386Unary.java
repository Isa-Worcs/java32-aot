/*
 * Copyright (c) 2015, 2015, Oracle and/or its affiliates. All rights reserved.
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

import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.COMPOSITE;
import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.HINT;
import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.REG;
import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.STACK;
import static jdk.vm.ci.code.ValueUtil.asRegister;
import static jdk.vm.ci.code.ValueUtil.isRegister;
import static jdk.vm.ci.code.ValueUtil.isStackSlot;

import org.graalvm.compiler.asm.i386.I386Address;
import org.graalvm.compiler.asm.i386.I386Assembler.I386MOp;
import org.graalvm.compiler.asm.i386.I386Assembler.I386MROp;
import org.graalvm.compiler.asm.i386.I386Assembler.I386RMOp;
import org.graalvm.compiler.asm.i386.I386Assembler.OperandSize;
import org.graalvm.compiler.asm.i386.I386MacroAssembler;
import org.graalvm.compiler.lir.LIRFrameState;
import org.graalvm.compiler.lir.LIRInstructionClass;
import org.graalvm.compiler.lir.Opcode;
import org.graalvm.compiler.lir.StandardOp.ImplicitNullCheck;
import org.graalvm.compiler.lir.asm.CompilationResultBuilder;

import jdk.vm.ci.meta.AllocatableValue;
import jdk.vm.ci.meta.Value;

/**
 * I386 LIR instructions that have one input and one output.
 */
public class I386Unary {

    /**
     * Instruction with a single operand that is both input and output.
     */
    public static class MOp extends I386LIRInstruction {
        public static final LIRInstructionClass<MOp> TYPE = LIRInstructionClass.create(MOp.class);

        @Opcode private final I386MOp opcode;
        private final OperandSize size;

        @Def({REG, HINT}) protected AllocatableValue result;
        @Use({REG, STACK}) protected AllocatableValue value;

        public MOp(I386MOp opcode, OperandSize size, AllocatableValue result, AllocatableValue value) {
            super(TYPE);
            this.opcode = opcode;
            this.size = size;

            this.result = result;
            this.value = value;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            I386Move.move(crb, masm, result, value);
            opcode.emit(masm, size, asRegister(result));
        }
    }

    /**
     * Instruction with separate input and output operands, and an operand encoding of RM.
     */
    public static class RMOp extends I386LIRInstruction {
        public static final LIRInstructionClass<RMOp> TYPE = LIRInstructionClass.create(RMOp.class);

        @Opcode private final I386RMOp opcode;
        private final OperandSize size;

        @Def({REG}) protected AllocatableValue result;
        @Use({REG, STACK}) protected AllocatableValue value;

        public RMOp(I386RMOp opcode, OperandSize size, AllocatableValue result, AllocatableValue value) {
            super(TYPE);
            this.opcode = opcode;
            this.size = size;

            this.result = result;
            this.value = value;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            if (isRegister(value)) {
                opcode.emit(masm, size, asRegister(result), asRegister(value));
            } else {
                assert isStackSlot(value);
                opcode.emit(masm, size, asRegister(result), (I386Address) crb.asAddress(value));
            }
        }
    }

    /**
     * Instruction with separate input and output operands, and an operand encoding of MR.
     */
    public static class MROp extends I386LIRInstruction {
        public static final LIRInstructionClass<MROp> TYPE = LIRInstructionClass.create(MROp.class);

        @Opcode private final I386MROp opcode;
        private final OperandSize size;

        @Def({REG, STACK}) protected AllocatableValue result;
        @Use({REG}) protected AllocatableValue value;

        public MROp(I386MROp opcode, OperandSize size, AllocatableValue result, AllocatableValue value) {
            super(TYPE);
            this.opcode = opcode;
            this.size = size;

            this.result = result;
            this.value = value;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            if (isRegister(result)) {
                opcode.emit(masm, size, asRegister(result), asRegister(value));
            } else {
                assert isStackSlot(result);
                opcode.emit(masm, size, (I386Address) crb.asAddress(result), asRegister(value));
            }
        }
    }

    /**
     * Instruction with a {@link I386AddressValue memory} operand.
     */
    public static class MemoryOp extends I386LIRInstruction implements ImplicitNullCheck {
        public static final LIRInstructionClass<MemoryOp> TYPE = LIRInstructionClass.create(MemoryOp.class);

        @Opcode private final I386RMOp opcode;
        private final OperandSize size;

        @Def({REG}) protected AllocatableValue result;
        @Use({COMPOSITE}) protected I386AddressValue input;

        @State protected LIRFrameState state;

        public MemoryOp(I386RMOp opcode, OperandSize size, AllocatableValue result, I386AddressValue input, LIRFrameState state) {
            super(TYPE);
            this.opcode = opcode;
            this.size = size;

            this.result = result;
            this.input = input;

            this.state = state;
        }

        @Override
        public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
            if (state != null) {
                crb.recordImplicitException(masm.position(), state);
            }
            opcode.emit(masm, size, asRegister(result), input.toAddress());
        }

        @Override
        public boolean makeNullCheckFor(Value value, LIRFrameState nullCheckState, int implicitNullCheckLimit) {
            if (state == null && input.isValidImplicitNullCheckFor(value, implicitNullCheckLimit)) {
                state = nullCheckState;
                return true;
            }
            return false;
        }
    }
}
