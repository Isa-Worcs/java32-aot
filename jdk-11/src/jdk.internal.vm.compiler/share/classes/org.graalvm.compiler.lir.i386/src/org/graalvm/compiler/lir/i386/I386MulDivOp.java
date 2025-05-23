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

import static org.graalvm.compiler.asm.i386.I386Assembler.I386MOp.DIV;
import static org.graalvm.compiler.asm.i386.I386Assembler.I386MOp.IDIV;
import static org.graalvm.compiler.asm.i386.I386Assembler.I386MOp.IMUL;
import static org.graalvm.compiler.asm.i386.I386Assembler.I386MOp.MUL;
import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.ILLEGAL;
import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.REG;
import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.STACK;
import static jdk.vm.ci.code.ValueUtil.asRegister;
import static jdk.vm.ci.code.ValueUtil.isIllegal;
import static jdk.vm.ci.code.ValueUtil.isRegister;
import static jdk.vm.ci.code.ValueUtil.isStackSlot;

import org.graalvm.compiler.asm.i386.I386Address;
import org.graalvm.compiler.asm.i386.I386Assembler.I386MOp;
import org.graalvm.compiler.asm.i386.I386Assembler.OperandSize;
import org.graalvm.compiler.asm.i386.I386MacroAssembler;
import org.graalvm.compiler.core.common.LIRKind;
import org.graalvm.compiler.lir.LIRFrameState;
import org.graalvm.compiler.lir.LIRInstructionClass;
import org.graalvm.compiler.lir.Opcode;
import org.graalvm.compiler.lir.asm.CompilationResultBuilder;

import jdk.vm.ci.i386.I386;
import jdk.vm.ci.meta.AllocatableValue;
import jdk.vm.ci.meta.Value;

/**
 * I386 mul/div operation. This operation has a single operand for the second input. The first
 * input must be in RAX for mul and in RDX:RAX for div. The result is in RDX:RAX.
 */
public class I386MulDivOp extends I386LIRInstruction {
    public static final LIRInstructionClass<I386MulDivOp> TYPE = LIRInstructionClass.create(I386MulDivOp.class);

    @Opcode private final I386MOp opcode;
    private final OperandSize size;

    @Def({REG}) protected AllocatableValue highResult;
    @Def({REG}) protected AllocatableValue lowResult;

    @Use({REG, ILLEGAL}) protected AllocatableValue highX;
    @Use({REG}) protected AllocatableValue lowX;

    @Use({REG, STACK}) protected AllocatableValue y;

    @State protected LIRFrameState state;

    public I386MulDivOp(I386MOp opcode, OperandSize size, LIRKind resultKind, AllocatableValue x, AllocatableValue y) {
        this(opcode, size, resultKind, Value.ILLEGAL, x, y, null);
    }

    public I386MulDivOp(I386MOp opcode, OperandSize size, LIRKind resultKind, AllocatableValue highX, AllocatableValue lowX, AllocatableValue y, LIRFrameState state) {
        super(TYPE);
        this.opcode = opcode;
        this.size = size;

        this.highResult = I386.rdx.asValue(resultKind);
        this.lowResult = I386.rax.asValue(resultKind);

        this.highX = highX;
        this.lowX = lowX;

        this.y = y;

        this.state = state;
    }

    public AllocatableValue getHighResult() {
        return highResult;
    }

    public AllocatableValue getLowResult() {
        return lowResult;
    }

    public AllocatableValue getQuotient() {
        return lowResult;
    }

    public AllocatableValue getRemainder() {
        return highResult;
    }

    @Override
    public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
        if (state != null) {
            crb.recordImplicitException(masm.position(), state);
        }
        if (isRegister(y)) {
            opcode.emit(masm, size, asRegister(y));
        } else {
            assert isStackSlot(y);
            opcode.emit(masm, size, (I386Address) crb.asAddress(y));
        }
    }

    @Override
    public void verify() {
        assert asRegister(highResult).equals(I386.rdx);
        assert asRegister(lowResult).equals(I386.rax);

        assert asRegister(lowX).equals(I386.rax);
        if (opcode == DIV || opcode == IDIV) {
            assert asRegister(highX).equals(I386.rdx);
        } else if (opcode == MUL || opcode == IMUL) {
            assert isIllegal(highX);
        }
    }
}
