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

import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.HINT;
import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.REG;
import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.STACK;
import static jdk.vm.ci.code.ValueUtil.asRegister;

import org.graalvm.compiler.asm.i386.I386Assembler.I386MOp;
import org.graalvm.compiler.asm.i386.I386Assembler.OperandSize;
import org.graalvm.compiler.asm.i386.I386MacroAssembler;
import org.graalvm.compiler.lir.LIRInstructionClass;
import org.graalvm.compiler.lir.Opcode;
import org.graalvm.compiler.lir.asm.CompilationResultBuilder;

import jdk.vm.ci.i386.I386;
import jdk.vm.ci.meta.AllocatableValue;

/**
 * I386 shift/rotate operation. This operation has a single operand for the first input and output.
 * The second input must be in the RCX register.
 */
public class I386ShiftOp extends I386LIRInstruction {
    public static final LIRInstructionClass<I386ShiftOp> TYPE = LIRInstructionClass.create(I386ShiftOp.class);

    @Opcode private final I386MOp opcode;
    private final OperandSize size;

    @Def({REG, HINT}) protected AllocatableValue result;
    @Use({REG, STACK}) protected AllocatableValue x;
    @Alive({REG}) protected AllocatableValue y;

    public I386ShiftOp(I386MOp opcode, OperandSize size, AllocatableValue result, AllocatableValue x, AllocatableValue y) {
        super(TYPE);
        this.opcode = opcode;
        this.size = size;

        this.result = result;
        this.x = x;
        this.y = y;
    }

    @Override
    public void emitCode(CompilationResultBuilder crb, I386MacroAssembler masm) {
        I386Move.move(crb, masm, result, x);
        opcode.emit(masm, size, asRegister(result));
    }

    @Override
    public void verify() {
        assert asRegister(y).equals(I386.rcx);
    }
}
