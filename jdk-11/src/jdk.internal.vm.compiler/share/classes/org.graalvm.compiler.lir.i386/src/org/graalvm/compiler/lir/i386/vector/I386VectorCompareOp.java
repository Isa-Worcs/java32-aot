/*
 * Copyright (c) 2013, 2018, Oracle and/or its affiliates. All rights reserved.
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
package org.graalvm.compiler.lir.i386.vector;

import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.REG;
import static org.graalvm.compiler.lir.LIRInstruction.OperandFlag.STACK;
import static jdk.vm.ci.code.ValueUtil.asRegister;
import static jdk.vm.ci.code.ValueUtil.isRegister;
import jdk.vm.ci.meta.AllocatableValue;

import org.graalvm.compiler.asm.i386.I386Address;
import org.graalvm.compiler.asm.i386.I386VectorAssembler;
import org.graalvm.compiler.asm.i386.I386VectorAssembler.VexRMOp;
import org.graalvm.compiler.lir.LIRInstructionClass;
import org.graalvm.compiler.lir.Opcode;
import org.graalvm.compiler.lir.asm.CompilationResultBuilder;
import org.graalvm.compiler.asm.i386.AVXKind.AVXSize;

public final class I386VectorCompareOp extends I386VectorLIRInstruction {
    public static final LIRInstructionClass<I386VectorCompareOp> TYPE = LIRInstructionClass.create(I386VectorCompareOp.class);

    @Opcode private final VexRMOp opcode;
    @Use({REG}) protected AllocatableValue x;
    @Use({REG, STACK}) protected AllocatableValue y;

    public I386VectorCompareOp(VexRMOp opcode, AllocatableValue x, AllocatableValue y) {
        super(TYPE);
        this.opcode = opcode;
        this.x = x;
        this.y = y;
    }

    @Override
    public void emitCode(CompilationResultBuilder crb, I386VectorAssembler vasm) {
        if (isRegister(y)) {
            opcode.emit(vasm, AVXSize.XMM, asRegister(x), asRegister(y));
        } else {
            opcode.emit(vasm, AVXSize.XMM, asRegister(x), (I386Address) crb.asAddress(y));
        }
    }
}
